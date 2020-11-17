% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2013 Rusty Klophaus
% Copyright (c) 2014-2020 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_template).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1,
    passthrough/1
]).

% TODO - Revisit parsing in the to_module_callback. This
% will currently fail if we encounter a string like:
% "String with ) will fail"
% or
% "String with ]]] will fail"


-spec reflect() -> [atom()].
reflect() -> record_info(fields, template).

-spec render_element(#template{}) -> body().
render_element(Record) ->
    % Parse the template file...

    File = wf:to_binary(Record#template.file),
    Template = get_cached_template(File, Record),

    % Let's figure out the appropriate module aliases
    ModuleAliases = get_module_aliases(Record),

    % Evaluate the template.

    %% erl_eval:exprs/2 expects Bindings to be an orddict, but Nitrogen does
    %% not have this requirement, so let's fix that.  create the needed
    %% Ord-Dict just once instead for every eval call down the chain
    OrdDictBindings = orddict:from_list(Record#template.bindings),
    Fixed_bindings_record = Record#template{bindings=OrdDictBindings},
    Body = eval(Template, Fixed_bindings_record, ModuleAliases),
    Body.

get_cached_template(File0, #template{from_type=FromType, to_type=ToType, options=Options, callouts=Callouts}) ->
    File = wf:to_binary(File0),
    FileKey = {File, FromType, ToType, Options},
   
    case is_time_to_recache(File, FileKey) of
        true ->
            wf:info("Recaching Template: ~s",[File]),
            %% Recache the template...
            Template = parse_template(File, FromType, ToType, Callouts, Options),
            wf:set_cache({tempate_last_recached, FileKey}, {date(), time()}),
            wf:set_cache({template, FileKey}, Template),
            Template;
        false ->
            wf:cache({template, FileKey}, fun() ->
                parse_template(File, FromType, ToType, Callouts, Options)
            end)
    end.

is_time_to_recache(File, FileKey) ->
    %% First we check the last time the template was recached/recompiled. This
    %% will be used to compare against the time the file was updated on the
    %% filesystem. When it's first loaded, it'll be recorded as a "Never" tuple
    %% ({0,0,0}, {0,0,0}}) to ensure that all future dates tuples are greater
    %% than it.
    Never = fun() -> {{0,0,0}, {0,0,0}} end,
    LastRecached = wf:cache({tempate_last_recached, FileKey}, infinity, Never),

    %% Now we load the file's last modified time from the filesystem, and cache
    %% that result for one second. That way we're not hammering the filesystem
    %% over and over for the same file.
    GetLastModified = fun() -> filelib:last_modified(File) end,
    LastModified = wf:cache({template_last_modified, FileKey}, 1000, GetLastModified),

    %% Finally if the file's last modification date is after the last time it
    %% was recached, we need to recache it.
    ?WF_IF(LastModified==0,wf:warning("File appears to be deleted or has no modified time: ~s",[File])),
    LastModified > LastRecached.
            
parse_template(File, FromType, ToType, Callouts, []) when FromType=:=ToType ->
    % TODO - Templateroot
    % File1 = filename:join(nitrogen:get_templateroot(), File),
    File1 = File,
    case file:read_file(File1) of
        {ok, B} when Callouts==true -> parse_template1(B);
        {ok, B} -> B;
        _ ->
            ?LOG("Error reading file: ~s~n", [File1]),
            throw({template_not_found, File1})
    end;
parse_template(File, FromType, ToType, Callouts, Options) ->
    File1 = File,
    case file:read_file(File1) of
        {ok, Bin} when Callouts==false ->
            wf_pandoc:convert(Bin, [{from, FromType}, {to, ToType} | Options]);
        {ok, Bin} ->
            {Bin2, CalloutMap} = remove_callouts(Bin),
            Bin3 = wf_pandoc:convert(Bin2, [{from, FromType}, {to, ToType} | Options]),
            Bin4 = add_callouts(CalloutMap, Bin3),
            parse_template1(Bin4);
        _ ->
            ?LOG("Error readibng file: ~s~n",[File1]),
            throw({template_not_found, File1})
    end.


remove_callouts(Bin) ->
    case re:run(Bin, "\\[\\[\\[.*?\\]\\]\\]", [{capture, first, binary}]) of
        nomatch -> {Bin, []};
        {match, [Matches]} ->
            lists:foldl(fun(Pattern, {AccBin, AccMap}) ->
                Suffix = wf:to_binary(?WF_RAND_UNIFORM(10000000000000, 9999999999999999)),
                Key = <<"wf_callout",Suffix/binary>>,
                NewBin = binary:replace(AccBin, Pattern, Key),
                NewMap = [{Key, Pattern} | AccMap],
                {NewBin, NewMap}
            end, Bin, Matches)
    end.

add_callouts(CalloutMap, Bin) ->
    lists:foldl(fun({Key, Callout}, AccBin) ->
        binary:replace(AccBin, Key, Callout)
    end, Bin, CalloutMap).

parse_template1(B) ->
    F = fun(Tag) ->
        try
            Tag1 = string:strip(wf:to_list(Tag)),
            to_module_callback(Tag1)
        catch _ : _ ->
            ?LOG("Invalid template tag: ~s~n", [Tag])
        end
    end,
    parse(B, F).


%%% PARSE %%%

%% parse/2 - Given a binary and a callback, look through the binary
%% for strings of the form [[[module]]] or [[[module:function(args)]]]
parse(B, Callback) ->
    parse(B, Callback, <<>>).

parse(<<>>, _Callback, Acc) -> 
    [Acc];
parse(<<"[[[", Rest/binary>>, Callback, Acc) ->
    { Token, Rest1 } = get_token(Rest, <<>>),
    [Acc, Callback(Token) | parse(Rest1, Callback, <<>>)];
parse(<<C, Rest/binary>>, Callback, Acc) -> 
    parse(Rest, Callback, <<Acc/binary,C>>).

get_token(<<"]]]", Rest/binary>>, Acc) -> { Acc, Rest };
get_token(<<H, Rest/binary>>, Acc) -> get_token(Rest, <<Acc/binary, H>>).

to_module_callback("mobile_script") -> mobile_script;
to_module_callback("script") -> script;
to_module_callback(Tag) ->
    % Get the module...
    {ModuleString, Rest1} = peel(Tag, $:),
    Module = wf:to_atom(string:strip(ModuleString)),

    % Get the function...
    {FunctionString, Rest2} = peel(Rest1, $(),
    Function = wf:to_atom(string:strip(FunctionString)),

    {ArgString, Rest3} = peel(Rest2, $)),

    case Rest3 of
        [] -> [{Module, Function, ArgString}];
        _ ->  [{Module, Function, ArgString}|to_module_callback(tl(Rest3))]
    end.

peel(S, Delim) -> peel(S, Delim, []).
peel([], _Delim, Acc) -> {lists:reverse(Acc), []};
peel([Delim|T], Delim, Acc) -> {lists:reverse(Acc), T};
peel([H|T], Delim, Acc) -> peel(T, Delim, [H|Acc]).


%%% EVALUATE %%%

eval(Bin, _, _) when is_binary(Bin) -> Bin;
eval([], _, _) -> [];
eval([H|T], Record, ModuleAliases) when H==script;
                                        H==mobile_script;
                                        is_binary(H) ->
    [H|eval(T, Record, ModuleAliases)];
eval([H|T], Record, ModuleAliases) ->
    [replace_callbacks(H, Record, ModuleAliases) | eval(T, Record, ModuleAliases)].

% Turn callbacks into a reference to #function_el {}.
replace_callbacks(CallbackTuples, Record, ModuleAliases) ->
    Bindings = Record#template.bindings,
    Functions = [convert_callback_tuple_to_function(M, F, ArgString, Bindings, ModuleAliases) || {M, F, ArgString} <- CallbackTuples],
    #function_el { anchor=page, function=Functions }.

convert_callback_tuple_to_function(Module, _Function='', _ArgString=[], Bindings, ModuleAliases) ->
    %% This is a special condition, where the callout is just [[[Variable]]].
    %% The parser extracted the Module as the only term, and the rest was
    %% ignored, so treat it as a simple Binding binding Lookup:
    convert_callback_tuple_to_function('element_template', 'passthrough', wf:to_list(Module), Bindings, ModuleAliases);
    
convert_callback_tuple_to_function(Module, Function, ArgString, Bindings, ModuleAliases) ->
    % De-reference to page module and custom module aliases...
    Module1 = get_module_from_alias(Module, ModuleAliases),
    _F = fun() ->
        % Convert args to term...
        Args = to_term(ArgString, Bindings),

        % If the function in exported, then call it.
        % Otherwise return undefined...
        case wf_utils:ensure_loaded(Module1) of
            {error, nofile} ->
                throw({unable_to_load_module, [
                    {original_module, Module},
                    {actual_module, Module1},
                    {function, Function},
                    {arg_string, ArgString}
                ]});
            {module, Module1} ->
                case erlang:function_exported(Module1, Function, length(Args)) of
                    true -> _Elements = erlang:apply(Module1, Function, Args);
                    false -> undefined
                end
        end
    end.

passthrough(Var) ->
    Var.

to_term([], _) ->
    [];
to_term(ArgString, Bindings) ->
    X = "[" ++ ArgString ++ "].",
    S = wf:to_list(X),
    {ok, Tokens, 1} = erl_scan:string(S),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
    Value.



get_module_aliases(Record) ->
    lists:append([
        Record#template.module_aliases,
        wf:config_default(module_aliases, []),
        [{page, wf_context:page_module()}]
    ]).

get_module_from_alias(Module, ModuleAliases) ->
    case lists:keyfind(Module, 1, ModuleAliases) of
        {Module, AliasedModule} ->
            AliasedModule;
        false ->
            Module
    end.

