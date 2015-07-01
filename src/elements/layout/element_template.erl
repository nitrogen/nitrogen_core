% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2013 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_template).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

%% Check the last modified time for templates every one second.
-define(RECACHE_CHECK_INTERVAL, 1000000).

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

    File = wf:to_list(Record#template.file),
    Template = get_cached_template(File),

    % Let's figure out the appropriate module aliases
    ModuleAliases = get_module_aliases(Record),

    % Evaluate the template.

    %% erl_eval:exprs/2 expects Bindings to be an orddict, but Nitrogen 
    %% does not have this requirement, so let's fix that.
    %% create the needed Ord-Dict just once instead for every eval call down the chain
    OrdDictBindings = orddict:from_list(Record#template.bindings),
    Fixed_bindings_record = Record#template{bindings=OrdDictBindings},
    Body = eval(Template, Fixed_bindings_record, ModuleAliases),
    Body.

get_cached_template(File) ->
    FileAtom = list_to_atom("template_file_" ++ File),
   
    case is_time_to_recache(File, FileAtom) of
        true ->
            wf:info("Recaching Template~n"),
            %% Recache the template...
            Template = parse_template(File),
            simple_cache:set(nitrogen, infinity, {template, FileAtom}, Template),
            simple_cache:set(nitrogen, infinity, {template_loaded, FileAtom}, true),
            Template;
        false ->
            %% Load template from cache...
            simple_cache:get(nitrogen, infinity, {template, FileAtom}, fun() ->
                %% Load the mochiglobal value for easier migration of a live system.
                %% In a few versions, we can just get rid of nitro_mochiglobal altogether.
                nitro_mochiglobal:get(FileAtom)
            end)
    end.

is_time_to_recache(File, FileAtom) ->
    IsLoaded = simple_cache:get(nitrogen, infinity, {template_loaded, FileAtom}, fun() -> false end),
    LastModified = simple_cache:get(nitrogen, 1000, {template_last_modified, FileAtom}, fun() ->
        filelib:last_modified(File)
    end),
    not(IsLoaded) orelse LastModified > {date(), time()}.
            
parse_template(File) ->
    % TODO - Templateroot
    % File1 = filename:join(nitrogen:get_templateroot(), File),
    File1 = File,
    case file:read_file(File1) of
        {ok, B} -> parse_template1(B);
        _ ->
            ?LOG("Error reading file: ~s~n", [File1]),
            throw({template_not_found, File1})
    end.

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


convert_callback_tuple_to_function(Module, Function, ArgString, Bindings, ModuleAliases) ->
    % De-reference to page module and custom module aliases...
    Module1 = get_module_from_alias(Module, ModuleAliases),
    _F = fun() ->
        % Convert args to term...
        Args = to_term("[" ++ ArgString ++ "].", Bindings),

        % If the function in exported, then call it.
        % Otherwise return undefined...
        {module, Module1} = wf_utils:ensure_loaded(Module1),
        case erlang:function_exported(Module1, Function, length(Args)) of
            true -> _Elements = erlang:apply(Module1, Function, Args);
            false -> undefined
        end
    end.

to_term(X, Bindings) ->
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

