% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

% Though this is defined as a handler, it is unlikely
% that anyone would want to override the default behaviour. 
% It is defined as a handler simply because it fit well 
% into the existing handler pattern.

-module (default_query_handler).
-behaviour (query_handler).
-include ("wf.hrl").
-export ([
    init/2, 
    finish/2,
    set_websocket_params/3,
    get_value/3,
    get_values/3,
    get_params/2
]).

-record(state, {request=[], websocket=[]}).

%% TODO: It's worth considering reworking this page to use simple_bridge for
%% all the query and post parameter evaluation, rather than reproducing a
%% sub-set of the functionality here. This, however, does more than the
%% typicaly query-string evaluation, as the normalized paths include all
%% possible element ids.  Something to consider.

init(_Config, _State) -> 
    % Get query params and post params
    % from the request bridge...
    RequestBridge = wf_context:bridge(),
    QueryParams = RequestBridge:query_params(),
    PostParams = RequestBridge:post_params(),

    % Load into state...
    Params = QueryParams ++ PostParams,

    % Pre-normalize the parameters.
    Params1 = normalize_params(Params),
    {ok, #state{request=Params1}}.

finish(_Config, _State) -> 
    % Clear out the state.
    {ok, []}.

set_websocket_params(Params, _Config, State) ->
    Params1 = normalize_params(Params),
    State1 = State#state{websocket=Params1},
    {ok, State1}.

%% Given a path, return the value that matches the path.
get_value(Path, Config, State) ->
    case get_values(Path, Config, State) of
        [] -> undefined;
        [One] -> 
            wf:to_unicode_list(One);
        _Many -> throw({?MODULE, too_many_matches, Path})
    end.

get_values(Path, _Config, #state{request=Request, websocket=Websocket} = _State) ->
    Path1 = normalize_path(Path),
    Vs = refine_params(Path1, Websocket ++ Request),
    Vs.

get_params(_Config, #state{request=Request, websocket=Websocket} = _State) ->
    Params = Websocket ++ Request,
    F = fun({KeyPartsReversed, Value}) ->
        KeyParts = lists:reverse(KeyPartsReversed),
        Key = string:join(KeyParts, "."),
        { Key, wf:to_unicode_list(Value) }
    end,
    lists:map(F, Params).

%% Next, narrow down the parameters by keeping only the parameters
%% that contain the next element found in path, while shrinking the 
%% parameter paths at the same time.
%% For example, if:
%% 	Path   = [a, b, c] 
%% 	Params = [{[x, a, y, b, c], _}] 
%% Then after the first round of refine_params/2 we would have:
%%   Path   = [b, c]
%%   Params = [y, b, c]
-spec refine_params(NormalizedPath :: list(), Params :: list()) -> Values :: list().
refine_params([], Params) -> 
    [wf:to_unicode_list(V) || {_, V} <- Params];
refine_params([H|T], Params) ->
    F = fun({Path, Value}, Acc) ->
        case split_on(H, Path) of
            {ok, RemainingPath} -> [{RemainingPath, Value}|Acc];
            false -> Acc
        end
    end,
    Params1 = lists:foldl(F, [], Params),
    refine_params(T, lists:reverse(Params1)).

split_on(_,  []) -> false;
split_on(El, [El|T]) -> {ok, T};
split_on(El, [_|T]) -> split_on(El, T).

normalize_path(Path) when is_binary(Path) ->
    normalize_path(binary_to_list(Path));
normalize_path(Path) when is_atom(Path) ->
    normalize_path(atom_to_list(Path));
normalize_path(Path) when ?IS_STRING(Path) ->
    Tokens = string:tokens(Path, "."),
    Tokens1 = [strip_array_brackets(strip_wfid(X)) || X <- Tokens],
    lists:reverse(Tokens1).

normalize_params(Params) ->
    %% In typical erlang fashion, this list is being built in reverse, and will
    %% need to be reversed when finished to ensure proper parameter order
    BackwardNormalizedParams = lists:foldl(fun(Param, Acc) ->
        normalize_param(Param) ++ Acc
    end, [], Params),
    lists:reverse(BackwardNormalizedParams).
    
normalize_param({undefined, _}) ->
    [];
normalize_param({[], _}) ->
    [];
normalize_param({<<>>, _}) ->
    [];
normalize_param({Path, Value}) when ?IS_STRING(Value);
                                    Value =:= [];
                                    is_binary(Value);
                                    is_integer(Value);
                                    is_atom(Value) ->
    [{normalize_path(Path), Value}];
normalize_param({Path, Values}) when is_list(Values) ->
    NPath = normalize_path(Path),
    %% Because this is running in the middle of a list that's being built in
    %% reverse, this will have to be built in reverse also.
    lists:reverse([{NPath, V} || V <- Values]).

%% Most tokens will start with "wfid_". Strip this out.
strip_wfid(Path) ->
    case Path of 
        "wfid_" ++ S -> S;
        S -> S
    end.

%% For multiselect elements, jquery appends [] to the element name if element's
%% data is an array We strip it out, since Nitrogen doesn't care if it's an
%% array or not, it just uses each key individually
strip_array_brackets(Path) ->
    case lists:reverse(Path) of
        [ $], $[ | Rest ] -> lists:reverse(Rest);
        _ -> Path
    end.

