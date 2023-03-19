% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2021 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (canister_session_handler).
-include("wf.hrl").
-behaviour (session_handler).
-export ([
    init/2, 
    finish/2,
    get_value/4, 
    set_value/4, 
    clear_all/2,
    session_id/2
]).
-record (state, {id}).

init(_Config, _State) -> 
    % Get the session cookie ...
    ?PRINT(get_cookie),
    Cookie = wf:cookie(get_cookie_name()),
    ?PRINT(depickle),
    State = case wf:depickle(Cookie) of
        undefined -> new_state();
        Other=#state{} -> Other;
        _ -> new_state()
    end,
    {ok, State}.

finish(_Config, State) -> 
    % Drop the session cookie...
    Timeout = wf:config_default(session_timeout, 20),
    SameSite = wf:config_default(session_cookie_same_site, lax),
    Secure = wf:config_default(session_cookie_secure, false),
    Opts = [
        {path, "/"},
        {minutes_to_live, Timeout},
        {http_only, true},
        {same_site, SameSite},
        {secure, Secure}
    ],
    ok = wf:cookie(get_cookie_name(), wf:pickle(State), Opts),
    {ok, []}.
    
get_value(Key, DefaultValue, Config, State) -> 
    ID = get_session_id(Config, State),
    Value = wf:coalesce([canister:get(ID, Key), DefaultValue]),
    {ok, Value, State}.

set_value(Key, Value, Config, State) -> 
    ID = get_session_id(Config, State),
    OldValue = canister:put(ID, Key, Value),
    {ok, OldValue, State}.

clear_all(Config, State) -> 
    ID = get_session_id(Config, State),
    canister:clear(ID),
    {ok, State}.

%% This is the external session-id
session_id(_Config, State) ->
    {ok, SessionId} = wf:hex_encode (State#state.id),
    {ok, SessionId, State}.

%%% PRIVATE FUNCTIONS

get_cookie_name() ->
    ?PRINT(getting_cookie),
    wf:config_default(cookie_name, "newcookie").

get_session_id(_Config, #state{id=ID}) ->
    ID.

new_state() ->
    ID = crypto:strong_rand_bytes(32),
    #state{id=ID}.
