% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (role_handler).
-include("wf.hrl").
-export ([
    get_has_role/1,
    set_has_role/2,
    get_roles/0,
    clear_all/0
]).


-callback init(         handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback finish(       handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback get_has_role( Role :: term(),
                        handler_config(),
                        handler_state()) -> IsInRole :: boolean().
-callback set_has_role( Role :: term(),
                        IsInRole :: boolean(),
                        handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback get_roles(    handler_config(),
                        handler_state()) -> [Role :: term()].

-callback clear_all(    handler_config(),
                        handler_state()) -> {ok, handler_state()}.

% get_has_role(Role, State) -> {ok, IsInRole, NewState}.
% Returns true or false depending on whether the user is in the specified role.
get_has_role(Role) ->
    _Boolean = wf_handler:call_readonly(role_handler, get_has_role, [Role]).

% set_has_role(Role, IsInRole, State) -> {ok, NewState}.
% Set whether the user is in the specified role.
set_has_role(Role, IsInRole) ->
    wf_handler:call(role_handler, set_has_role, [Role, IsInRole]).

% roles(State) -> {ok, [Roles], NewState}
% Return a list of roles held by the current user
get_roles() ->
    _Roles = wf_handler:call_readonly(role_handler, get_roles).

% clear_all(State) -> {ok, NewState}.
% Clear all roles.
clear_all() ->
    wf_handler:call(role_handler, clear_all).
