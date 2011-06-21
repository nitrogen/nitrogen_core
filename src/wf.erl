% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf).
-include_lib ("wf.hrl").
-compile (export_all).

%%% EXPOSE WIRE, UPDATE, FLASH %%%
wire(Actions) -> 
    ok = wire(undefined, undefined, Actions).

wire(Target, Actions) -> 
    ok = wire(Target, Target, Actions).

wire(Trigger, Target, Actions) -> 
    ok = action_wire:wire(Trigger, Target, Actions).

update(Target, Elements) -> 
    ok = action_update:update(Target, Elements).

replace(Target, Elements) ->
    ok = action_update:replace(Target, Elements).

insert_top(Target, Elements) -> 
    ok = action_update:insert_top(Target, Elements).

insert_bottom(Target, Elements) -> 
    ok = action_update:insert_bottom(Target, Elements).

insert_before(Target, Elements) ->
    ok = action_update:insert_before(Target, Elements).

insert_after(Target, Elements) ->
    ok = action_update:insert_after(Target, Elements).

remove(Target) ->
    ok = action_update:remove(Target).

flash(Elements) ->
    element_flash:add_flash(Elements).

flash(FlashID, Elements) ->
    element_flash:add_flash(FlashID, Elements).

%%% EXPOSE WF_UTILS %%%

f(S) -> 
    _String = wf_utils:f(S).

f(S, Args) -> 
    _String = wf_utils:f(S, Args).

coalesce(L) -> 
    _Value = wf_utils:coalesce(L).

%%% WF_REDIRECT %%%
redirect(Url) -> 
    action_redirect:redirect(Url).

redirect_to_login(LoginUrl) -> 
    action_redirect:redirect_to_login(LoginUrl).

redirect_from_login(DefaultUrl) -> 
    action_redirect:redirect_from_login(DefaultUrl).


%%% EXPOSE WF_PICKLE %%%
pickle(Data) -> 
    _SerializedData = wf_pickle:pickle(Data).

depickle(SerializedData) -> 
    _Data = wf_pickle:depickle(SerializedData).

depickle(SerializedData, TTLSeconds) ->
    _Data = wf_pickle:depickle(SerializedData, TTLSeconds).


%%% EXPOSE WF_CONVERT %%%
to_list(T) -> 
    _String = wf_convert:to_list(T).

to_atom(T) -> 
    _Atom = wf_convert:to_atom(T).

to_binary(T) -> 
    _Binary = wf_convert:to_binary(T).

to_integer(T) -> 
    _Integer = wf_convert:to_integer(T).

to_string_list(Term) ->
    _StringList = wf_convert:to_string_list(Term).

clean_lower(S) -> 
    _String = wf_convert:clean_lower(S).

html_encode(S) -> 
    _String = wf_convert:html_encode(S).

html_encode(S, Encode) -> 
    _String = wf_convert:html_encode(S, Encode).

url_encode(S) ->
    _String = wf_convert:url_encode(S).

url_decode(S) ->
    _String = wf_convert:url_decode(S).

hex_encode(S) ->
    _String = wf_convert:hex_encode(S).

hex_decode(S) ->
    _String = wf_convert:hex_decode(S).

js_escape(String) -> 
    _String = wf_convert:js_escape(String).


%%% EXPOSE WF_BIND %%%
% TODO
set(Element, Value) -> 
    ok = action_set:set(Element, Value).

% bind(BindingTuple, Record) -> wf_bind:bind(BindingTuple, Record).
% reverse_bind(BindingTuple) -> wf_bind:reverse_bind(BindingTuple).
% reverse_bind(BindingTuple, Record) -> wf_bind:reverse_bind(BindingTuple, Record).



%%% OTHER %%%
% TODO
logout() -> clear_user(), clear_roles(), clear_state(), clear_session().

to_js_id(Path) -> 
    _String = wf_render_actions:to_js_id(Path).

temp_id() -> 
    _String = wf_render_elements:temp_id().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% EXPOSE REQUEST INFORMATION %%%

page_module() -> 
    wf_context:page_module().

path_info() ->
    wf_context:path_info().

status_code() -> 
    ok = wf_context:status_code().

status_code(StatusCode) ->
    ok = wf_context:status_code(StatusCode).

content_type(ContentType) ->
    ok = wf_context:content_type(ContentType).

headers() -> 
    wf_context:headers().

header(Header) ->
    wf_context:header(Header).

header(Header, Value) ->
    ok = wf_context:header(Header, Value).

cookies() ->
    wf_context:cookies().

cookie(Cookie) ->
    wf_context:cookie(Cookie).

cookie_default(Cookie,DefaultValue) ->
	wf_context:cookie_default(Cookie,DefaultValue).

cookie(Cookie, Value) ->
    ok = wf_context:cookie(Cookie, Value).

cookie(Cookie, Value, Path, MinutesToLive) ->
    ok = wf_context:cookie(Cookie, Value, Path, MinutesToLive).

delete_cookie(Cookie) ->
	ok = wf_context:delete_cookie(Cookie).

socket() ->
    wf_context:socket().

peer_ip() ->
    wf_context:peer_ip().

peer_ip(Proxies) ->
    wf_context:peer_ip(Proxies).

peer_ip(Proxies,ForwardedHeader) ->
    wf_context:peer_ip(Proxies,ForwardedHeader).

%%% EXPOSE QUERY_HANDLER %%%
q(Key) -> 
    _String = query_handler:get_value(Key).

qs(Key) -> 
    query_handler:get_values(Key).

mq(KeyList) when is_list(KeyList) ->
    [q(X) || X<-KeyList].

mqs(KeyList) when is_list(KeyList) ->
    [qs(X) || X<-KeyList].




%%% EXPOSE LOG_HANDLER %%%
info(String, Args) -> 
    ok = log_handler:info(String, Args).

info(String) -> 
    ok = log_handler:info(String).

warning(String, Args) -> 
    ok = log_handler:warning(String, Args).

warning(String) -> 
    ok = log_handler:warning(String).

error(String, Args) -> 
    ok = log_handler:error(String, Args).

error(String) -> 
    ok = log_handler:error(String).



%%% EXPOSE SESSION_HANDLER %%% 
session(Key) -> 
    _Value = session_handler:get_value(Key).

session(Key, Value) -> 
    _Value = session_handler:set_value(Key, Value).

session_default(Key, DefaultValue) ->
    _Value = session_handler:get_value(Key, DefaultValue).

clear_session() -> 
    ok = session_handler:clear_all().



%%% EXPOSE IDENTITY_HANDLER %%%
user() -> 
    _User = identity_handler:get_user().	

user(User) -> 
    ok = identity_handler:set_user(User).

clear_user() -> 
    ok = identity_handler:clear().



%%% EXPOSE ROLE_HANDLER %%%
role(Role) -> 
    _Boolean = role_handler:get_has_role(Role).

role(Role, IsInRole) -> 
    ok = role_handler:set_has_role(Role, IsInRole).

roles() ->
    _Roles = role_handler:get_roles().

clear_roles() -> 
    ok = role_handler:clear_all().



%%% EXPOSE STATE_HANDLER %%%
state(Key) -> 
    _Value = state_handler:get_state(Key).

state_default(Key, DefaultValue) ->
    _Value = state_handler:get_state(Key, DefaultValue).

state(Key, Value) -> 
    ok = state_handler:set_state(Key, Value).

clear_state(Key) ->
    ok = state_handler:clear(Key).

clear_state() -> 
    ok = state_handler:clear_all().



%%% EXPOSE ACTION_COMET %%%

comet(Function) -> 
    action_comet:comet(Function).

comet(Function, Pool) -> 
    action_comet:comet(Function, Pool).

comet_global(Function, Pool) ->  
    action_comet:comet_global(Function, Pool).

send(Pool, Message) ->
    ok = action_comet:send(Pool, Message).

send_global(Pool, Message) ->
    ok = action_comet:send_global(Pool, Message).

flush() ->
    ok = action_comet:flush().

async_mode() -> wf_context:async_mode().
async_mode(AsyncMode) -> wf_context:async_mode(AsyncMode).
switch_to_comet() -> async_mode(comet).
switch_to_polling(IntervalInMS) -> async_mode({poll, IntervalInMS}).

%%% EXPOSE ACTION_CONTINUE %%%

continue(Tag, Function) -> action_continue:continue(Tag, Function).

continue(Tag, Function, TimeoutMS) -> action_continue:continue(Tag, Function, TimeoutMS).


%%% CONFIGURATION %%%

config(Key) -> 
    config_handler:get_value(Key).

config_default(Key, DefaultValue) -> 
    config_handler:get_value(Key, DefaultValue).

%%% DEBUGGING %%%
debug() -> wf_utils:debug().
break() -> wf_utils:break().
assert(true, _) -> ok;
assert(false, Error) -> erlang:error(Error).
