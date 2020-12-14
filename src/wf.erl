% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf).
-include("wf.hrl").
-compile (export_all).

%%% EXPOSE WIRE, UPDATE, FLASH %%%
wire(Actions) ->
    ok = wire(undefined, undefined, Actions).

wire(Target, Actions) ->
    ok = wire(Target, Target, Actions).

wire(Trigger, Target, Actions) ->
    ok = action_wire:wire(Trigger, Target, Actions).

defer(Actions) ->
    ok = defer(undefined, undefined, Actions).

defer(Target, Actions) ->
    ok = defer(Target, Target, Actions).

defer(Trigger, Target, Actions) ->
    ok = action_wire:defer(Trigger, Target, Actions).

eager(Actions) ->
    ok = eager(undefined, undefined, Actions).

eager(Target, Actions) ->
    ok = eager(Target, Target, Actions).

eager(Trigger, Target, Actions) ->
    ok = action_wire:eager(Trigger, Target, Actions).

priority_wire(Priority, Actions) ->
    ok = priority_wire(Priority, undefined, undefined, Actions).

priority_wire(Priority, Target, Actions) ->
    ok = priority_wire(Priority, Target, Target, Actions).

priority_wire(eager, Trigger, Target, Actions) ->
    ok = wf:eager(Trigger, Target, Actions);

priority_wire(normal, Trigger, Target, Actions) ->
    ok = wf:wire(Trigger, Target, Actions);

priority_wire(defer, Trigger, Target, Actions) ->
    ok = wf:defer(Trigger, Target, Actions).

set(Element, Value) ->
    ok = set(normal, Element, Value).

set(Priority, Element, Value) when ?IS_ACTION_PRIORITY(Priority) ->
    ok = action_set:set(Priority, Element, Value).

set_multiple(Element, Values) ->
    ok = set_multiple(normal, Element, Values).

set_multiple(Priority, Element, Values) when is_list(Values), ?IS_ACTION_PRIORITY(Priority) ->
    ok = action_set_multiple:set(Priority, Element, Values).


update(Target, Elements) ->
    ok = update(normal, Target, Elements).

update(Priority, Target, Elements) when ?IS_ACTION_PRIORITY(Priority) ->
    ok = action_update:update(Priority, Target, Elements).

replace(Target, Elements) ->
    ok = replace(normal, Target, Elements).

replace(Priority, Target, Elements) when ?IS_ACTION_PRIORITY(Priority) ->
    ok = action_update:replace(Priority, Target, Elements).

insert_top(Target, Elements) ->
    ok = insert_top(normal, Target, Elements).

insert_top(Priority, Target, Elements) when ?IS_ACTION_PRIORITY(Priority) ->
    ok = action_update:insert_top(Priority, Target, Elements).

insert_bottom(Target, Elements) ->
    ok = insert_bottom(normal, Target, Elements).

insert_bottom(Priority, Target, Elements) when ?IS_ACTION_PRIORITY(Priority) ->
    ok = action_update:insert_bottom(Priority, Target, Elements).

insert_before(Target, Elements) ->
    ok = insert_before(normal, Target, Elements).

insert_before(Priority, Target, Elements) when ?IS_ACTION_PRIORITY(Priority) ->
    ok = action_update:insert_before(Priority, Target, Elements).

insert_after(Target, Elements) ->
    ok = insert_after(normal, Target, Elements).

insert_after(Priority, Target, Elements) when ?IS_ACTION_PRIORITY(Priority) ->
    ok = action_update:insert_after(Priority, Target, Elements).

remove(Target) ->
    ok = remove(normal, Target).

remove(Priority, Target) when ?IS_ACTION_PRIORITY(Priority) ->
    ok = action_update:remove(Priority, Target).

disable(Target) ->
    ok = disable(normal, Target).

disable(Priority, Target) when ?IS_ACTION_PRIORITY(Priority) ->
    ok = action_disable:disable(Priority, Target).

enable(Target) ->
    ok = enable(normal, Target).

enable(Priority, Target) when ?IS_ACTION_PRIORITY(Priority) ->
    ok = action_enable:enable(Priority, Target).

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

redirect_to_login(LoginUrl, PostLoginUrl) ->
    action_redirect:redirect_to_login(LoginUrl, PostLoginUrl).

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

to_unicode_list(T) ->
    _String = wf_convert:to_unicode_list(T).

to_atom(T) ->
    _Atom = wf_convert:to_atom(T).

to_existing_atom(T) ->
    _Atom = wf_convert:to_existing_atom(T).

to_binary(T) ->
    _Binary = wf_convert:to_binary(T).

to_unicode_binary(T) ->
    _Binary = wf_convert:to_unicode_binary(T).

to_integer(T) ->
    _Integer = wf_convert:to_integer(T).

to_float(T) ->
    _Float = wf_convert:to_float(T).

to_string_list(Term) ->
    _StringList = wf_convert:to_string_list(Term).

clean_lower(S) ->
    _String = wf_convert:clean_lower(S).

html_encode(S) ->
    _String = wf_convert:html_encode(S).

html_encode(S, Encode) ->
    _String = wf_convert:html_encode(S, Encode).

html_decode(S) ->
    _String = wf_convert:html_decode(S).

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

json_encode(Data) ->
    _String = wf_convert:json_encode(Data).

json_decode(Json) ->
    _Data = wf_convert:json_decode(Json).

to_qs(List) ->
    _Iolist = wf_convert:to_qs(List).

parse_qs(String) ->
    _Proplist = wf_convert:parse_qs(String).

join(List,Delimiter) ->
    _Result = wf_convert:join(List,Delimiter).

%%% EXPOSE WF_BIND %%%
% TODO

% bind(BindingTuple, Record) -> wf_bind:bind(BindingTuple, Record).
% reverse_bind(BindingTuple) -> wf_bind:reverse_bind(BindingTuple).
% reverse_bind(BindingTuple, Record) -> wf_bind:reverse_bind(BindingTuple, Record).

%%% OTHER %%%

logout() -> clear_user(), clear_roles(), clear_state(), clear_session().

to_js_id(Path) ->
    _String = wf_render_actions:to_js_id(Path).

temp_id() ->
    _String = wf_render_elements:temp_id().

normalize_id(Path) ->
    _String = wf_render_elements:normalize_id(Path).

render_isolated(Elements) ->
    {ok, _Html, _Actions} = wf_render_elements:render_and_trap_actions(Elements).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% EXPOSE REQUEST INFORMATION %%%

in_request() ->
    wf_context:in_request().

page_module() ->
    wf_context:page_module().

page_module(Mod) ->
    wf_context:page_module(Mod).

path() ->
    wf_context:path().

path_info() ->
    wf_context:path_info().

uri() ->
    wf_context:uri().

url() ->
    wf_context:url().

status_code() ->
    ok = wf_context:status_code().

status_code(StatusCode) ->
    ok = wf_context:status_code(StatusCode).

content_type(ContentType) ->
    ok = wf_context:content_type(ContentType).

encoding() ->
    wf_context:encoding().

encoding(Encoding) ->
    wf_context:encoding(Encoding).

download_as(Filename) ->
    wf_context:download_as(Filename).

headers() ->
    wf_context:headers().

header(Header) ->
    wf_context:header(Header).

header(Header, Value) ->
    ok = wf_context:header(Header, Value).

cookies() ->
    wf_cookies:cookies().

cookie(Cookie) ->
    wf_cookies:get_cookie(Cookie).

cookie_default(Cookie,DefaultValue) ->
    wf_cookies:get_cookie(Cookie,DefaultValue).

cookie(Cookie, Value) ->
    ok = wf_cookies:set_cookie(Cookie, Value).

cookie(Cookie, Value, Options) ->
    ok = wf_cookies:set_cookie(Cookie, Value, Options).

script_nonce() ->
  wf_context:script_nonce().

script_nonce(Value) ->
  wf_context:script_nonce(Value).

%% Deprecated
cookie(Cookie, Value, Path, MinutesToLive) ->
    ok = wf_cookies:set_cookie(Cookie, Value, Path, MinutesToLive).

delete_cookie(Cookie) ->
    ok = wf_cookies:delete_cookie(Cookie).

socket() ->
    wf_context:socket().

peer_ip() ->
    wf_context:peer_ip().

peer_ip(Proxies) ->
    wf_context:peer_ip(Proxies).

peer_ip(Proxies,ForwardedHeader) ->
    wf_context:peer_ip(Proxies,ForwardedHeader).

request_method() ->
    wf_context:request_method().

request_body() ->
    wf_context:request_body().

%%% EXPOSE QUERY_HANDLER %%%
q(Key) ->
    _String = query_handler:get_value(Key).

qs(Key) ->
    query_handler:get_values(Key).

mq(KeyList) when is_list(KeyList) ->
    [q(X) || X<-KeyList].

mqs(KeyList) when is_list(KeyList) ->
    [qs(X) || X<-KeyList].

%% Returns a proplist formed from the list of Keys
q_pl(KeyList) when is_list(KeyList) ->
    [{K,q(K)} || K <- KeyList].

qs_pl(KeyList) when is_list(KeyList) ->
    [{K,qs(K)} || K <- KeyList].

q_map(KeyList) when is_list(KeyList) ->
    PL = q_pl(KeyList),
    maps:from_list(PL).

qs_map(KeyList) when is_list(KeyList) ->
    PL = qs_pl(KeyList),
    maps:from_list(PL).

params() ->
    query_handler:get_params().


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

%% console_log is not part of the log handler, but  relevant
console_log(String) ->
    action_console_log:console_log(String).

console_log(Priority, String) ->
    action_console_log:console_log(Priority, String).

%%% EXPOSE SESSION_HANDLER %%%
session(Key) ->
    _Value = session_handler:get_value(Key).

session(Key, Value) ->
    _Value = session_handler:set_value(Key, Value).

session_default(Key, DefaultValue) ->
    _Value = session_handler:get_value(Key, DefaultValue).

clear_session() ->
    ok = session_handler:clear_all().

session_id() ->
    session_handler:session_id().

%%% EXPOSE CACHE_HANDLER %%%

cache(Key) ->
    cache(Key, fun() -> undefined end).

cache(Key, Fun) ->
    cache(Key, infinity, Fun).

cache(Key, TTL, Fun) ->
    {ok, Value} = cache_handler:get_cached(Key, Fun, TTL),
    Value.

set_cache(Key, Value) ->
    set_cache(Key, infinity, Value).

set_cache(Key, TTL, Value) ->
    cache_handler:set_cached(Key, Value, TTL).

clear_cache(Key) ->
    ok = cache_handler:clear(Key).

clear_all_cache() ->
    ok = cache_handler:clear_all().

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
