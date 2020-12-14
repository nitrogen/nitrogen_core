-module(wf_cookies).
-include("wf.hrl").
-export([
	cookies/0,
	get_cookie/1,
	get_cookie/2,
	set_cookie/2,
    set_cookie/3,
	set_cookie/4,
	delete_cookie/1
]).

cookies() ->
	Bridge = wf_context:bridge(),
	sbw:cookies(Bridge).

get_cookie(Cookie) ->
	get_cookie(Cookie, undefined).

get_cookie(Cookie, Default) ->
	Bridge = wf_context:bridge(),
	case sbw:cookie(Cookie, Bridge) of
		undefined -> Default;
		Value -> Value
	end.

set_cookie(Cookie, Value) ->
    set_cookie(Cookie, Value, [{path, "/"}, {minutes_to_live, 20}]).

%% Deprecated
set_cookie(Cookie, Value, Path, MinutesToLive) ->
    set_cookie(Cookie, Value, [{path, Path}, {minutes_to_live, MinutesToLive}]).

set_cookie(Cookie, Value, Options) ->
    Type = wf_context:type(),
    set_cookie_inner(Type, Cookie, Value, Options).

set_cookie_inner(postback_websocket, Cookie, Value, Options) ->
	%% Websocket connections have already send the headers and can't set the
	%% cookies as a "frame header" (such a thing doesn't exist), so we need to
	%% set these cookies with javascript
	set_websocket_cookie(Cookie, Value, Options);
set_cookie_inner(_Type, Cookie, Value, Options) ->
	set_bridge_cookie(Cookie, Value, Options).

set_websocket_cookie(Cookie, Value, Options) ->
    SetCookie = #set_cookie{
        cookie=Cookie,
        value=Value,
        path=proplists:get_value(path, Options, "/"),
        domain=proplists:get_value(path, Options, undefined),
        minutes_to_live=proplists:get_value(minutes_to_live, Options, 20),
        secure=proplists:get_value(secure, Options, false),
        http_only=proplists:get_value(http_only, Options, false)
    },
    wf:wire(SetCookie).

set_bridge_cookie(Cookie, Value, Options) ->
	Bridge = wf_context:bridge(),
  Options2 = minutes_to_live_to_max_age(Options),
	NewBridge = sbw:set_cookie(Cookie, Value, Options2, Bridge),
	wf_context:bridge(NewBridge),
    ok.

minutes_to_live_to_max_age([{minutes_to_live, MTL}|Rest]) ->
    [{max_age, MTL*60} | minutes_to_live_to_max_age(Rest)];
minutes_to_live_to_max_age([H|T]) ->
    [H|minutes_to_live_to_max_age(T)];
minutes_to_live_to_max_age([]) ->
    [].

delete_cookie(Cookie) ->
	set_cookie(Cookie, "", "/", 0).
