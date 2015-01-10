-module(wf_cookies).
-include("wf.hrl").
-export([
	cookies/0,
	get_cookie/1,
	get_cookie/2,
	set_cookie/2,
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
	set_cookie(Cookie, Value, "/", 20).

set_cookie(Cookie, Value, Path, MinutesToLive) ->
	Type = wf_context:type(),
	set_cookie(Type, Cookie, Value, Path, MinutesToLive),
	ok.

set_cookie(postback_websocket, Cookie, Value, Path, MinutesToLive) ->
	%% Websocket connections have already send the headers and can't set the
	%% cookies as a "frame header" (such a thing doesn't exist), so we need to
	%% set these cookies with javascript
	set_websocket_cookie(Cookie, Value, Path, MinutesToLive);
set_cookie(_Type, Cookie, Value, Path, MinutesToLive) ->
	set_bridge_cookie(Cookie, Value, Path, MinutesToLive).

set_websocket_cookie(Cookie, Value, Path, MinutesToLive) ->
	wf:wire(#set_cookie{cookie=Cookie, value=Value, path=Path, minutes_to_live=MinutesToLive}).
	
set_bridge_cookie(Cookie, Value, Path, MinutesToLive) ->
	Bridge = wf_context:bridge(),
	NewBridge = sbw:set_cookie(Cookie, Value, Path, MinutesToLive, Bridge),
	wf_context:bridge(NewBridge).

delete_cookie(Cookie) ->
	set_cookie(Cookie, "", "/", -1).
