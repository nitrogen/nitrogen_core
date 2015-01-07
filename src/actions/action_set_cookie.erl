-module(action_set_cookie).
-include("wf.hrl").
-export([render_action/1]).

render_action(#set_cookie{cookie=Cookie0, value=Value0, path=Path0, minutes_to_live=Mins0}) ->
	Cookie = wf:js_escape(Cookie0),
	Value = wf:js_escape(Value0),
	Path = wf:js_escape(Path0),
	Mins = wf:to_list(Mins0),
	wf:f(<<"Nitrogen.$set_cookie('~ts','~ts','~ts','~ts');">>,[Cookie, Value, Path, Mins]).
