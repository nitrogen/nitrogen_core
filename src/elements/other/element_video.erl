-module (element_video).
-include("wf.hrl").

-export([reflect/0, render_element/1]).

reflect() -> record_info(fields, video).

render_element(#video{class=Class, html_id=HtmlID, url=URL,
					  height=Height, width=Width, autoplay=Autoplay,
					  controls=Controls, loop=Loop, muted=Muted,
					  poster_url=PosterURL, preload=Preload,
					  body_no_support=NoSupport}) ->
	wf_tags:emit_tag(video,NoSupport,[
		{id, HtmlID},
		{class, Class},
		{src, URL},
		{poster, PosterURL},
		{width, wf:to_list(Width)},
		{height, wf:to_list(Height)},
		{preload, Preload},
		?WF_IF(Loop, loop),
		?WF_IF(Muted, muted),
		?WF_IF(Autoplay, autoplay),
		?WF_IF(Controls, controls)
	]).
