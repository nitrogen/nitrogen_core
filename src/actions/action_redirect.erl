% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_redirect).
-include("wf.hrl").
-export([
        render_action/1,
        redirect/1,
        redirect_to_login/1,
        redirect_to_login/2,
        redirect_from_login/1
    ]).

-spec render_action(#redirect{}) -> text().
render_action(Record) ->
    DestinationUrl = Record#redirect.url,
    wf:f("window.location=\"~ts\";", [wf:js_escape(DestinationUrl)]).

-spec redirect(url()) -> html().
redirect(Url) -> 
    wf:wire(#redirect { url=Url }),
    wf:f("<script>window.location=\"~ts\";</script>", [wf:js_escape(Url)]).

-spec redirect_to_login(url(), url()) -> html().
redirect_to_login(LoginUrl, PostLoginUrl) ->
    PickledUrl = wf:pickle(PostLoginUrl),
    redirect(LoginUrl ++ "?x=" ++ wf:to_list(PickledUrl)).

-spec redirect_to_login(url()) -> html().
redirect_to_login(LoginUrl) ->
    % Assemble the original
    Request = wf_context:request_bridge(),
    PostLoginUrl = Request:uri(),
    redirect_to_login(LoginUrl, PostLoginUrl).

-spec redirect_from_login(url()) -> html().
redirect_from_login(DefaultUrl) ->	
    PickledUrl = wf:q(x),
    case wf:depickle(PickledUrl) of
        undefined -> redirect(DefaultUrl);
        Other -> redirect(Other)
    end.
