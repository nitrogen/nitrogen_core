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
    RedirectUrl = case Record#redirect.login of
                      false -> DestinationUrl;
                      true -> login_redirect_url(DestinationUrl);
                      Url -> login_redirect_url(DestinationUrl, Url)
                  end,
    wf:f("window.location=\"~ts\";", [wf:js_escape(RedirectUrl)]).

-spec redirect(url()) -> html().
redirect(Url) -> 
    wf:wire(#redirect { url=Url }),
    wf:f("<script>window.location=\"~ts\";</script>", [wf:js_escape(Url)]).

-spec login_redirect_url(url()) -> url().
login_redirect_url(LoginUrl) ->
    % Assemble the original
    PostLoginUrl = wf_context:uri(),
    login_redirect_url(LoginUrl, PostLoginUrl).

-spec login_redirect_url(url(), url()) -> url().
login_redirect_url(LoginUrl, PostLoginUrl) ->
    PickledUrl = wf:pickle(PostLoginUrl),
    wf:to_list([LoginUrl, "?x=", PickledUrl]).


-spec redirect_to_login(url(), url()) -> html().
redirect_to_login(LoginUrl, PostLoginUrl) ->
    redirect(login_redirect_url(LoginUrl, PostLoginUrl)).

-spec redirect_to_login(url()) -> html().
redirect_to_login(LoginUrl) ->
    redirect(login_redirect_url(LoginUrl)).

-spec redirect_from_login(url()) -> html().
redirect_from_login(DefaultUrl) ->	
    PickledUrl = wf:q(x),
    case wf:depickle(PickledUrl) of
        undefined -> redirect(DefaultUrl);
        Other -> redirect(Other)
    end.
