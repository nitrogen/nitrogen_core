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
        redirect_from_login/1,
        login_redirect_url/1,
        login_redirect_url/2
    ]).

-spec render_action(#redirect{}) -> text().
render_action(#redirect{url=Url, login=Login}) ->
    RedirectUrl = redirect_url(Login, Url),
    wf:f("window.location=\"~ts\";", [wf:js_escape(RedirectUrl)]).

-spec redirect(url()) -> html().
redirect(Url) ->
    wf:wire(#redirect { url=Url }),
    wf:f("<script nonce=\"~s\">window.location=\"~ts\";</script>",
         [wf_context:script_nonce(), wf:js_escape(Url)]).

-spec redirect_url(Login :: boolean() | url(), Url :: url()) -> url().
redirect_url(_Login=false, Url) ->
    Url;
redirect_url(_Login=true, Url) ->
    login_redirect_url(Url);
redirect_url(Login, Url) when is_list(Login); is_binary(Login)  ->
    %% In this situation, `Login` is the URL To redirect to after successful login.
    login_redirect_url(Url, Login).

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
