%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_oauth_facebook).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).


-spec reflect() -> [atom()].
reflect() -> record_info(fields, oauth_facebook).

-spec render_element(#oauth_facebook{}) -> body().
render_element( #oauth_facebook{app_id = AppId, 
                                scope  = Scope}) ->
    [make_facebook_script(AppId),
    tag(Scope)].

tag(Scope)->

    wf_tags:emit_tag('fb:login-button', [
                                         {scope,   Scope},
                                         {onlogin, "page.setup_facebook"}
                                        ]).

make_facebook_script(AppId) ->
    facebook_postback(),
    [
     "\n<script>\n",
     facebook_load_api(),
     "\n",
     facebook_init(AppId),
     "</script>\n\n"].

facebook_postback() ->
    wf:wire(#api { 
               name     = setup_facebook,
               delegate = facebook
              }). 


facebook_load_api() ->
    "  // Load the SDK asynchronously
  (function(d, s, id) {
    var js, fjs = d.getElementsByTagName(s)[0];
    if (d.getElementById(id)) return;
    js = d.createElement(s); js.id = id;
    js.src = \"//connect.facebook.net/en_US/sdk.js\";
    fjs.parentNode.insertBefore(js, fjs);
  }(document, 'script', 'facebook-jssdk'));".

facebook_init(AppId) ->
    ["     window.fbAsyncInit = function() {\n"
     "FB.init({",
     "appId      : '",AppId,"',\n" ,
     "cookie     : true,\n", 
     "xfbml      : true,\n",
     "version    : 'v2.1'\n",
    "})};\n"].
