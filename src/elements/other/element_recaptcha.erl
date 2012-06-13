%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_recaptcha).
-include_lib("nitrogen_core/include/wf.hrl").
-export([reflect/0, render_element/1, event/1]).
%% implemented according to google-documentation
%% https://developers.google.com/recaptcha/docs/display
%% https://developers.google.com/recaptcha/docs/verify

reflect() -> record_info(fields, recaptcha).

render_element(#recaptcha{id=ID, class=Cl, delegate=Delegate,
               fail_body=FB, captcha_opts=COPts, 
               button_label=ButtonLabel, button_id=ButtonId})->
    % Since the recaptcha is rendered outside of the nitrogen framework
    % it doesn't play along when it comes to data exchange.
    % This hack transfers the alien content into #hidden elements before
    % postback. The content is then accessible via wf:q()
    wf:wire(ButtonId, #event{type=click, actions=[
        % transfer happens here
        #script{script="Nitrogen.$from_alien('recaptcha_challenge_field')"},
        #script{script="Nitrogen.$from_alien('recaptcha_response_field')" }]}),
    Src = lists:flatten([challenge_url(), "?k=", public_key()]),
    Postback={eval_recaptcha, ID, Delegate, FB},
    #panel{id=ID, class=Cl, style="width:315px",
        body=[
            render_options(COPts, default_opts()),
            % the hidden elements
            #hidden{id=recaptcha_challenge_field},
            #hidden{id=recaptcha_response_field},
            "<script type=\"text/javascript\" src=\""++ Src ++ "\"></script>",
            #panel{body=[
                #panel{id=fail_msg,
                       style="visibility:hidden;float:left",
                       body=FB},
                #button{id=ButtonId, style="float:right",
                        text=ButtonLabel,
                        postback=Postback, delegate=?MODULE}]},
            #p{style="clear:both"}]}.

event({eval_recaptcha, CaptchaID, Delegate, Fail}) ->
    Challenge = wf:q(recaptcha_challenge_field),
    Response = wf:q(recaptcha_response_field),
    Evaled = evaluate_captcha(CaptchaID, Challenge, Response),
    case Evaled of
        ok     -> process_callback({recaptcha, Evaled}, Delegate);
        _else  -> wf:wire("Recaptcha.reload()"),
		  replace_error_message(Fail)
    end.

%% internal
get_config_fields(Field)->
    Cfg = config_handler:get_value(recaptcha),
    proplists:get_value(Field, Cfg).

private_key() ->
    get_config_fields(private_key).

public_key() ->
    get_config_fields(public_key).

challenge_url() ->
    get_config_fields(challenge_url).

verify_url() ->
    get_config_fields(verify_url).

ip_to_str(IPTuple) ->
    case IPTuple of
        V4Addr when is_tuple(IPTuple) andalso
                    size(IPTuple) == 4 ->
            IL = tuple_to_list(V4Addr),
            string:join([integer_to_list(E) || E <-IL], ".");
        V6Addr when is_tuple(IPTuple) andalso
                    size(IPTuple) == 8 ->
            IL = tuple_to_list(V6Addr),
            string:join([integer_to_list(E,16) || E <-IL], ":")
    end.

mk_post_request(Url, Params)->
    MkField = fun(K, V) ->
                      lists:flatten([atom_to_list(K), "=",
                                     http_uri:encode(V)])
              end,
    Fields = [MkField(K,V) || {K,V} <- Params],
   {Url, [], "application/x-www-form-urlencoded", string:join(Fields,"&")}.

render_options(OptProps, Defaults) ->
    ValToStr = fun(V) ->
                       case V of
                           V when is_list(V) ->
                               ": '" ++ V ++ "'";
                           V when V == null ->
                               ": null";
                           V when is_atom(V) ->
                               ": '" ++  atom_to_list(V) ++ "'";
                           (V) when is_integer(V)->
                               ": " ++ integer_to_list(V)
                       end
               end,
    SortedOpts = lists:keysort(1, OptProps),
    SortedDef  = lists:keysort(1, Defaults),
    Merged = lists:ukeymerge(1, SortedOpts, SortedDef),
    Opts = [ atom_to_list(K) ++ ValToStr(V) || {K,V} <- Merged],
    "<script type=\"text/javascript\">var RecaptchaOptions={"
        ++ string:join(Opts, ",\n")
        ++ "};</script>".

%% https://developers.google.com/recaptcha/docs/customization
default_opts() ->
    [{theme,               red},
     {lang,                en},
     {custom_translations, null},
     {custom_theme_widget, null}, %not supported
     {tabindex,            0}].

evaluate_captcha(CaptchaID, Challenge, Response) ->
    Url = verify_url(),
    Re = httpc:request(post,
                       mk_post_request(Url, [{privatekey, private_key()},
                              {challenge,  Challenge},
                              {response,   Response},
                              {remoteip, ip_to_str(wf_context:peer_ip())}]),
        [],[]),
    case Re of
        {ok, {{_Version, 200, _Reason}, _Hdr, Body}} ->
            parse_response(CaptchaID, Body);
        {error, Reason}                              ->
            {error, Reason}
    end.

replace_error_message(Msg) ->
    wf:replace(fail_msg,
	       #panel{id=fail_msg,
		      style="visibility:visible;float:left",
		      body=Msg}).

parse_response(_CaptchaID, Body) ->
    case string:tokens(Body, "\n") of
        ["true", "success"] -> ok;
        ["false", Error]    -> {error, Error};
        ERROR               -> {unknown, ERROR}
    end.

process_callback(Event, Delegate) ->
    Module = wf:coalesce([Delegate, wf:page_module()]),
    case Module:event(Event) of
        ok          -> ok;
        error       -> wf:wire("Recaptcha.reload()");
        {error,Msg} -> wf:wire("Recaptcha.reload()"),
		       replace_error_message(Msg)
    end.
