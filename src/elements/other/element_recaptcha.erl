%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_recaptcha).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1]).
%% implemented according to google-documentation
%% https://developers.google.com/recaptcha/docs/display
%% https://developers.google.com/recaptcha/docs/verify

-define(DEFAULT_CHALLENGE_URL,"http://www.google.com/recaptcha/api/challenge").
-define(DEFAULT_VERIFY_URL,"http://www.google.com/recaptcha/api/verify").

reflect() -> record_info(fields, recaptcha).

render_element(Rec = #recaptcha{id=ID, class=Cl,
               captcha_opts=COPts,
               button_label=ButtonLabel, button_id=ButtonId})->
    % Since the recaptcha is rendered outside of the nitrogen framework
    % it doesn't play along when it comes to data exchange.
    % This hack transfers the alien content into #hidden elements before
    % postback. The content is then accessible via wf:q()

    ChallengeFieldID = "recaptcha_challenge_field", %wf:temp_id(),
    ResponseFieldID = "recaptcha_response_field", 
    FailPanelID = wf:temp_id(),

    Src = wf:to_list([challenge_url(Rec), "?k=", public_key(Rec)]),
    Postback={eval_recaptcha, Rec, ChallengeFieldID, ResponseFieldID, FailPanelID},

    % wf:defer instead of wire so a recaptcha can be made during a postback
    wf:defer(ButtonId, #event{type=click, actions=[
        % transfer happens here
        #script{script="Nitrogen.$from_alien('" ++ ChallengeFieldID ++ "')"},
        #script{script="Nitrogen.$from_alien('" ++ ResponseFieldID ++ "')" }]
    }),


    #panel{id=ID, class=[Cl,recaptcha], body=[
        render_options(COPts, default_opts()),
        % the hidden elements
        #hidden{id=ChallengeFieldID},
        #hidden{id=ResponseFieldID},
        "<script type=\"text/javascript\" src=\""++ Src ++ "\"></script>",
        #panel{body=[
            #panel{id=FailPanelID,
                   style="visibility:hidden;float:left",
                   body=[]
            },
            #button{id=ButtonId, style="float:right",
                    text=ButtonLabel,
                    postback=Postback,
                    delegate=?MODULE
            }
        ]},
        #p{style="clear:both"}
    ]}.

event({eval_recaptcha, Rec, ChallengeFieldID, ResponseFieldID, FailPanelID}) ->
    #recaptcha{
        tag=Tag,
        delegate=Delegate,
        fail_body=FailBody
    } = Rec,
    Challenge = wf:q(ChallengeFieldID),
    Response = wf:q(ResponseFieldID),
    Evaled = evaluate_captcha(Rec, Challenge, Response),
    case Evaled of
        ok     -> process_successful_callback(FailPanelID, Tag, Delegate);
        {error, Error}  -> 
            process_failure_callback(FailPanelID, FailBody, Tag, Delegate, Error)
    end.

%% internal
get_config(Field)->
    Cfg = wf:config_default(recaptcha,[]),
    proplists:get_value(Field, Cfg).

challenge_url(#recaptcha{challenge_url=Url}) ->
    ?WF_IF(Url=/=undefined,Url,wf:coalesce([get_config(challenge_url),?DEFAULT_CHALLENGE_URL])).

verify_url(#recaptcha{verify_url=Url}) ->
    ?WF_IF(Url=/=undefined,Url,wf:coalesce([get_config(verify_url),?DEFAULT_VERIFY_URL])).

public_key(#recaptcha{public_key=Key}) ->
    ?WF_IF(Key=/=undefined,Key,get_config(public_key)).

private_key(#recaptcha{private_key=Key}) ->
    ?WF_IF(Key=/=undefined,Key,get_config(private_key)).

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
        wf:to_list([atom_to_list(K), "=", http_uri:encode(V)])
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

evaluate_captcha(Rec, Challenge, Response) ->
    Url = verify_url(Rec),
    CaptchaID = Rec#recaptcha.id,
    Re = httpc:request(post,
        mk_post_request(Url, [
            {privatekey, private_key(Rec)},
            {challenge,  Challenge},
            {response,   Response},
            {remoteip, ip_to_str(wf_context:peer_ip())}
        ]),[],[]),
    case Re of
        {ok, {{_Version, 200, _Reason}, _Hdr, Body}} ->
            parse_response(CaptchaID, Body);
        {error, Reason}                              ->
            {error, Reason}
    end.

replace_error_message(FailPanelID, Msg) ->
    wf:replace(FailPanelID,
           #panel{id=FailPanelID,
              style="visibility:visible;float:left",
              body=Msg
           }).

parse_response(_CaptchaID, Body) ->
    case string:tokens(Body, "\n") of
        ["true", "success"] -> ok;
        ["false", Error]    -> {error, Error};
        ERROR               -> {unknown, ERROR}
    end.

process_successful_callback(FailPanelID, Tag, Delegate) ->
    Module = wf:coalesce([Delegate, wf:page_module()]),
    case Module:recaptcha_event(Tag, ok) of
        ok          -> ok;
        error       -> wf:wire("Recaptcha.reload()");
        {error,Msg} -> wf:wire("Recaptcha.reload()"),
                       replace_error_message(FailPanelID, Msg)
    end.

process_failure_callback(FailPanelID, FailMsg, Tag, Delegate, Error) ->
    Module = wf:coalesce([Delegate, wf:page_module()]),
    wf:wire("Recaptcha.reload()"),
    case Module:recaptcha_event(Tag, {error, Error}) of
        ok          -> replace_error_message(FailPanelID,FailMsg);
        error       -> replace_error_message(FailPanelID,FailMsg);
        {error,Msg} -> replace_error_message(FailPanelID,Msg)
    end.
