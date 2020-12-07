% vim: sw=4 ts=4 et ft=erlang
-module(wf_security_policy).
-include("wf.hrl").

-export([
          generate/0
        , nonce/0
        ]).

generate() ->
    case wf:config_default(content_security_policy, []) of
        [] ->
            undefined;
        Spec ->
            build_csp(Spec)
    end.

build_csp(Spec) when is_list(Spec) ->
    lists:foldl(fun process_csp_rule/2, [], Spec);
build_csp(Spec) ->
    Spec.

process_csp_rule({Domain, Sources}, Acc) ->
    NormalizedSources = string:join([source(S) || S <- Sources], " "),
    domain(Domain) ++ " " ++ NormalizedSources ++ "; " ++ Acc.

%% create a random nonce value; hard coded to be 12 characters
nonce() ->
    Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
    Length = length(Chars),
    lists:foldl(
      fun(_, Acc) ->
              [lists:nth(rand:uniform(Length), Chars)] ++ Acc
      end, [], lists:seq(1, 12)).


%% allow atomized aliases for CSP domains
domain(default_src)     -> "default-src";
domain(script_src)      -> "script-src";
domain(style_src)       -> "style-src";
domain(img_src)         -> "img-src";
domain(connect_src)     -> "connect-src";
domain(font_src)        -> "font-src";
domain(object_src)      -> "object-src";
domain(media_src)       -> "media-src";
domain(frame_src)       -> "frame-src";
domain(sandbox)         -> "sandbox";
domain(report_uri)      -> "report-uri";
domain(child_src)       -> "child-src";
domain(form_action)     -> "form-action";
domain(frame_ancestors) -> "frame-ancestors";
domain(plugin_types)    -> "plugin-types";
domain(base_uri)        -> "base-uri";
domain(report_to)       -> "report-to";
domain(worker_src)      -> "worker-src";
domain(manifest_src)    -> "manifest-src";
domain(prefetch_src)    -> "prefetch-src";
domain(navigate_to)     -> "navigate-to";
domain(Domain)          -> Domain.


%% allow atomized aliases for CSP sources
source(none)           -> "'none'";
source(self)           -> "'self'";
source(data)           -> "'data:'";
source(https)          -> "'https:'";
source(unsafe_inline)  -> "'unsafe-inline'";
source(unsafe_eval)    -> "'unsafe-eval'";
source(strict_dynamic) -> "'strict-dynamic'";
source(unsafe_hashes)  -> "'unsafe-hashes'";
source(nonce)          -> "'nonce-" ++ wf:script_nonce() ++ "'";
source(Source)         -> Source.
