% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_article).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, article).

-spec render_element(#article{}) -> body().
render_element(Record) ->
    wf_tags:emit_tag(article, Record#article.body, [
        {id, Record#article.html_id},
        {class, ["article", Record#article.class]},
        {title, Record#article.title},
        {style, Record#article.style},
        {data_fields, Record#article.data_fields}
    ]).
