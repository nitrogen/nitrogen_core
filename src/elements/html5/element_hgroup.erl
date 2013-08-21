% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_hgroup).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, hgroup).

-spec render_element(#hgroup{}) -> body().
render_element(Record) ->
    CheckH1ThruH6 = fun (X) ->
                        if
                            is_record(X, h1) ->
                                true;
                            is_record(X, h2) ->
                                true;
                            is_record(X, h3) ->
                                true;
                            is_record(X, h4) ->
                                true;
                            is_record(X, h5) ->
                                true;
                            is_record(X, h6) ->
                                true;
                            true ->
                                false
                        end
                    end,
    Y = lists:all(CheckH1ThruH6, Record#hgroup.body),
    if
        Y ->
            wf_tags:emit_tag(hgroup, Record#hgroup.body, [
                {id, Record#hgroup.html_id},
                {class, ["hgroup", Record#hgroup.class]},
                {style, Record#hgroup.style},
                {data_fields, Record#hgroup.data_fields}
            ]);
        true ->
            "<b style=\"color: red;\">hgroup can only have h1 thru h6 as child elements</b>"
    end.
