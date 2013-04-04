% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_image).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, image).

render_element(Record) ->
    Attributes = [
        {id, Record#image.html_id},
        {class, [image, Record#image.class]},
        {style, Record#image.style},
        {src, wf:to_list(Record#image.image)}
    ],

    WidthAtts = case Record#image.width of
      undefined -> Attributes;
      Width -> [{width, Width}|Attributes]
    end,

    HeightAtts = case Record#image.height of
      undefined -> WidthAtts;
      Height -> [{height, Height}|WidthAtts]
    end,

    FinalAttributes = case Record#image.alt of
      undefined -> HeightAtts;
      ImageAlt -> [{alt, ImageAlt}|HeightAtts]
    end,

    wf_tags:emit_tag(img, FinalAttributes).
