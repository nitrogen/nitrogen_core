% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_lightbox).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, lightbox).

render_element(Record) -> 
    ?PRINT(Record#lightbox.id),
    Panel = #panel {
        id=Record#lightbox.id,
        anchor=Record#lightbox.anchor,
        class=[lightbox, Record#lightbox.class],
        style=wf:to_list(Record#lightbox.style),
        body=[
            #panel { 			
                class=lightbox_background
            },
            #table { 
                class=lightbox_table,
                rows=#tablerow {
                    cells=#tablecell { align=center, valign=middle, style="vertical-align: middle;", body=[
                        "<center><table><tr><td>",
                        Record#lightbox.body,
                        "</td></tr></table></center>"
                    ]} 
                }
            }
        ]
    },
    element_panel:render_element(Panel).
