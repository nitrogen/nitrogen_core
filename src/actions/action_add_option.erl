% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2019 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (action_add_option).
-include("wf.hrl").
-export([render_action/1]).

render_action(#add_option{target=Target, option=Option, location=Location}) ->
    OptionHtml = element_dropdown:create_option(Option),
    Type = case Location of
        bottom -> insert_bottom;
        top -> insert_top
    end,
    #update{type=Type, target=Target, elements=OptionHtml}.
