% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_open_window).
-include("wf.hrl").
-export([
        render_action/1,
        open_window/3
    ]).

-spec render_action(#open_window{}) -> text().
render_action(#open_window{
        url=Url, 
        name=Name,
        height=Height,
        width=Width,
        left=Left,
        top=Top,
        menubar=Menubar,
        statusbar=Statusbar,
        titlebar=Titlebar,
        options=OtherOptions}) ->

    FullOpts = [
        {height, Height},
        {width, Width},
        {left, Left},
        {top, Top},
        {menubar, yesno(Menubar)},
        {statusbar, yesno(Statusbar)},
        {titlebar, yesno(Titlebar)}
        | OtherOptions
    ],
    
    Opts = make_opts(FullOpts),

    wf:f("window.open(\"~ts~\", \"~ts\", \"~ts\")", [Url, Name, Opts]).

-spec open_window(Url :: url(), Name :: text(), Options :: list()) -> ok.
open_window(Url, Name, Options) ->
    open_window(Url, Name, Options).


yesno(true) -> "yes";
yesno(false) -> "no".

make_opts(Opts0) ->
    Opts = make_opts_(Opts0),
    wf:join(Opts, ",").

make_opts_([]) ->
    [];
make_opts_([{_, undefined} | Opts]) ->
    make_opts(Opts);
make_opts_([{Opt, Val} | Opts]) ->
    NewOpt = wf:f("~ts=~ts", [Opt, Val]),
    [NewOpt | make_opts(Opts)].
