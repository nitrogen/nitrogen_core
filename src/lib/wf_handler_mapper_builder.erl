-module(wf_handler_mapper_builder).
-export([build/1]).
-include("wf.hrl").
-include_lib("syntax_tools/include/merl.hrl").

%% Basically, this is an optimization mechanism to skip ets lookups, and just
%% convert a global handler atom name to a module name.
%% The produced module (wf_handler_mapper) will look something like this:
%%
%% -module(wf_handler_mapper).
%% -compile(export_all).
%%
%% lookup(route_handler) -> {default_route_handler, Config, State};
%% lookup(security_handler) -> {default_security_handler, Config, State};
%% ...
%% lookup(_) -> context.
%% ...

%% It's very simple in concept, and it skips having to do an ETS lookup or
%% process dictionary lookup for every call.
%% 
%% The `context` return indicates that the provided handler is not a global
%% handler, and so it should be looked up in the request context's
%% `handler_list` attribute.

build(Handlers) ->
	msg("Building new wf_handler_lookup"),
	Modtext = build_module(Handlers),
	Forms = merl:quote(Modtext),
	Res = merl:compile_and_load(Forms),
    %logger:info("Building Module:~n~ts~n",[Modtext]),
	case Res of 
		{ok, _} -> ok;
		error ->
			msg(wf:f("Unable to build new wf_handler_lookup module with the text:~n~p",[Modtext])),
			error
	end.

build_module(Handlers) ->
	lists:flatten([
		build_header(),
		build_exports(Handlers)
	]).

build_header() ->
	"-module(wf_handler_mapper).\n"
	"-export([lookup/1]).\n\n".

build_exports(Handlers) ->
    [
        [build_clause(H) || H <- Handlers],
        catchall_clause()
    ].

build_clause(#handler_context{name=Name, module=Mod, config=Config, state=State}) ->
    wf:f("lookup('~ts') -> {'~ts', ~p, ~p};~n", [Name, Mod, Config, State]).

catchall_clause() ->
    "lookup(_) -> context.".

msg(Msg) ->
	io:format("Nitrogen Global Handler: ~ts~n",[Msg]).
