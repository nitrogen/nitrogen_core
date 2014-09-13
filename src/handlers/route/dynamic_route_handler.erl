% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (dynamic_route_handler).
-behaviour (route_handler).
-include("wf.hrl").
-export ([
    init/2, 
    finish/2
]).

%% @doc
%% dynamic_route_handler looks at the requested path and file extension
%% to determine how a request should be served. If the request path has no 
%% extension, then it assumes this request should be handled by a Nitrogen
%% page module that matches the path with slashes converted to underscores.
%% If no module is found, then it will chop off the last part of the path 
%% (storing it for later access in wf:path_info/0) and try again, repeating
%% until either a module is found, or there are no more parts to chop. If
%% a module still can't be found, then the web_404 module is used if defined
%% by the user, otherwise a 404 is generated internally.
%% 
%% Requests for "/" are automatically sent to index.
%%
%% If the request path does have an extension, then it is treated like a request
%% for a static file. This is delegated back to the HTTP server.

init(_Config, State) -> 
    % Get the path...
    Bridge = wf_context:bridge(),
    Path = sbw:path(Bridge),

    % Convert the path to a module. If there are no routes defined, then just
    % convert everything without an extension to a module.
    % Otherwise, look through all routes for the first matching route.
    {Module, EntryPoint, PathInfo} = route(Path),
    {Module1, PathInfo1} = check_for_404(Module, PathInfo, Path),

    wf_context:page_module(Module1),
    wf_context:path_info(PathInfo1),
    wf_context:entry_point(EntryPoint),

    {ok, State}.

finish(_Config, State) -> 
    {ok, State}.

%%% PRIVATE FUNCTIONS %%%

% First, check if this is a request for the root path. If so
% then just send it to index.
% Check if there is an extension, if so, it's static.
% Otherwise, try to load a module according to each part of the path.
% First, cycle through code:all_loaded(). If not there, then check erl_prim_loader:get_file()
% If still not there, then 404.
route("/") -> 
    {list_to_atom(module_name(["index"])), main, []};

route(Path) ->
    case extension_type(Path) of
        static ->
            {static_file, main, Path};
        {module, EntryFun, ProcessingFun} ->
            Path1 = string:strip(filename:rootname(Path), both, $/),
            Tokens = string:tokens(Path1, "/"),
            % Check for a loaded module. If not found, then try to load it.
            case try_load_module(EntryFun, ProcessingFun, Tokens) of
                {Module, EntryPoint, PathInfo} -> 
                    {Module, EntryPoint, PathInfo};
                undefined ->
                    {web_404, main, Path1}
            end
    end.

extension_type(Filename) ->
    SmartExtensions = wf:config_default(smart_extensions, []),
    Ext = string:strip(filename:extension(Filename), left, $.),
    case lists:keyfind(Ext, 1, SmartExtensions) of
        {Ext, EntryFun, ProcessingFun={_,_}} ->
            {module, EntryFun, ProcessingFun};
        {Ext, EntryFun, undefined} ->
            {module, EntryFun, undefined};
        {Ext, EntryFun} ->
            {module, EntryFun, undefined};
        false ->
            case Ext of
                [] -> {module, main, undefined};
                _ -> static
            end;
        Other ->
            throw({invalid_smart_extension, Other})
    end.

module_name(Tokens) ->
    ModulePrefix = wf:config_default(module_prefix, ""),
	AllTokens = case ModulePrefix of
	    "" -> Tokens;
	    _ -> [ ModulePrefix | Tokens ]
	end,
	_ModuleName = string:join(AllTokens, "_").

verify_and_atomize_module(ModuleName) when is_list(ModuleName) ->
    try 
        list_to_existing_atom(ModuleName)
    catch _:_ ->
        case erl_prim_loader:get_file(ModuleName ++ ".beam") of
            {ok, _, _} -> list_to_atom(ModuleName);
            _ -> list_to_atom("$not_found")
        end
    end.

try_load_module(EntryFun, ProcessingFun, Tokens) ->
    try_load_module(EntryFun, ProcessingFun, Tokens, []).

try_load_module(_EntryFun, _ProcessingFun, [], _ExtraTokens) ->
    undefined;
try_load_module(EntryFun, ProcessingFun, Tokens, ExtraTokens) ->
    ModuleName = module_name(Tokens),
    Module = verify_and_atomize_module(ModuleName),

    %% Load the module, check if it exports the right method...
    code:ensure_loaded(Module),

    %% EntryFun will usually be 'main', meaning we're looking for Module:main()
    %% to enter by default. Smart Extensions might look for a different entry
    %% function like 'json_main'.
    case erlang:function_exported(Module, EntryFun, 0) of
        true -> 
            PathInfo = string:join(ExtraTokens, "/"),
            RealEntry = case ProcessingFun of
                undefined ->
                    %% There's no processing function, so we just enter the
                    %% module normally
                    EntryFun;
                {ProcMod, ProcFun} ->
                    %% We've identified that this has the entry point for the
                    %% associated smart extension, and this smart extension has
                    %% a processing Mod:Fun combination, so we'll pass that as
                    %% the entry point.
                    fun() -> ProcMod:ProcFun(EntryFun) end
            end,
            {Module, RealEntry, PathInfo};
        false ->
            case is_rest_module(Module) of
                true ->
                    PathInfo = string:join(ExtraTokens, "/"),
                    %% If this is a rest request, we handle it with the nitrogen_rest module
                    Entry = fun() -> nitrogen_rest:handle_request(Module) end,
                    {Module, Entry, PathInfo};
                false ->
                    next_try_load_module(EntryFun, ProcessingFun, Tokens, ExtraTokens)
            end
    end.


next_try_load_module(EntryFun, ProcessingFun, Tokens, ExtraTokens) ->
    Tokens1 = lists:reverse(tl(lists:reverse(Tokens))),
    ExtraTokens1 = [hd(lists:reverse(Tokens))|ExtraTokens],
    try_load_module(EntryFun, ProcessingFun, Tokens1, ExtraTokens1).

is_rest_module(Module) ->
    wf_utils:has_behaviour(Module, nitrogen_rest).


check_for_404(static_file, _PathInfo, Path) ->
    {static_file, Path};

check_for_404(Module, PathInfo, Path) ->
    % Make sure the requested module is loaded. If it
    % is not, then try to load the web_404 page. If that
    % is not available, then default to the 'file_not_found_page' module.
    case code:ensure_loaded(Module) of
        {module, Module} -> {Module, PathInfo};
        _ -> 
            case code:ensure_loaded(web_404) of
                {module, web_404} -> {web_404, Path};
                _ -> {file_not_found_page, Path}
            end
    end.
