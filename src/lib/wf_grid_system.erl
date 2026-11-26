-module(wf_grid_system).
-export([
    initial_config/0,
    default_grid_system/0,
    default_grid_system/1,
    default_grid_system_module/0,
    grid_system_module/1,
    grid_system_module/2
]).

-type grid_system_name() :: atom().

-define(DEFAULT_GRID_SYSTEM_NAME, grid960).
-define(DEFAULT_GRID_SYSTEM_MODULE, element_grid_960).
-define(GRID_SYSTEM_KEY(Name), {grid_system, Name}).

-spec grid_system_module(grid_system_name(), module()) -> ok.
grid_system_module(Name, Module) when is_atom(Name), is_atom(Module) ->
    wf_fastmap:set(?GRID_SYSTEM_KEY(Name), Module),
    ok.

-spec grid_system_module(grid_system_name()) -> module().
grid_system_module(Name) ->
    wf_fastmap:get(?GRID_SYSTEM_KEY(Name)).

-spec default_grid_system(grid_system_name() | {grid_system_name(), module()}) -> ok.
default_grid_system(Name) when is_atom(Name) ->
    case grid_system_module(Name) of
        undefined ->
            wf:error("You are attempting to change the default grid system to ~p, but the grid system has not yet been assigned.~n Call nitrogen:grid_system(~p, HandlerModule) before attempting to change this.");
        Mod ->
            NewConfig = create_default_grid_system_config(Name, Mod),
            wf_fastmap:set(NewConfig)
    end,
    ok;
default_grid_system({Name, Module}) when is_atom(Name), is_atom(Module) ->
    NewConfig = create_default_grid_system_config(Name, Module),
    wf_fastmap:set(NewConfig).

-spec default_grid_system() -> grid_system_name().
default_grid_system() ->
    wf_fastmap:get(default_grid_system).

-spec default_grid_system_module() -> module().
default_grid_system_module() ->
    wf_fastmap:get(default_grid_system_module).

initial_config() ->
    Config1 = get_base_grid_system_config(),
    Config2 = add_grid_system_list_from_config(Config1),
    Config3 = add_default_grid_system_from_config(Config2),
    Config3.

%init() ->
%    Config = initial_config(),
%    wf_fastmap:set(Config).
  
get_base_grid_system_config() ->
    create_default_grid_system_config(?DEFAULT_GRID_SYSTEM_NAME, ?DEFAULT_GRID_SYSTEM_MODULE).

create_default_grid_system_config(Name, Module) ->
    #{
        ?GRID_SYSTEM_KEY(Name) => Module,
        default_grid_system => Name,
        default_grid_system_module => Module
     }.

add_grid_system_list_from_config(Config) ->
    GridSystems = application:get_env(nitrogen_core, grid_systems, []),
    ConfigRows = [format_grid_system_for_config(GS) || GS <- GridSystems],
    ds:set(Config, ConfigRows).

add_default_grid_system_from_config(Config) ->
    case get_default_grid_system_from_config(Config) of
        undefined ->
            %% If there is no default_grid_system defined, we can ignore it
            Config;
        {Name, Mod} ->
            NewSettings = create_default_grid_system_config(Name, Mod),
            ds:set(Config, NewSettings)
    end.        

get_default_grid_system_from_config(Config) ->
    Default = application:get_env(nitrogen_core, default_grid_system, undefined),
    case Default of 
        undefined ->
            undefined;
        Name when is_atom(Name) ->
            Mod = ds:get(Config, ?GRID_SYSTEM_KEY(Name), undefined),
            case Mod of
                undefined ->
                    error({grid_system_has_no_module_defined, Name});
                _ ->
                    {Name, Mod}
            end;
        {Name, Mod} when is_atom(Name), is_atom(Mod) ->
            {Name, Mod}
    end.

format_grid_system_for_config({Name, Mod}) when is_atom(Name), is_atom(Mod) ->
    {?GRID_SYSTEM_KEY(Name), Mod}.

