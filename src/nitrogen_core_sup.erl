-module(nitrogen_core_sup).
-behaviour(supervisor).

-export([start_link/0,
         init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
    #{
        id=>nitrogen_handler_srv,
        start=>{nitrogen_handler_srv, start_link, []}
    },
    {ok, {SupFlags, ChildSpecs}}.


