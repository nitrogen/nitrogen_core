% vim: sw=4 ts=4 et ft=erlang
-module(element_grid).
-include("wf.hrl").
-export([
    reflect/0,
    transform_element/1,
    to_grid_record/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, grid).

-spec transform_element(nitrogen_element()) -> body().
transform_element(Record0)  ->
    Record = to_grid_record(Record0),
    Module = nitrogen:grid_system_module(Record#grid.system),
    Record#grid{module=Module}.

-spec to_grid_record(X :: nitrogen_element()) -> #grid{}.
to_grid_record(X) -> 
    setelement(1, X, grid).

