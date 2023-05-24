% vim: sw=4 ts=4 et ft=erlang
-module (element_grid).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, grid).

-spec render_element(nitrogen_element()) -> body().
render_element(#grid_clear {}) ->
    "<div class='clear'></div>\n";
render_element(Record0)  ->
    Record = to_grid_record(Record0),
    Body = rewrite_body(lists:flatten([Record#grid.body])),

    #panel{
        html_id=Record#grid.html_id,
        class=to_classes(Record),
        data_fields=Record#grid.data_fields,
        aria=Record#grid.aria,
        body=case Record#grid.type of
            clear ->
                Body;
            container ->
                Body;
            grid ->
                #panel{ 
                    id=Record#grid.id,
                    anchor=Record#grid.anchor,
                    class=[grid, Record#grid.class],
                    style=Record#grid.style,
                    body=Body
                }
        end
    }.

-spec to_grid_record(X :: nitrogen_element()) -> #grid{}.
to_grid_record(X) -> 
    setelement(1, X, grid).

%% - Add omega to last grid element.
%% - Add a clear statement at end of body.
-spec rewrite_body(Body :: body()) -> body().
rewrite_body(Body) ->
    case is_grid_body(Body) of
        true -> 
            %% Add alpha to first grid element.
            [First|L1] = Body,
            First1 = to_grid_record(First),
            Body1 = [First1#grid {alpha = true }|L1],
            Body2 = append_clear_if_necessary(Body1),
            Body2;

        false -> 
            Body
    end.

%% Will append a grid_clear element if the last element isn't already a grid_clear{}
-spec append_clear_if_necessary(body()) -> body().
append_clear_if_necessary(Body) ->
    {BodyMinusLast, Last} = lists:split(length(Body) - 1, Body),
    case Last of
        [#grid_clear{}] -> 
            %% The last element is already grid_clear, so we'll just trust the body to be just fine,
            Body;
        [#grid{type=clear}] ->
            %% This is the basically the same as the previous clause, just a different way to write it
            Body;
        _ -> 
            Last1 = to_grid_record(hd(Last)),
            OmegaLast = Last1#grid{omega=true},
            Clear = #grid_clear{}, 
            [BodyMinusLast,OmegaLast,Clear]
    end.

%% Return true if all elements are grid elements.
-spec is_grid_body(body()) -> boolean().
is_grid_body(Body) ->
    F = fun(X) ->
        is_tuple(X) andalso size(X) > 3 andalso element(3, X) == ?MODULE
    end,
    Body /= [] andalso lists:all(F, Body).


%% Given a grid record, create the list of 960.gs classes to position
%% this grid.
-spec to_classes(#grid{}) -> [class()].
to_classes(Record) ->
    C = case Record#grid.type of
        clear -> 
            [clear];
        container -> 
            %% Construct the container_N class, and add any other user
            %% defined classes.
            ContainerClass = "container_" ++ integer_to_list(Record#grid.columns),
            [ContainerClass, Record#grid.class];
        grid -> 
            %% Just construct the grid_N class. User defined classes
            %% are added to inner panel.
            GridClass = "grid_" ++ integer_to_list(Record#grid.columns),
            [GridClass]
    end,
    
    %% Check for alpha...
    C1 = case Record#grid.alpha of
        true -> [alpha|C];
        _ -> C
    end,

    %% Check for omega...
    C2 = case Record#grid.omega of
        true -> [omega|C1];
        _ -> C1
    end,
    
    %%% Check for prefix...
    C3 = case Record#grid.prefix of
        undefined -> C2;
        Prefix -> ["prefix_" ++ integer_to_list(Prefix)|C2]
    end,

    %%% Check for suffix...
    C4 = case Record#grid.suffix of
        undefined -> C3;
        Suffix -> ["suffix_" ++ integer_to_list(Suffix)|C3]
    end,

    %%% Check for push...
    C5 = case Record#grid.push of
        undefined -> C4;
        Push -> ["push_" ++ integer_to_list(Push)|C4]
    end,

    %%% Check for pull...
    C6 = case Record#grid.pull of
        undefined -> C5;
        Pull -> ["pull_" ++ integer_to_list(Pull)|C5]
    end,
    C6.
