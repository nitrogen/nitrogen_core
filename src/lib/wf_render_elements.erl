% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_render_elements).
-include("wf.hrl").
-export ([
    render_elements/1,
    render_and_trap_actions/1,
    temp_id/0,
    normalize_id/1,
    recurse_body/2
]).

-spec render_and_trap_actions(Elements :: body() | fun()) -> {ok, Html :: binary(), Actions :: binary()}.
render_and_trap_actions(Elements) ->
    ?WF_IF(not(wf:in_request()), wf_context:init_context(undefined)),
    OldActionQueue = wf_context:action_queue(),
    wf_context:clear_action_queue(),
    {ok, Html} = case is_function(Elements) of
        true -> render_elements(Elements());
        false -> render_elements(Elements)
    end,
    {ok, JS} = wf_render_actions:render_action_queue(),
    wf_context:action_queue(OldActionQueue),
    {ok, Html, wf:to_unicode_binary(JS)}.


% Render elements and return the HTML that was produced.
% Puts any new actions into the current context.
-spec render_elements(Elements :: body()) -> {ok, html()}.
render_elements(Elements) ->
    {ok, inner_render_elements(Elements)}.

-spec inner_render_elements(E :: body()) -> html().
inner_render_elements(undefined) ->
    [];
inner_render_elements([]) ->
    [];
inner_render_elements([E|T]) ->
    [inner_render_elements(E) | inner_render_elements(T)];
inner_render_elements(E) when is_tuple(E) ->
    render_element(E);
inner_render_elements(mobile_script) ->
    mobile_script;
inner_render_elements(script) ->
    script;
inner_render_elements(Atom) when is_atom(Atom) ->
    wf:to_binary(Atom);
inner_render_elements(E)
    when is_integer(E); is_binary(E); ?IS_STRING(E)->
    E;
inner_render_elements(Unknown) ->
    throw({unanticipated_case_in_render_elements, Unknown}).

% This is a Nitrogen element, so render it.
-spec render_element(nitrogen_element()) -> html().
render_element(Element) when is_tuple(Element) ->
    % Get the element's backing module...
    Base = wf_utils:get_elementbase(Element),
    verify_and_render(Base, Element).

verify_and_render(Base = #elementbase{is_element=is_element}, Element) ->
    inner_render_element(Base, Element);
verify_and_render(_, Element) ->
    wf:error("Attempting to render an element that is not an element: ~p",[Element]),
    wf:f("Unrenderable Element: <pre>~p</pre>",[Element]).
    %throw({not_an_element, Element}).

-spec inner_render_element(#elementbase{}, nitrogen_element()) -> html().
inner_render_element(#elementbase{show_if=false}, _Element) ->
    [];
inner_render_element(Base = #elementbase{show_if=true}, Element) ->
    Module = Base#elementbase.module, 
    case code:ensure_loaded(Module) of
        {module, Module} ->
            prepare_and_render_or_transform(Module, Base, Element);
        Response ->
            ElementName = element(1, Element),
            exit({failed_to_ensure_module_loaded, [{element, ElementName}, {'response_from code:ensure_loaded', Response}, {module, Module}, {record, Element}]})
    end.

-spec prepare_and_render_or_transform(Module :: atom(), #elementbase{}, nitrogen_element()) -> html().
prepare_and_render_or_transform(Module, Base, Element) ->
    case erlang:function_exported(Module,transform_element,1) of
        true ->
            %% Module:transform_element is a special shortcut mechanism
            %% of rendering elements without any of the overhead of
            %% the pre-rendering that goes on with each element. This
            %% should be used for custom elements that are simply
            %% defined in terms of other Nitrogen elements.
            _Html = call_element_render(transform_element, Module, Element);
        false ->
            prepare_and_render(Module, Base, Element)
    end.

-spec prepare_and_render(Module :: atom(), #elementbase{}, nitrogen_element()) -> html().
prepare_and_render(Module, Base, Element) ->
    % TODO: Revisit generating an ID for each element, instead
    % generating the ID only if an element has actions.
    % Otherwise, if an element needs an ID for something
    % special (like how the #button element needs an anchor to
    % wire the postback to and for validation stuff), let the
    % element_render function take care of that itself.
    %
    % This change will provide a handful of performance
    % improvements:
    %   + Removes having to call temp_id() for every element
    %   + Removes having to call normalize_id() possibly twice
    %     for each element
    %   + Lightens the page size since every element won't have
    %     an unnecessary 'tempABCXYZ' class.
        
    % Get the anchor, ID, and Class, or create a new ones if not defined...
    Anchor = extract_anchor(Base),
    ID = extract_id(Base, Anchor),
    Class = extract_class(Base, Anchor, ID),

    % Update the base element with the new id and class...
    Base1 = Base#elementbase { id=ID, anchor=Anchor, class=Class },
    Element1 = wf_utils:replace_with_base(Base1, Element),

    % Wire the actions...           
    wf_context:anchor(Anchor),
    wf:wire(Base1#elementbase.actions),

    % Render the element...
    Html = call_element_render(render_element, Module, Element1),

    % Reset the anchor (likely changed during the inner render)...
    wf_context:anchor(Anchor),
    Html.

extract_anchor(#elementbase{anchor=undefined}) ->
    normalize_id(temp_id());
extract_anchor(#elementbase{anchor=Anchor}) ->
    normalize_id(Anchor).

% Get the ID, or use the anchor if it's not defined...
extract_id(#elementbase{id=undefined}, Anchor) ->
    Anchor;
extract_id(#elementbase{id=ID}, _Anchor) ->
    normalize_id(ID).

extract_class(#elementbase{class=Class}, ID, Anchor) when ID==Anchor ->
    [ID, Class];
extract_class(#elementbase{class=Class}, ID, Anchor) ->
    [ID, Anchor, Class].


% call_element_render(RenderOrTransform, Module, Element) -> {ok, Html}.  Calls
% the render_element/3 function of an element to turn an element record into
% HTML.
-spec call_element_render(RenderOrTransform :: render_element | transform_element,
                          Module :: module(),
                          Element :: nitrogen_element() ) -> html().
call_element_render(RenderOrTransform, Module, Element) ->
    %{Time, NewElements} = timer:tc(Module, RenderOrTransform, [Element]),
    NewElements = Module:RenderOrTransform(Element),
    %io:format("Time to render: ~p: ~p~n",[element(1, Element), Time]),
    inner_render_elements(NewElements).

-spec normalize_id(list() | atom() | binary()) -> string().
normalize_id(ID) -> 
    case wf:to_string_list(ID) of
        [".wfid_" ++ _] = [NormalizedID] -> NormalizedID;
        ["page"] -> "page";
        [NewID]  -> ".wfid_" ++ NewID
    end.

-spec temp_id() -> string().
temp_id() ->
    Num = ?WF_UNIQUE,  %% For Erlang 18+, is erlang:unique_integer,
                       %% For Erlang <18, is parts of erlang:now()
                       %% see compat.escript, and include/compat.hrl for the
                       %% definition.
    "temp" ++ integer_to_list(Num).


recurse_body(Fun, List) when is_list(List) ->
    [recurse_body(Fun, X) || X <- List];
recurse_body(Fun, Rec0) when is_tuple(Rec0), element(2, Rec0)==is_element ->
    try
        Rec = Fun(Rec0),
        Mod = element(3, Rec),
        Fields = Mod:reflect(),
        case lists:member(body, Fields) of
            true ->
                Idx = wf_utils:indexof(body, Fields),
                Body = element(Idx, Rec),
                Body2 = recurse_body(Fun, Body),
                setelement(Idx, Rec, Body2);
            false ->
                Rec
        end
    catch E:T ->
        error_logger:warning_msg("Error trying to apply function to an element tuple: ~p~nError: ~p: ~p.~nStacktrace: ~p",[Rec0, E, T, erlang:get_stacktrace()]),
        Rec0
    end;
recurse_body(_Fun, X) ->
    X.

            

    
    
