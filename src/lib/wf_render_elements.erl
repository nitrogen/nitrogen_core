% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_render_elements).
-include("wf.hrl").
-export ([
    render_elements/1,
    temp_id/0,
    normalize_id/1
]).

% Render elements and return the HTML that was produced.
% Puts any new actions into the current context.
-spec render_elements(Elements :: body()) -> {ok, html()}.
render_elements(Elements) ->
    {ok, inner_render_elements(Elements)}.

-spec inner_render_elements(E :: body()) -> html().
% @doc ire = inner_render_elements
inner_render_elements(undefined) ->
    [];
inner_render_elements([]) ->
    [];
inner_render_elements(<<>>) ->
    <<>>;
inner_render_elements(E)
    when is_integer(E); is_binary(E); ?IS_STRING(E)->
    E;
inner_render_elements(Es) when is_list(Es) ->
    [inner_render_elements(E) || E <- Es];
inner_render_elements(E) when is_tuple(E) ->
    render_element(E);
inner_render_elements(mobile_script) ->
    mobile_script;
inner_render_elements(script) ->
    script;
inner_render_elements(Atom) when is_atom(Atom) ->
    wf:to_binary(Atom);
inner_render_elements(Unknown) ->
    throw({unanticipated_case_in_render_elements, Unknown}).

% This is a Nitrogen element, so render it.
-spec render_element(nitrogen_element()) -> {ok, html()}.
render_element(Element) when is_tuple(Element) ->
    % Get the element's backing module...
    Base = wf_utils:get_elementbase(Element),

    % Verify that this is an element...
    case Base#elementbase.is_element of
        is_element -> ok;
        _ -> throw({not_an_element, Element})
    end,

    case Base#elementbase.show_if of
        false ->
            [];
        true ->
            Module = Base#elementbase.module, 
            {module, Module} = code:ensure_loaded(Module),
            case erlang:function_exported(Module,transform_element,1) of
                true ->
                    %% Module:transform_element is a special shortcut mechanism
                    %% of rendering elements without any of the overhead of
                    %% the pre-rendering that goes on with each element. This
                    %% should be used for custom elements that are simply
                    %% defined in terms of other Nitrogen elements.
                    _Html = call_element_render(transform_element, Module, Element);

                false ->
                    % If no ID is defined, then use the same
                    % temp_id() for both the HtmlID and TempID.
                    % Otherwise, create a new TempID. Update the class
                    % with either one or both.

                    % Get the anchor, or create a new one if it's not defined...
                    Anchor = case Base#elementbase.anchor of
                        undefined -> normalize_id(temp_id());
                        Other1 -> normalize_id(Other1)
                    end,

                    % Get the ID, or use the anchor if it's not defined...
                    ID = case Base#elementbase.id of
                        undefined -> Anchor;
                        Other2 -> normalize_id(Other2)
                    end,
                    
                    % Update the class...
                    Class = case Anchor == ID of
                        true  -> [ID, Base#elementbase.class];
                        false -> [ID, Anchor, Base#elementbase.class]
                    end,

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
                    Html
            end
    end.

% call_element_render(RenderOrTransform, Module, Element) -> {ok, Html}.
% Calls the render_element/3 function of an element to turn an element record into
% HTML.
-spec call_element_render(RenderOrTransform :: render_element | transform_element,
                          Module :: module(),
                          Element :: nitrogen_element() ) -> html().
call_element_render(RenderOrTransform, Module, Element) ->
    NewElements = Module:RenderOrTransform(Element),
    inner_render_elements(NewElements).

-spec normalize_id(list()) -> string().
normalize_id(ID) -> 
    case wf:to_string_list(ID) of
        [".wfid_" ++ _] = [NormalizedID] -> NormalizedID;
        ["page"] -> "page";
        [NewID]  -> ".wfid_" ++ NewID
    end.

-spec temp_id() -> string().
temp_id() ->
    {_, _, C} = now(), 
    "temp" ++ integer_to_list(C).
