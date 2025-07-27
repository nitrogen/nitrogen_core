<!-- dash: Handlers - Modal | Guide | ###:Section -->

## Modal Handler

Modals are popups that present a message or interface. In Nitrogen, they are
the basis for the `#modal`, `#confirm`, and `#prompt` actions.

The Modal Handler allows you to define the mechanism by which the `#modal` is
rendered. Nitrogen provides a default modal handler, which is independent of
any front-end Javascript frameworks. That said, if you are building a Nitrogen
app using a front-end framework, you are able to add your own modal handler
that relies on that front-end framework's concepts.

### Behavior Functions

#### `init(Config, State) -> {ok, NewState}`

Initialize the role handler

#### `finish(Config, State) -> {ok, NewState}`

Clean up the handler

#### `process_open_action(Record, Config, State) -> {ok, ID, Actions}`

Renders the provided `#modal{}` record. Returning the `ID` of the `#modal{}`
and the rendered `Actions`

- `Record` - The `#modal{}` record to render.

- `ID` - The ID of the modal (or `undefined`)

- `Actions` - The rendered actions that will open the custom modal, this can be
  a combination of Nitrogen Actions and Raw Javascript.

#### `process_close_action(Record, Config, State) -> {ok, ID, Actions}`

Renders the provided `Record` (a `#close_modal{}` record) and returns the `ID`
of the closed modal, as well as the generated `Actions` (either as Nitrogen
Actions or Raw Javascript). Overall, this should do the work of removing the
`#modal{}` from the DOM as well as performing any other clean-up releated to
the closed modal.

- `Record` - The `#modal{}` record to render.

- `ID` - The ID of the modal (or `undefined`)

- `Actions` - The rendered actions that will open the custom modal, this can be
  a combination of Nitrogen Actions and Raw Javascript.

### Example

Here is the complete text of the default role handler

```erlang
-module(default_modal_handler).
-include("wf.hrl").
-behaviour(modal_handler).
-export ([
    init/2,
    finish/2,
    process_open_action/3,
    process_close_action/3
]).


init(_Config, _State) ->
    {ok, []}.

finish(_Config, _State) ->
    {ok, []}.

process_open_action(Rec = #modal{}, _Config, _State) ->
    ID = wf:coalesce([Rec#modal.id, wf:temp_id()]),
    Body = build_body(ID, Rec),

    %% Rendering Body and capturing the Actions, otherwise the button actions will
    %% get wired to the page before the buttons exist
    {ok, RenderedBody, BodyActions} = wf:render_isolated(Body),

    Action = [
        #insert_top{
            trigger = Rec#modal.trigger,
            anchor = Rec#modal.anchor,
            target="body",
            elements=[
                #lightbox{id=ID, body=[
                    #panel{class=lightbox_form, body=RenderedBody}
                ]}
            ]
        },
        BodyActions,
        %% TODO: replace with scrollTo when it's added
        #js_fun{function="Nitrogen.$scroll_to_modal", args=[ID]},
        #js_fun{function="Nitrogen.$add_modal", args=[ID]},
        #js_fun{function="Nitrogen.$add_modal_zindex", args=[ID]}
    ],

    {ok, ID, Action}.

build_body(ID, #modal{text=Text, body=Body,
                 title_text=TitleT, title_body=TitleB,
                 close_text=CloseT, close_body=CloseB,
                 show_close_button=ShowCloseButton,
                 buttons=Buttons0, options=_Opts}) ->
    Buttons = process_buttons(Buttons0),
    [
        title(TitleT, TitleB),
        body(Text, Body),
        #panel{class=lightbox_buttons, body=[
            Buttons,
            close_button(ShowCloseButton, ID, CloseT, CloseB)
        ]}
    ].

title(Text, Body) when ?WF_BLANK(Text) andalso ?WF_BLANK(Body) ->
    [];
title(Text, Body) ->
    #h3{class=modal_title, text=Text, body=Body}.

body(Text, Body) when ?WF_BLANK(Text) andalso ?WF_BLANK(Body) ->
    [];
body(Text, Body) ->
    #panel{class=modal_body, text=Text, body=Body}.

process_buttons([X | Rest]) when ?WF_BLANK(X) ->
    process_buttons(Rest);
process_buttons([{Text, Postback} | Rest]) ->
    [#button{text=Text, postback=Postback} | process_buttons(Rest)];
process_buttons([{Text, Postback, Delegate} | Rest]) ->
    [#button{text=Text, postback=Postback, delegate=Delegate} | process_buttons(Rest)];
process_buttons([Button | Rest]) when ?IS_ELEMENT(Button) ->
    [Button | process_buttons(Rest)];
process_buttons([Other | Rest]) ->
    [Other | process_buttons(Rest)];
process_buttons([]) ->
    [].

close_button(false, _, _, _) ->
    [];
close_button(true, ID, Text, Body) when ?WF_BLANK(Text) andalso ?WF_BLANK(Body) ->
    #button{text="Close", click=#close_modal{id=ID}};
close_button(true, ID, Text, Body) ->
    #button{text=Text, body=Body, click=#close_modal{id=ID}}.

process_close_action(#close_modal{id=ID}, _Config, _State) ->
    RemoveModalAndGetID = #js_fun{function="Nitrogen.$remove_modal", args=[ID]},
    Script = #js_fun{function="Nitrogen.$remove", args=["body", RemoveModalAndGetID]},
    {ok, ID, Script}.
```

### See Also

- [Handler Overview](./handlers.md)
- [Action: Modal](modal_action.md)
- [Demo: Modals](//demos/modal)
- [Demos: Notices](//demos//notices)
- [API: Authentication and Authorization](./api.md)
