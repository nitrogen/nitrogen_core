% vim: sw=4 ts=4 et
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_upload).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1
]).

%% #upload allows a user to upload a file.
%% 
%% How it works: - This element creates an <input type=file ...> HTML element
%% on the page, wrapped in a <form>, with all of the required parameters
%% necessary to fake the system into believing it is a real postback call. 
%%
%% - When the user clicks the upload button, first the 'upload_started' event
%% gets fired, calling start_upload_event(Tag) on the Module or Page.
%%
%% - Then, the browser begins uploading the file to the server. The multipart
%% file is parsed in SimpleBridge.
%%
%% - Finally, once the upload is complete, control is passed on to Nitrogen,
%% which reads the parameters sent over in the first step and calls the
%% 'upload_finished' event in this module.
%%
%% - The 'upload_finished' emits Javascript that causes *another* postback,
%% this time to the 'upload_event' event in this module, which then calls
%% Module:finish_upload_event(Tag, OriginalName, TempFile, Node).  The reason
%% we do this extra postback is because the upload itself happens in a form
%% separate from the main Nitrogen page (otherwise the main Nitrogen page would
%% need to refresh) so this is our way of getting the main page to see the
%% event.

-spec reflect() -> [atom()].
reflect() -> record_info(fields, upload).

-spec overall_progress(OverallProgress :: boolean() | undefined | auto,
                       Multiple :: boolean()) -> boolean().
overall_progress(OverallProgress, _Multiple) when is_boolean(OverallProgress) ->
    OverallProgress;
overall_progress(undefined, Multiple) ->
    Multiple;
overall_progress(auto, Multiple) ->
    Multiple.

-spec render_element(#upload{}) -> body().
render_element(Record = #upload{
        id=ID,
        class=Class,
        anchor=Anchor,
        multiple=Multiple,
        overall_progress=OverallProgress0,
        droppable=Droppable,
        droppable_text=DroppableText,
        file_text=FileInputText,
        show_button=ShowButton,
        button_text=ButtonText,
        button_class=ButtonClass,
        data_fields=DataFields,
        aria=Aria }) ->
    
    StartedTag = {upload_started, Record},
    FinishedTag = {upload_finished, Record}, 
    FormID = wf:temp_id(),
    IFrameID = wf:temp_id(),
    ButtonID = wf:temp_id(),
    DropID = wf:temp_id(),
    DropListingID = wf:temp_id(),
    FileInputID = wf:temp_id(),

	Param = [
		{droppable,Droppable},
		{autoupload,not(ShowButton)},
        {multiple, Multiple},
        {overall_progress, overall_progress(OverallProgress0, Multiple)}
	],

	JSONParam = nitro_mochijson2:encode({struct,Param}),
	SubmitJS = wf:f("Nitrogen.$send_pending_files(jQuery('#~s').get(0),jQuery('#~s').get(0));",[FormID,FileInputID]),
    UploadJS = wf:f("Nitrogen.$attach_upload_handle_dragdrop(jQuery('#~s').get(0),jQuery('#~s').get(0),~s);", [FormID,FileInputID,JSONParam]),

    PostbackInfo = wf_event:serialize_event_context(FinishedTag, ID, undefined, false, ?MODULE),

    % Create a postback that is called when the user first starts the upload...
    case ShowButton of
        true ->
            wf:wire(ButtonID, #event {type=click, delegate=?MODULE, postback=StartedTag }),
            wf:wire(ButtonID, #event {type=click, actions=SubmitJS });
        false ->
            % If the button is invisible, then start uploading when the user selects a file.
            wf:wire(Anchor, #event {type=change, delegate=?MODULE, postback=StartedTag })
    end,

    wf:wire(UploadJS),

    %% Two things:
    %% 1) This will resize elements only when they are visible, otherwise the
    %% "Select File" button gets resized to almost invisible.
    %% 2) This object must be prefixed with 'form' since ID and Anchor could be
    %% the same, and we need to differentiate for the form.
    %% (It's a little hacky)
    wf:defer(wf:f("Nitrogen.$recalculate_upload_dimensions($('form~s'));", [ID])),

    WrapperContent = [
        wf_tags:emit_tag(input, [
            {type, button},
            {class, ['upload-button']},
            {value, FileInputText}
        ]),
        wf_tags:emit_tag(input, [
            {name, file},
            {data_fields, DataFields},
            {aria, Aria},
            {class, [no_postback, 'upload-input', FileInputID|Anchor]},
            {id, FileInputID},
            {type, file},
            ?WF_IF(Multiple,multiple,[])
        ])
    ],

    % Render the controls and hidden iframe...
    FormContent = [
        %% IE9 does not support the droppable option, so let's just hide the drop field
        "<!--[if lte IE 9]>
            <style type='text/css'> .upload_drop {display: none} </style>
        <![endif]-->",

        #panel{
            show_if=Droppable,
            id=DropID,
            class=[upload_drop,'dropzone-container'],
            body=[
                #panel{
                    class=[dropzone,'ui-corner-all'],
                    text=DroppableText
                }
            ]
        },
        #panel{
            style="display:none",
            class=upload_overall_progress
        },
        #list{
            id=DropListingID,
            class=upload_droplist
        },

        wf_tags:emit_tag('div', WrapperContent, [
            {class, 'upload-content'}
        ]),

        wf_tags:emit_tag(input, [
            {name, eventContext},
            {type, hidden},
            {class, no_postback},
            {value, PostbackInfo}
        ]),

        wf_tags:emit_tag(input, [
            {name, pageContext},
            {type, hidden},
            {class, no_postback},
            {value, ""}
        ]),

        wf_tags:emit_tag(input, [
            {type, hidden},
            {class, no_postback},
            {value, ""}
        ]),

        #button { id=ButtonID, show_if=ShowButton, text=ButtonText, class=ButtonClass}
    ],

    [
        wf_tags:emit_tag(form, FormContent, [
            {id, FormID},
            {name, upload}, 
            {method, 'POST'},
            {enctype, "multipart/form-data"},
            {class, [no_postback, Class]},
            {target, IFrameID}
        ])
    ].


-spec event(any()) -> any().
% This event is fired when the user first clicks the upload button.
event({upload_started, Record}) ->
    Module = wf:coalesce([Record#upload.delegate, wf:page_module()]),
    Module:start_upload_event(Record#upload.tag);


% This event is called once the upload post happens behind the scenes.
% It happens somewhat outside of Nitrogen, so the next thing we do
% is trigger a postback that happens inside of Nitrogen. 
event({upload_finished, Record}) ->
    wf_context:type(first_request),
    Req = wf_context:bridge(),

    % % Create the postback...
    {Filename,NewTag} = case sbw:post_files(Req) of
        [] -> 
            {undefined,{upload_event, Record, undefined, undefined, undefined}};
        [UploadedFile | _] ->
            OriginalName = sb_uploaded_file:original_name(UploadedFile),
            TempFile = sb_uploaded_file:temp_file(UploadedFile),
            {OriginalName,{upload_event, Record, OriginalName, TempFile, node()}}
    end,

    % Make the tag...
    Anchor = wf_context:anchor(),
    ValidationGroup = wf_context:event_validation_group(),
    HandleInvalid = wf_context:event_handle_invalid(),
    Postback = wf_event:generate_postback_script(NewTag, undefined, Anchor, ValidationGroup, HandleInvalid, undefined, ?MODULE, undefined),

    % Set the response...
    wf_context:data([
        "Nitrogen.$upload_finished(\"",wf:js_escape(Filename),"\");",
        Postback
    ]);

% This event is fired by the upload_finished event, it calls
% back to the page or control that contained the upload element.
event({upload_event, Record, OriginalName, TempFile, Node}) ->
    Module = wf:coalesce([Record#upload.delegate, wf:page_module()]),
    Module:finish_upload_event(Record#upload.tag, OriginalName, TempFile, Node).
