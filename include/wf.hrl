% vim: sw=4 ts=4 et ft=erlang
-ifndef(wf_inc).
-define(wf_inc, ok).
-include("compat.hrl").
-include("wf_test.hrl").
-compile(nowarn_export_all).

%% This is the parse_transform to allow extending fields
-compile({parse_transform, rekt}).

-define(WF_EXTEND(OrigRec, NewRec, Module, Fields), -extend({OrigRec, NewRec, [{module, Module} | Fields]})).


%%% TYPES FOR DIALYZER %%%

%% Allow Dialzer to be run on the .ebin files
-compile(debug_info).

-type nitrogen_element()    :: tuple().
-type template_script_element() :: script | mobile_script.
-type body_element()        :: nitrogen_element() | binary() | string() | iolist() 
                                | template_script_element().
-type body()                :: body_element() | [body_element()].
-type action_element()      :: undefined | tuple() | string() | binary() | iolist().
-type actions()             :: action_element() | [action_element()].
-type validator_element()   :: nitrogen_element().
-type validators()          :: validator_element() | [validator_element()].
-type tag()                 :: term().
-type id()                  :: atom() | string() | binary().
-type proplist()            :: [{term(), term()}].
-type data_field_name()     :: atom() | text().
-type data_field_value()    :: atom() | text().
-type data_fields()         :: [data_field_name() 
                                | {data_field_name()} 
                                | {data_field_name(),data_field_value()}].
-type wire_priority()       :: eager | normal | defer.
-type class()               :: string() | binary() | atom().
-type text()                :: string() | binary() | iolist().
-type html_encode()         :: boolean() | whites | fun((term()) -> text()).
-type html()                :: string() | binary() | iolist().
-type script()              :: string() | binary() | iolist().
-type url()                 :: text() | atom().
-type path()                :: string() | binary().
-type html_name()           :: string() | binary() | atom().
-type mobile_theme()        :: string() | binary() | atom().
-type comet_name()          :: term().
-type comet_restart_msg()   :: term().
-type comet_function()      :: pid() | function() 
                                | {comet_name(), function()}
                                | {comet_name(), function(), comet_restart_msg()}.
-type handler_config()      :: any().
-type handler_state()       :: any().

-type google_chart_type()   :: line | sparkline | stacked_horizontal_bar
                                | stacked_vertical_bar | grouped_horizontal_bar
                                | grouped_vertical_bar | pie | pie3d.
-type color()               :: string() | binary() | atom().
-type google_chart_position()      :: top | left | bottom | right.
-type module_function()     :: {atom(), atom()}.
-type encoding_function()   :: module_function() | fun((iolist()) -> iolist()).
-type encoding()            :: none | unicode | auto | encoding_function().
-type context_data()        :: iolist() | {file, Filename :: path()}
                                | {stream, Size :: integer(), fun()}
                                | {sendfile, integer(), Size :: integer(), Path :: any()}.
-type context_type()        :: first_request | postback_request | static_file | postback_websocket | undefined.
-type mermaid_code()        :: binary() | string() | iolist().
-type mermaid_diagram()     :: flowchart | sequence | gantt.
-type mermaid_diagram_options(Diagram)      :: {Diagram, proplist()}.
%%% CONTEXT %%%

% Page Request Information.
-record(page_context, {
    series_id,   % A unique ID assigned to the first request which stays constant on repeated requests.
    module,      % The requested page module
    entry_point=main, % The entry point for the module. Either an atom name
                      % (for Module:Atom/0) or can be a function arity 0
    path_info,   % Any extra info passed with the request
    async_mode= comet % {poll, Interval}, comet, or {websocket, WebsocketConnectionPid}
}).

% Event Information. A serialized version of this record
% is sent by the browser when an event is called.
-record(event_context, {
    module,     % The module that should handle the event
    tag,        % An Erlang term that is passed along with the event
    type,       % The type of event postback, comet, continuation, upload
    anchor,     % The element id to which trigger and target are relative.
    validation_group, % The validation group that should be run when this event is fired.
    handle_invalid % When an invalidation evaluates to false, instead of silently not
                   % issuing a postback event, call Delegate:event_invalid with the same tag.
}).

% Handlers Context-
% Handlers are used to encapsulate Nitrogen's behaviour, and
% to allow other frameworks to substitute their own behaviour.
% These are set in wf_context:make_context/1
-record(handler_context, {
    name,       % The name of a handler. See wf_context for a list.
    module,     % A module that provides the logic for a handler. This can be substituted by your app.
    config,     % The config of the handler, set at the beginning of each request.
    state       % The state of the handler, serialized and maintained between postbacks in a series.
    %serialize=true % whether or not to serialize and deserialize this handler during requests. Usually, just the state handler needs to be serialized.
}).

-record(context, {
    % Transient Information
    type                    :: context_type(),
    bridge                  :: undefined | simple_bridge:bridge(), %% will only be undefined when "not in a request"
    anchor=undefined        :: id(), 
    data=[]                 :: context_data(),
    encoding=auto           :: encoding(),
    action_queue=undefined  :: wf_action_queue:action_queue() | undefined,
    % These are all serialized, sent to the browser
    % and de-serialized on each request.
    page_context            :: undefined | #page_context{},
    event_context           :: undefined | #event_context{},
    handler_list            :: undefined | list()
}).

%%% LOGGING %%%
-ifndef(debug_print).
-define(debug_print, true).
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p~n~p~n  ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-define(LOG(Msg, Args), error_logger:info_msg(Msg, Args)).
-define(DEBUG, error_logger:info_msg("DEBUG: ~p:~p~n", [?MODULE, ?LINE])).
-endif.

%%% GUARDS %%%

-define (IS_STRING(Term),
    (is_list(Term) andalso Term /= [] andalso is_integer(hd(Term)))).

-define (IS_ACTION_PRIORITY(Priority),
    (Priority=:=normal orelse Priority=:=eager orelse Priority=:=defer)).

-define (PRIORITY_WIRE(Priority),
        (case Priority of
            eager   -> eager;
            defer   -> defer;
            normal  -> wire;
            wire    -> wire
        end)
    ).

%%% TERNARY IF AND VARIATIONS %%%
-define(WF_IF(Term,IfTrue,IfFalse),
    fun() ->
        %% This is wrapped in a fun to contain the leaky-case expression. It
        %% allows for nesting ?WF_IF calls. Though it obviously does incur the
        %% minor overhead of creating an anonymous function then executing it.
        case Term of
            %% We use the long "WF_IF_VALUE" variable to prevent the likelyhood
            %% of a variable naming clash. This will throw some matching or
            %% "shadowing" errors if, in a function, you define a variable
            %% called "WF_IF_VALUE" before calling ?WF_IF. Given that this is
            %% unlikely to happen, this is an acceptable compromise.
            WF_IF_VALUE when WF_IF_VALUE==false;
                         WF_IF_VALUE==undefined;
                         WF_IF_VALUE==[] ->
                IfFalse;
            _ -> 
                IfTrue
        end
    end()).

-define(WF_IF(Term,IfTrue), ?WF_IF(Term,IfTrue,"")).

%%% FRAMEWORK %%%

%%% Elements %%%
-define(ELEMENT_BASE(Module),
        %% attribute is_element should only ever be the atom `is_element` but
        %% because Nitrogen copies the base element when it needs to, it's
        %% possible it might accidentally copy the wrong kind of element that
        %% it may think is an element but its not. In such an event, we allow
        %% any() here, so that it passes the dialyzer warning, and will throw
        %% an error appropriately during runtime.
        %%
        %% If we kept it as just `is_element`, that may be just fine, but then
        %% users would probably have to use dialyzer to debug the "is not an
        %% element" error
        is_element=is_element   :: is_element | any(), 
        module=Module           :: atom(),
        id                      :: id(),
        anchor                  :: id(),
        actions                 :: actions(),
        show_if=true            :: boolean(),
        class=""                :: class() | [class()],
        style=""                :: text(),
        html_id=""              :: id(),
        title=""                :: undefined | text(),
        data_fields=[]          :: data_fields()        
    ).

-record(elementbase, {?ELEMENT_BASE(undefined)}).
-record(template, {?ELEMENT_BASE(element_template),
       file                     :: string(),
       module_aliases=[]        :: [{atom(), atom()}],
       bindings=[]              :: proplist()
    }).
-record(function_el, {?ELEMENT_BASE(element_function),
        function=fun() ->[]end  :: fun() | [fun()]
    }).
-record(body, {?ELEMENT_BASE(element_body),
        body=[]                 :: body()
    }).

-define(H_ELEMENT(Size), {?ELEMENT_BASE(element_h),
        text=""                 :: text(),
        body=[]                 :: body(),
        html_encode=true        :: html_encode(),
        size=Size               :: undefined | integer()
    }).
-record(h,  ?H_ELEMENT(undefined)).
-record(h1, ?H_ELEMENT(1)).
-record(h2, ?H_ELEMENT(2)).
-record(h3, ?H_ELEMENT(3)).
-record(h4, ?H_ELEMENT(4)).
-record(h5, ?H_ELEMENT(5)).
-record(h6, ?H_ELEMENT(6)).

-record(list, {?ELEMENT_BASE(element_list),
        numbered=false          :: boolean(),
        body=[]                 :: body()
    }).
-record(listitem, {?ELEMENT_BASE(element_listitem),
        body=[]                 :: body(),
        text=""                 :: text(),
        html_encode=true        :: html_encode(),
        role=""                 :: term()
    }).
-record(br, {?ELEMENT_BASE(element_br) }).
-record(hr, {?ELEMENT_BASE(element_hr) }).
-record(p, {?ELEMENT_BASE(element_p),
        body=[]                 :: body(),
        text=""                 :: text(),
        html_encode=true        :: html_encode()
    }).
-record(label, {?ELEMENT_BASE(element_label),
        body=[]                 :: body(),
        text=""                 :: text(),
        html_encode=true        :: html_encode(),
        for=""                  :: id()
    }).
-record(pre, {?ELEMENT_BASE(element_pre),
        text=""                 :: text(),
        html_encode=true        :: html_encode()
     }).
-record(strong, {?ELEMENT_BASE(element_strong),
        body=""                 :: body(),
        text=""                 :: text(),
        html_encode=true        :: html_encode()
    }).
-record(em, {?ELEMENT_BASE(element_em),
        body=""                 :: body(),
        text=""                 :: text(),
        html_encode=true        :: html_encode()
    }).
-record(value, {?ELEMENT_BASE(element_value),
        text=""                 :: text(),
        html_encode=true        :: html_encode()
    }).
-record(link, {?ELEMENT_BASE(element_link),
        text=""                 :: text(),
        body=""                 :: body(),
        image=undefined         :: undefined | url(),
        new=false               :: boolean(),
        html_encode=true        :: html_encode(),
        mobile_target=false     :: boolean(),
        mobile_dialog=false     :: boolean(),
        url="javascript:"       :: script() | url(),
        click                   :: actions(),
        postback                :: term(),
        handle_invalid=false    :: boolean(),
        on_invalid              :: undefined | actions(),
        delegate                :: module()
    }).
-record(email_link, {?ELEMENT_BASE(element_email_link),
        text=""                 :: text(),
        body=""                 :: body(),
        html_encode=true        :: html_encode(),
        email=""                :: text()
    }).
-record(error, {?ELEMENT_BASE(element_error),
        text=""                 :: text(),
        html_encode=true        :: html_encode()
    }).
-record(span, {?ELEMENT_BASE(element_span),
        body=""                 :: body(),
        text=""                 :: text(),
        html_encode=true        :: html_encode()
    }).
-record(delay_body, {?ELEMENT_BASE(element_delay_body), 
        delegate                :: module(),
        tag=undefined           :: term(),
        placeholder             :: body(),
        delay=0                 :: integer()  %% milliseconds to wait to populate
    }).
-record(button, {?ELEMENT_BASE(element_button),
        text=""                 :: text(),
        body=""                 :: body(),
        image=undefined         :: undefined | url(),
        html_encode=true        :: html_encode(),
        next                    :: id(),
        click                   :: actions(),
        enter_clicks=[]         :: [id()],
        postback                :: term(),
        disabled=false          :: boolean(),
        handle_invalid=false    :: boolean(),
        on_invalid              :: undefined | actions(),
        delegate                :: module()
    }).
-record(literal, {?ELEMENT_BASE(element_literal),
        text=""                 :: text(),
        html_encode=true        :: html_encode()
    }).
-record(textbox, {?ELEMENT_BASE(element_textbox),
        text=""                 :: text(),
        maxlength=""            :: integer() | string(),
        size=""                 :: integer() | string(),
        placeholder=""          :: text(),
        html_encode=true        :: html_encode(),
        disabled=false          :: boolean(),
        readonly=false          :: boolean(),
        next                    :: id(),
        postback                :: term(),
        handle_invalid=false    :: boolean(),
        on_invalid              :: undefined | actions(),
        delegate                :: module(),
        html_name               :: html_name(),
        type=text               :: string() | atom(),
        autocomplete="off"      :: string() | atom()
    }).
-record(datepicker_textbox, {?ELEMENT_BASE(element_datepicker_textbox),
        text=""                 :: text(),
        maxlength=""            :: integer() | string(),
        size=""                 :: integer() | string(),
        placeholder=""          :: text(),
        html_encode=true        :: html_encode(),
        readonly=false          :: boolean(),
        disabled=false          :: boolean(),
        next                    :: id(),
        handle_invalid=false    :: boolean(),
        on_invalid              :: undefined | actions(),
        html_name               :: html_name(),
        validators=[]           :: validators(),
        options = [{dateFormat, "yy-mm-dd"}]    :: proplist()
    }).
-record(date_dropdown, {?ELEMENT_BASE(element_date_dropdown),
        value=""                :: text() | erlang:date() | {erlang:date(),erlang:time()},
        format=iso              :: ymd | iso | mdy | usa | dmy,
        min_year=undefined      :: undefined | integer(),
        max_year=undefined      :: undefined | integer(),
        month_names=true        :: boolean(),
        month_fun={element_date_dropdown, months} :: {module(), atom()} | fun(),
        wrapperid=undefined     :: id(),
        allow_blank=false	:: boolean()
}).
-record(textbox_autocomplete, {?ELEMENT_BASE(element_textbox_autocomplete),
        tag                     :: term(),
        text=""                 :: text(),
        minLength=2             :: integer(),
        size=""                 :: integer() | string(),
        html_encode=true        :: html_encode(),
        placeholder=""          :: text(),
        disabled=false          :: boolean(),
        readonly=false          :: boolean(),
        delay=300               :: integer(),
        next                    :: id(),
        postback                :: term(),
        handle_invalid=false    :: boolean(),
        on_invalid              :: undefined | actions(),
        delegate                :: module()
    }).
-record(hidden, {?ELEMENT_BASE(element_hidden),
        text=""                 :: text(),
        html_encode=true        :: html_encode(),
        html_name               :: html_name(),
        disabled=false          :: boolean()
    }).
-record(textarea, {?ELEMENT_BASE(element_textarea),
        text=""                 :: text(),
        placeholder=""          :: text(),
        disabled=false          :: boolean(),
        readonly=false          :: boolean(),
        trap_tabs=false         :: boolean(),
        next                    :: id(),
        columns                 :: undefined | integer(),
        rows                    :: undefined | integer(),
        html_encode=true        :: html_encode(),
        html_name               :: html_name()
    }).
-record(range, {?ELEMENT_BASE(element_range),
        min=0                   :: integer(),
        max=100                 :: integer(),
        step=1                  :: integer(),
        value=0                 :: integer(),
        next                    :: id(),
        postback                :: term(),
        handle_invalid=false    :: boolean(),
        on_invalid              :: undefined | actions(),
        delegate                :: module()
    }).

-record(option, {
        text=""                 :: text(),
        value=undefined         :: text() | atom() | integer(),
        selected                :: boolean() | undefined,
        show_if=true            :: boolean(),
        disabled=false          :: boolean()
    }).

-type short_option()        :: {text() | atom() | integer(), text()} | text().
-record(option_group, {
        text=""                 :: text(),
        options=[]              :: [#option{} | short_option()],
        show_if=true            :: boolean(),
        disabled=false          :: boolean()
    }).

-type options()             :: [#option{} | short_option()] | [#option_group{}].
-record(dropdown, {?ELEMENT_BASE(element_dropdown),
        options=[]              :: undefined | options(),
        size=auto               :: auto | integer(),
        html_encode=true        :: html_encode(),
        postback                :: term(),
        handle_invalid=false    :: boolean(),
        on_invalid              :: undefined | actions(),
        delegate                :: module(),
        value                   :: atom() | text() | integer(),
        next                    :: id(),
        multiple=false          :: boolean(),
        disabled=false          :: boolean(),
        html_name               :: html_name()
    }).

-record(checkbox, {?ELEMENT_BASE(element_checkbox),
        text=""                 :: text(),
        label_position='after'  :: 'after' | before | none,
        html_encode=true        :: html_encode(),
        checked=false           :: boolean(),
        value="on"              :: text(),
        next                    :: id(),
        postback                :: term(),
        handle_invalid=false    :: boolean(),
        disabled=false          :: boolean(),
        on_invalid              :: undefined | actions(),
        delegate                :: module(),
        html_name               :: html_name()
    }).
-record(radiogroup, {?ELEMENT_BASE(element_radiogroup),
        body=[]                 :: body()
    }).
-record(radio, {?ELEMENT_BASE(element_radio),
        body=""                 :: body(),
        text=""                 :: text(),
        html_encode=true        :: html_encode(),
        value                   :: text(),
        label_class             :: class() | [class()],
        next                    :: id(),
        name                    :: html_name(),
        checked=false           :: boolean(),
        postback                :: term(),
        handle_invalid=false    :: boolean(),
        on_invalid              :: undefined | actions(),
        delegate                :: module(),
        html_name               :: html_name(),
        disabled=false          :: boolean()
    }).
-record(password, {?ELEMENT_BASE(element_password),
        text=""                 :: text(),
        maxlength=""            :: integer() | string(),
        size=""                 :: integer | string(),
        placeholder=""          :: text(),
        html_encode=true        :: html_encode(),
        disabled=false          :: boolean(),
        readonly=false          :: boolean(),
        next                    :: id(),
        postback                :: term(),
        handle_invalid=false    :: boolean(),
        on_invalid              :: undefined | actions(),
        delegate                :: module(),
        html_name               :: html_name()
    }).
-record(restful_form, {?ELEMENT_BASE(element_restful_form),
        method="POST"           :: string() | atom(),
        action                  :: url() | undefined,
        html_name               :: html_name(),
        target                  :: string() | atom(),
        enctype                 :: text() | undefined,
        body=[]                 :: body()
      }).
-record(restful_submit, {?ELEMENT_BASE(element_restful_submit),
        text="Submit"           :: text(),
        body=[]                 :: body(),
        html_encode=true        :: html_encode(),
        html_name               :: html_name()
    }).
-record(restful_reset, {?ELEMENT_BASE(element_restful_reset),
        text="Cancel"           :: text(),
        html_encode=true        :: html_encode(),
        html_name               :: html_name()
    }).
-record(restful_upload, {?ELEMENT_BASE(element_restful_upload),
        html_encode=true        :: html_encode(),
        html_name               :: html_name()
    }).
-record(panel, {?ELEMENT_BASE(element_panel),
        body=[]                 :: body(),
        text=""                 :: text(),
        html_encode=true        :: html_encode()
    }).
-record(sync_panel, {?ELEMENT_BASE(element_sync_panel),
        body_fun                :: undefined | fun(),
        triggers=[]             :: [term()],
        pool=sync_panel         :: atom()
    }).
-record(fieldset, {?ELEMENT_BASE(element_fieldset),
        body=[]                 :: body(),
        text=""                 :: text(),
        html_encode=true        :: html_encode(),
        legend_body=[]          :: body(),
        legend_text=""          :: text(),
        legend_html_encode=true :: html_encode()
    }).
-record(spinner, {?ELEMENT_BASE(element_spinner),
        image="/nitrogen/spinner.gif"   :: url()
    }).
-record(image, {?ELEMENT_BASE(element_image),
        image=""                :: url(),
        alt=""                  :: text(),
        width                   :: undefined|integer(),
        height                  :: undefined|integer()
    }).
-record(video, {?ELEMENT_BASE(element_video),
        url=""                  :: url(),
        poster_url=""           :: url(),
        width=560               :: integer(),
        height=315              :: integer(),
        preload=metadata        :: auto | metadata | none,
        loop=false              :: boolean(),
        muted=false             :: boolean(),
        autoplay=false          :: boolean(),
        controls=false          :: boolean(),
        body_no_support="Your browser does not support the video tag" :: body()
    }).
-record(youtube, {?ELEMENT_BASE(element_youtube),
        width=560               :: integer(),
        height=315              :: integer(),
        key=""                  :: text(),
        allowfullscreen=true    :: boolean()
    }).
-record(lightbox, {?ELEMENT_BASE(element_lightbox),
        body=[]                 :: body()
    }).
-record(table, {?ELEMENT_BASE(element_table),
        rows                    :: body(),
        border = 0              :: integer(),
        header=[]               :: body(),
        footer=[]               :: body()
    }).
-record(tablerow, {?ELEMENT_BASE(element_tablerow),
        cells                   :: body()
    }).
-record(tableheader, {?ELEMENT_BASE(element_tableheader),
        text=""                 :: text(),
        html_encode=true        :: html_encode(),
        body=[]                 :: body(),
        align="left"            :: text() | atom(),
        valign="middle"         :: text() | atom(),
        colspan=1               :: integer(),
        rowspan=1               :: integer()
    }).
-record(tablecell, {?ELEMENT_BASE(element_tablecell),
        text=""                 :: text(),
        html_encode=true        :: html_encode(),
        body=""                 :: body(),
        align="left"            :: text() | atom(),
        valign="middle"         :: text() | atom(),
        colspan=1               :: integer(),
        rowspan=1               :: integer()
    }).
-record(singlerow, {?ELEMENT_BASE(element_singlerow),
        cells                   :: body()
    }).
-record(file, {?ELEMENT_BASE(element_file),
        file                    :: path(),
        include_panel=true      :: boolean(),
        html_encode=false       :: boolean()
    }).
-record(flash, {?ELEMENT_BASE(element_flash)}).
-record(placeholder, {?ELEMENT_BASE(element_placeholder),
        body=[]                 :: body()
    }).
-record(bind, {?ELEMENT_BASE(element_bind),
        data=[]                 :: list(),
        map=[]                  :: list() | tuple(),
        transform               :: fun() | undefined,
        acc=[]                  :: term(),
        body=[]                 :: body(),
        empty_body=[]           :: body()
    }).
-record(sortblock, {?ELEMENT_BASE(element_sortblock),
        tag                     :: term(),
        items=[]                :: body(),
        group                   :: atom() | string() | binary(),
        connect_with_groups=none :: atom() | string() | binary() | [atom() | string() | binary()],
        handle                  :: class(),
        placeholder=""          :: class(),
        force_placeholder_size=false :: boolean(),
        delegate                :: module()
    }).
-record(sortitem, {?ELEMENT_BASE(element_sortitem),
        tag                     :: term(),
        body=[]                 :: body()
    }).
-record(draggable, {?ELEMENT_BASE(element_draggable),
        tag                     :: term(),
        body=[]                 :: body(),
        group                   :: atom() | string() | binary(),
        handle                  :: class(),
        clone=true              :: boolean(),
        revert=true             :: boolean() | valid | invalid,
        scroll=true             :: boolean(),
        container               :: atom() | binary() | string(),
        zindex                  :: integer() | undefined,
        distance=3              :: integer(),
        options=[]              :: proplist()
    }).
-record(droppable, {?ELEMENT_BASE(element_droppable),
        tag                     :: term(),
        body=[]                 :: body(),
        accept_groups=all       :: atom() | string() | binary() | [atom() | string() | binary()],
        active_class=active     :: class(),
        hover_class=hover       :: class(),
        delegate                :: module()
    }).
-record(gravatar, {?ELEMENT_BASE(element_gravatar),
        email=""                :: text(),
        size="80"               :: integer() | text(),
        rating="g"              :: text(),
        default=""              :: text()
    }).

-record(inplace_textarea, {?ELEMENT_BASE(element_inplace_textarea),
        tag                     :: term(),
        text=""                 :: text(),
        html_encode=true        :: html_encode(),
        start_mode=view         :: view | edit,
        validators=[]           :: validators(),
        delegate                :: module()
    }).
-record(inplace_textbox, {?ELEMENT_BASE(element_inplace_textbox),
        tag                     :: term(),
        text=""                 :: text(),
        html_encode=true        :: html_encode(),
        start_mode=view         :: view | edit,
        validators=[]           :: validators(),
        delegate                :: module()
    }).
-record(inplace, {?ELEMENT_BASE(element_inplace),
        tag                     :: term(),
        text=""                 :: text(),
        delegate                :: module(),
        view                    :: body(),
        edit                    :: body(),
        start_mode=view         :: view | edit
    }).

-record(upload, {?ELEMENT_BASE(element_upload),
        delegate                :: module(),
        tag                     :: term(),
        show_button=true        :: boolean(),
        file_text="Select file" :: text(),
        button_text="Upload"    :: text(),
        button_class=""         :: class() | [class()],
        droppable=false         :: boolean(),
        droppable_text="Drop Files Here" :: text(),
        multiple=false          :: boolean(),
        overall_progress=auto   :: boolean() | undefined | auto
    }).
-record(wizard, {?ELEMENT_BASE(element_wizard),
        tag                     :: term(),
        titles                  :: undefined | [text()],
        steps=[]                :: [body()],
        next="Next"             :: text(),
        back="Back"             :: text(),
        finish="Finish"         :: text(),
        show_progress=true      :: boolean(),
        progress_text="(Step ~p of ~p)" :: text(),
        next_class=""           :: text() | [text()],
        back_class=""           :: text() | [text()],
        finish_class=""         :: text() | [text()]
    }).
-record(sparkline, {?ELEMENT_BASE(element_sparkline),
        type=line               :: line | bar | tristate | bullet | discrete | pie | box,
        values=[]               :: [integer() | float() | null | undefined],
        options=[]              :: proplist()
    }).
-record(recaptcha, {?ELEMENT_BASE(element_recaptcha),
        captcha_opts=[]         :: proplist(),
        button_id               :: id(),
        button_label="Check!"   :: text(),
        button_class            :: class(),
        delegate                :: module(),
        tag                     :: term(),
        fail_body="Please try again!" :: body(),
        public_key              :: string() | undefined,
        private_key             :: string() | undefined,
        challenge_url           :: url() | undefined,
        verify_url              :: url() | undefined
    }).

%% Mobile Shortcut elements
-record(mobile_list, {?ELEMENT_BASE(element_mobile_list),
        body=[]                 :: body(),
        theme                   :: mobile_theme(),
        inset=true              :: boolean()
     }).
-record(mobile_list_divider, {?ELEMENT_BASE(element_mobile_list_divider),
        theme                   :: mobile_theme(),
        text=""                 :: text(),
        body=[]                 :: body(),
        role=heading            :: atom() | string()
     }).
-record(mobile_listitem, {?ELEMENT_BASE(element_mobile_listitem),
        text=""                 :: text(),
        body=[]                 :: body(),
        theme=""                :: atom() | string()
    }).
-record(mobile_toggle, {?ELEMENT_BASE(element_mobile_toggle),
        on_text="On"            :: text(),
        on_value="on"           :: text(),
        off_text="Off"          :: text(),
        off_value="off"         :: text(),
        selected="on"           :: atom() | text(),
        postback                :: term(),
        handle_invalid=false    :: boolean(),
        on_invalid              :: undefined | actions(),
        delegate                :: module(),
        width                   :: integer() | undefined,
        theme=""                :: mobile_theme()
    }).
-record(mobile_collapsible, {?ELEMENT_BASE(element_mobile_collapsible),
        header_theme            :: mobile_theme(),
        content_theme           :: mobile_theme(),
        header_text=""          :: text(),
        content_body=[]         :: body(),
        content_text=""         :: text(),
        header_size=3           :: integer(),
        mini=false              :: boolean(),
        collapsed=true          :: boolean()
    }).
-record(mobile_collapsible_set, {?ELEMENT_BASE(element_mobile_collapsible_set),
        header_theme            :: mobile_theme(),
        content_theme           :: mobile_theme(),
        mini=false              :: boolean(),
        body=[]                 :: body()
    }).
-record(mobile_grid_block, {?ELEMENT_BASE(element_mobile_grid_block),
        text=""                 :: text(),
        body=[]                 :: body(),
        new_row=default         :: atom()
    }).
-record(mobile_grid, {?ELEMENT_BASE(element_mobile_grid),
        columns=2               :: integer(),
        blocks=[]               :: [#mobile_grid_block{}]
    }).
-record(mobile_panel, {?ELEMENT_BASE(element_mobile_panel),
        mini=false              :: boolean(),
        position=left           :: left | right,
        dismissible=true        :: boolean(),
        theme                   :: mobile_theme(),
        display_mode=reveal     :: reveal | overlay | push,
        body=[]                 :: body()
    }).
-record(iframe, {?ELEMENT_BASE(element_iframe),
        align                   :: text() | atom(),
        frameborder             :: integer() | undefined,
        height                  :: integer(),
        name=""                 :: text(),
        sandbox=""              :: text(),
        seamless                :: atom() | text(),
        src                     :: url(),
        srcdoc=""               :: text(),
        width                   :: integer(),
        allowfullscreen=true    :: boolean()
    }).
        
%% HTML5 semantic elements
-record(section, {?ELEMENT_BASE(element_section),
        body=""                 :: body(),
        role=""                 :: text()
    }).
-record(nav, {?ELEMENT_BASE(element_nav),
        body=""                 :: body(),
        role=""                 :: text()
    }).
-record(article, {?ELEMENT_BASE(element_article),
        body=""                 :: body(),
        role=""                 :: text()
    }).
-record(aside, {?ELEMENT_BASE(element_aside),
        body=""                 :: body(),
        role=""                 :: text()
    }).
-record(html5_header, {?ELEMENT_BASE(element_html5_header),
        body=""                 :: body(),
        role=""                 :: text()
    }).
-record(html5_footer, {?ELEMENT_BASE(element_html5_footer),
        body=""                 :: body(),
        role=""                 :: text()
    }).
-record(time, {?ELEMENT_BASE(element_time),
        datetime=""             :: text(),
        body=""                 :: body(),
        text=""                 :: text(),
        html_encode=true        :: html_encode(),
        role=""                 :: text()
    }).
-record(mark, {?ELEMENT_BASE(element_mark),
        body=""                 :: body(),
        text=""                 :: text(),
        html_encode=true        :: html_encode(),
        role=""                 :: text()
    }).
-record(main, {?ELEMENT_BASE(element_main),
        body=""                 :: body(),
        role=""                 :: text()
    }).

%% 960.gs Grid

-define(GRID_ELEMENT(Type, Columns), {?ELEMENT_BASE(element_grid),
        type=Type               :: undefined | container | grid | clear,
        columns=Columns         :: undefined | integer(),
        alpha=false             :: boolean(),
        omega=false             :: boolean(),
        push                    :: integer() | undefined,
        pull                    :: integer() | undefined,
        prefix                  :: integer() | undefined,
        suffix                  :: integer() | undefined,
        body=[]                 :: body()
    }).

-record(grid,           ?GRID_ELEMENT(undefined, undefined)).
-record(container_12,   ?GRID_ELEMENT(container,12)).
-record(container_16,   ?GRID_ELEMENT(container,16)).
-record(grid_1,         ?GRID_ELEMENT(grid, 1)).
-record(grid_2,         ?GRID_ELEMENT(grid, 2)).
-record(grid_3,         ?GRID_ELEMENT(grid, 3)).
-record(grid_4,         ?GRID_ELEMENT(grid, 4)).
-record(grid_5,         ?GRID_ELEMENT(grid, 5)).
-record(grid_6,         ?GRID_ELEMENT(grid, 6)).
-record(grid_7,         ?GRID_ELEMENT(grid, 7)).
-record(grid_8,         ?GRID_ELEMENT(grid, 8)).
-record(grid_9,         ?GRID_ELEMENT(grid, 9)).
-record(grid_10,        ?GRID_ELEMENT(grid, 10)).
-record(grid_11,        ?GRID_ELEMENT(grid, 11)).
-record(grid_12,        ?GRID_ELEMENT(grid, 12)).
-record(grid_13,        ?GRID_ELEMENT(grid, 13)).
-record(grid_14,        ?GRID_ELEMENT(grid, 14)).
-record(grid_15,        ?GRID_ELEMENT(grid, 15)).
-record(grid_16,        ?GRID_ELEMENT(grid, 16)).
-record(grid_clear,     ?GRID_ELEMENT(clear, undefined)).

-record(progress_bar,   {?ELEMENT_BASE(element_progress_bar),
        value=undefined         :: integer() | undefined,
        max=100                 :: integer(),
        color = <<"909090">>    :: color(),
        label                   :: undefined | percent | ratio | both | string()
    }).

%% Google Charts
-record(chart_axis, {
        position                :: google_chart_position(),
        labels                  :: undefined | [text()],
        color = <<"909090">>    :: color(),
        font_size=10            :: integer()
    }).

-record(chart_data, {
        color                   :: color(),
        legend                  :: text(),
        values                  :: [text() | integer()],
        min_value=0             :: integer(),
        max_value=100           :: integer(),
        line_width=1            :: integer(),
        line_length=1           :: integer(),
        blank_length=0          :: integer()
    }).
-record(google_chart,   {?ELEMENT_BASE(element_google_chart),
        type=line               :: google_chart_type(),
        color = <<"909090">>    :: color(),
        font_size=10            :: integer(),
        width=300               :: integer(),
        height=150              :: integer(),
        axes=[]                 :: undefined | [#chart_axis{}],
        data=[]                 :: undefined | [#chart_data{}],
        grid_x=undefined        :: undefined | integer(),
        grid_y=undefined        :: undefined | integer(),
        grid_line_length=1      :: integer(),
        grid_blank_length=5     :: integer(),
        background_color=ffffff :: color(),
        chart_color=ffffff      :: color(),
        legend_location=bottom  :: google_chart_position(),
        bar_space=3             :: integer(),
        bar_group_space=7       :: integer()
    }).
-record(qr, {?ELEMENT_BASE(element_qr),
        data=undefined          :: any(),
        size=200                :: integer()
    }).

-record(mermaid, {?ELEMENT_BASE(element_mermaid),
        code=[]                 :: mermaid_code(),
        options=[]              :: proplist(),
        diagram_options=undefined      :: undefined | mermaid_diagram_options(mermaid_diagram())
    }).

%%% Actions %%%
-define(AV_BASE(Module,Type),
   is_action=Type               :: is_action | is_validator,
   module=Module                :: module(),
   anchor                       :: id(),
   trigger                      :: id(),
   target                       :: id(),
   actions                      :: actions(),
   show_if=true                 :: boolean(),
   dependency_js                :: url() | undefined
).

-define(ACTION_BASE(Module), ?AV_BASE(Module,is_action)).

-record(actionbase, {?ACTION_BASE(undefined)}).
-record(wire, {?ACTION_BASE(action_wire)}).
-record(click, {?ACTION_BASE(action_click)}).

-define(ACTION_UPDATE(Type), {?ACTION_BASE(action_update),
        type=Type               :: atom(),
        elements=[]             :: body()
    }).

%% #update records and its derivitives should all use the same template to
%% ensure simple conversion performed by action_update:render_action/1
-record(update, ?ACTION_UPDATE(update)).
-record(replace, ?ACTION_UPDATE(replace)).
-record(insert_top, ?ACTION_UPDATE(insert_top)).
-record(insert_bottom, ?ACTION_UPDATE(insert_bottom)).
-record(insert_before, ?ACTION_UPDATE(insert_before)).
-record(insert_after, ?ACTION_UPDATE(insert_after)).
-record(remove, ?ACTION_UPDATE(remove)).

-record(comet, {?ACTION_BASE(action_comet),
        pool=undefined          :: term(),
        scope=local             :: local | global,
        function                :: comet_function(),
        dying_message           :: term(),
        reconnect_actions       :: actions()
    }).
-record(continue, {?ACTION_BASE(action_continue),
        function                :: undefined | fun(),
        delegate                :: module(),
        tag                     :: term(),
        timeout                 :: integer() | infinity | undefined
    }).
-record(api, {?ACTION_BASE(action_api),
        name                    :: string() | binary() | atom(),
        tag                     :: term(),
        delegate                :: module()
    }).
-record(function, {?ACTION_BASE(action_function),
        function                :: fun() | undefined
    }).
-record(js_fun, {?ACTION_BASE(action_js_fun),
        function                :: atom() | text(),
        args=[]                 :: [text()]
    }).
-record(set, {?ACTION_BASE(action_set),
        value=""                :: text() | integer()
    }).
-record(set_multiple, {?ACTION_BASE(action_set_multiple),
        values=[]               :: [text()]
    }).
-record(redirect, {?ACTION_BASE(action_redirect),
        url=""                  :: url(),
        login=false             :: boolean() | url()
    }).
-record(open_window, {?ACTION_BASE(action_open_window),
        url=""                  :: url(),
        height                  :: integer() | undefined,
        width                   :: integer() | undefined,
        left                    :: integer() | undefined,
        top                     :: integer() | undefined,
        menubar=true            :: boolean(),
        statusbar=true          :: boolean(),
        titlebar=true           :: boolean(),
        name='_blank'           :: '_blank' | '_parent' | '_self' | '_top' | text(),
        options=[]
    }).
-record(event, {?ACTION_BASE(action_event),
        type=default            :: atom(),
        keycode=undefined       :: integer() | undefined,
        shift_key=false         :: boolean(),
        delay=0                 :: integer(),
        postback                :: term(),
        handle_invalid=false    :: boolean(),
        on_invalid              :: undefined | actions(),
        validation_group        :: string() | binary() | atom(),
        delegate                :: module(),
        extra_param             :: string() | binary() | undefined
    }).
-record(before_postback, {?ACTION_BASE(action_before_postback),
        script=""               :: string()
    }).
%% we want validation assignments to happen last, so we use AV_BASE and set deferral to zero first
-record(validate, {?ACTION_BASE(action_validate),
        on=submit               :: atom(),
        success_text=" "        :: text(),
        group                   :: string() | binary() | atom(),
        validators              :: validators(),
        attach_to               :: id()
    }).
-record(validation_error, {?ACTION_BASE(action_validation_error),
        text=""                 :: text(),
        attach_to               :: id()
    }).
-record(clear_validation, {?ACTION_BASE(action_clear_validation),
        validation_trigger      :: id(),
        validation_target       :: id(),
        validation_all          :: id()
    }).
-record(alert, {?ACTION_BASE(action_alert),
        text=""                 :: text()
    }).
-record(confirm, {?ACTION_BASE(action_confirm),
        text=""                 :: text(),
        postback                :: term(),
        delegate                :: module()
    }).
-record(console_log, {?ACTION_BASE(action_console_log),
        text=""                 :: any()
    }).
-record(script, {?ACTION_BASE(action_script),
        script                  :: text()
    }).
-record(disable_selection, {?ACTION_BASE(action_disable_selection)}).
-record(disable_option, {?ACTION_BASE(action_toggle_option),
        value                   :: atom() | string() | binary() | integer()
}).
-record(enable_option, {?ACTION_BASE(action_toggle_option),
        value                   :: atom() | string() | binary() | integer()
}).
-record(add_option, {?ACTION_BASE(action_add_option),
        option                  :: #option{} | short_option(),
        location=bottom         :: top | bottom
}).
-record(remove_option, {?ACTION_BASE(action_remove_option),
        value                   :: atom() | string() | binary() | integer()
}).
-record(jquery_effect, {?ACTION_BASE(action_jquery_effect),
        type                    :: atom() | string() | binary(),
        effect                  :: atom() | string() | binary(),
        speed                   :: integer(),
        options=[]              :: proplist(),
        class                   :: class(),
        easing                  :: atom()
    }).
-record(show, {?ACTION_BASE(action_show),
        effect=none             :: atom(),
        options=[]              :: proplist(),
        speed=500               :: integer()
    }).
-record(hide, {?ACTION_BASE(action_hide),
        effect=none             :: atom(),
        options=[]              :: proplist(),
        speed=500               :: integer()
    }).
-record(appear, {?ACTION_BASE(action_appear),
        speed=500               :: integer()
    }).
-record(fade, {?ACTION_BASE(action_fade),
        speed=500               :: integer()
    }).
-record(slide_down, {?ACTION_BASE(action_slide_down),
        speed=500               :: integer()
    }).
-record(slide_up, {?ACTION_BASE(action_slide_up),
        speed=500               :: integer()
    }).
-record(effect, {?ACTION_BASE(action_effect),
        effect=none             :: atom(),
        options=[]              :: proplist(),
        speed=500               :: integer()
    }).
-record(toggle, {?ACTION_BASE(action_toggle),
        effect=none             :: atom(),
        options=[]              :: proplist(),
        speed=500               :: integer()
    }).
-record(toggle_mobile_panel, {?ACTION_BASE(action_toggle_mobile_panel)}).
-record(add_class, {?ACTION_BASE(action_add_class),
        class=none              :: class(),
        speed=0                 :: integer()
    }).
-record(remove_class, {?ACTION_BASE(action_remove_class),
        class=none              :: class(),
        speed=0                 :: integer()
    }).
-record(animate, {?ACTION_BASE(action_animate),
        options=[]              :: proplist(),
        speed=500               :: integer(),
        easing=swing            :: atom()
    }).
-record(buttonize, {?ACTION_BASE(action_buttonize)}).
-record(disable, {?ACTION_BASE(action_disable)}).
-record(enable, {?ACTION_BASE(action_enable)}).
-record(make_readonly, {?ACTION_BASE(action_make_readonly)}).
-record(make_writable, {?ACTION_BASE(action_make_writable)}).
-record(set_cookie, {?ACTION_BASE(action_set_cookie),
        cookie                  :: atom() | text(),
        value=""                :: atom() | text(),
        path="/"                :: text(),
        domain=undefined        :: undefined | text(),
        minutes_to_live=20      :: integer(),
        secure=false            :: boolean(),
        http_only=false         :: boolean()
    }).

%%% Validators %%%
%%% %% TODO: Switch this from is_action to is_validator once deferred is implemented
%%% This will allow users to bind validators directly, instead of needing the #validate{} action
-define(VALIDATOR_BASE(Module), ?AV_BASE(Module,is_action),
    text="Failed."              :: text(),
    attach_to                   :: undefined | id()
).
-record(validatorbase, {?VALIDATOR_BASE(undefined)}).
-record(is_required, {?VALIDATOR_BASE(validator_is_required),
        unless_has_value        :: undefined | [id()]
    }).
-record(is_email, {?VALIDATOR_BASE(validator_is_email)}).
-record(is_integer, {?VALIDATOR_BASE(validator_is_integer),
        min                     :: undefined | integer(),
        max                     :: undefined | integer(),
        allow_blank=false       :: boolean()
    }).
-record(is_number, {?VALIDATOR_BASE(validator_is_number),
        min                     :: undefined | integer(),
        max                     :: undefined | integer(),
        allow_blank=false       :: boolean()
    }).
-record(min_length, {?VALIDATOR_BASE(validator_min_length),
        length                  :: undefined | integer()
    }).
-record(max_length, {?VALIDATOR_BASE(validator_max_length),
        length                  :: undefined | integer()
    }).
-record(confirm_password, {?VALIDATOR_BASE(validator_confirm_password),
        password                :: id()
    }).
-record(confirm_same, {?VALIDATOR_BASE(validator_confirm_same),
        confirm_id              :: id()
    }).
-record(custom, {?VALIDATOR_BASE(validator_custom),
        function                :: fun(),
        tag                     :: term()
    }).
-record(js_custom, {?VALIDATOR_BASE(validator_js_custom),
        function                :: atom() | script(),
        args="{}"               :: text(),
        when_empty=false        :: boolean()
    }).
-record(if_value, {?ACTION_BASE(action_if_value),
        value                   :: atom() | text(),
        map                     :: undefined | [{atom() | text(), actions()}],
        else=[]                 :: actions()
    }).

-endif.
