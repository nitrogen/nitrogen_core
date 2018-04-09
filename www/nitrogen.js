// vim: sw=4 ts=4 et
// The idea is to have one high level
// Nitrogen object, created from NitrogenClass, that 
// encapsulates everything in order to prevent collisions.

var nitrogen_jqm_loaded=false;

function NitrogenClass(o) {
    this.$url = document.location.href;
    this.$div = document;
    this.$sleep_autoreconnect_interval = 25000;
    this.$last_reconnection_interval_time = 0;
    this.$anchor_root_path = document;
    this.$params = new Object();
    this.$event_queue = new Array();
    this.$event_is_running = false;
    this.$event_success_fun = null;
    this.$event_error_fun = null;
    this.$event_data_type = "text";
    this.$event_obj = null;
    this.$event_timeout = null;
    this.$system_event_queue = new Array();
    this.$system_event_is_running = false;
    this.$system_event_obj = null;
    this.$system_event_timeout = null;
    this.$system_reconnection_events = Array();
    this.$last_system_event = null;
    this.$going_away = false;
    this.$maybe_going_away = false;
    this.$live_validation_data_field = "LV_live_validation";
    this.$before_postback_list = new Array();
    this.$js_dependencies = new Array();
    this.$websocket = null;
    this.$websockets_enabled = false; // default to off
    this.$websockets_ever_succeeded = false;
    this.$websocket_reconnect_timer = null;
    this.$disconnected = false;
    this.$allow_redirect = true;
    this.$redirect_prompt = "Are you sure you want to leave?"
    return this;
}


/*** EVAL CONTROL ***/

// Control the evaulation yourself. If you'd rather capture the responses on
// your own and evaulate them at your control, you would rebind Nitrogen.$eval.
// After loading nitrogen.js, add:
// 
//   <script>
//     NitrogenClass.prototype.$eval = function(data) {
//       ... do your own evaluation ...
//     }
//   </script>
NitrogenClass.prototype.$eval = function(data) {
    return eval(data);
}

/*** PRIVATE METHODS ***/


NitrogenClass.prototype.$path_alias = function(path) {
    if (path === 'page') {
        return document;
    } else {
        return path;
    }
}

NitrogenClass.prototype.$anchor = function(anchor, target) {
    this.$anchor_path = this.$path_alias(anchor);
    this.$target_path = this.$path_alias(target);
}


NitrogenClass.prototype.$anchor_root = function(anchor_root) {
    this.$anchor_root_path = anchor_root;
}

NitrogenClass.prototype.$set_param = function(key, value) {
    this.$params[key] = value;
}

NitrogenClass.prototype.$destroy = function() {
    document.comet_started = false;
    this.$going_away = true;

    // Clear the system event queue and abort any pending system events.
    this.$system_event_queue = new Array();
    if( this.$system_event_obj !== null ) {
        this.$system_event_obj.abort();
    }
    if(this.$websocket) {
        this.$websocket.close();
    }
    this.$system_event_is_running = false;

    // Let the event loop keep running until the event queue is empty. 
}


/*** EVENT QUEUE ***/

NitrogenClass.prototype.$queue_event = function(validationGroup, onInvalid, eventContext, extraParam, ajaxSettings) {
    // Put an event on the event_queue.
    this.$event_queue.push({
        validationGroup : validationGroup,
        onInvalid       : onInvalid,
        eventContext    : eventContext,
        extraParam      : extraParam,
        ajaxSettings    : ajaxSettings
    });
}

NitrogenClass.prototype.$queue_system_event = function(eventContext) {
    // Put an event on the event_queue.
    this.$system_event_queue.push({
        eventContext : eventContext
    });
}

NitrogenClass.prototype.$requeue_last_system_event = function() {
    // This event never ran, so we need to put it at the beginning of the queue
    this.$system_event_queue.unshift(this.$last_system_event);
    //this.$last_system_event = null;
}

NitrogenClass.prototype.$requeue_last_event = function() {
    this.$event_queue.unshift(this.$last_event);
}

NitrogenClass.prototype.$is_disconnected = function() {
    return this.$disconnected;
};

NitrogenClass.prototype.$set_disconnected = function(disconnected) {
    this.$disconnected = disconnected;
    if(disconnected) {
        this.$show_disconnected_notice();
    }
    else {
        this.$hide_disconnected_notice_worked();
    }
};

NitrogenClass.prototype.$register_system_reconnection_event = function(tag, fun) {
    this.$system_reconnection_events.push({tag:tag, fun:fun});
};

NitrogenClass.prototype.$cancel_system_reconnection_event = function(tag) {
    for(var i=0; i<this.$system_reconnection_events.length; i++) {
        if(this.$system_reconnection_events[i].tag==tag) {
            this.$system_reconnection_events.splice(i, 1);
            return;
        }
    }
};

NitrogenClass.prototype.$reconnect_system = function() {
    if(this.$is_disconnected()) {
        for(var i=0; i<this.$system_reconnection_events.length; i++) {
            this.$system_reconnection_events[i].fun();
        }
        this.$set_disconnected(false);
        this.$system_reconnection_events = [];
    }
}

NitrogenClass.prototype.$event_loop = function() {
    // Create a local copy of this for setTimeout callbacks.
    var this2 = this;

    var now = new Date().getTime();

    // If a system event has been running for longer than 20 seconds, then something is wrong - kill the event
    if(this.$system_event_is_running && this.$system_event_obj !== null && now > this.$system_event_started + 20000) {
        this.$cancel_system_event();
    }

    // Similarly, if a normal event has been running fir longer than 20 seconds, kill it too
    if(this.$event_is_running && this.$event_obj !== null && now > this.$event_started + 20000) {
        this.$cancel_event();
    }

    // If no events are running and an event is queued, then fire it.
    if (!this.$system_event_is_running && this.$system_event_queue.length > 0) {
        var o = this.$system_event_queue.shift();
        this.$last_system_event = o;
        this.$system_event_started = now;
        this.$do_system_event(o.eventContext);
    }

    // If no events are running and an event is queued, then fire it.
    if (!this.$event_is_running && this.$event_queue.length > 0) {
        var o = this.$event_queue.shift();
        this.$last_event = o;
        this.$event_started = now;
        this.$do_event(o.validationGroup, o.onInvalid, o.eventContext, o.extraParam, o.ajaxSettings);
    }

    if (this.$system_event_queue.length == 0 || this.$event_queue.length == 0) {
        if( this.$going_away ) {
            // $destroy has been called for this Nitrogen object
            // and the event queue is empty - let the event loop stop.
            return;
        }
        else {
            // No more events, sleep for 50 ms...
            setTimeout( function() { this2.$event_loop() }, 50);
            return;
        }
    }

    // Events queued, but one is running, sleep for 10 ms...
    if (this.$event_is_running || this.$system_event_is_running) {
        setTimeout( function() { this2.$event_loop() }, 10);
        return;
    }

    // Events queued, loop and grab it...
    setTimeout( function() { this2.$event_loop() }, 1);
}

/*** NOTICE BAR AT THE TOP ***/
//notice_type == error|warning|ok
NitrogenClass.prototype.$show_notice_bar = function(notice_type, msg) {
    var add_class = "nitrogen_notice_" + notice_type + "_bar";
    if($("div.nitrogen_notice_bar").length == 0) {
        var disc_bar = $("<div>")
            .addClass("nitrogen_notice_bar");
        $("body").prepend(disc_bar);
    }
    $("div.nitrogen_notice_bar")
        .addClass(add_class)
        .html(msg)
        .slideDown();
}

NitrogenClass.prototype.$hide_notice_bar = function() {
    $("div.nitrogen_notice_bar").slideUp(500, function() {
        $("div.nitrogen_notice_bar").remove()
    });
}

NitrogenClass.prototype.$show_disconnected_notice = function() {
    if(!this.$going_away && !this.$maybe_going_away) {
        var msg = "&#9889; Connection Broken! Attempting to reconnect... &#9889;";
        this.$show_notice_bar("error", msg);
    }
}

NitrogenClass.prototype.$hide_disconnected_notice = function() {
    this.$hide_notice_bar();
}

NitrogenClass.prototype.$hide_disconnected_notice_worked = function() {
    if($("div.nitrogen_notice_bar").is(":visible")) {
        this.$show_notice_bar("ok", "Reconnected!");
        this.$hide_disconnected_notice();
    }
}



/*** VALIDATE AND SERIALIZE ***/

NitrogenClass.prototype.$before_postback = function(f) {
    this.$before_postback_list.push(f);
}

NitrogenClass.prototype.$execute_before_postbacks = function() {
    var before_list = this.$before_postback_list;
    for(var i=0; i<before_list.length; i++) {
        try{
            before_list[i]();
        }catch(ex){
        }
    }
}

NitrogenClass.prototype.$validate_and_serialize = function(validationGroup) {
    // Check validatation, build object of params...
    var is_valid = true,
        params = {},
        n = this;

    this.$execute_before_postbacks();
    
    jQuery(":input").not(".no_postback").each(function(i) {
        var LV = Nitrogen.$get_validation(this);
        if (LV && LV.group == validationGroup && !LV.validate()) {
            // Set a flag, but keep validating to show all messages.
            is_valid = false;
        } else {
            // Skip any unchecked radio boxes.
            if ((this.type=="radio" || this.type=="checkbox") && !this.checked) return;
            if (this.type=="select-multiple" && $(this).val()==null) return;
            if (this.type=='button' || this.type=='submit') return;
            
            // Skip elements that aren't nitrogen elements (they won't have a
            // properly named Nitrogen 'id')
            var id = n.$make_id(this);
            if(id == "") return;

            // It's a good element. Let's get the value, and return convert to
            // an empty string if it's null
            var val = $(this).val();
            if(val == null) val = "";
        
            // Add to the parameter list to send to the server
            params[id] = val;
        }
    });
    // Return the params if valid. Otherwise, return null.
    return is_valid && params || null;
}

NitrogenClass.prototype.$add_validation = function(element, args) {
    if($(element)){
        if(!$(element).data(Nitrogen.$live_validation_data_field))
            $(element).data(Nitrogen.$live_validation_data_field, new LiveValidation(element, args));
        return Nitrogen.$get_validation(element);
    } else
        return null;
}

NitrogenClass.prototype.$get_validation = function(element) {
    return $(element).data(Nitrogen.$live_validation_data_field);
}

// TODO: This needs to be made smarter. Right now, I'm pretty sure elements have
// single validation groups, while it should be a list of groups that get validated
NitrogenClass.prototype.$destroy_specific_validation = function(trigger, target) {
    var v = Nitrogen.$get_validation(target);
    if(v.group==trigger)
        Nitrogen.$destroy_target_validation(target);
}

NitrogenClass.prototype.$destroy_target_validation = function(element) {
    var v = Nitrogen.$get_validation(element);
    if(v) {
        v.destroy();
        $(element).data(Nitrogen.$live_validation_data_field,null);
    }
}

NitrogenClass.prototype.$destroy_validation_group = function(validationGroup) {
    jQuery(":input").not(".no_postback").each(function(i) {
        var LV = Nitrogen.$get_validation(this);
        if( LV && LV.group == validationGroup) {
            Nitrogen.$destroy_target_validation(this);
        }
    });
}

NitrogenClass.prototype.$destroy_all_validation = function() {
    $("*").each(function() {
        Nitrogen.$destroy_target_validation(this);
    });
}

NitrogenClass.prototype.$make_id = function(element) {
    var a = [];
    var re = new RegExp("\.wfid_(.[^\\s]*)", "g");
    while(element && element.className) {
        var matches = element.className.match(/wfid_([^\s])+/g);
        if (matches) {
            a.unshift.apply(a, matches);
        }       
        element = element.parentNode;
    }  
    return a.join(".");
}


/*** AJAX METHODS ***/

NitrogenClass.prototype.$show_spinner = function() {
    $("div.spinner").show();
}

NitrogenClass.prototype.$hide_spinner = function() {
    $("div.spinner").hide();
}

NitrogenClass.prototype.$do_event = function(validationGroup, onInvalid, eventContext, extraParam, ajaxSettings) {
    var n = this;
    
    // Flag to prevent firing multiple postbacks at the same time...
    this.$event_is_running = true;

    // Run validation...
    this.$show_spinner();
    var validationParams = this.$validate_and_serialize(validationGroup);   
    this.$hide_spinner();
    if (validationParams == null) {
        this.$event_is_running = false;
        this.$event_started = null;

        // Since validation failed, call onInvalid callback
        if (onInvalid) {
            onInvalid();
        }

        return;
    }

    // Assemble other parameters...
    var params = jQuery.extend({}, n.$params, validationParams, extraParam, { eventContext: eventContext });

    var s = jQuery.extend({
        dataType: 'text',
        cache: false,
        success: null,
        error: null
    }, ajaxSettings);

    this.$event_data_type = s.dataType;
    this.$event_success_fun = s.success;
    this.$event_error_fun = s.error;
    
    this.$show_spinner();

    if(this.$websockets_enabled) {
        delete params["pageContext"];
        var bertified = Bert.encode_to_bytearray(Bert.tuple(Bert.atom("nitrogen_postback"), params));
        this.$websocket.send(bertified.buffer);
    }else{
        this.$event_obj = jQuery.ajax({ 
            url: this.$url,
            type:'post',
            data: jQuery.param(params),
            dataType: s.dataType,
            cache: s.cache,
            timeout: 18000,
            success: function(data, textStatus) { n.$event_success(data, textStatus) },
            error: function(XHR, textStatus, errorThrown) { n.$event_error(XHR, textStatus, errorThrown) }
        });
    }
}

NitrogenClass.prototype.$cancel_event = function() {
    if(this.$event_is_running) {
        if(this.$event_obj !== null) {
            this.$event_obj.abort();
            this.$event_obj = null;
        }
        this.$event_started = null;
        this.$set_disconnected(true);
        this.$event_is_running = false;
        this.$hide_spinner();
    }
}

NitrogenClass.prototype.$event_success = function(data, textStatus) {
    this.$event_is_running = false;
    this.$event_started = null;
    this.$hide_spinner();
    if(this.$is_disconnected()) {
        this.$reconnect_system();
    }
    if(typeof this.$event_success_fun == 'function') {
        this.$event_success_fun(data, textStatus);
    }
    else{
        this.$eval(data);
    }
}

NitrogenClass.prototype.$event_error = function(XHR, textStatus, errorThrown) {
    var n = this;
    this.$event_started = null;
    this.$hide_spinner();
    if(textStatus == "timeout" || textStatus=="error") {
        n.$set_disconnected(true);
        setTimeout(function() {
            n.$requeue_last_event();
        }, 500);
    }
        
    this.$event_is_running = false;
    if(typeof this.$event_error_fun == 'function') {
        this.$event_error_fun(XHR, textStatus, errThrown)
    }
}

/*** SYSTEM EVENTS (FOR ASYNC) ***/

NitrogenClass.prototype.$do_system_event = function(eventContext) {
    var n = this;
    // Flag to prevent firing multiple postbacks at the same time...
    n.$system_event_is_running = true;

    // Assemble other parameters...
    var params = jQuery.extend( {}, n.$params, { eventContext: eventContext, is_system_event: 1 });
    if(this.$websockets_enabled) {
        delete params["pageContext"];
        var bertified = Bert.encode_to_bytearray(Bert.tuple(Bert.atom("nitrogen_postback"), params));
        n.$websocket.send(bertified.buffer);
    }
    else{
        n.$system_event_obj = $.ajax({
            url: this.$url,
            type:'post',
            data: jQuery.param(params),
            dataType: 'text',
            cache: false,
            timeout: 18000,
            success: function(data, textStatus) { n.$system_event_success(data) },
            error: function(XHR, textStatus, errorThrown) { n.$system_event_error(XHR, textStatus, errorThrown) }
        });
    }
}

NitrogenClass.prototype.$cancel_system_event = function() {
    if(this.$system_event_is_running) {
        this.$system_event_obj.abort();
        this.$system_event_started = null;
        this.$set_disconnected(true);
        this.$system_event_is_running = false;
    }
}

NitrogenClass.prototype.$system_event_success = function(data) {
    var n = this;
    if(n.$is_disconnected()) {
        n.$reconnect_system();
    }
    n.$system_event_is_running = false;
    n.$system_event_obj = null;
    n.$system_event_started = null;
    // A system event shouldn't clobber the pageContext.
    // Easiest to account for it here.
    var pc = n.$params["pageContext"];
    n.$eval(data);
    n.$set_param("pageContext", pc);
}

NitrogenClass.prototype.$system_event_error = function(XHR, textStatus, errorThrown) {
    var n = this;
    this.$system_event_started = null;
    if(textStatus == "timeout" || textStatus == "error") {
        n.$set_disconnected(true);
        setTimeout(function() {
            n.$requeue_last_system_event();
        }, 5000);
    }
    this.$system_event_is_running = false;
    this.$system_event_obj = null;
}


/*** FILE UPLOAD ***/

NitrogenClass.prototype.$send_pending_files = function(form,input) {
    var file=null;
    if(typeof(form.$nitrogen_pending_files)=="object") {
        // not a typo, doing an assignment here
        while(file=form.$nitrogen_pending_files.shift()) {
            file.submit();
        }
    }
}

NitrogenClass.prototype.$recalculate_upload_dimensions = function(form) {
    // If the provided form is a string, let's get the object with objs() which
    // does a little massaging.
    if(typeof form == "string") {
        form = objs(form);
    }

    var fakeinput = $(form).find(".upload-button");
    var w = $(fakeinput).outerWidth(true);
    var h = $(fakeinput).outerHeight(true);
    $(form).find(".upload-content:visible").width(w).height(h);
}

NitrogenClass.prototype.$attach_upload_handle_dragdrop = function(form,input,settings) {
    var thisNitro = this;
    if(typeof(settings) == "undefined")
        settings={};
    if(typeof (form.$nitrogen_pending_files) == "undefined")
        form.$nitrogen_pending_files = [];

    thisNitro.$dependency_register_function("/nitrogen/jquery.fileupload.min.js",function() {
        var dropzone = $(form).children(".upload_drop");
        $(input).fileupload({
            dropZone:(settings.droppable ? dropzone : null),
            singleFileUploads:true,
            sequentialUploads:true,
            url:thisNitro.$url,
            paramName:"file",
            formData: function() {
                form.elements["pageContext"].value = thisNitro.$params["pageContext"];
                var d = $(form).serializeArray();
                return d;
            },
            start: function(e, data) {
                form.pageContext.value = thisNitro.$params["pageContext"];
                if(settings.overall_progress) 
                    $(form).children(".upload_overall_progress").progressbar({}).slideDown();
            },
            progressall: function(e,data) {
                if(settings.overall_progress) {
                    $(form).children(".upload_overall_progress")
                        .progressbar("option", "value", data.loaded)
                        .progressbar("option", "max", data.total);
                    if(data.loaded == data.total) {
                        $(form).children(".upload_overall_progress").slideUp();
                    }
                }
            },
            progress: function(e,data) {
                var loaded = data.loaded;
                $.each(data.files, function(i,f) {
                    $("li[filename=\"" + f.name + "\"] .upload_progress").progressbar("option", "value", loaded);
                });
            },
            send: function(e,data) {
            },
            stop: function(e,data) {
            },
            always: function(e,data) {
            },
            fail: function(e,data, options) {
                Nitrogen.$increment_pending_upload_counter(form,-1);
            },
            add: function(e,data) {
                if(!settings.multiple) {
                    if (data.files.length>1) {
                        window.alert("Sorry, you can only upload single files with this element");
                        return;
                    }
                    else if(form.$nitrogen_pending_files.length > 0) {
                        form.$nitrogen_pending_files = [];
                        $(form).children(".upload_droplist").html("");
                    }
                }
                $.each(data.files,function(i,f) {
                    // Let's add the visual list of pending files
                    $(form).children(".upload_droplist")
                        .prepend($("<li></li>").attr("filename",f.name).prepend([
                            $("<span>").text(f.name + " (" + thisNitro.$format_filesize(f.size) + ")"),
                            $("<a>")
                                .attr("href","javascript:;")
                                .addClass("upload_delete_file")
                                .bind("click", function() {
                                    $("li[filename=\"" + f.name + "\"]").slideUp();
                                    thisNitro.$delete_pending_file(form, f.name);
                                })
                                .html("&times;"),

                            $("<div>")
                                .addClass("upload_progress")
                                .progressbar({
                                    max:f.size,
                                    value:0
                                }),
                        ]));
                    Nitrogen.$increment_pending_upload_counter(form,1);
                });

                if(settings.autoupload)
                    data.submit();
                else if(settings.multiple || form.$nitrogen_pending_files.length==0)
                    form.$nitrogen_pending_files.push(data);
                else
                    form.$nitrogen_pending_files[0] = data;
            },
            done: function(e,data) {
                if(typeof data.result == "string") {
                    // Good browsers will use XHR file transfers, and so this
                    // will return a string
                    var Postback = data.result;
                } else if(typeof data.result == "object") {
                    // Crappy browsers (IE9 and below) will do the transfer
                    // as with an iframe and return a document-type object
                    var Postback = data.result[0].body.innerHTML;
                } else {
                    // IE also has data.result as "undefined" on failure
                    // So let's just treat it as an empty string
                    var Postback = "";
                }

                $.globalEval(Postback);
                Nitrogen.$increment_pending_upload_counter(form,-1);
            }
        })
    })
}

NitrogenClass.prototype.$delete_pending_file = function(form, name) {
    for(var i=0;i<form.$nitrogen_pending_files.length;i++) {
        for(var j=0;j<form.$nitrogen_pending_files[i].files.length;j++) {
            if(form.$nitrogen_pending_files[i].files[j].name==name) {
                form.$nitrogen_pending_files[i].files.splice(j,1);
                if(form.$nitrogen_pending_files[i].files.length==0) {
                    form.$nitrogen_pending_files.splice(i, 1);
                }
                return;
            }
        }
    }
}

NitrogenClass.prototype.$format_filesize = function(size) {
    if(size > 1000000000) return (size/1000000000).toFixed(1) + " GB";
    else if(size > 1000000) return (size/1000000).toFixed(1) + " MB";
    else if(size > 1000) return (size/1000).toFixed(1) + " KB";
    else return size + " B";
}

NitrogenClass.prototype.$increment_pending_upload_counter = function(form,incrementer) {
    var counter = $(form).data("pending_uploads");
    if(typeof(counter)=="undefined")
        counter=0;
    counter+=incrementer;
    $(form).data("pending_uploads",counter);
    if(counter==0) {
        Nitrogen.$alert_unfinished_files(form);
    }
}


NitrogenClass.prototype.$upload_finished = function(Name) {
    $(".upload_droplist").children("li[filename=\"" + Name + "\"]")
        .css("text-decoration","line-through")
        .addClass("upload_successful")
        .fadeOut();
}

NitrogenClass.prototype.$alert_unfinished_files = function(form) {
    var files = $(form).find(".upload_droplist li:not(.upload_successful):visible");
    if(files.length > 0) {
        $(form).find(".upload_droplist li:not(.upload_successful)").css("color","red").fadeOut("slow");

        var filenames = $(files).get().map(function(f) { return $(f).text() }).join("\r\n");
        window.alert("There was an error uploading the following file(s):\r\n" + filenames + "\r\n\r\nThis is likely due to the file(s) being too large or a misconfiguration on the server");
    }
} 


/*** PATH LOOKUPS ***/

function obj(path, anchor) {
    return objs(path, anchor).get(0);
}

function objs(path, anchor) {
    if(typeof path != "string")
        throw {invalid_path_in_objs: path};
    // Trim the path...
    path = jQuery.trim(path);

    // If no anchor is specified, then use the last anchor set...
    if (!anchor) {
        anchor = Nitrogen.$anchor_path;
    } else {
        anchor = Nitrogen.$path_alias(anchor);
    }

    // Multiple parts, so split and combine results...
    if (path.indexOf(",") != -1) {
        var paths=path.split(",");
        var a = $();
        for (var i=0; i<paths.length; i++) {
            a = a.add(objs(paths[i], anchor));
        }
        return a;
    }

    // Selector is "page", so return the document...
    if (path == "page" || path == ".page") {
    return jQuery(document);
    }

    // Replace "##" with ".wfid_"...
    path = path.replace(/##/g, '.wfid_');

    // Replace "me" with anchor...
    path = path.replace(/\bme\b/g, anchor);

    // If this is a single word, then rewrite it to a Nitrogen element id.
    if (path.indexOf(" ") == -1 && path.indexOf(".") == -1 && path.indexOf("#") == -1) {
        var results = objs(".wfid_" + path, anchor);
        
        // If we found results, then return them...
        if (results.length > 0) {
            return results;
        }

        // If no results, and this is not a valid HTML element name, then return. Otherwise,
        // keep trying with the assumption that this is an HTML element...
        if (results.length == 0 && jQuery.inArray(path.toLowerCase(), Nitrogen.$valid_elements) == -1) {
            return jQuery();
        }
    }

    // If path begins with "body", then try matching across the entire
    // body...
    var re = new RegExp(/^body\b/);
    if (re.test(path)) {
        return jQuery(path);
    }    

    var anchor_obj = jQuery(Nitrogen.$anchor_root_path).find(anchor);
    // Find all results under the anchor...
    var results = anchor_obj.find(path);
    if (results.length > 0) {
        return results;
    }
    
    // If no results under the anchor, then try on each parent, moving upwards...
    var results = anchor_obj.parentsUntil( Nitrogen.$anchor_root_path );
    for (var i=0; i<results.length; i++) {
        var results2 = jQuery(results.get(i)).find(path);
        if (results2.length > 0) {
            return results2;
        }       
    }

    // No results, so try in context of entire page.
    return jQuery(path);
}

NitrogenClass.prototype.$valid_elements = [
    "a", "abbr", "acronym", "address", "applet", "area", "b", "base", "basefont", 
    "bdo", "big", "blockquote", "body", "br", "button", "caption", "center", "cite", 
    "code", "col", "colgroup", "dd", "del", "dfn", "dir", "div", "dl", "dt", "em", 
    "fieldset", "font", "form", "frame", "frameset", "h1", "h2", "h3", "h4", 
    "h5", "h6", "head", "hr", "html", "i", "iframe", "img", "input", "ins", "isindex", 
    "kbd", "label", "legend", "li", "link", "map", "menu", "meta", "noframes", "noscript", 
    "object", "ol", "optgroup", "option", "p", "param", "pre", "q", "s", "samp", "script", "select", 
    "small", "span", "strike", "strong", "style", "sub", "sup", "table", "tbody", "td", "textarea", 
    "tfoot", "th", "thead", "title", "tr", "tt", "u", "ul", "var"
];


/*** EVENT WIRING ***/

NitrogenClass.prototype.$observe_event = function(anchor, path, type, func) {
    objs(path, anchor).bind(type, func);
}

/*** DYNAMIC UPDATING ***/

NitrogenClass.prototype.$update = function(anchor, path, html) {
    objs(path, anchor).html(html);
}

NitrogenClass.prototype.$replace = function(anchor, path, html) {
    objs(path, anchor).replaceWith(html);
}

NitrogenClass.prototype.$insert_top = function(anchor, path, html) {
    objs(path, anchor).prepend(html);
}

NitrogenClass.prototype.$insert_bottom = function(anchor, path, html) {
    objs(path, anchor).append(html);
}

NitrogenClass.prototype.$insert_before = function(anchor, path, html) {
    objs(path, anchor).before(html);
}

NitrogenClass.prototype.$insert_after = function(anchor, path, html) {
    objs(path, anchor).after(html);
}

NitrogenClass.prototype.$remove = function(anchor, path) {
    var x = objs(path, anchor).remove();
    $(x).next('.LV_validation_message').remove();
    x.remove();
}



/*** REQUIRING SCRIPT BEFORE EXECUTION ***/
/* This process works when a depdendency javascript file is requested, it's
 * first registered as a pending dependency, then the ajax request is sent to
 * load and execute the script. Additionally, any functions that depend on that
 * particular file will not be executed, but will instead be queued for
 * execution after the script is finally loaded.  When the script finally
 * successfully loads, the pending requests depending on that script will be
 * executed one at a time. Finally, any calls depending on the script made
 * after the script has been loaded will be executed right away.
 */

// Queue up functions to be run when a dependency is loaded. If it's already
// loaded, execute right away. If it's not loaded, start the process of loading
// it.
NitrogenClass.prototype.$dependency_register_function = function(dependency, fun) {
    if(this.$is_dependency_loaded(dependency)) {
        fun();
    }
    else {
        this.$load_js_dependency(dependency);
        this.$js_dependencies[dependency].pending_calls.push(fun);
    }
};

// Load the js file (if it's not already pending)
NitrogenClass.prototype.$load_js_dependency = function(url) {
    var n = this;
    // This check will ensure that a js dependency isn't loaded more than once
    if(!n.$is_dependency_initialized(url)) {
        n.$init_dependency_if_needed(url);

        // Request the file, and when it finishes, mark the file is loaded, and
        // execute any pending requests
        $.ajax({
            url: url,
            dataType: "script",
            success: function(data, textStatus, jqxhr) { 
                    n.$js_dependencies[url].loaded=true;
                    n.$execute_dependency_scripts(url);
                },
            error: function(jqxhr, textStatus, errorThrown) {
                    n.$console_log({
                        error_loading_file: url,
                        error: textStatus,
                        errorThrown: errorThrown
                    })
                }, 
        });
            
    }
};

NitrogenClass.prototype.$init_dependency_if_needed = function(dependency) {
    if(this.$js_dependencies[dependency]===undefined)
        this.$js_dependencies[dependency] = {
            loaded: false,
            pending_calls: []
        };
};

NitrogenClass.prototype.$is_dependency_initialized = function(dependency) {
    return this.$js_dependencies[dependency]!==undefined
};

NitrogenClass.prototype.$is_dependency_loaded = function(dependency) {
    if(!this.$is_dependency_initialized(dependency))
        return false;
    else
        return this.$js_dependencies[dependency].loaded;
}

// Loop through each pending call and execute them in queue order
NitrogenClass.prototype.$execute_dependency_scripts = function(dependency) {
    var fun;
    while(fun = this.$js_dependencies[dependency].pending_calls.shift())
        fun();
}

/*** MISC ***/

NitrogenClass.prototype.$console_log = function(text) {
    try {
        console.log(text);
    } catch (e) {
        // console.log failed, let's just do nothing
        // If you're feeling adventurous, you could put an alert(text) here.
    }
}

NitrogenClass.prototype.$return_false = function(value, args) { 
    return false; 
}

NitrogenClass.prototype.$is_key_code = function(event, keyCode, shiftKey) {
    return (event && event.keyCode == keyCode && event.shiftKey == shiftKey);
}

NitrogenClass.prototype.$go_next = function(controlID) {
    var o = obj(controlID);
    if (o.focus) o.focus();
    if (o.select) o.select();
}

NitrogenClass.prototype.$disable_selection = function(element) {
    element.onselectstart = function() {
        return false;
    };
    element.unselectable = "on";
    element.style.MozUserSelect = "none";
    element.style.cursor = "default";
}

NitrogenClass.prototype.$set_value = function(anchor, element, value, optional_label) {
    var n = this;
    if (!element.id) element = objs(element);
    element.each(function(index, el) {
        if (el.value != undefined) $(el).val(value);
        else if (el.checked != undefined) el.checked = value;
        else if (el.src != undefined) el.src = value;
        else if($(el).hasClass("ui-progressbar")) n.$set_progress_bar_value(el, value, optional_label);
        else $(el).html(value);
    });
}

NitrogenClass.prototype.$set_values = function(anchor, element, values) {
    var n = this;
    if(!element.id) element = objs(element);
    element.each(function(index, el) {
        if (el.type == "select-multiple") {
            $(el).val(values);
        }
        else if (el.type == "checkbox") {
            var to_check = ($.inArray(el.value, values) != -1);
            el.checked = to_check;
        }
    });
}

NitrogenClass.prototype.$get_value = function(anchor, element) {
    if (!element.id) element = objs(element);
    el = element.get(0);
    if (el.value != undefined) return el.value;
    else if (el.checked != undefined) return el.checked;
    else if (el.src != undefined) return el.src;
    else if($(el).hasClass("ui-progressbar")) n.$get_progress_bar_value(el);
    else return $(el).html();
}


NitrogenClass.prototype.$normalize_param = function(key, value) {
    // Create the key=value line to add.
    // Sometimes, the user will pass a bunch of params in the key field.
    var s = "";
    if (key) { s = key; }
    if (key && value) { s = key + "=" + value; }
    return key + "&" + value;
}

NitrogenClass.prototype.$encode_arguments_object = function(Obj) {
    if (! Bert) { window.alert("Bert.js library not included in template.") }
    else {
        Bert.assoc_array_key_encoding("binary");
        var a = new Array();
        for (var i=0; i<Obj.length; i++) {
            a.push(Obj[i]);
        }
        // We want to encode to base64 due to $.param interpreting this as a unicode array.
        var s = Bert.encode_to_base64(a);
        return {args: s};
    }
}

NitrogenClass.prototype.$urlencode = function(str) {
    return escape(str).replace(/\+/g,'%2B').replace(/%20/g, '+').replace(/\*/g, '%2A').replace(/\//g, '%2F').replace(/@/g, '%40');
}


NitrogenClass.prototype.$set_cookie = function(cookie, value, path, minutes_to_live) {
    var expires;
    if (minutes_to_live) {
        var date = new Date();
        var milliseconds = minutes_to_live * 60 * 1000;
        date.setTime(date.getTime() + milliseconds);
        expires = "; expires=" + date.toUTCString();
    }
    else {
        expires = "";
    }

    document.cookie = [
        encodeURIComponent(cookie), "=", encodeURIComponent(value),
        expires,
        "; path=",path
    ].join("");
}   

/*** DATE PICKER ***/

NitrogenClass.prototype.$datepicker = function(pickerObj, pickerOptions) {
    jQuery(pickerObj).datepicker(pickerOptions);
}

/*** AUTOCOMPLETE TEXTBOX ***/
NitrogenClass.prototype.$autocomplete = function(path, autocompleteOptions, enterPostbackInfo, selectPostbackInfo) {
    var n = this;
    jQuery.extend(autocompleteOptions, {
        select: function(ev, ui) {
          var val = ui.item.value.replace(/"/g,'\\"');
          var item = (ui.item) && '{"id":"'+ui.item.id+'","value":"'+val+'"}' || '';
          n.$queue_event(null, null, selectPostbackInfo, {select_item: item});
        },
        source: function(req, res) {
          n.$queue_event(null, null, enterPostbackInfo, {search_term: req.term}, {
              dataType: 'json',
              success: function(data) {
                 res(data);
              }
          });
        }
    });
    jQuery(path).autocomplete(autocompleteOptions);
}

/*** DRAG AND DROP ***/

NitrogenClass.prototype.$draggable = function(path, dragOptions, dragTag) {
    objs(path).each(function(index, el) {
          el.$drag_tag = dragTag;
        jQuery(el).draggable(dragOptions);
    });
}

NitrogenClass.prototype.$droppable = function(path, dropOptions, dropPostbackInfo) {
    var n = this;
    dropOptions.drop = function(ev, ui) {
        var dragItem = ui.draggable[0].$drag_tag;
        n.$queue_event(null, null, dropPostbackInfo, {drag_item: dragItem});
    };
    objs(path).each(function(index, el) {
          jQuery(el).droppable(dropOptions);
    });
}


/*** SORTING ***/

NitrogenClass.prototype.$sortitem = function(el, sortTag) {
    var sortItem = obj(el);
    sortItem.$sort_tag = sortTag;
    sortItem.$drag_tag = sortTag;
}

NitrogenClass.prototype.$sortblock = function(el, sortOptions, sortPostbackInfo) {
    var n = this;
    sortOptions.update = function() {
        var sortItems = "";
        for (var i=0; i<this.childNodes.length; i++) {
            var childNode = this.childNodes[i];
            if (sortItems != "") sortItems += ",";
            if (childNode.$sort_tag) sortItems += childNode.$sort_tag;
        }
        n.$queue_event(null, null, sortPostbackInfo, {sort_items: sortItems});
    };
    objs(el).sortable(sortOptions);
}

/*** TEXTAREA TAB-TRAPPING ***/
/*** With a little help from http://stackoverflow.com/questions/6140632/how-to-handle-tab-in-textarea ***/
NitrogenClass.prototype.$trap_tabs = function(el) {
    $(el).keydown(function(e) {
        if(e.keyCode == 9) {
            var start = this.selectionStart;
            var end = this.selectionEnd;
            var $this = $(this);
            var val = $this.val();
            $this.val(val.substring(0, start) + "\t" + val.substring(end));
            this.selectionStart = this.selectionEnd = start + 1;
            return false;
        }
    });
}

/*** RECAPTCHA ***/
/*** transfer content of an alien elment into a nitrogen form
 * used in src/elements/other/element_recaptcha.erl
 * ***/
NitrogenClass.prototype.$from_alien = function(nativeID) {
    var input = $("input#" + nativeID).val();
    objs(nativeID).val(input);
};

/*** PROGRESS BAR ***/
NitrogenClass.prototype.$init_progress_bar = function(el, value, max, color) {
    objs(el).progressbar({
        value: value,
        max: max
    });
    color = this.$normalize_color(color);
    objs(el).find(".ui-progressbar-value")
        .css("background", color)
        .css("border-color", color);
};


NitrogenClass.prototype.$normalize_color = function(color) {
    if(color.match(/^#/)) {
        return color;
    }
    else if(color.match(/^[0-9a-fA-F]{3,6}$/)) {
        return "#" + color;
    }
    else {
        return color;
    }
}


NitrogenClass.prototype.$set_progress_bar_value = function(el, value, label) {
    var newlabel = ""
    if(typeof el == "object") el = $(el);
    else el = objs(el);

    if(typeof value=="string") value = parseInt(value);

    el.progressbar("option", "value", value);
    if(el.hasClass("progressbar-label-string")) {
        newlabel = label;
    }
    else if(el.hasClass("progressbar-label-none")) {
        newlabel="";
    }
    else if(value !== false) {
        var max = el.progressbar("option","max");
        var percent_label = Math.floor(value * 100 / max) + "%";
        var ratio_label = value + "/" + max;
        if(el.hasClass("progressbar-label-percent")) {
            newlabel = percent_label;
        }
        else if(el.hasClass("progressbar-label-ratio")) {
            newlabel = ratio_label;
        }
        else if(el.hasClass("progressbar-label-both")) {
            newlabel = percent_label + " (" + ratio_label + ")";
        }
    }
    el.find(".progressbar-label").text(newlabel);
};

NitrogenClass.prototype.$get_progress_bar_max = function(el) {
    return objs(el).progressbar("option", "max");
};

NitrogenClass.prototype.$get_progress_bar_value = function(el) {
    return objs(el).progressbar("option", "value");
};

/*** WEBSOCKETS ***/

NitrogenClass.prototype.$enable_websockets = function() {
    this.$console_log("Websockets Enabled");
    this.$websockets_enabled = true;
    this.$websockets_ever_succeeded = true;
    this.$flush_switchover_comet_actions();
};

NitrogenClass.prototype.$flush_switchover_comet_actions = function() {
    var bertified = Bert.encode_to_bytearray(Bert.atom("flush_switchover_comet_actions"));
    this.$websocket.send(bertified);
    // On success, this will run "Nitrogen.$reconnect_system()" (found in
    // nitrogen:ws_message_catched/1)
};

NitrogenClass.prototype.$disable_websockets = function() {
    var n = this;
    n.$console_log("Websockets disabled or disconnected");
    n.$websockets_enabled = false;

    // If websockets had ever succeeded, then disabling websockets is an
    // indicator that the websockets should try to reconnect and in the
    // meantime, mark us as disconnected for comet purposes (which will end up
    // popping up the "You are disconnected" bar).
    //
    // If, however, websockets never connected, then it's likely there's a
    // proxy that's blocking the websockets, so no need to continue to try.
    // Just never try again.
    if(n.$websockets_ever_succeeded) {
        n.$set_disconnected(true);

        if(typeof n.$websocket_reconnect_timer != "null") {
            clearTimeout(n.$websocket_reconnect_timer);
        }
        n.$websocket_reconnect_timer = setTimeout(function() { n.$ws_init() }, 5000);
    }
};

NitrogenClass.prototype.$ws_init = function() {
    try {
        Bert.assoc_array_key_encoding("binary");
        var this2 = this;
        var e = new Error();
        var ws_url = this.$ws_url(location.href);
        this.$websocket = new WebSocket(ws_url);
        this.$websocket.binaryType="arraybuffer";
        this.$websocket.onopen = function(evt) {this2.$ws_open()};
        this.$websocket.onclose = function(evt) {this2.$ws_close()};
        this.$websocket.onmessage = function(evt) {this2.$ws_message(evt.data) };
        this.$websocket.onerror = function(evt) {this2.$ws_close()};
    }catch(ex){ }
};

NitrogenClass.prototype.$ws_url = function(url) {
    // Will ensure that http is replaced with ws and https is replaced with wss
    return url.replace(/^http/, "ws");
};

NitrogenClass.prototype.$ws_open = function() {
    this.$send_pagecontext();
    // On success, will run Nitrogen.$enable_websockets(), but is sent from the
    // server and executed there. See the file nitrogen.erl the function
    // nitrogen:ws_message_catched/1.
};

NitrogenClass.prototype.$send_pagecontext = function() {
    var pageContext = this.$params["pageContext"];
    var bertified = Bert.encode_to_bytearray(Bert.tuple(Bert.atom("page_context"), Bert.binary(pageContext)));
    this.$websocket.send(bertified);
};

NitrogenClass.prototype.$ws_close = function() {
    if(this.$system_event_is_running) {
        this.$requeue_last_system_event();
    }
    if(this.$event_is_running) {
        this.$requeue_last_event();
    }
    this.$disable_websockets();
};

NitrogenClass.prototype.$ws_message = function(data) {
    var matches = null;
    if(matches = data.match(/^nitrogen_system_event:([\s\S]*)/)) {
        this.$system_event_success(matches[1]);
    }
    else if(matches = data.match(/^nitrogen_event:([\s\S]*)/)) {
        var response_data = null;
        if(this.$event_data_type == "json") {
            // while other evals() are replaced with Nitrogen.$eval(), this is
            // not necessary here, since we actually *want* the json to be
            // evaluated, which is then passed to $event_success() to acually
            // be eval()'d
            response_data = eval(matches[1]);
        }
        else{
            response_data=matches[1];
        }
        this.$event_data_type = null;
        this.$event_success(response_data);
    }    
};


NitrogenClass.prototype.$attempt_websockets = function() {
    var n = this;
    $(document).ready(function() {
        if(typeof Bert == "object") {
            n.$ws_init();
        }
        else{
            n.$console_log("Bert not linked from template. Attempting to load dynamically.");
            n.$dependency_register_function("/nitrogen/bert.js", function() {
                n.$console_log("Bert successfully loaded");
                n.$ws_init();
            }); 
        }
    });
};

var page = document;

var Nitrogen = new NitrogenClass();


$(window).on("beforeunload", function() {
    // Give a "redirect prompt" if presented to prevent such.
    if(!Nitrogen.$allow_redirect) {
        return Nitrogen.$redirect_prompt;
    }
    // Prevent the red "connection broken" bar from showing when navigating away.
    Nitrogen.$maybe_going_away = true;
    setTimeout(function() {
        Nitrogen.$maybe_going_away = false;
    },1000);
    return;
});

$(document).ready(function() {
    if(!nitrogen_jqm_loaded) {
        Nitrogen.$attempt_websockets();
        Nitrogen.$event_loop();
    }
});
