// This is the basic example of augmenting the validation functionality.  You
// can redefine the following functions:
//
// $get_validation(element) - returns the validation object
// $set_validation(element, data) - sets the validation settings on the element
//
// $init_validation(element,args) will end up being called by the validation
// handler - in this case, src/handlers/validation/lv_validation_handler.erl
//
// NOTE: It's extremely important that if your validation handler requires
// certain javascript files, that you ensure they are loaded before you try to
// work with that validation.  The example here loads the file:
// 
//     /nitrogen/livevalidation.min.js
//

Nitrogen.$dependency_register_function("/nitrogen/livevalidation.min.js", function() {

	var $validation_field = "LV_validation_field";

	NitrogenClass.prototype.$get_validation = function(element) {
		return $(element).data($validation_field);
	};

	NitrogenClass.prototype.$set_validation = function(element, data) {
		return $(element).data($validation_field, data);
	};

	NitrogenClass.prototype.$remove_validation_artifacts = function(element) {
		$(element).next(".LV_validation_message").remove();
	};


	NitrogenClass.prototype.$init_validation = function(element, group, args) {
		// If there is no element, we'll just return null;
		//console.log({init_validation: element});
		if($(element)) {
			// if the element doesn't have any validation, initialize the element for validation
			if(!(this.$get_validation(element))) {
				var v = new LiveValidation(element, args);
				v.group = group;
				this.$set_validation(element, v);
			}
			// Then return the validators connected to the element
			return this.$get_validation(element);
		}else{
			return null;
		}
	};

	NitrogenClass.prototype.$add_validation = function(element, update_v_fun) {
		var v = this.$get_validation(element);
		//console.log(v);
		v = update_v_fun(v);
		//console.log(v);
		this.$set_validation(element, v);
	};


	NitrogenClass.prototype.$validate_element = function(element, validationGroup) {
		//console.log({validating_group: validationGroup});
		var LV = this.$get_validation(element);
		if(LV && (validationGroup=="" || validationGroup==null || LV.group==validationGroup)) {
			// This element has validation and the validation group matches, so
			// let's validate it and return the value of the validation (which
			// should be a boolean)
			return LV.validate();
		}else{
			return true;
		}
	};

	NitrogenClass.prototype.$destroy_target_validation = function(element) {
		var v = this.$get_validation(element);
		if(v) {
			v.destroy();
			this.$set_validation(null);
		}
	};

	NitrogenClass.prototype.$validation_error = function(element, attachTo, text) {
		var opts = {onlyOnSubmit: true};
		if(attachTo) {
			opts.InsertAfterWhatNode = attachTo
		}
		console.log(opts);

		var v = new LiveValidation(obj(element), opts);
		v.add(Validate.Custom, {
			against: this.$return_false,
			failureMessage: text,
			displayMessageWhenEmpty: true
		});
		v.validate();
	};

	Nitrogen.$set_validation_loaded("livevalidation");

	/*
	NitrogenClass.prototype.$instant_validation_failure = function(element, message, attach_to) {
		var LVOptions = {onlyOnSubmit: true};
		if(attach_to) {
			LVOptions.insertAfterWhatNode = attach_to;
		}
		var v = new LiveValidation(obj(element), LVOptions);
		v.add(Validate.Custom, {
			against: this.$return_false,
			failureMessage: message,
			displayMessageWhenEmpty: true
		});
		v.validate();
	}
	*/
		
});
