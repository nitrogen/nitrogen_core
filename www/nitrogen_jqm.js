// Destroy the Nitrogen object that is created in nitrogen.js
Nitrogen.$destroy();

$(document).on("pageshow", "#pagediv", function(event){
	Nitrogen = new NitrogenClass();
	Nitrogen.$event_loop();
	eval( $(this).jqmData("code") );
	Nitrogen.$anchor_root( this );
});

$(document).on("pagebeforehide", "#pagediv", function(event){
	Nitrogen.$destroy();
});
