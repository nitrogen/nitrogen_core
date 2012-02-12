// Destroy the Nitrogen object that is created in nitrogen.js
Nitrogen.$destroy();

$( '#pagediv' ).live( 'pageshow', function(event){
	Nitrogen = new NitrogenClass();
	Nitrogen.$event_loop();
	eval( $(this).jqmData("code") );
	Nitrogen.$anchor_root( this );
});

$( '#pagediv' ).live( 'pagebeforehide', function(event){
	Nitrogen.$destroy();
});
