$( '#pagediv' ).live( 'pageinit', function(event){
	eval( $(this).jqmData("code") );
	Nitrogen.$anchor_root( this );
});
