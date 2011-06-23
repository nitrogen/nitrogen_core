<?php
$headers = getallheaders();
// create the object and assign property
	$file = new stdClass;
	$file->name = basename($headers['X-File-Name']);
	$file->size = $headers['X-File-Size'];
	$file->content = file_get_contents("php://input");
	
	// if everything is ok, save the file somewhere
	echo $file->content;	

?>