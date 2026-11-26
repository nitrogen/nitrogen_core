#!/bin/sh

## This needs to be built into the make system

if [ -z "$1" ]; then
	VSN="9.8.0"
else
	VSN="$1"
fi

rm -f jquery.ui.widget.js
rm -f jquery.iframe-transport.js
rm -f jquery.fileupload.js

wget https://raw.githubusercontent.com/blueimp/jQuery-File-Upload/$VSN/js/vendor/jquery.ui.widget.js --progress=dot
wget https://raw.githubusercontent.com/blueimp/jQuery-File-Upload/$VSN/js/jquery.iframe-transport.js --progress=dot
wget https://raw.githubusercontent.com/blueimp/jQuery-File-Upload/$VSN/js/jquery.fileupload.js --progress=dot
