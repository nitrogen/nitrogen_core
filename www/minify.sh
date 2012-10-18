#!/bin/sh

min="yui-compressor"

$min -o bert.min.js bert.js
$min -o nitrogen.min.js nitrogen.js
cat fileupload/jquery.ui.widget.js fileupload/jquery.iframe-transport.js fileupload/jquery.fileupload.js | $min --type js -o jquery.fileupload.min.js

