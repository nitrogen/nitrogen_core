#!/bin/sh

## On ubuntu/debian machines, the package to install is called yui-compressor:
##   $ sudo apt-get install yui-compressor
## 

min="yui-compressor"

$min -o bert.min.js bert.js
$min -o nitrogen.min.js nitrogen.js
cat jquery-ui.js jquery.ui.touch-punch.js | $min --type js -o jquery-ui.min.js
cat fileupload/jquery.ui.widget.js fileupload/jquery.iframe-transport.js fileupload/jquery.fileupload.js | $min --type js -o jquery.fileupload.min.js
$min -o jquery.sparkline.min.js jquery.sparkline.js
$min -o livevalidation.min.js livevalidation.js
$min -o jquery.mobile.min.js jquery.mobile.js

