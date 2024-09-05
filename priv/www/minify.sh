#!/bin/sh

## On ubuntu/debian machines, this will use the node/nom package uglify-js
##   $ https://www.tecmint.com/minify-css-and-js-files-linux/
## 

min="uglifyjs -c"

$min -o bert.min.js bert.js
$min -o nitrogen.min.js nitrogen.js
cat jquery-ui.js jquery.ui.touch-punch.js | $min -o jquery-ui.min.js
cat fileupload/jquery.ui.widget.js fileupload/jquery.iframe-transport.js fileupload/jquery.fileupload.js | $min -o jquery.fileupload.min.js
$min -o jquery.sparkline.min.js jquery.sparkline.js
$min -o livevalidation.min.js livevalidation.js
$min -o nitro_livevalidation.min.js nitro_livevalidation.js

