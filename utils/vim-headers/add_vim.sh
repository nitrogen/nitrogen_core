#!/bin/sh

vim4erl=`cat utils/vim-headers/vim4.erl.src`;
find src/ -name "*.erl" | xargs grep -L "vim:" | xargs sed -i "1i$vim4erl"
