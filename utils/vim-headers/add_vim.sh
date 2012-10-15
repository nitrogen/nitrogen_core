#!/bin/sh

vim4erl=`cat utils/vim-headers/vim4.erl.src`;
vim3org=`cat utils/vim-headers/vim3.org.src`;

find doc/org-mode -name "*.org" | xargs grep -L "vim:" | xargs sed -i "1i$vim3org"
find src/ -name "*.erl" | xargs grep -L "vim:" | xargs sed -i "1i$vim4erl"
