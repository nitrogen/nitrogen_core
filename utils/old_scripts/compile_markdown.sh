#!/bin/sh

rm -fr html
mkdir html

find markdown -name "*.md" -exec ./markdown_to_html.pl "{}" \;
