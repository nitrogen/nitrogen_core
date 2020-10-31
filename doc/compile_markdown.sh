#!/bin/sh

find markdown -name "*.md" -exec ./markdown_to_html.pl "{}" \;
