#!/bin/sh

find org-mode -name "*.org" -exec ./org_to_markdown.pl "{}" \;
