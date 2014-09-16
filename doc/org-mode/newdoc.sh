#!/usr/bin/env bash

cp blank.org ${2}.org
./replace_header.pl "org-headers/${1}_header.org.src" "${2}.org"
