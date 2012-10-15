#!/bin/sh

./replace_header.pl org-headers/actions_header.org.src actions/*.org
./replace_header.pl org-headers/elements_header.org.src elements/*.org
./replace_header.pl org-headers/validators_header.org.src validators/*.org
./replace_header.pl org-headers/handlers_header.org.src handlers/*.org

