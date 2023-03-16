#!/bin/bash

echo 'digraph lua {'

for f in "$@"; do
    awk -f print-requirements < "$f"
done | awk '{ print "\"" $1 "\"->\"" $2 "\"" }'

echo '}'
