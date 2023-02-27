#!/bin/bash

for f in "$@"; do
    awk < "$f" '
        /^\/\/ scope/ {
            name = $3
        }
        /^\/\/ REQUIRES / && name {
            for(i=3; i <= NF; i++){
                print name " " $i
            }
        }
        #END {
        #    if ( !name)
        #        print error " " error
        #}
    '
done | tsort | tac | \
while read -r name; do
    echo "out/$name.j"
done
