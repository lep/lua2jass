#!/usr/bin/env bash

#alias lli=runhaskell compile.hs|python interperter.py /dev/stdin

for x in lua/*.lua; do
    echo "$x"
    ./compare.sh "$x" || echo "Error in $x"


    #if ! [ "$orig" = "$mine" ]; then
    #    #echo "orig: $orig"
    #    #echo "mine: $mine"
    #    #md5sum <<< "$orig"
    #    #md5sum <<< "$mine"
    #    #diff <(echo "$orig") <(echo "$mine")
    #fi

done
