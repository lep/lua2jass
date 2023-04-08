#!/bin/bash

# we shuffle to find non-declared dependencies
# but our dependency graph is so tight, we always get the same answer
# from tsort anyways
args=( $* )
args=( $(shuf -e "${args[@]}") )

for f in "${args[@]}"; do
    awk -f print-requirements < "$f"
done | tsort | tac | \
while read -r name; do
    echo "out/$name.j"
done
