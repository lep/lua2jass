#!/bin/bash

for f in "$@"; do
    awk -f print-requirements < "$f"
done | tsort | tac | \
while read -r name; do
    echo "out/$name.j"
done
