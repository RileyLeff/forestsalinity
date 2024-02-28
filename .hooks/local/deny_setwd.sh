#!/bin/bash

files=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(R|r|rmd|RMD)$')

if [ -n "$files" ]; then
    forbidden=$(grep -nH --color=always "setwd(" $files)
    if [ -n "$forbidden" ]; then
        echo "Error: 'setwd()' function detected in the following files:"
        echo "$forbidden"
        echo "Please remove 'setwd()' calls before committing."
        exit 1
    fi
fi
