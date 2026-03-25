#!/bin/bash

load() {
    local filename="$1"
    if [[ -f "$filename" ]]; then
        cat "$filename" | xargs # xargs acts as a simple string trimmer
    fi
}

save() {
    local user="$1"
    local filename="$2"
    echo "$user" > "$filename"
}

user=$(load "datafile.txt")

if [[ -z "$user" ]]; then
    echo "Hello, I don't believe we have met."
    read -r user
    save "$user" "datafile.txt"
    echo "Nice to meet you $user."
else
    echo "It's good to see you again, $user."
fi
