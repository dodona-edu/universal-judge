#!/bin/bash
read -r input_line
if [ -s "input2.txt" ]; then
    # -s: True if file exists and has a size greater than zero
    # Read the file content, trim it, and print
    file_content=$(cat "input2.txt")
    # Trimming trailing whitespace
    echo "${file_content%"${file_content##*[![:space:]]}"}"
else
    echo "$input_line"
fi
