#!/bin/sh

# kill all child processes on exit
trap "pkill -P $$" EXIT

"$1"
