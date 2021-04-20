#!/usr/bin/env bash

sum=0
for var in "$@"
do
    re='^-?[0-9]+$'
    if ! [[ $var =~ $re ]] ; then
       echo "som: ongeldige argumenten" >&2
       exit 1
    fi
    sum=$(( sum + var ))
done
echo $sum
