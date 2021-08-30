#!/bin/bash
data=$(sed "s/[12357]//g" | sed "s/[0469]/1/g" | sed "s/[8]/2/g" )
sum="0"
for((i=0;i<${#data};i++));
do
    ((sum+=${data:i:1}))
done
echo $sum
