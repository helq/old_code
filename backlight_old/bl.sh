#!/usr/bin/env bash

flevel="/home/helq/Development/Utilities/backlight/status"

level=$(cat "$flevel" || echo 100)

case $1 in
    up    |u*) level=$(( level < 100 ? level+1 : level )) ;;
    down  |d*) level=$(( level > 0   ? level-1 : level )) ;;
    pause |p*) redshift -o -l 0:0 -t 6500:6500 -b 1.0; exit ;;
    reload|*)  echo "reloading ..." ;;
esac

echo "Current light level: $level"
redshift -o -l 0:0 -t 6500:6500 -b $(bc -l <<< "$level/100")

echo "$level" > "$flevel"
