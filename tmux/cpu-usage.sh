#!/bin/bash

if [[ $OSTYPE =~ "darwin" ]]; then
  top -l 1 | head -n 4 | tail -n 1 | awk '{ printf "%3d%%\n", $3 }' | xargs # Last line of the first 4 lines == CPU usage: x% user y% sys, z% idle
else
  grep 'cpu ' /proc/stat | awk '{ usage=(($2 + $4) * 100) / ($2 + $4 + $5) } END { print usage "%" }'
fi

