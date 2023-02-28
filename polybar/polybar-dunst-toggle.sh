#!/bin/bash

if [[ $(dunstctl is-paused) == 'false' ]]; then
  notify-send -u normal 'Do not disturb on!'
  sleep 3
  dunstctl close
  dunstctl set-paused true
else
  notify-send -u normal 'Do not disturb off'
  dunstctl set-paused false
fi

exit 0;

