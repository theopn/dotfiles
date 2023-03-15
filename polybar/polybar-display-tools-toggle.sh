#!/bin/bash

case $1 in
  nightlight)
    if [[ $(ps -aux | grep [r]edshift | wc -l) > 0 ]]; then
      redshift -x
      pkill -USR1 redshift
    else
      pkill -USR1 redshift
      redshift -P -l 39.2:-86.5 -t 5600:3500 -m randr
    fi
    ;;
  compositor)
    if [[ $(ps -aux | grep [p]icom | wc -l) > 0 ]]; then
      pkill -9 picom
    else
      picom -b
    fi
    ;;
  *)
    echo 'Invalid option'
    exit 1;
esac

exit 0;

