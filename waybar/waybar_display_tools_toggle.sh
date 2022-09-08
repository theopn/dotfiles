#!/bin/sh

case "$1" in
  nightlight)
    if (($(ps -aux | grep [g]ammastep | wc -l) > 0))
    then
      gammastep -x &
      pkill -9 gammastep &
      gammastep -x
    else
      gammastep -P -l 39.2:-86.5 -t 5600:3500
    fi
    ;;
  *)
    echo Invalid option
    exit 2;
esac

exit 0;

