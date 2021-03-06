#!/bin/sh

case "$1" in
  nightlight)
    if (($(ps -aux | grep [r]edshift | wc -l) > 0))
    then
      redshift -x & pkill -9 redshift
    else
      redshift -P -l 39.2:-86.5 -t 5600:3500 -m randr
    fi
    ;;
  compositor)
    if (($(ps -aux | grep [p]icom | wc -l) > 0))
    then
      pkill -9 picom
    else
      picom -b
    fi
    ;;
  *)
    echo Invalid option
    exit 2;
esac

exit 0;

