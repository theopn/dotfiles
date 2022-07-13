#!/bin/sh

case "$1" in
  on)
    notify-send -u normal "Do Not Disturb On!"
    sleep 1.7
    kill -USR1 $(pidof dunst)
    ;;
  off)
    notify-send -u critical "Do Not Disturb Off!"
    kill -USR2 $(pidof dunst)
    ;;
  *)
    echo "Invalid Option!"
    exit 2
    ;;
esac

exit 0;

