#!/bin/sh

#case "$1" in
#  on)
#    #kill -USR1 $(pidof dunst)
#    dunstctl set-paused true
#    ;;
#  off)
#    notify-send -u critical "Do Not Disturb Off!"
#    #kill -USR2 $(pidof dunst)
#    dunstctl set-paused false
#    ;;
#  *)
#    echo "Invalid Option!"
#    exit 2
#    ;;
#esac

notify-send -u normal "If you see this, DND is off"
dunstctl set-paused toggle

exit 0;

