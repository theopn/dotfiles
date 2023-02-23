#!/bin/sh

notify-send -u normal "If you see this, DND is off"
dunstctl set-paused toggle

exit 0;

