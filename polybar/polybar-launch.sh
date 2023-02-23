#!/bin/bash

# Kill previous instances of polybar and wait until they are actually killed
killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

polybar -c ~/dotfiles/polybar/config-simple.inl -r

exit 0

