#!/bin/bash

case $1 in
  dracula)
    bar=~/dotfiles/polybar/config-dracula.inl
    ;;
  *)
    echo 'No such bar exists!'
    exit 1
    ;;
esac

# Kill previous instances of polybar and wait until they are actually killed
killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

polybar -c $bar -r -q

exit 0

