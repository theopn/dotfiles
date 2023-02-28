#!/bin/sh

LOCK_SCREEN=~/dotfiles/assets/naomi-solarsys-draculafied-lockscreen.png

function lock() {
  if [ -e $LOCK_SCREEN ]
  then
    i3lock -i $LOCK_SCREEN
  else
    i3lock -c '#282A36'
  fi
}

lock

exit 0

