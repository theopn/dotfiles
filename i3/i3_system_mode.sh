#!/bin/sh
lock() {
  if [ -d "$HOME/dotfiles" ]
  then
    i3lock -i $HOME/dotfiles/pictures/2022-06-29_i3lock_solarsys-dracula.png
  else
    i3lock -c #282a36
  fi
}

case "$1" in
  lock)
    lock
    ;;
  suspend)
    systemctl suspend
    ;;
  reboot)
    systemctl reboot
    ;;
  shutdown)
    systemctl poweroff
    ;;
  *)
    echo "Invalid option!"
    exit 2
esac

exit 0

