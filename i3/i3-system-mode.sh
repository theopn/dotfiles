#!/bin/sh
lock() {
  if [ -d ~/dotfiles ]
  then
    i3lock -i ~/dotfiles/assets/naomi-solarsys-draculafied-lockscreen.png
  else
    i3lock -c "#282a36"
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

