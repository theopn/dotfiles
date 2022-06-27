#!/bin/sh
lock() {
  i3lock -i ~/dotfiles/pictures/2022-06-25_i3lock_solarsys_lock.png
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

