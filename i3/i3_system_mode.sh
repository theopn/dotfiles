#!/bin/sh
lock() {
  i3lock -i ~/Pictures/screenshot.png
}

case "$1" in
  lock)
    lock
    ;;
  suspend)
    lock && systemctl suspend
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

