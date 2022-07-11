#!/usr/bin/env sh

# Kill previous instances of polybar and wait until they are actually killed
killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

case "$1" in
  onebar)
    echo "oops onebar no longer exists"
    ;;
  twobar)
    polybar -c ~/dotfiles/polybar/config.inl hamilton -r &
    polybar -c ~/dotfiles/polybar/config.inl bottas -r
    ;;
  *)
    echo "Invalid option!"
    exit 2;
esac

exit 0
