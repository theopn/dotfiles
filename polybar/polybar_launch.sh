#!/usr/bin/env sh

killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

case "$1" in
  onebar)
    polybar -c ~/.config/polybar/config-onebar.inl onebar
    ;;
  twobar)
    polybar -c ~/.config/polybar/config-twobar.inl hamilton -r &
    polybar -c ~/.config/polybar/config-twobar.inl bottas -r
    ;;
  *)
    echo "Invalid option!"
    exit 2;
esac

exit 0
