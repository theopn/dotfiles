#!/usr/bin/env sh

killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
polybar -c $HOME/.config/polybar/config.inl hamilton -r &
polybar -c ~/.config/polybar/config.inl bottas -r
