#!/usr/bin/env sh

killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
polybar -c $HOME/.config/polybar/config.inl myBarTop -r 
#& polybar -c ~/.config/polybar/config.inl myBarBottom -r
