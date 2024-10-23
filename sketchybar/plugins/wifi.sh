#!/bin/sh

# Wifi module breaks pretty much every macOS update, so let's settle with IP Address
# macOS 14 Sonoma: https://github.com/FelixKratz/SketchyBar/issues/407
# macOS 15 Sequoia: https://github.com/FelixKratz/SketchyBar/issues/517
#INFO="$(networksetup -listallhardwareports | awk '/Wi-Fi/{getline; print $2}' | xargs networksetup -getairportnetwork | sed "s/Current Wi-Fi Network: //")"
#
#if [ "$SENDER" = "wifi_change" ]; then
#  WIFI=${INFO:-"Not Connected"}
#  sketchybar --set $NAME label="${WIFI}"
#fi

INFO="$(scutil --nwi | grep address | sed 's/.*://' | tr -d ' ' | head -1)"

if [ "$SENDER" = "wifi_change" ]; then
  WIFI=${INFO:-"Not Connected"}
  sketchybar --set $NAME label="${WIFI}"
fi
