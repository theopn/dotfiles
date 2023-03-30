#!/bin/bash

bar_color="#F1FA8C"

function get_volume() {
  pactl get-sink-volume @DEFAULT_SINK@ | grep -Po '[0-9]{1,3}(?=%)' | head -1
}

function get_mute() {
  pactl get-sink-mute @DEFAULT_SINK@ | grep -Po '(?<=Mute: )(yes|no)'
}

function get_volume_icon() {
  volume=$(get_volume)
  mute=$(get_mute)
  if [ $volume -eq 0 ] || [ $mute == "yes" ] ; then
    volume_icon="婢"
  elif [ $volume -lt 50 ]; then
    volume_icon=""
  else
    volume_icon=""
  fi
}

function show_volume_notif() {
  volume=$(get_volume)
  get_volume_icon
  dunstify -i audio-volume-muted-blocking -t 1000 -r 2593 -u normal "${volume_icon} ${volume}%" -h int:value:${volume} -h string:hlcolor:${bar_color}
}

case $1 in
  up)
    pactl set-sink-volume @DEFAULT_SINK@ +5%
    ;;
  down)
    pactl set-sink-volume @DEFAULT_SINK@ -5%
    ;;
  mute)
    pactl set-sink-mute @DEFAULT_SINK@ toggle
    ;;
esac

show_volume_notif

