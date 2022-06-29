#!/bin/sh

year=$(date +%Y)
case $1 in
  "prev") month=$(($(date +%m)-1));;
  "curr") month=$(date +%m);;
  "next") month=$(($(date +%m)+1));;
esac

notify-send "      Calendar" "$(cal $month $year)"

