#!/bin/bash

year=$(date +%Y)
month=$(date +%m)
case $1 in
  'prev') ((month--));;
  'next') ((month++));;
esac

notify-send "      Calendar" "$(cal $month $year)"
