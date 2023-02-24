#!/bin/bash

# Revised from https://github.com/adi1090x/rofi/tree/master/files/powermenu

# Options
shutdown='  shutdown'
reboot=' 󰜉 reboot'
lock='  lock'
suspend=' 󰤄 suspend'
logout=' 󰗼 logout'
yes='  yes'
no=' 󰜺 no'

# Rofi CMD
function rofi_cmd() {
  rofi -dmenu \
    -p "Action for $(hostname)" \
    -mesg "Uptime: $(uptime -p | sed -e 's/up //g')"
}

# Confirmation CMD
function confirm_cmd() {
  rofi -theme-str 'window {location: center; anchor: center; fullscreen: false; width: 250px;}' \
    -theme-str 'mainbox {children: [ "message", "listview" ];}' \
    -theme-str 'listview {columns: 2; lines: 1;}' \
    -theme-str 'element-text {horizontal-align: 0.5;}' \
    -theme-str 'textbox {horizontal-align: 0.5;}' \
    -dmenu \
    -p 'Confirmation' \
    -mesg 'Are you Sure?'
}

# Ask for confirmation
function confirm_exit() {
  echo -e "$yes\n$no" | confirm_cmd
}

# Pass variables to rofi dmenu
run_rofi() {
  echo -e "$lock\n$suspend\n$logout\n$reboot\n$shutdown" | rofi_cmd
}

# Execute Command
run_cmd() {
  selected="$(confirm_exit)"
  if [[ "$selected" == "$yes" ]]; then
    if [[ $1 == "--shutdown" ]]; then
      systemctl poweroff
    elif [[ $1 == "--reboot" ]]; then
      systemctl reboot
    elif [[ $1 == '--suspend' ]]; then
      systemctl suspend
    elif [[ $1 == "--logout" ]]; then
      if [[ "$DESKTOP_SESSION" == "i3" ]]; then
        i3-msg exit
      fi
    fi
  else
    exit 0
  fi
}

# Actions
function main() {
  chosen="$(run_rofi)"
  case ${chosen} in
    $shutdown)
      run_cmd --shutdown
    ;;
    $reboot)
      run_cmd --reboot
    ;;
    $lock)
      ~/dotfiles/i3/i3lock-launch.sh
    ;;
    $suspend)
    run_cmd --suspend
    ;;
    $logout)
    run_cmd --logout
    ;;
  esac
}

main
