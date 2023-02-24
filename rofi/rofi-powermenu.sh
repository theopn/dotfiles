#!/bin/bash

#
# Revised from https://github.com/adi1090x/rofi/tree/master/files/powermenu
#

# Options
shutdown='  shutdown'
reboot=' 󰜉 reboot'
lock='  lock'
suspend=' 󰤄 suspend'
logout=' 󰗼 logout'
yes='  yes'
no=' 󰜺 no'

# List of commands
shutdown_cmd='systemctl poweroff'
suspend_cmd='systemctl suspend'
reboot_cmd='systemctl reboot'
function lock() {
  LOCK_SCREEN=~/dotfiles/assets/naomi-solarsys-draculafied-lockscreen.png

  if [ -e $LOCK_SCREEN ]
  then
    i3lock -i $LOCK_SCREEN
  else
    i3lock -c "#282a36"
  fi
}

function exit_wm() {
  if [[ "$DESKTOP_SESSION" == "i3" ]]; then
    i3-msg exit
  fi
}

# Power menu window for rofi
function rofi_selection_window() {
  rofi -dmenu \
    -p "Action for $(hostname)" \
    -mesg "Uptime: $(uptime -p | sed -e 's/up //g')"
}

# Pass variables to rofi dmenu
function run_rofi_selection() {
  echo -e "$lock\n$suspend\n$logout\n$reboot\n$shutdown" | rofi_selection_window
}

# Confirmation CMD
function rofi_confirmation_window() {
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
function run_rofi_confirmation() {
  echo -e "$yes\n$no" | rofi_confirmation_window
}

# Execute Command
function confirm_then_run() {
  selected="$(run_rofi_confirmation)"
  if [[ "$selected" == "$yes" ]]; then
    if [[ $1 == "--shutdown" ]]; then
      $shutdown_cmd
    elif [[ $1 == "--reboot" ]]; then
      $reboot_cmd
    elif [[ $1 == '--suspend' ]]; then
      $suspend_cmd
    elif [[ $1 == "--logout" ]]; then
      exit_wm
    fi
  else
    exit 0
  fi
}

# Actions
function main() {
  chosen="$(run_rofi_selection)"
  case ${chosen} in
    $lock)
      lock # No confirmation needed for lock
    ;;
    $shutdown)
      confirm_then_run --shutdown
    ;;
    $reboot)
      confirm_then_run --reboot
    ;;
    $suspend)
      confirm_then_run --suspend
    ;;
    $logout)
      confirm_then_run --logout
    ;;
  esac
}

main
