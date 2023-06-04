#!/bin/zsh

##### Make sure "THEOSHELL_TRASH_DIR" env var is set (mine is in .zshrc) #####
#
# These functions integrate with lf file manager, although they can be used separately
# They are somewhat buggy (limitations of mv command without deleting same name files, etc.), but they are good for my use
#

function yellow_echo() {
  echo -e "\033[0;33m[Trash] ${1}\033[0m"
}

function red_echo() {
  echo -e "\033[0;31m[Trash] ${1}\033[0m"
}

function trash() {
  [[ ! -d ${THEOSHELL_TRASH_DIR} ]] && mkdir -p ${THEOSHELL_TRASH_DIR}
  if [[ -z $@ ]]; then red_echo ':( Please select file(s) to trash!'; return; fi
  for file in $@; do
    mv ${file} ${THEOSHELL_TRASH_DIR} && yellow_echo ":) ${file} moved to trash!" || red_echo ":( Failed to move ${file} to trash"
  done
}

function trash_cd() {
  if [[ ! -d ${THEOSHELL_TRASH_DIR} ]]; then
    red_echo ':O Trash direcoty does not exist! Execute LF file manager with my config to create one'
  else
    cd ${THEOSHELL_TRASH_DIR}
    yellow_echo ":) you're @ ${THEOSHELL_TRASH_DIR}"
  fi
}

function trash_print() {
  if [[ ! -d ${THEOSHELL_TRASH_DIR} ]]; then
    red_echo ':O Trash direcoty does not exist! Execute LF file manager with my config to create one'
  else
    # You can use {.[!.]*,*}, but that does not exist if directory does not contain BOTH .* and *
    # Separate for loop will not work since ZSH will terminate if no wild card match is found (requires nullglob)
    # So settle for *
    for file in ${THEOSHELL_TRASH_DIR}/*
    do
      yellow_echo "${file}"
    done
  fi
}

function trash_empty() {
  if [[ ! -d ${THEOSHELL_TRASH_DIR} ]]; then
    red_echo ':O Trash direcoty does not exist! Execute LF file manager with my config or trash() command to create one'
  else
    for file in ${THEOSHELL_TRASH_DIR}/*
    do
      yellow_echo "Removing ${file} in trash..."
      rm -rf ${file}
    done
  fi
}

