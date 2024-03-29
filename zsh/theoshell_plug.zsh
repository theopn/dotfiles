#!/bin/zsh

##### Make sure "THEOSHELL_PLUGIN_DIR" env var is set (mine is in .zshrc) #####

# Double check double check
function source_file() {
  [ -f ${THEOSHELL_PLUGIN_DIR}/$1 ] && source ${THEOSHELL_PLUGIN_DIR}/$1
}

# Function to source or load a plugin
function theoshell_plug() {
  PLUGIN_NAME=$(echo $1 | cut -d "/" -f 2)
  if [[ -d ${THEOSHELL_PLUGIN_DIR}/${PLUGIN_NAME} ]]; then
    source_file ${PLUGIN_NAME}/${PLUGIN_NAME}.plugin.zsh || \
    source_file ${PLUGIN_NAME}/${PLUGIN_NAME}.zsh
  else
    git clone "https://github.com/${1}.git" ${THEOSHELL_PLUGIN_DIR}/${PLUGIN_NAME}
  fi
}

function theoshell_upgrade() {
  for repo in ${THEOSHELL_PLUGIN_DIR}/*
  do
    echo "refreshing ${repo}:"
    cd ${repo} && git pull && cd - > /dev/null
  done
}

