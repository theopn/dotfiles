#!/bin/bash

export DOT_DIR=~/dotfiles
export DOT_BACKUP_DIR=~/dotfiles.bu

########## HELPER FUNCTIONS ##########

# Prints regular messages
function green_echo() {
  echo -e "\033[0;32m[Message] ${1}\033[0m"
}

# Prints prompts requiring user attention
function yellow_echo() {
  echo
  echo -e "\033[0;33m[Attention] ${1}\033[0m"
  echo
}

# Prints error prompts
function red_echo() {
  echo
  echo -e "\033[0;31m[Fatal] ${1}\033[0m"
  echo
}

# Checks if the script is located in $DOT_DIR. Else, end the script
function verify_script_dir() {
  script_dir=$( cd -- "$( dirname -- ${BASH_SOURCE[0]} )" &> /dev/null && pwd )
  if [[ "$script_dir" != "$DOT_DIR" ]]; then
    red_echo "${DOT_DIR} directory not found"
    exit 1
  fi
}

# Prompts user yes/no for the installation of $1
function selection_prompt() {
  yellow_echo "Would you like to install $1 related files?"
  read -p "y/n? > " -n1 -r REPLY # -p for prompt, -n1 for reading 1 character, -r for reading literally
  echo
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    green_echo "Deploying $1 related files..."
    true
  else
    green_echo "Skipping $1 related files..."
    false
  fi
}

# Creates a symlink for $1 at $2
# If a file/dir already exists at $2, move to $DOT_BACKUP_DIR
# E.g.: backup_then_symlink ~/dotfiles/i3/config ~/.config/i3/config
function backup_then_symlink() {
  if [[ -e $2 ]]; then
    yellow_echo "Existing $2 will be moved to ${DOT_BACKUP_DIR}."
    mkdir -p $DOT_BACKUP_DIR
    mv $2 ${DOT_BACKUP_DIR}/
  fi
  green_echo "Creating symlink for $1 at $2..."
  ln -s $1 $2
}

########## INSTALL FUNCTIONS ##########

# Installs cross-platform core utilities
function install() {
  green_echo "
   _____ _              ___      _    __ _ _
  |_   _| |_  ___ ___  |   \ ___| |_ / _(_) |___ ___
    | | | ' \/ -_) _ \ | |) / _ \  _|  _| | / -_|_-<
    |_| |_||_\___\___/ |___/\___/\__|_| |_|_\___/__/
  "

  verify_script_dir

  if selection_prompt 'Doom'; then
    CURRENT_FILES=("init.el" "config.el" "packages.el")
    mkdir -p ~/.doom.d/
    for FILE in ${CURRENT_FILES[@]}; do
      backup_then_symlink ${DOT_DIR}/doom.d/${FILE} ~/.doom.d/${FILE}
    done
  fi

  if selection_prompt 'Fish'; then
    CURRENT_FILES=("config.fish" "alias.fish")
    mkdir -p ~/.config/fish/
    for FILE in ${CURRENT_FILES[@]}; do
      backup_then_symlink ${DOT_DIR}/fish/${FILE} ~/.config/fish/${FILE}
    done
    # Fish functions
    CURRENT_FILES=("fish_prompt.fish")
    mkdir -p ~/.config/fish/functions/
    for FILE in ${CURRENT_FILES[@]}; do
      backup_then_symlink ${DOT_DIR}/fish/functions/${FILE} ~/.config/fish/functions/${FILE}
    done
  fi

  if selection_prompt 'Git'; then
    CURRENT_FILES=("gitignore_global" "gitconfig")
    for FILE in ${CURRENT_FILES[@]}; do
      backup_then_symlink ${DOT_DIR}/git/${FILE} ~/.${FILE}
    done
  fi

  if selection_prompt 'lf'; then
    CURRENT_FILES=('lfrc' 'icons')
    mkdir -p ~/.config/lf/
    for FILE in ${CURRENT_FILES[@]}; do
      backup_then_symlink ${DOT_DIR}/lf/${FILE} ~/.config/lf/${FILE}
    done
  fi

  if selection_prompt 'Qutebrowser'; then
    if [[ "${OSTYPE}" == "darwin"* ]]; then
      mkdir -p ~/.qutebrowser/
      backup_then_symlink ${DOT_DIR}/qutebrowser/config.py ~/.qutebrowser/config.py
    elif [[ "${OSTYPE}" == "linux-gnu"* ]];then
      mkdir -p ~/.config/qutebrowser/
      backup_then_symlink ${DOT_DIR}/qutebrowser/config.py ~/.config/qutebrowser/config.py
    fi
  fi

  if selection_prompt 'Tmux'; then
    backup_then_symlink ${DOT_DIR}/tmux/tmux.conf ~/.tmux.conf
  fi

  if selection_prompt 'Vim'; then
    backup_then_symlink ${DOT_DIR}/vim/vimrc ~/.vimrc
    mkdir -p ~/.vim/
    backup_then_symlink ${DOT_DIR}/vim/colors ~/.vim/colors
  fi

  if selection_prompt 'Wezterm'; then
    mkdir -p ~/.config/wezterm/
    backup_then_symlink ${DOT_DIR}/wezterm/wezterm.lua ~/.config/wezterm/wezterm.lua
  fi

  if selection_prompt 'Zsh'; then
    backup_then_symlink ${DOT_DIR}/zsh/zshrc ~/.zshrc
  fi

  yellow_echo 'Ending the dotfiles installation...'

  green_echo "
     ____   __   __U _____ u
  U | __')u \ \ / /\| ___'|/
   \|  _ \/  \ V /  |  _|'
    | |_) | U_|'|_u | |___
    |____/    |_|   |_____|
   _|| \\_.-,//|(_  <<   >>
  (__) (__)\_) (__)(__) (__)
  "
}

# Installs Homebrew formulae and runs macOS specific 
function macos-install() {
  green_echo "Starting macOS specific installlation process..."

  verify_script_dir

  if [[ "$OSTYPE" != "darwin"* ]]; then
    red_echo "You are not using macOS! OSTYPE == $OSTYPE"
    exit 1
  fi

  green_echo 'Homebrew Core Formulae/Casks:
  Existing formulae will be uninstalled.
  Some casks might not be compitable with non-macOS systems.
  Do you want to proceed?'
  if selection_prompt 'Homebrew Core'; then
    # brew remove --force $(brew list --formula)
    # brew remove --cask --force $(brew list)
    brew bundle --file ${DOT_DIR}/homebrew/Brewfile_core
  fi

  if selection_prompt 'macOS Settings'; then
    . "${DOT_DIR}/macos/macos-settings.sh"
  fi

  green_echo 'Homebrew Optional Formulae/Casks:
  This might take a while, and I honestly recommend you to go through each program manually.
  Do you want to proceed?'
  if selection_prompt 'Homebrew Optional'; then
    brew bundle --file ${DOT_DIR}/homebrew/Brewfile_optional
  fi

  yellow_echo 'Ending the macos specific installation...'
}

function i3_install() {
  green_echo "Starting i3 WM specific installlation process..."

  verify_script_dir

  if [[ "${OSTYPE}" != "linux-gnu"* ]]; then
    red_echo "Theo you are not using Linux and therefore you are uncool. OSTYPE == $OSTYPE"
    exit 1
  fi

  if selection_prompt 'i3'; then
    mkdir -p ~/.config/i3/
    backup_then_symlink ${DOT_DIR}/i3/config ~/.config/i3/config
  fi

  yellow_echo 'Polybar will be launched from the dotfiles directory; no install needed'

  if selection_prompt 'dunst'; then
    mkdir -p ~/.config/dunst/
    backup_then_symlink ${DOT_DIR}/dunst/dunstrc ~/.config/dunst/dunstrc
  fi

  if selection_prompt 'rofi'; then
    mkdir -p ~/.config/rofi/
    backup_then_symlink ${DOT_DIR}/rofi/config.rasi ~/.config/rofi/config.rasi
  fi
}

########## AUXILIARY FUNCTIONS ##########

# Deletes $DOT_BACKUP_DIR directory
function delete_backup() {
  yellow_echo "Deleting ${DOT_BACKUP_DIR}..."
  rm -rf $DOT_BACKUP_DIR
}

# Prompts user and adds SSH host and user to ~/.ssh/config
function add_ssh_shortcut() {
  mkdir -p ~/.ssh/
  [[ ! -e ~/.ssh/config ]] && touch ~/.ssh/config
  echo -n 'add_ssh_shortcut) Enter host nickname (e.g. data): '
  read host_nickname
  echo -n 'add_ssh_shortcut) Enter host URL (e.g. data.cs.best.school.in.the.world.located.in.west.lafayette.edu): '
  read host_url
  echo -n 'add_ssh_shortcut) Enter username for the host (e.g. good_student_69): '
  read username
  echo "Host $host_nickname
    Hostname $host_url
    User $username" >> ~/.ssh/config
  green_echo "${username}@${host_url} has been added to the SSH config! Try <ssh ${host_nickname}>."
}

# Installs font from the supplied URL to ~/.local/share/fonts
# $1: URL of the font zip file to be installed
#     It is highly recommended that the URL comes from https://nerdfonts.com/font-downloads
function install_font() {
  if [[ ! $1 ]]; then red_echo 'Target URL is missing!'; exit 1; fi
  mkdir -p ~/.local/share/fonts
  cd ~/.local/share/fonts
  wget -O temp-font.zip $1
  unzip temp-font.zip
  rm temp-font.zip
  fc-cache -vf
  cd - > /dev/null 2>&1
  green_echo "A font from $1 successfully installed!"
}

# Prints help message about this script
function help() {
  green_echo "
                    Dotfiles Utility Script Usage

  ===================================================================

  Syntax: ./dotfiles-util.sh <arg>
  -------------------------------------------------------------------

  args:
    --install             : Deploy configuration symlinks for cross-platform utilities
    --macos-install       : Deploy configuration symlinks for macOS and related utilities
    --delete-backup       : Delete $DOT_BACKUP_DIR
    --add-ssh-shortcut    : Add a new SSH shortcut at ~/.ssh/config
    --install-font <URL>  : wget a font file from URL (preferably from NERDFont website) and install it at ~/.local/share/fonts/
  "
}

########### MAIN CALL HERE ##########

# Executes util functionalities based on the supplied flag ($1)
function main() {
  case $1 in
    "--install")
      install
    ;;
    "--macos-install")
      macos-install
    ;;
    "--delete-backup")
      delete_backup
    ;;
    "--add-ssh-shortcut")
      add_ssh_shortcut
    ;;
    "--install-font")
      install_font $2
    ;;
    "--help")
      help
    ;;
    *)
     red_echo 'Invalid option'
     help
    ;;
  esac

  exit 0
}

main $@

########### MAIN CALL HERE ##########

