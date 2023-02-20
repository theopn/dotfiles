#!/bin/bash

DOT_DIR=~/dotfiles
DOT_BACKUP_DIR=~/dotfiles.bu

# From https://github.com/vsbuffalo/dotfiles/blob/master/setup.sh
# For regular messages
function green_echo() {
  echo -e "\033[0;32m[Message] $1\033[0m"
}

# For prompts requiring user attention
function yellow_echo() {
  echo
  echo -e "\033[0;33m[Attention] $1\033[0m"
  echo
}

# For error prompts
function red_echo() {
  echo
  echo -e "\033[0;31m[Fatal] $1\033[0m"
  echo
}

function verify_script_dir() {
  script_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
  if [[ "$script_dir" != "$DOT_DIR" || ! -d $DOT_DIR ]]; then
    red_echo "$DOT_DIR directory not found"
    exit 1
  fi
}

# $1 = Name of the related files, $2 command to be prompt when user selected yes
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

# $1 = Current location, $2 Target location
function backup_then_symlink() {
  if [[ -f $2 ]]; then
    green_echo "Existing $2 will be moved to $DOT_BACKUP_DIR."
    mkdir -p $DOT_BACKUP_DIR
    mv $2 $DOT_BACKUP_DIR/
  fi
  green_echo "Creating symlink for $1 at $2..."
  ln -s $1 $2
}

function install() {
  green_echo "
   _____ _              ___      _    __ _ _
  |_   _| |_  ___ ___  |   \ ___| |_ / _(_) |___ ___
    | | | ' \/ -_) _ \ | |) / _ \  _|  _| | / -_|_-<
    |_| |_||_\___\___/ |___/\___/\__|_| |_|_\___/__/
  "

  verify_script_dir

  green_echo "Homebrew Core Formulae/Casks:
  Existing formulae will be uninstalled.
  Some casks might not be compitable with non-macOS systems.
  Do you want to proceed?"
  if selection_prompt "Homebrew Core"; then
    #brew remove --force $(brew list --formula)
    #brew remove --cask --force $(brew list)
    brew bundle --file "$DOT_DIR"/homebrew/Brewfile_core
  fi

  if selection_prompt "Bash"; then # Do not use bracket around a function that returns command (which true/false are)
    backup_then_symlink "$DOT_DIR"/bash/bashrc ~/.bashrc
  fi

  if selection_prompt "Doom"; then
    CURRENT_FILES=("init.el" "config.el" "packages.el")
    mkdir -p ~/.doom.d/
    for FILE in ${CURRENT_FILES[@]}; do
      backup_then_symlink "$DOT_DIR"/doom.d/"$FILE" ~/.doom.d/"$FILE"
    done
  fi

  if selection_prompt "Git"; then
    CURRENT_FILES=("gitignore_global" "gitconfig")
    for FILE in ${CURRENT_FILES[@]}; do
      backup_then_symlink "$DOT_DIR"/git/"$FILE" ~/."$FILE"
    done
  fi

  if selection_prompt "Kitty"; then
    mkdir -p ~/.config/kitty/
    backup_then_symlink "$DOT_DIR"/kitty/kitty.conf ~/.config/kitty/kitty.conf
  fi

  if selection_prompt "Mutt"; then
    CURRENT_FILES=("mailcap" "muttrc")
    mkdir -p ~/.mutt/
    for FILE in ${CURRENT_FILES[@]}; do
      backup_then_symlink "$DOT_DIR"/mutt/"$FILE" ~/.mutt/"$FILE"
    done
  fi

  if selection_prompt "Neofetch"; then
    mkdir -p ~/.config/neofetch/
    backup_then_symlink "$DOT_DIR"/neofetch/config.conf ~/.config/neofetch/config.conf
  fi

  if selection_prompt "Tmux"; then
    backup_then_symlink "$DOT_DIR"/tmux/tmux.conf ~/.tmux.conf
  fi

  if selection_prompt "Vim"; then
    backup_then_symlink "$DOT_DIR"/vim/vimrc ~/.vimrc
  fi

  if selection_prompt "Vim Colorscheme"; then
    mkdir -p ~/.vim/
    mkdir -p ~/.vim/colors/
    backup_then_symlink "$DOT_DIR"/vim/colors/drakai.vim ~/.vim/colors/drakai.vim
  fi

  if selection_prompt "Zsh"; then
    backup_then_symlink "$DOT_DIR"/zsh/zshrc ~/.zshrc
  fi

  if selection_prompt "macOS Settings"; then
    source "$DOT_DIR"/macos/macos_settings.sh
  fi

  green_echo "Homebrew Optional Formulae/Casks:
  This might take a while, and I honestly recommend you to go through each program manually.
  Do you want to proceed?"
  if selection_prompt "Homebrew Optional"; then
    brew bundle --file "$DOT_DIR"/homebrew/Brewfile_optional
  fi

  yellow_echo "Ending the dotfiles installation..."

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

function delete_backup() {
  yellow_echo "Deleting $DOT_BACKUP_DIR..."
  rm -rf $DOT_BACKUP_DIR
}

function help() {
  green_echo "
                    Dotfiles Utility Script Usage

  ===================================================================

  Syntax: ./dotfiles-util.sh <arg>
  -------------------------------------------------------------------

  args:
    insall: Go through the installation process
    delete_backup: Delete $DOT_BACKUP_DIR
  "
}

function main() {
  case $1 in
    install)
      install
    ;;
    delete_backup)
      delete_backup
    ;;
    help)
      help
    ;;
    *) # Invalid option
     red_echo "Invalid option"
     help
    ;;
  esac

  exit 0
}

########### MAIN CALL HERE ##########
main $1
########### MAIN CALL HERE ##########

