#!/bin/bash

# $1 = File to create a symlink for, $2 = target directory, $3 backup directory
safe_symlink() {
  if [[ -e "$2" ]]; then
    mkdir -p "$3/"
    mv "$2" "$3"
    echo "$2" already exists. Moving to "$3"
  fi
  if [[ ! -e "$1" ]]; then
    echo "$1" not found!
  else
    ln -sf "$1" "$2"
    echo symlink for "$1" created at "$2"
  fi
}

printf "
  _____ _              ___      _    __ _ _
 |_   _| |_  ___ ___  |   \ ___| |_ / _(_) |___
   | | | ' \/ -_) _ \ | |) / _ \  _|  _| | / -_)
   |_| |_||_\___\___/ |___/\___/\__|_| |_|_\___|
\n"

printf "\n1. Homebrew Core Formulae/Casks.
All existing formulae will be uninstalled.
Some formulae might compitable with non-MacOS System.
Do you want to proceed? y/n: "
read -n1 homebrew_input # read -sk1 in zsh
case $homebrew_input in
  y|Y)
    #brew remove --force $(brew list --formula)
    #brew remove --cask --force $(brew list)
    brew bundle --file ~/dotfiles/homebrew/Brewfile_core
    ;;
  *)
    printf "\nSkipping Homebrew core file installation...\n" ;;
esac

printf "\n2. Dotfiles. Do you wish to replace dotfiles? y/n: \n"
read -n1 dotfile_input
case $dotfile_input in
  y|Y)
    # Git
    git_files=("gitignore_global" "gitconfig")
    for v in ${git_files[@]}; do
      safe_symlink ~/dotfiles/git/"$v" ~/."$v" ~/dotfiles_backup/
    done

    # Zsh
    zsh_files=("zshrc" "zsh_plugins")
    for v in ${zsh_files[@]}; do
      safe_symlink ~/dotfiles/zsh/"$v" ~/."$v" ~/dotfiles_backup/
    done

    # Tmux
    tmux_files=("tmux.conf")
    for v in ${tmux_files[@]}; do
      safe_symlink ~/dotfiles/tmux/"$v" ~/."$v" ~/dotfiles_backup/
    done

    # Tmux
    bash_files=("bashrc")
    for v in ${bash_files[@]}; do
      safe_symlink ~/dotfiles/bash/"$v" ~/."$v" ~/dotfiles_backup/
    done

    # Vim
    current="vim"
    vim_files=("vimrc")
    for v in ${vim_files[@]}; do
      safe_symlink ~/dotfiles/vim/"$v" ~/."$v" ~/dotfiles_backup/
    done
    ;;
  *)
    printf "\nSkipping dotfile setups...\n" ;;
esac

