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
    # bash
    current_files=("bashrc")
    for v in ${current_files[@]}; do
      safe_symlink ~/dotfiles/bash/"$v" ~/."$v" ~/dotfiles_backup/
    done

    # Doom Emacs
    current_files=("init.el" "config.el" "packages.el")
    mkdir -p ~/.doom.d/
    for v in ${current_files[@]}; do
      safe_symlink ~/dotfiles/doom.d/"$v" ~/.doom.d/"$v" ~/dotfiles_backup/
    done

    # git
    current_files=("gitignore_global" "gitconfig")
    for v in ${current_files[@]}; do
      safe_symlink ~/dotfiles/git/"$v" ~/."$v" ~/dotfiles_backup/
    done

    # kitty
    current_files=("kitty.conf")
    mkdir -p ~/.config/kitty/
    for v in ${current_files[@]}; do
      safe_symlink ~/dotfiles/kitty/"$v" ~/.config/kitty/"$v" ~/dotfiles_backup/
    done

    # mutt
    current_files=("mailcap" "muttrc")
    mkdir -p ~/.mutt/
    for v in ${current_files[@]}; do
      safe_symlink ~/dotfiles/mutt/"$v" ~/.mutt/"$v" ~/dotfiles_backup/
    done

    # neofetch
    current_files=("config.conf")
    mkdir -p ~/.config/neofetch/
    for v in ${current_files[@]}; do
      safe_symlink ~/dotfiles/neofetch/"$v" ~/.config/neofetch/"$v" ~/dotfiles_backup/
    done

    # nvim
    current_files=("init.lua" "lua")
    mkdir -p ~/.config/nvim/
    for v in ${current_files[@]}; do
      safe_symlink ~/dotfiles/nvim/"$v" ~/.config/nvim/"$v" ~/dotfiles_backup/
    done

    # Tmux
    tmux_files=("tmux.conf")
    for v in ${tmux_files[@]}; do
      safe_symlink ~/dotfiles/tmux/"$v" ~/."$v" ~/dotfiles_backup/
    done

    # Vim
    vim_files=("vimrc")
    for v in ${vim_files[@]}; do
      safe_symlink ~/dotfiles/vim/"$v" ~/."$v" ~/dotfiles_backup/
    done
    # Vim color
    mkdir -p ~/.vim/
    safe_symlink ~/dotfiles/vim/colors ~/.vim/colors ~/dotfiles_backup/

    # Zsh
    zsh_files=("zshrc" "zsh_plugins")
    for v in ${zsh_files[@]}; do
      safe_symlink ~/dotfiles/zsh/"$v" ~/."$v" ~/dotfiles_backup/
    done

    ;;
  *)
    printf "\nSkipping dotfile setups...\n" ;;
esac

printf "\n3. MacOS specific Settings. Do you want to proceed? y/n: "
read -n1 macos_input
case $macos_input in
  y|Y)
    source ~/dotfiles/macos/macos_settings.sh ;;
  *)
    printf "\nSkipping MacOS specific settings... \n" ;;
esac

printf "\n4. Homebrew Optional Formulae/Casks.
Some formulae might compitable with non-MacOS System.
This might take a while
Do you want to proceed? y/n: "
read -n1 homebrew_opt_input
case $homebrew_opt_input in
  y|Y)
    brew bundle --file ~/dotfiles/homebrew/Brewfile_optional
    ;;
  *)
    printf "\nSkipping Homebrew optonal file installation...\n" ;;
esac

printf "\nEnding the dotfile installation..."
printf "
   ____   __   __U _____ u
U | __')u \ \ / /\| ___'|/
 \|  _ \/  \ V /  |  _|'
  | |_) | U_|'|_u | |___
  |____/    |_|   |_____|
 _|| \\_.-,//|(_  <<   >>
(__) (__)\_) (__)(__) (__)\n"
