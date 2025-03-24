#!/usr/bin/env bash

if [[ "$OSTYPE" != "darwin"* ]] && [[ "$OSTYPE" != "macOS" ]]; then
  stow aerospace
  stow sketchybar
fi

stow bash
stow git
stow kitty
stow lf
stow nvim
stow tmux

# Create symlink for individual "leaf" files instead of directories
stow --no-folding fish
stow --no-folding fish

#stow wezterm
#stow zsh

