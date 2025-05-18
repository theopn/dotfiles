#!/usr/bin/env bash

# macOS utilities
if [[ "$OSTYPE" == "darwin"* ]] || [[ "$OSTYPE" == "macOS" ]]; then
  stow aerospace
  stow sketchybar
fi

stow bash
stow fastfetch
# Create symlink for individual "leaf" files instead of directories
stow --no-folding fish
stow git
stow kitty
stow lf
stow nvim
stow neovide
stow tmux
stow --no-folding vim
stow wezterm
stow zsh

