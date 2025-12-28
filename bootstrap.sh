#!/usr/bin/env bash

# macOS utilities
if [[ "$OSTYPE" == "darwin"* ]] || [[ "$OSTYPE" == "macOS" ]]; then
  stow aerospace
fi

stow bash
stow fastfetch
# Create symlink for individual "leaf" files instead of directories
stow --no-folding fish
stow git

stow --no-folding kitty
if type kitten > /dev/null; then
  kitten themes --dump-theme nordfox >> ~/.config/kitty/current-theme.conf
else
  echo "Install Kitty and re-run the kitten themes command!"
fi

stow lf
stow nvim
stow neovide
stow tmux
stow --no-folding vim
stow wezterm
stow zsh

