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
stow zathura # this has extra steps, read step.md
stow borders
stow skhd

cp ~/.dotfiles/com.user.skhd.plist ~/Library/LaunchAgents/
launchctl unload ~/Library/LaunchAgents/com.user.skhd.plist 2>/dev/null
launchctl load ~/Library/LaunchAgents/com.user.skhd.plist
