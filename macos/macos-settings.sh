#!/bin/zsh

# A good reference: https://github.com/mathiasbynens/dotfiles/blob/master/.macos

# Automatically hide and show the Dock
defaults write com.apple.dock autohide -bool true
# Dock hide delay
defaults write com.apple.dock autohide-delay -float 0
defaults write com.apple.dock autohide-time-modifier -int 0
# Add a spacer to a dock to separate pinned and opened apps
defaults write com.apple.dock persistent-apps -array-add '{tile-data={}; tile-type="spacer-tile";}'
# Make Dock icons of hidden applications translucent
defaults write com.apple.dock showhidden -bool true
killall Dock

# Show file extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true
# Show hiddne files
defaults write com.apple.finder AppleShowAllFiles -bool true
# Show full POSIX path in the title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
# Path bar in the bottom
defaults write com.apple.finder ShowPathbar -bool true
# Keep the folder first when sorting
defaults write com.apple.finder _FXSortFoldersFirst -bool true
# Use list view in all Finder windows by default: Four-letter codes for the other view modes: `icnv`, `clmv`, `glyv`
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"
# No animation
defaults write com.apple.finder DisableAllAnimations -bool true
killall Finder

# Set the screenshot location
defaults write com.apple.screencapture location -string "${HOME}/Downloads"
# Set the screenshot format to be jpeg
defaults write com.apple.screencapture type -string "jpg"

echo "Mac OS settings have been completed!"
