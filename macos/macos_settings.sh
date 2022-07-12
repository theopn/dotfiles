defaults write com.apple.dock autohide-delay -float 0
defaults write com.apple.dock autohide-time-modifier -int 0
defaults write com.apple.dock persistent-apps -array-add '{tile-data={}; tile-type="spacer-tile";}'
killall Dock
defaults write com.apple.finder AppleShowAllFiles -bool true
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
defaults write com.apple.screencapture location -string "$HOME/Pictures"
defaults write com.apple.screencapture type -string "jpg"
killall Finder
echo "Mac OS settings have been completed!"
