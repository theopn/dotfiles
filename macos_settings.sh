defaults write com.apple.screencapture location -string "$HOME/Download"
defaults write com.apple.screencapture type -string "jpg"
defaults write com.apple.finder AppleShowAllFiles -bool true
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
killall Finder
echo "Mac OS settings have been completed!"
