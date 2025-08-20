### Installation

Zathura : https://github.com/homebrew-zathura/homebrew-zathura

https://www.reddit.com/r/MacOS/comments/17s6xil/how_do_i_use_zathura_after_homebrew_installation/

setup vide add to this file -


https://gist.github.com/sdushantha/fd0b4f7d69b814317bc33da3a57fdf49



==> sketchybar
Copy the example configuration into your home directory and make the scripts executable:
  mkdir -p ~/.config/sketchybar/plugins
  cp /opt/homebrew/opt/sketchybar/share/sketchybar/examples/sketchybarrc ~/.config/sketchybar/sketchybarrc
  cp -r /opt/homebrew/opt/sketchybar/share/sketchybar/examples/plugins/ ~/.config/sketchybar/plugins/
  chmod +x ~/.config/sketchybar/plugins/*

To start felixkratz/formulae/sketchybar now and restart at login:
  brew services start felixkratz/formulae/sketchybar
Or, if you don't want/need a background service you can just run:
  LANG="en_US.UTF-8" /opt/homebrew/opt/sketchybar/bin/sketchybar
mumbo@unix dotfiles % 


==> Caveats
Copy the example configuration into your home directory and make the scripts executable:
  mkdir -p ~/.config/sketchybar/plugins
  cp /opt/homebrew/opt/sketchybar/share/sketchybar/examples/sketchybarrc ~/.config/sketchybar/sketchybarrc
  cp -r /opt/homebrew/opt/sketchybar/share/sketchybar/examples/plugins/ ~/.config/sketchybar/plugins/
  chmod +x ~/.config/sketchybar/plugins/*

To start felixkratz/formulae/sketchybar now and restart at login:
  brew services start felixkratz/formulae/sketchybar
Or, if you don't want/need a background service you can just run:
  LANG="en_US.UTF-8" /opt/homebrew/opt/sketchybar/bin/sketchybar
==> Summary


https://github.com/pyenv/pyenv

rubby setup


defaults write com.apple.finder CreateDesktop false; killall Finder
