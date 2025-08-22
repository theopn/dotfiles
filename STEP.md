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
chmod +x ~/.config/sketchybar/plugins/\*

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
chmod +x ~/.config/sketchybar/plugins/\*

To start felixkratz/formulae/sketchybar now and restart at login:
brew services start felixkratz/formulae/sketchybar
Or, if you don't want/need a background service you can just run:
LANG="en_US.UTF-8" /opt/homebrew/opt/sketchybar/bin/sketchybar
==> Summary

https://github.com/pyenv/pyenv

rubby setup

defaults write com.apple.finder CreateDesktop false; killall Finder

PDF.file lookup -> more infor -> then applicaoitn change to zathura

==> zathura-pdf-mupdf
To enable this plugin you will need to link it in place.
First create the plugin directory if it does not exist yet:
$ mkdir -p $(brew --prefix zathura)/lib/zathura
Then link the .dylib to the directory:
$ ln -s $(brew --prefix zathura-pdf-mupdf)/libpdf-mupdf.dylib $(brew --prefix zathura)/lib/zathura/libpdf-mupdf.dylib

      More information as to why this is needed: https://github.com/zegervdv/homebrew-zathura/issues/19

==> zathura-pdf-poppler
To enable this plugin you will need to link it in place.
First create the plugin directory if it does not exist yet:
$ mkdir -p $(brew --prefix zathura)/lib/zathura
Then link the .dylib to the directory:
$ ln -s $(brew --prefix zathura-pdf-poppler)/libpdf-poppler.dylib $(brew --prefix zathura)/lib/zathura/libpdf-poppler.dylib

      More information as to why this is needed: https://github.com/zegervdv/homebrew-zathura/issues/19

curl https://raw.githubusercontent.com/homebrew-zathura/homebrew-zathura/refs/heads/master/convert-into-app.sh | sh

Now you can run the app by double clicking on it.

Next steps:
To change the icon, follow the README.md in the repo.
You will notice that when Zathura opens, no file is showing. To open a file, type `:open <path to file>` while within zathura, or `zathura example.pdf` from the command line. Pressing <Tab> will show recent files that the viewer has opened

SKHD setup

screencapture image save fix

peek Installation

