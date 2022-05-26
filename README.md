```
  _____ _              ___      _    __ _ _
 |_   _| |_  ___ ___  |   \ ___| |_ / _(_) |___
   | | | ' \/ -_) _ \ | |) / _ \  _|  _| | / -_)
   |_| |_||_\___\___/ |___/\___/\__|_| |_|_\___|
```
# Theo's dotfiles
Here are dotfiles for my system, currently 2020 Macbook Air with M1 processor.
I try to make it as generic as possible, and  it should work on most devices with a little pre-requisites.
Dots in front of the files in repo are removed for readability reason, which is ironic.
Don't worry, assuming my script works, it should create symbolic links with dots in front of the file names.

## Pre-requisites (in order of importance)
- *nix system (Yeah not you Microsoft)
- Git and internet connection (to clone the repo)
- Z Shell (no oh-my-zsh!!) (or bash with a little change in the script)

## Usage
Clone this repository in the home directory.
`git clone https://github.com/theopn/dotfiles.git ~/dotfiles`

Run the installation script (might ask for a password).
`zsh ~/dotfiles/dotfiles_install.sh`

## After installation
- Vim: `:PlugInstall`
- NeoVim: `:PackerSync`, then `:Coqdeps`

# Highlights

## Bash
Barebone shell setting with just ls alias and prompt.

## Zsh
Vim keybinding, list, clear, NeoVim alias, and prompt setting inspired by "fino-time" theme in oh-my-zsh.
zsh-autocomplete theme installed.

## Git
Basic gitignore and name/email config. I might delete email or modify it for the privacy reason. 

## Vim
I use vanilla Vim as a light text editor, so most of the configurations were done by default settings.
Two plug-ins are: Dracula theme (one of the few themes that work on my colorblind eyes) and NERDTree.
I am planning on utilizing netrw instead of NERDTree in the future, eleminating the need for VimPlug.
Notable settings include
- 2 spaces as a tab. > for the \t, ␣ for the trailing whitespace, + for the non-breaking space
- A little wacky way of telling where you're in the file (cursorline/column)
- Automatic bracket closers, HJKL to navigate split panes, ESC to escape terminal, and jj as an ESC in insert mode
- Handmade statusline

## NeoVim
NeoVim is my IDE for C, Lua, Python, and other languages. All the configurations are written in Lua, and Plug-ins are managed by Packer!
- TokyoNight theme
- Lualine
- Treesitter and NvimTree
- coq for the completion engine
  - lspconfig is the default LSP engine
  - coq.artifacts include a lot of LSP engines

## Homebrew
Items with strikethrough are one's that are not automated, probably because I don't want them to be installed on every machine.

Formulae:
- Bat: Fancier `cat` replacement (though I don't do alias cat="bat")
- ffmpeg: Great media type converter. `ffmpeg -i in.xxx out.yyy`
- neofetch: You know what's up

Casks:
- Browser
  - Brave
  - Firefox
- Personal productivity
  - Obsidian: Knowledge base based on local markdown files.
  - Notion: Good for Database.
- Productivity
  - ~~Libre Office~~
- Development
  - IntelliJ CE: Ugh Java
  - iTerm 2
  - MacVim
  - VSCode
- Entertainment
    - ~~Spotify~~
    - ~~Minecraft~~
- System
  - ~~Alfred: Lovely Spotlight replacement~~
  - AppCleaner
  - Bitwarden: The BEST password manager
  - ~~Hidden bar: Hides MacOS menu bar items~~
  - ~~Spectacle: Window manager for MacOS. Officially discontinued and might not work.~~
  - ~~iStats Menu: System monitoring tool~~


## MacOS Setting
Shows hidden file and full path on the Finder window. Puts screenshot as .jpg file in ~/Downloads by default.