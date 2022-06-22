# Theo's dotfiles

```php
  _____ _              ___      _    __ _ _
 |_   _| |_  ___ ___  |   \ ___| |_ / _(_) |___
   | | | ' \/ -_) _ \ | |) / _ \  _|  _| | / -_)
   |_| |_||_\___\___/ |___/\___/\__|_| |_|_\___|
```

Here are dotfiles for my systems, the 2020 MacBook Air with M1 processor and Lenovo ThinkPad T460s with 6th generation Intel i5 running Fedora.
You are free to use all or some of the dotfiles in your system, but

1. My dotfiles are tailored toward me, and they might not suit your taste.
2. The installation script, Homebrew formulae, and some other components are developed on macOS. Other dotfiles work on my Linux machine, but be cautious when utilizing them on a Linux machine (or any machine).
3. If you are beginning to customize your \*nix system, I highly recommend you explore what each file does before copying them over to your system. I once started my dotfiles and \*nix customization journey by referencing other people's systems (and I still do), but you always should try to understand and learn from them.

Dots in front of the files in the repo are removed for readability reasons, which is ironic.
Don't worry, assuming my script works, it should create symbolic links with dots in front of the file names.

## Pre-requisites (in order of importance)

- \*nix system (Yeah not you Microsoft)
- Git and internet connection (to clone the repo)
- Bash

## Usage

Clone this repository in the home directory.

`git clone https://github.com/theopn/dotfiles.git ~/dotfiles`

Run the installation script (do not run with sudo as Homebrew will not like that).

`bash ~/dotfiles/dotfiles_install.sh`

## After installation

- NeoVim: `:PackerSync`, then `:COQdeps`.
- Firefox:
  - `about:config` and set `toolkit.legacyUserProfileCustomizations.stylesheets` to true.
  - `about:profiles`, spot your default-release profile or the profile in use, and create symlinks for the `chrome` folder within the profile folder.
- Install fonts. You can utilize the included `fonts/font_test.sh` to test if your terminal emulator correctly displays the fonts.
- Manually deploy some config files for GUI applications.
- Dotfiles for Linux are not automatically deployed. Manually make symlinks for them if needed.

## Highlights

### Cross-platform

#### Fonts

I have an exquisite taste in fonts.

- [Comic Mono](https://dtinth.github.io/comic-mono-font/): Mono-spaced version of Comic Sans. The best font in the world. Just trust me and use it, you'll love it.
- [NERD font version](https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/FantasqueSansMono) of [Fantasque Sans Mono](https://github.com/belluzj/fantasque-sans). Formally known as *Cosmic Sans Neue Mono*, the name "comes from [author's] realization that at some point it looked like the mutant child of Comic Sans and Helvetica Neue."

As you can tell, I like rather childish fonts, but why serious fonts? When I'm stressed out with work, the last thing I want is to stare at the screen filled with Consolas or other generic fonts and 56 error messages. I highly recommend you explore fonts, especially if you have dyslexia.
I also recommend [DaddyTimeMono Nerd Font](https://www.programmingfonts.org/#daddytimemono) and [Hermit Nerd Font](https://www.programmingfonts.org/#hermit), available to download at [Nerd Fonts website](https://www.nerdfonts.com/font-downloads).

#### Bash

Basic shell setting with just ls alias and prompt.

#### Zsh

Vim keybinding, list, clear, Neoim alias, and prompt setting inspired by "fino-time" theme in oh-my-zsh.
zsh-autocomplete plug-in included.

#### tmux

Tmux is very usable out of the box. I changed the prefix to `ctrl + a`, use Vim keybinding for navigation, and configured the status bar.

#### Git

Basic gitignore and name/email config.

#### Vim

I use vanilla Vim as a light text editor, and I have moved away from having plugins (theme, NERDTree, statusline, etc) to utilize built-in features.
Modified version of [gruvbox](https://github.com/morhetz/gruvbox) theme is bundled (I commented out italics font, which does not play well with MacOS), which I found to be working great with my colorblindness and overall compatibility with tmux/iTerm. There is no plugin installed.

Notable settings include

- 2 spaces as a tab. > for the \t, ␣ for the trailing whitespace, + for the non-breaking space
- Automatic bracket closers, HJKL to navigate split panes, ESC to escape terminal, and jj as an ESC in insert mode.

#### Neovim

NeoVim is my IDE for C, Lua, Python, and other languages. All the configurations are written in Lua, and Plug-ins are managed by Packer!

#### kitty

kitty is my choice of terminal emulator, it is fast and cross-platform. My config is nothing out of ordinary with Gruvbox theme that I boosted contrast up. `color_test.sh` is also included for testing colors.

#### Firefox

userChrome files based on [minimal-functional-fox](https://github.com/mut-ex/minimal-functional-fox) and HTML for custom startup page based on [Prismatic-Night Firefox theme](https://github.com/3r3bu5x9/Prismatic-Night/) are included. Just be aware that user profile customization using CSS is deprecated and might break anytime with future update of Firefox.

### Linux

#### i3 WM (i3-gap)

Here are difference I made from the stock configuration.

0. Reorganization, terminal, and workspace name.
1. Vim keybinding instead of the weird semicolon, super + space to launch `rofi drun`.
2. Regular reload, restart, and exit key has been remapped to a "system_mode," which is a custom mode connected to `i3_system_mode.sh` and provides i3/systemctl functions (reload, exit, suspend, shutdown, etc).
3. Auto executed programs on startup: wallpaper `feh`, `polybar`, and blue light filter `redshift`.

#### Polybar

Polybar goes with i3. Just a simple Gruvbox bar, it displays two batteries since my ThinkPad T460s has two.

### macOS

#### Homebrew

Bolded items are in `Brewfile_core`, and other items are in `Brewfile_optional`, either because I don't want them to be installed on every machine or is too large.

| Formulae | Description |
| ---- | ----------- |
| **Bat** | Fancier `cat` replacement (though I don't do alias cat="bat") |
| ffmpeg | Great media type converter. `ffmpeg -i in.xxx out.yyy` |
| **neofetch** | |
| **NeoVim** | |
| **tmux** | |
| **Lua** | |
| **node** | |

| Type | Casks |
| ---- | ----- |
| Browser | - Brave <br/> - **Firefox** |
| Productivity| - **Obsidian: Knowledge base based on local markdown files.** <br/> - Notion <br/> - Libre Office |
| Development | - **kitty** <br/> - iTerm 2 <br/> - IntelliJ CE: Ugh Java. <br/> - **MacVim** <br/> - **VSCode**
| Entertainment | - Spotify <br/> - Minecraft
| Tools | - Bitwarden: The best password manager. <br/> - Cryptomator: File encryption before uploading to cloud storage <br/> - Syncthing: File synchronization across multiple devices. 
| System (MacOS) | - Alfred: Spotlight replacement. <br/> - AppCleaner: App remover for MacOS. <br/> - **Amethyst: Tiling manager for MacOS.** <br/> - iStat Menus: System monitoring tool.

#### Settings

Shows hidden file and full path on the Finder window. Puts screenshot as .jpg file in ~/Downloads by default.
