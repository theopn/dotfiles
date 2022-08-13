# Theo's dotfiles

> Collection of my personal configuration files. The reason why this README is so long is because I forget things often, definitely not because I like wasting time writing a documentation that no one will ever read instead of doing actual works.

![fedora_sc](./pictures/2022-07-25_Fedora_Screenshot.png)
![macos_sc](./pictures/2022-07-02_macOS_Screenshot.jpg)

Here are dotfiles for my systems, the 2020 MacBook Air with M1 processor and Lenovo ThinkPad T460s with i5.
MBA runs the latest version of macOS, and T460s runs the latest version of Fedora Workstation with i3 and Sway window manager.
You are free to use all or some of the dotfiles in your system, but

1. My dotfiles are tailored toward me, and they might not suit your taste (in other words, they suck).
2. The installation script is a failed project. I will re-write it one day, but don't bother with it if you're not using Mac.
3. If you're beginning to customizing your \*nix system, don't blindly copy over someone else's dotfiles. You would learn a lot more by understanding and recreating the config to your taste. Also why would you want to use my configuration.

Dots in front of the files in the repo are removed for readability reasons, which is ironic.
Don't worry, assuming my script works, it should create symbolic links with dots in front of the file names.

## Pre-requisites (in order of importance)

- \*nix system (Yeah not you Microsoft)
- Git and internet connection (to clone the repo)
- Bash

## Usage

Clone this repository in the home directory.

`git clone https://github.com/theopn/dotfiles.git $HOME`

Run the installation script (do not run with sudo as Homebrew will not like that).

`bash $HOME/dotfiles/dotfiles_install.sh`

## After installation

- NeoVim: `:PackerSync`, then `:Mason` to check the available lsp servers to install.
- Emacs: `list-packages` then `all-the-icons-install-fonts`.
- Firefox:

  1. `about:config` and set `toolkit.legacyUserProfileCustomizations.stylesheets` to true.
  2. `about:profiles`, spot your default-release profile or the profile in use, and create symlinks for the `chrome` folder within the profile folder.

- Install fonts. You can utilize the included `fonts/font_test.sh` to test if your terminal emulator correctly displays the fonts.
- Dotfiles for Linux are not automatically deployed. Manually make symlinks for them if needed.

## Highlights

### Cross-platform

#### Bash

Basic shell setting with just ls alias and prompt.

#### Emacs

Part-time text editor, full-time to-do list application. My configuration is focused on making Emacs function as a to-do list application by utilizing the Org-mode.

#### Firefox

userChrome files based on [minimal-functional-fox](https://github.com/mut-ex/minimal-functional-fox) and HTML for custom startup page based on [Prismatic-Night Firefox theme](https://github.com/3r3bu5x9/Prismatic-Night/) are included. Just be aware that user profile customization using CSS is deprecated and might break anytime with future update of Firefox.

#### Fonts

I have an exquisite taste in fonts.

- [Comic Mono](https://dtinth.github.io/comic-mono-font/): Mono-spaced version of Comic Sans. The best font in the world. Just trust me and use it, you'll love it.
- [NERD font version](https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/FantasqueSansMono) of [Fantasque Sans Mono](https://github.com/belluzj/fantasque-sans). Formally known as *Cosmic Sans Neue Mono*, the name "comes from [author's] realization that at some point it looked like the mutant child of Comic Sans and Helvetica Neue."

I like rather childish fonts, but why serious fonts? When I'm stressed out with work, the last thing I want is to stare at the screen filled with Consolas and 56 error messages. I highly recommend you explore fonts, especially if you have dyslexia.
Honorable mentions for [DaddyTimeMono Nerd Font](https://www.programmingfonts.org/#daddytimemono) and [Hermit Nerd Font](https://www.programmingfonts.org/#hermit).

#### Git

No comment.

#### kitty

Cross-platform, easily configurable with one file, and fast. All I want from a terminal emulator. `color_test.sh` is also included for testing colors.

#### Mutt

It does everything I need from an email client, which are receiving, reading, deleting, and moving emails to different folders, composing email in my favorite text editor, and doing them efficiently. It's a shame that emails nowadays are filled with HTML contents and images. It can utilize w3m to view *most* HTML contents.

#### Neofetch

Happy ricing!

#### Neovim

Where I spend most of my time on (both the configuration and the text editor itself). It is my IDE for Lua and C, note taker, and so much more. Configuration is written 99.9% in Lua.

#### tmux

Prefix changed from `C-b` to `C-a`, status bar with some information.

#### Vim

Formally where I spent most of my time on, but since I have Neovim now, my configuration does not have any plug-in (other than `netrw`). Still has contains many of the keybindings that is in my muscle memory, and I use it as a simple, zero-distraction text editor.

`drakai.vim`, which is a custom-made colorscheme of mix between Dracula and Monokai is included. I probably will make this a separate project.

#### Zsh

Prompt setting inspired by "fino-time" theme in oh-my-zsh and included zsh-autocomplete plug-in. My favorite aliases are `fzfvi` to quickly search files to open in Neovim, `google` to open up Google search in w3m, and `nvi` because not typing 'm' saves me 3 milliseconds every time I open Neovim.

### Linux

Fedora Fedora Fedora.

#### Awesome

Migrated to separate repository. Configuring Awesome is a Lua software engineering project.

#### Dunst

Notification daemon, and is also used to grep result of `cal` command and weather information to display them as a pop-up.

#### i3 (i3-gap)

Below are the packages mentioned in my configuration.

- Brightnessctl: Backlight control
- Clipit: Clipboard manager
- Dunst: Notification daemon
- Feh: Wallpaper
- Flameshot: Screenshot on X
- i3lock: Simple lock program
- Picom: Compositor
- Polybar: This is where all the ricing takes place.
- Redshift: Blue light filter/nightlight/nightshift
- Rofi: App launcher and more
- setxkbmap: Swap caps lock and control, your pinky will thank you.
- xinput: Enabling trackpad natural scrolling and tap to click
- xss-lock: Calling i3lock before suspending

Below is a list of keybindings that are unique to my setup.

- Instead of `jkl;`, Vim keybinding of `hjkl`.
- Instead of `$mod+h`, `$mod+z` toggles horiZontal split.
- In addition to the default binding of `$mod+d`, `$mod+Space` launches the app launcher.
- `$mod+Shift+c, r, e` (originally config reload, restart, exit) all redirects to `system_mode`, which you can choose all the above actions and `systemctl` actions.
- `Passthrough mode ($mod+Shift+p)` lets you use keybindings that are bound to both i3 and other program (e.g: Kitty uses `super + 0` to reset the font adjustment, which overlaps with i3's "move to workspace number 10." This can be solved by executing `super + 0` in the passthrough mode).
- `$mod+Shift+v` to execute the clipboard manager.
- `$mod+Shift+n` to launch a floating terminal with Vim open for a quick note (saved as `~/Documents/i3_quicknote.txt`)
- `$mod+Shift+s` to execute screenshot tool.

Below are packages that you might want to install as well.

- Blueman: GUI Bluetooth manager
- network-manager-applet: GUI network manager
- Pavucontrol: Volume control for Pulseaudio (or Pipewire that pretends to be Pulseaudio)
- xrandr: External display output control, should be a dependency for X11 server

#### Polybar

Two configuration, they are both semi-transparent with utilizing very similar modules, but one is more simplified with just one bar and the other has the top and bottom bars.

#### Sway

Wayland will be the norm one day, but today is not that day. Wayland is just too buggy for me to use everyday. My Sway setup is almost a copy of the i3 setup.

- Clipman & wl-clipboard: Wayland clipboard utility (wl-clipboard) and terminal command-line clipboard history manager (clipman).
- fzf: Fuzzy finder is needed to launch the [sway-launcher-desktop](https://github.com/Biont/sway-launcher-desktop).
- Gammastep: Redshift replacement
- Grim & Slurp: Select a region in Wayland compositor (Slurp) and take a screenshot (Grim).
- Waybar: Polybar replacement, arguably better

#### Waybar

I like the look better than Polybar. Colorful top bar that contains all the necessary information.

### macOS

#### Homebrew

Bolded items are in `Brewfile_core`, and other items are in `Brewfile_optional`, either because I don't want them to be installed on every machine or is too large.

Formulae:

- Bat: Fancier `cat` replacement
- ffmpeg: Primarily used for `ffmpeg -i in.xxx out.yyy`
- fzf: Command line fuzzy finder
- figlet: ASCII art generator
- Hugo: Static website generator
- htop: System monitor
- **Lua**
- mutt: TUI email client
- **node**: Node JS
- **neofetch**
- **Neovim**
- ranger: TUI file explorer
- **tmux**
- **trash-cli: `trash` command sends the file to `~/.local/share/Trash` folder**
- w3m: TUI web browser. Useful for a quick Google search or viewing HTML contents in Mutt

| Type | Casks |
| ---- | ----- |
| Anti-Productivity | - Spotify <br> - Minecraft <br> - Discord: Ugh
| Development | - Docker <br> - **kitty** <br> - IntelliJ CE: Ugh Java <br> - **MacVim** <br> - **VSCode**
| Productivity| - Emacs (GUI version): To-do list app <br> - **Obsidian: Knowledge base based on local markdown files.** <br> - Notion <br> - Libre Office |
| System (MacOS) | - Alfred: Spotlight replacement <br> - AppCleaner: App remover for MacOS <br> - **Amethyst: "Tiling window manager" for MacOS** <br> - iStat Menus: System monitoring tool
| Tools | - Bitwarden: Password manager <br> - Cryptomator: File encryption tool <br> - Syncthing: File synchronization across multiple devices <br> - Nextcloud: I host a personal Nextcloud server on VPS
| Web | - Brave: Secondary Chromium browser. Would use a pure Chromium, but hey, free money <br> - **Firefox** <br> - Thunderbird |

#### Settings

Remove Dock unhide animation, add a Dock spacer, Finder hidden file, screenshot format and location, etc.

