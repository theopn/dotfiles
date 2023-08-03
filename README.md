# Theo's dotfiles

> Collection of my configuration files. The reason why this README is so long is definitely not because I like wasting time writing documentation that no one will ever read instead of doing actual work.

![macos-sc](./assets/2023-03-18-macos-rice-sc.jpg)
![fedora-sc](./assets/2023-03-12-fedora-rice-sc.png)

Here are dotfiles for my systems, 2020 MacBook Air with M1 processor and Lenovo ThinkPad X270 (6th-gen i5).
MBA runs the latest version of macOS, and X270 runs the latest version of Fedora i3 Spin with different tiling WM (primarily i3, AwesomeWM, and Sway window manager -- *actually, Sway is a Wayland compositor :nerd-emoji:*).

You are welcome to take inspirations from any files in this repository, but I do not take any responsibility for any of the contents of the configurations. **Read the code before you use them!**

## Installation

- To configure cross-platform utilities:

```bash
git clone https://github.com/theopn/dotfiles.git ~/dotfiles
~/dotfiles/dotfiles-util.sh --install
~/dotfiles/dotfiles-util.sh --delete-backup # Optional
```

- To configure macOS-specific utilities and settings:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew analytics off

~/dotfiles/dotfiles-util.sh --macos-install
```

- To configure i3 WM and related utilities:

```bash
~/dotfiles/dotfiles-util.sh --i3-install
```

### Install My Other Projects

> Shameless plugs

- [Theovim](https://github.com/theopn/theovim) is my Neovim configuration written 100% in Lua. Theovim includes ~30 carefully selected plug-ins and custom dashboard, statusline, winbar, and other UI components
- [Hunted Tiles](https://github.com/theopn/hunted-tiles/) is a collection of my Wayland compositor configurations (Sway and Hyprland) along with scripts to provide a better Wayland experience

### Post-Installation

- To install Doom Emacs:

```bash
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

- To add SSH shortcut for frequently used servers:

```bash
~/dotfiles/dotfiles-util.sh --add-ssh-shortcut
```

- To install fonts using `fontconfig` and the included function in `dotfiles-util.sh`:

1. Navigate to [NERD Fonts download](https://www.nerdfonts.com/font-downloads) website
2. Right-click on the font download and copy the link
3. Execute the following

```bash
~/dotfiles/dotfiles-util.sh --install-font <URL>
```

- To install CaskayadiaCove and FantasqueSansMono Nerd Fonts using Homebrew:

```bash
brew tap homebrew/cask-fonts &&
brew install --cask font-caskaydia-cove-nerd-font font-fantasque-sans-mono-nerd-font
```

## Shells

### Bash

> Mother of all shells

- Usage: the default shell for my college SSH server and most of the scripts I write
- Config: kept to minimum with basic aliases and env var exports

### Fish

> Rich built-in features, questionable syntax

- Usage: de factor default shell that launches with Wezterm open. Fantastic built-in auto-completion and asynchronous Git status make my life easier. But it ain't my default shell -- zsh is my `$SHELL` for its POSIX computability. In fact, it's not even in my `/etc/shells`
- Config: Cute greeting message, simple prompt with CWD and Git status, and many aliases/functions I use on daily basis

#### Zsh

> The shell

- Greeting message with Oliver ASCII arts
- `theoshell_plug` and `theoshell_upgrade` automatically download and load ZSH plug-ins I need (currently only zsh-autocomplete), eliminating the need for a bloated shell plug-in manager
- Miscellaneous functions like `trash()` and `updater()`

## Terminal Emulators

### kitty

> Feature-rich terminal emulator. Maybe too many features

- Usage: my secondary terminal emulator due to lack of multiplexer and weird font rendering. Intended to use with Tmux
- Config: kept minimal with borderless look and Dracula theme

### tmux

> Actual terminal emulator

Keybindings are drastically different from stock bindings so use the `<C-a> ?` key to read the help documentation. Statusbar with various information without using any external plugins.

### Wezterm

> Over-engineered terminal emulator, nailed the fundamental features. And it's configured int Lua!

- Usage

Most of the keybindings are same as my tmux bindings, except I utilize key table feature to create a custom mode.

## Text Editor

### Doom Emacs

> Good OS, mediocre text editor even with Evil mode

- Usage: to-do list, idea capture, knowledge databases, and other [Second Brain](https://fortelabs.com/blog/basboverview/) functionalities
- Config: includes settings for Org-roam, Org-agenda, and Org-capture along with custom keybindings and functions (e.g. `%` to jump to a matching parenthesis) to make a better editing experience

### Vim

> Focused note taker

- Very simple configurations (almost everything is built without a plugin, including TabLine and StatusLine!) for [Vimwiki](https://github.com/vimwiki/vimwiki) and other note-taking related plug-ins managed by [vim-plug](https://github.com/junegunn/vim-plug)
- `pastelcula.vim`, a custom-made [base16](https://github.com/chriskempson/base16-vim) theme loosely based on Dracula is included - thanks [Jonathan](https://github.com/JonathanOppenheimer) for helping your colorblind friend

## Other Tools

### Git

> Hail Linus

No comment.

### IdeaVim

> Java...

Probably one of the few proprietary SW in my dotfiles, but it is hard to deny that it is a great IDE for Java development.

#### Neofetch

> Essential

Happy ricing!

#### lf

> Best terminal file manager

Very simple (as I prefer) but a complete file manager for my use. Here are my keybindings other than default Vim-style bindings:

- `~` : Go to the home directory
- `ee`: Open a file in `$EDITOR`
- `ec`: You choose what editor you want to open a file in
- `DD`: Move a file to `~/.theoshell/trash` (it integrates with ZSH trash functions)
- `gs`: [g]it [s]tatus
- `md`: mkdir
- `mf`: Open a file with the supplied name in Neovim
- `ml`, `mr`, `ms`: [m]ark [l]oad, [m]ark [r]emove, [m]ark [s]ave
- `mo`: chmod
- `sh`: Launch `$SHELL` at the current directory

#### Qutebrowser

> Modern keyboard-based browser

It's based on the Chromium engine, uses Vim keybindings, and is configured through Python. What more can I ask?

## Linux

Fedora Fedora Fedora.

### Dunst

> Notification daemon

Dracula theme, that's it.

### i3 (i3-gap) - Now merged!

> Tiling WM that just works

List of packages required for my configuration:

- Brightnessctl: Backlight control
- Clipit: Clipboard manager
- Dunst: Notification daemon
- Feh: Wallpaper
- Flameshot: Screenshot on X
- i3lock: Simple lock program
- network-manager-applet: GUI network manager
- Picom: Compositor
- Polybar: This is where all the ricing takes place
- Redshift: Blue light filter/nightlight/nightshift
- Rofi: App and custom power menu launcher
- setxkbmap: Swap caps lock and control, your pinky will thank you
- xinput: Enabling trackpad natural scrolling and tap-to-click
- xss-lock: Calling i3lock before suspending

Not necessary but helpful packages:

- Blueman: GUI Bluetooth manager
- Pavucontrol: Volume control for Pulseaudio (or Pipewire that pretends to be Pulseaudio)
- xrandr: External display output control, should be a dependency for X11 server

List of keybindings differing from the default

- `jkl;` -> `hjkl`
- `$mod + h` -> `$mod + z`: "hori[z]ontal" split
- `$mod + Space`: launches Rofi as well as the default `$mod+d`
- `$mod + Shift + c/r/e`: launches `i3_mode`, from which you can choose config reload, restart, or exit i3
- `$mod + Shift + p`: launches a Rofi menu with power options (lock, suspend, shutdown, etc.)
- `$mod + Shift + s`: Screenshot tool

### Polybar

> The bar

Colorful Dracula themed bar. I change this a lot so I will not comment further.

### Rofi

> The launcher

Dracula theme ~~and Vim keybindings for selection~~. Some simple utilities like `rofi-powermenu.sh` is included.

## macOS

### Homebrew

Bolded items are in `Brewfile_core`, and other items are in `Brewfile_optional`, either because I don't want them to be installed on every machine or are too large.

Formulae:

- **ffmpeg**: `ffmpeg -i in.xxx out.yyy`
- **fzf**: Command line fuzzy finder
- figlet: ASCII art generator
- fish
- imagemagick
- **lf**
- Hugo: Static website generator
- **htop**
- **lua**
- **node**
- **neofetch**
- **neovim**
- rclone
- **rust**
- **tmux**
- **wget**

| Type              | Casks                                                                                            |
|-------------------|--------------------------------------------------------------------------------------------------|
| Anti-Productivity | - Discord<br> - Minecraft<br> - Spotify                                                          |
| Development       | - Docker<br> - **kitty**<br> - IntelliJ CE<br> - MacTex (No GUI)<br> - **MacVim**<br> - Wezterm  |
| Productivity      | - **Emacs**<br> - Notion<br> - **Obsidian**                                                      |
| System (MacOS)    | - AppCleaner<br> - **Raycast**<br> - Stats<br> - **Spaceman**                                    |
| Tools             | - **Bitwarden**<br> - Cryptomator<br> - GIMP<br> - OBS<br> - **Skim**<br> - VLC                  |
| Web               | - **Firefox**<br> - Qutebrowser<br> - Thunderbird                                                |

### Settings

Remove Dock unhide animation, add a Dock spacer, Finder hidden file, screenshot format and location, etc.

