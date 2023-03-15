# Theo's dotfiles

> Collection of my configuration files. The reason why this README is so long is that I forget things often, definitely not because I like wasting time writing documentation that no one will ever read instead of doing actual work.

![macos-sc](./assets/macos-rice-sc-2023-02-15.jpg)
![fedora-sc](./assets/2023-03-12-fedora-rice-sc.png)

Here are dotfiles for my systems, the 2020 MacBook Air with M1 processor and Lenovo ThinkPad X270 (6th-gen i5).
MBA runs the latest version of macOS, and X270 runs the latest version of Fedora i3 Spin with i3 and Sway window manager (actually, Sway is a Wayland compositor :nerd-emoji:).

You are welcome to take inspiration from any file in this repository, but I do not take any responsibility for any content of the configurations. **Read the code before you use them!**

## Installation

- Configure cross-platform utilities:

```bash
git clone https://github.com/theopn/dotfiles.git ~/dotfiles
~/dotfiles/dotfiles-util.sh --install
~/dotfiles/dotfiles-util.sh --delete-backup # Optional
```

- Configure macOS specific utilities and settings:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew analytics off

~/dotfiles/dotfiles-util.sh --macos-install
```

- Configure i3 WM and related utilities:

```bash
~/dotfiles/dotfiles-util.sh --i3-install
```

### Post-Installation

- Install Doom Emacs:

```bash
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

- Add SSH shortcut for frequently used servers:

```bash
~/dotfiles/dotfiles-util.sh --add-ssh-shortcut
```

- Install fonts using `fontconfig` and the included function in `dotfiles-util.sh`:

1. Navigate to [NERD Fonts download](https://www.nerdfonts.com/font-downloads) website
2. Right click on the font download and copy the link
3. Execute the following

```bash
~/dotfiles/dotfiles-util.sh --install-font #URL
```

- Install CaskayadiaCove and FantasqueSansMono Nerd Fonts using Homebrew:

```bash
brew tap homebrew/cask-fonts &&
brew install --cask font-caskaydia-cove-nerd-font font-fantasque-sans-mono-nerd-font
```

## Highlights

### Cross-platform

#### Bash

> For school SSH server

Basic shell settings with some aliases and prompts.

#### Doom Emacs

> Everything you could think of but a text editor

I use it for Org-agenda (to-do list), Org-roam (the "Second Brain"), and general note-taking in Org mode. I tried to use it as my main text editor a couple times, but I unfortunately do not see an appeal over Neovim.

#### Git

> Hail Linus

No comment.

#### IdeaVim

> Java...

Probably one of the few proprietary SW in my dotfiles, but it is hard to deny that it is a great IDE for Java development. At least I can use Flatpak to sandbox it and emulate some of the Vim keybindings using IdeaVim.

#### kitty

> Terminal emulator

Configuration includes the Dracula color scheme and a borderless look.

#### lf

> Best terminal file manager

Very simple (as I prefer) but complete file manager for my use. Some keybindings other than default vim-style bindings:

- `~` : Go to the home directory
- `ee`: Open a file in `$EDITOR`
- `ec`: You choose what editor you want to open a file in
- `DD`: Move a file to `~/.trash-lf`
- `gs`: [g]it [s]tatus
- `md`: mkdir
- `mf`: Open a file with the supplied name in Neovim
- `ml`, `mr`, `ms`: [m]ark [l]oad, [m]ark [r]emove, [m]ark [s]ave
- `mo`: chmod
- `sh`: Launch `$SHELL` at the current directory

#### Neofetch

> Essential

Happy ricing!

#### Neovim

> I code Neovim to code in Neovim

Due to requests from my friends, my Neovim configuration has migrated to a separate repository [Theovim](https://github.com/theopn/theovim)

#### Qutebrowser

> Modern keyboard based browser

It's based on modern Chromium engine, uses Vim keybindings, and configured through Python. What more can I ask.

#### tmux

> Actual terminal emulator

Keybindings that are drastically different from stock bindings (that make more sense to me), Dracula theme, and status bar with time, current command, and uptime information. No external plugins.

#### Vim

> Focused note taker

- Very simple configurations for [Vimwiki](https://github.com/vimwiki/vimwiki) and other note-taking related plug-ins managed by [vim-plug](https://github.com/junegunn/vim-plug)
- `pastelcula.vim`, a custom-made [base16](https://github.com/chriskempson/base16-vim) theme loosely based on Dracula is included - thanks [Jonathan](https://github.com/JonathanOppenheimer) for helping your colorblind friend

#### Zsh

> The shell

- Greeting message
- `theoshell_plug` and `theoshell_upgrade` automatically download and load ZSH plug-ins I need (currently only zsh-autocomplete), eliminating the need for a bloated shell plug-in manager
- Aliases utilizing Tmux pop-up window (`nvif`, `note`, `wiki`) and miscellaneous functions like `trash()` and `update_dotfiles()`
- Set theory themed prompt:

```bash
echo "
 ⦰ [ parktheo0 ϵ ~/dotfiles ] {main *} $                                              41ms
"
```

### Linux

Fedora Fedora Fedora.

#### Awesome

> Software engineering project, except it's in Lua

Migrated to a separate [repository](https://github.com/theopn/hunted-tiles).

#### Dunst

> Notification daemon

Dracula theme, that's it.

#### i3 (i3-gap) - Now merged!

> Tiling WM that just works

List of packages needed for my configuration:

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
- `$mod + h` -> `$mod + z`: "horiZontal" split
- `$mod + Space`: launches Rofi as well as the default `$mod+d`
- `$mod + Shift + c/r/e`: launches `i3_mode`, from which you can choose config reload, restart, or exit i3
- `$mod + Shift + p`: launches a Rofi menu with power options (lock, suspend, shutdown, etc.)
- `$mod + Shift + s`: Screenshot tool

#### Polybar

> The bar

Colorful Dracula themed bar. I change this a lot so I will not comment further.

#### Rofi

> The launcher

Dracula theme ~~and Vim keybindings for selection~~. Some simple utilities like `rofi-powermenu.sh` is included.

### macOS

#### Homebrew

Bolded items are in `Brewfile_core`, and other items are in `Brewfile_optional`, either because I don't want them to be installed on every machine or are too large.

Formulae:

- **ffmpeg**: `ffmpeg -i in.xxx out.yyy`
- **fzf**: Command line fuzzy finder
- **figlet**: ASCII art generator
- **lf**
- Hugo: Static website generator
- **htop**
- **lua**
- **node**
- **neofetch**
- **neovim**
- **tmux**
- **wget**

| Type              | Casks                                                                                           |
|-------------------|-------------------------------------------------------------------------------------------------|
| Anti-Productivity | - Discord<br> - Minecraft<br> - Spotify                                                         |
| Development       | - Docker<br> - **kitty**<br> - IntelliJ CE<br> - MacTex (No GUI)<br> - **MacVim**               |
| Productivity      | - **Emacs** (GUI version)<br> - Notion<br> - **Obsidian**                                       |
| System (MacOS)    | - AppCleaner<br> - Raycast<br> - Stats                                                          |
| Tools             | - Bitwarden<br> - Cryptomator<br> - GIMP<br> - Nextcloud<br> - OBS<br> - Skim<br> - VLC         |
| Web               | - **Firefox**<br> - Qutebrowser<br> - Thunderbird                                               |

#### Settings

Remove Dock unhide animation, add a Dock spacer, Finder hidden file, screenshot format and location, etc.

