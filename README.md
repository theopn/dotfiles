# Theo's dotfiles

> Collection of my configuration files. The reason why this README is so long is that I forget things often, definitely not because I like wasting time writing documentation that no one will ever read instead of doing actual work.

![macos-sc](./assets/macos-rice-sc-2023-02-15.jpg)
![fedora-sc](./assets/fedora-rice-sc-2023-02-25.png)

Here are dotfiles for my systems, the 2020 MacBook Air with M1 processor and Lenovo ThinkPad X270 (6th-gen i5).
MBA runs the latest version of macOS, and X270 runs the latest version of Fedora i3 Spin with i3 and Sway window manager (actually, Sway is a Wayland compositor :nerd-emoji:).

You are welcome to take inspiration from any file in this repository, but I do not take any responsibility for any content of the configurations. **Read the code before you use them!**

## Installation

Install Homebrew:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew analytics off # Sorry Brew
```

Install packages and make symlinks using `dotfiles-util.sh`:

```bash
git clone https://github.com/theopn/dotfiles.git ~/dotfiles
~/dotfiles/dotfiles-util.sh install
~/dotfiles/dotfiles-util.sh delete_backup # Optional
```

For Linux machine with dependencies needed for my i3 configuration:

```bash
~/dotfiles/dotfiles-util.sh i3_install
```

### After installation

- Doom Emacs:

```bash
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

- Fonts: CaskayadiaCove and FantasqueSansMono Nerd Fonts are default fonts in my configurations

Using `fontconfig`:

Navigate to [NERD Font](https://www.nerdfonts.com/font-downloads) website and download fonts. Use the following commands to install fonts.

```bash
mkdir -p ~/.local/share/fonts
mv /path/to/otf/or/ttf/file ~/.local/share/fonts/
fc-cache -vf
fc-list <font-name> # Verifying the installation
```

Using Homebrew:

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

I use it every day for a to-do list and project management (Org mode), file management (Dired), and Git interaction (Magit). But I still do not understand why it is a better text editor than Neovim (sorry Emacs users).

#### Git

> Hail Linus

No comment.

#### kitty

> Terminal emulator

Configuration includes the Dracula color scheme and a borderless look.

#### Mutt

> Command-line email client

Configuration includes automatically opening HTML contents in w3m, Vim style keybindings, and Dracula theme. Templates to add a new email account are included.

#### Neofetch

> Essential

Happy ricing!

#### Neovim

> I code Neovim to code in Neovim

Due to requests from my friends, my Neovim configuration has migrated to a separate repository [Theovim](https://github.com/theopn/theovim)

#### tmux

> Actual terminal emulator

Keybindings that are drastically different from stock bindings (that make more sense to me), Dracula theme, and status bar with time, current command, and uptime information. No external plugins.

#### Vim

> Focused note taker

- Very simple configurations for [Vimwiki](https://github.com/vimwiki/vimwiki) and other note-taking related plug-ins managed by [vim-plug](https://github.com/junegunn/vim-plug)
- `drakai.vim`, which is a custom-made colorscheme of a mix between Dracula and Monokai is included in `.vim` folder

#### Zsh

> The shell

- Greeting message
- `theoshell_plug` and `theoshell_upgrade` automatically download and load ZSH plug-ins I need (currently only zsh-autocomplete), eliminating the need for a bloated shell plug-in manager
- Aliases utilizing Tmux pop-up window (`nvif`, `note`, `wiki`) and miscellaneous functions like `trash()` and `update_dotfiles()`
- Choice of a two-line prompt and a simpler one-line prompt (both support Git information)

```bash
echo "
╭─ ⊊ parktheo0 @ lightsaber ϵ ~/dotfiles ⊋
╰─ ⊄ (main *) 14:57:42 ⊅ $                                                            41ms
"
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

#### i3 (i3-gap) -> Now merged!

> Tiling WM that just works

I try to keep my configuration somewhat close to the default, although I'm not sure if I succeeded.

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

```
tray | disp_opt | do_not_disturb | temp | mem | CPU | wifi    workspaces    volume | brightness | battery | date-time | music | weather
```

#### Rofi

> The launcher

Dracula theme ~~and Vim keybindings for selection~~. Some simple utilities like `rofi-powermenu.sh` is included.

#### Sway

> Sorry Wayland, I don't think you are quite ready yet

- Clipman & wl-clipboard: Wayland clipboard utility (wl-clipboard) and terminal command-line clipboard history manager (clipman).
- fzf: Fuzzy finder is needed to launch the [sway-launcher-desktop](https://github.com/Biont/sway-launcher-desktop).
- Gammastep: Redshift replacement
- Grim & Slurp: Select a region in Wayland compositor (Slurp) and take a screenshot (Grim).
- Waybar: Polybar replacement, arguably better

#### Waybar

> Bar 2

I like the look better than Polybar. A colorful top bar contains all the necessary information.

### macOS

#### Homebrew

Bolded items are in `Brewfile_core`, and other items are in `Brewfile_optional`, either because I don't want them to be installed on every machine or are too large.

Formulae:

- bat: Fancier `cat` replacement
- **fd**
- **ffmpeg**: `ffmpeg -i in.xxx out.yyy`
- **fzf**: Command line fuzzy finder
- **figlet**: ASCII art generator
- Hugo: Static website generator
- **htop**
- **Lua**
- mutt: CLI email client
- **node**
- **neofetch**
- **neovim**
- **ranger**: CLI file explorer
- **tmux**
- w3m: web browser

| Type              | Casks                                                                                 |
|-------------------|---------------------------------------------------------------------------------------|
| Anti-Productivity | - Discord<br> - Minecraft<br> - Spotify                                               |
| Development       | - Docker<br> - **kitty**<br> - IntelliJ CE<br> - MacTex (No GUI)<br> - **MacVim**     |
| Productivity      | - **Emacs** (GUI version)<br> - Notion<br> - **Obsidian**                             |
| System (MacOS)    | - AppCleaner<br> - Raycast<br> - Stats                                                |
| Tools             | - Bitwarden<br> - Cryptomator<br> - Nextcloud<br> - OBS<br> - Skim                    |
| Web               | - Chromium<br> - **Firefox**<br> - Thunderbird                                        |

#### Settings

Remove Dock unhide animation, add a Dock spacer, Finder hidden file, screenshot format and location, etc.

