# Theo's dotfiles

| ![macos-sc](./assets/macos-sc.jpg) |
|:--:|
| my macbook |

Here are dotfiles for my systems, M1 MacBook Air and Lenovo ThinkPad X270.
MBA runs the latest version of macOS, and X270 runs the latest version of Fedora Sway Spin.

Tools in this repository are mostly open-source utilities for development.

> [!IMPORTANT]
> **Read the code if you decide to use them!**

## Installation

```sh
# Homebrew bootstrap
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew analytics off

# Install formulae
cd dotfiles
brew bundle --file ./homebrew/Brewfile_core
brew bundle --file ./homebrew/Brewfile_optional

# tap font repository and install Ubuntu Mono Nerd Font
brew tap homebrew/cask-fonts &&
brew install --cask font-ubuntu-mono-nerd-font

# Deploy dotfiles using custom Stow bootstrap script
./bootstrap.sh

# macOS settings
./misc/macos-settings.sh
```

## Utilities

### Shells

- Bash: Basic configuration for my school SSH server
- ZSH: `$SHELL`
    - Functions utilizing fzf (`cdf`, `sshf`, etc.)
    - Minimal plugin manager (`plug`, `update`) to download and source [zsh-autocompletion](https://github.com/marlonrichert/zsh-autocomplete)
    - Rudimentary implementation of trash-cli (`trash`)
    - Git info in the prompt
- Fish: Main interactive shell with personal env var and more weird functions

### Terminal emulators and multiplexers

- Tmux:
    - For the complete list of keybindings, disabled default keybindings, and frequently used default bindings, use `C-a ?` and read the comment in line 20
    - `C-a` is the prefix
    - `PFX c`: Copy mode
    - `PFX hjkl`: Pane navigation
    - `PFX r/x`: Swap panes
    - `PFX s/v`: Create a split horizontally/vertically
    - `PFX -+<>`: Resize pane
    - `PFX C-s`: Send the current pane to the window of the given index (will be prompted for the index)
    - `PFX C-j`: Join the pane from the window of the given index (will be prompted for the index)
    - `PFX t`: Create a new window
    - `PFX q`: Kill a pane (there is no separate `kill-window` binding; close all panes to kill a window)
    - `PFX [/]`: Navigate windows
    - `PFX {/}`: Swap window indices with adjacent windows
    - `PFX m`: Move the current window to the given index
    - `PFX ?`: Open `~/.tmux.conf` in a floating popup

- Kitty: The secondary terminal
- Wezterm: My favorite terminal emulator. Watch my YouTube video [Configure Wezterm terminal emulator in Lua with me [ASMR Coding]](https://youtu.be/I3ipo8NxsjY) :)
    - `LDR` = `C-a`
    - `LDR c`: Copy mode
    - `LDR s/v`: Create a split pane
    - `LDR hjkl`: Navigate pane
    - `LDR q`: Close pane
    - `LDR z`: Zoom pane
    - `LDR o`: Rotate pane
    - `LDR r`: `resize_pane` mode. Use `hjkl` to resize pane and `ESC` or `Enter` to confirm
    - `LDR t`: New tab
    - `LDR [/]` Navigate tab
    - `LDR 1-9`: Navigate tab by index
    - `LDR n`: Launch tab navigator
    - `LDR e`: Rename tab title
    - `LDR m`: `move_tab` mode. Use `hj`/`kl` to move tabs and `ESC` or `Enter` to confirm
        - `LDR {/}`: Move tab without entering the `move_tab` mode
    - `LDR w`: Workspace launcher
    - `$ wezterm show-keys --lua` to get the Lua table of all keybindings available

### Text editors:

- Neovim: So many things to talk about, use `:h theovim` to access the help documentation I wrote
- Vim: Because Neovim is my main text editor, Vim is kept as a simple Vim config with Vimwiki for my journal writing
    - My Vimrc is built using [Kickstart.vim](https://github.com/theopn/kickstart.vim), features [40-line Vimscript bufferline](https://theopark.me/i-made-a-bufferline-with-40-lines-of-vimscript/)

### Others

- LF: Simple and fast terminal file manager
    - `ee`: Open a file in `$EDITOR`
    - `ec`: You choose what editor you want to open a file in
    - `DD`: Move a file to `~/.theoshell/trash`
    - `gs`: [g]it [s]tatus
    - `ml`, `mr`, `ms`: [m]ark [l]oad, [m]ark [r]emove, [m]ark [s]ave
    - `md`: mkdir
    - `mf`: Create a file and open in `$EDITOR`
    - `mo`: chmod
- Git: Some Git settings and global `.gitignore` file

## macOS

### macOS Tiling WM Setup

Tools used to create my Tokyo-Night-themed tiling WM setup for my macOS environment:

- [Aerospace](https://github.com/nikitabobko/AeroSpace)
- [Sketchybar](https://github.com/FelixKratz/SketchyBar)

Install Aerospace and Sketchybar

```bash
brew install nikitabobko/tap/aerospace FelixKratz/formulae/sketchybar
```

Add Aerospace to your login item in System Settings.
Aerospace will automatically launch Sketchybar on the startup.

The configuration and keybindings are very close to the default with a couple of tweaks (the `opt`/`alt`/`⌥` key is the modifier):

- `alt-shift-space`: Toggle between floating and tiling for the current window
- `alt-enter`: Open Wezterm
- `alt-slash`: Toggle between horizontal and vertical tiling
- `alt-comma`: Toggle between horizontal and vertical accordion mode
- `alt-h/j/k/l`: Focus windows
- `alt-shift-h/j/k/l`: Move windows
- `alt-minus/equal` (`-/+`): Resize window
- `alt-1 - 6`: Move to workspace 1 - 6
- `alt-shift-1 - 6`: Move the current tree to workspace 1 - 6
- `alt-tab`: Switch between previously used workspaces
- `alt-shift-tab`: Move current workspace to a different monitor
- `alt-shift-space`: Enter the service mode
    - `esc`: Reload the config and exit the service mode
    - `r`: Reset layout and exit the service mode
    - `alt-shift-h/j/k/l`: Join windows into different trees

### Homebrew

Bolded items are in `Brewfile_core`, and other items are in `Brewfile_optional`, either because I don't want them to be installed on every machine or are too large.

Formulae:

- fd: faster alternative to `find` (Neovim Telescope dependency)
- ffmpeg: `ffmpeg -i in.xxx out.yyy`
- figlet: ASCII art generator
- **fish**: De facto default shell
- **fzf**: Command line fuzzy finder
- git-filter-repo: tool to rewrite Git history
- **htop**: System monitor
- hugo: Static website generator
- imagemagick: Command line image manipulation
- **lf**: My favorite CLI file manager
- **lua**
- **neovim**: My second home
- **node**
- ripgrep: faster alternative to `grep` (Neovim Telescope dependency)
- **rust**
- **tmux**: Universal terminal multiplexer
- tree: Tree-like directory view
- **wget**: Be careful with what you download

Casks:

| Development     | Fun       | Productivity | System (macOS) | Tools         | Web         |
|-----------------|-----------|--------------|----------------|---------------|-------------|
| Docker          | Discord   | **Itsycal**  | **Aerospace**  | **Bitwarden** | **Firefox** |
| IntelliJ CE     | Minecraft | Notion       | App Cleaner    | Cryptomator   | Thunderbird |
| kitty           | Spotify   |              | **Ice**        | GIMP          | Zen Browser |
| MacTex (no GUI) |           |              | Maccy          | OBS           |             |
| **Macvim**      |           |              | **Stats**      | **Skim**      |             |
| **Wezterm**     |           |              |                | VLC           |             |

### Settings

`./misc/macos-settings.sh` includes some macOS settings like adding a Dock spacer, show hidden files in Finder, changing screenshot format and location, etc.

## Archive

> [!WARNING]
> Configuration files in the archive directory are no longer maintained

See the [list of archived configurations](./archive/README.md).

## Inspirations and Shameless Plug

- macOS wallpaper: [arsenicxs](https://www.deviantart.com/arsenixc/art/Tokyo-Street-Night-684804497)
- Aerospace: [`default-config.toml`](https://nikitabobko.github.io/AeroSpace/guide.html#default-config) and [DevOps Toolbox's Dotfiles](https://github.com/omerxx/dotfiles)
- LF: [example `lfrc`](https://github.com/gokcehan/lf/tree/master/etc) and [Brodie Robertson's Dotfiles](https://github.com/BrodieRobertson/dotfiles/blob/master/config/lf/lfrc)
- Neovim: [Theovim](https://github.com/theopn/theovim) but it has changed a lot since Neovim 0.11
- Sketchybar: [the default plugins](https://github.com/FelixKratz/SketchyBar/tree/master/plugins)
- Vim: [kickstart.vim hehe](https://github.com/theopn/kickstart.vim)
- Wezterm: [my Wezterm config video](https://www.youtube.com/watch?v=I3ipo8NxsjY)

