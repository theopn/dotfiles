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

- Install Homebrew, Homebrew formulae, and run settings script (macOS)

```sh
# Homebrew bootstrap
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew analytics off

# Install formulae
cd dotfiles
brew bundle --file ./homebrew/Brewfile_core
brew bundle --file ./homebrew/Brewfile_optional

# macOS settings
zsh ./macos/macos-settings.sh
```

- Install Stow (dnf)

```sh
dnf install stow
```

- Deploy dotfiles

```sh
stow ~/dotfiles/
```

TODO: Stow manual, Homebrew installation & Brewfile deployment, macOS settings
TOOD: Adding SSH shortcut, installing fonts

## Shells

### Fish

> Rich built-in features, questionable syntax

It's the de facto default shell that launches when Wezterm opens.
It has a fantastic built-in auto-completion and stupidly fast asynchronous Git status, but I cannot say I prefer the syntax over POSIX in contrary to the popular opinion.
Because it's not POSIX compatible, Zsh is my `$SHELL`.

Usage: Most of the aliases and functions of my Zsh config are supported in Fish.

### Zsh

> The shell

- Usage:
    - Prompt:
        ```
        [vi-mode]` ➜ /current/path/ git-branch(* for unstaged, + for staged changes) | last-exit-code ❱
        ```
    - Basic aliases: `cdf` to navigate directories quickly using `fzf`,
        `cl` to `clear`, `l` to `ls` with list view and other options, `histgrep` to look up previous commands
    - `trash`, `trash_cd`, `trash_empty`, `trash_print`: trash related functions.
        The trash directory is located in `~/.theoshell/trash`. This directory will be used again for LF
    - `theoshell_plug <github-username>/<repo-name>`: installs Zsh plug-in from a GitHub repository (to `~/.theoshell/zsh-plugins`) and/or source it
        - I only install [zsh-autocomplete](https://github.com/marlonrichert/zsh-autocomplete) by default
    - `theoshell_upgrade`: Upgrade all Zsh plug-ins in `~/.theoshell/zsh-plugins`

## Terminal Emulators and Multiplexers

### tmux

> Universal terminal multiplexer

I usually pair Tmux with a simple terminal emulator like Kitty or Alacritty.
Because I am not the biggest fan of the default keybindings, many are unbound and rebound to Vim-style navigation and window/pane management.

- Usage:
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

### Wezterm

> Over-engineered terminal emulator, nailed the fundamental features, and it is configured in Lua!

Wezterm is my primary terminal emulator/multiplexer!
Watch my YouTube video [Configure Wezterm terminal emulator in Lua with me [ASMR Coding]](https://youtu.be/I3ipo8NxsjY) :)

- Usage:
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

## Text Editors (besides Neovim)

### Doom Emacs

> Good OS, mediocre text editor even with Evil mode

Emacs is my to-do list, idea capture, knowledge databases, and tools for other [Second Brain](https://fortelabs.com/blog/basboverview/) functionalities.

- Usage:
    - All the stock Emacs + Evil mode keybindings
    - `%`: Jump between parenthesis like in Vim
    - `C-c t`: Toggle transparency
    - `C-c a`: Org Agenda
    - `C-c c`: Org Capture
    - `C-c o`: Display Org file outline using `occur`
    - `C-c f`: Find Org-roam node
    - `C-c i`: Insert Org-roam node
    - `C-c r b`: List all Org-roam references in the current buffer
    - `C-c r r`: Sync Org-roam database

### Neovim

> IDE, main text editor, and much more

So many things to talk about, use `:h theovim` to access the documentation I wrote.

### Vim

> Focused note-taker

Because of [my extensive Neovim IDE config](https://github.com/theopn/theovim), My Vimrc is kept minimal as my journal writer with the Vimwiki plug-in.

My Vimrc is built using [Kickstart.vim](https://github.com/theopn/kickstart.vim).

- Config:
    - Kickstart.vim contents, including sensible defaults and LSP setup
    - [Handmade TabLine](https://theopark.me/writing/2023-03-17-vimscript-bufferline/)
    - Vimwiki setup for my personal journal writing

## File Manager

### lf

> My favorite terminal file manager

When I see a CLI file manager with Vim keybindings and a minimalistic feature set, I like it. I use it.

- Usage:
    - `~` : Go to the home directory
    - `ee`: Open a file in `$EDITOR`
    - `ec`: You choose what editor you want to open a file in
    - `DD`: Move a file to `~/.theoshell/trash` (it integrates with my Zsh trash functions)
    - `gs`: [g]it [s]tatus
    - `md`: mkdir
    - `mf`: Open a file with the supplied name in Neovim
    - `ml`, `mr`, `ms`: [m]ark [l]oad, [m]ark [r]emove, [m]ark [s]ave
    - `mo`: chmod
    - `sh`: Launch `$SHELL` at the current directory

## Other Tools

### Git

> Thanks Linus

No comments.

## Miscellaneous Configurations

> [!NOTE]
> These are single-file, minimal configurations that do not change very often.
> These are meant to be manually deployed as needed.
> Use the commands in `./misc/README.md` to deploy these configurations.

- `bashrc`: I prioritize simplicity and performance since Zsh and Fish take care of my interactive uses and most of my scripts are written in Bash.
    Thus, my `.bashrc` is kept minimal with a simple prompt, some aliases, and variables
- `kitty.conf`: Kitty is my secondary terminal emulator. The configuration is kept minimal since I always pair it up with Tmux
- `ideavimrc`: Sorry to disappoint you, but I code in Java sometimes

## macOS

### macOS Tiling WM Setup

Tools used to create my Tokyo-Night-themed tiling WM setup for my macOS environment:

- [Yabai](https://github.com/koekeishiya/yabai)
- [Skhd](https://github.com/koekeishiya/skhd)
- [Sketchybar](https://github.com/FelixKratz/SketchyBar)

To begin, modify the macOS settings as follows:

- "Desktop & Dock" (Mission Control) -> "Displays have separate Spaces" -> On
- "Desktop & Dock" (Menu Bar) -> "Automatically hide and show the menu bar" -> "Always"
- Make shortcuts for switching desktops using a built-in macOS key modifier (if you are to use Yabai for this, it requires disabling SIP)
    - Create 6 Mission Control desktops
    - "Keyboard" -> "Keyboard Shortcuts" -> "Mission Control" -> "Mission Control" -> Turn on "Switch to Desktop n" (where "n" is the number 1 - 6)
    - Set the shortcut to `^n` (`Ctrl n`) or `⌥n` (`Opt n`)
    - While you are at it, go to "Modifier Keys" and switch "Caps Lock key" and "Control key". Your pinky will thank you

Copy or create symlinks for Yabai, Skhd, and Sketchybar configuration files.

Install and start utilities:

- Install Yabai, Skhd, and Sketchybar:
    ```bash
    brew install koekeishiya/formulae/skhd koekeishiya/formulae/yabai FelixKratz/formulae/sketchybar
    ```
- Start Skhd:
    ```bash
    skhd --start-service
    ```
- Use `ctrl + alt - s` keybinding (ctrl + opt + s) to start sketchybar and Yabai.
- Use `ctrl + alt - q` keybinding (ctrl + opt + q) to stop sketchybar and Yabai.

Keybindings:

- The `opt`/`alt` (`⌥`) key is the modifier
- `mod + ret`: Open Wezterm
- `mod + hjkl`: Navigate windows
- `mod + shift + hjkl`: Swap windows
- `mod + ctrl + hjkl`: Move the focused window (tiled with what is already there)
- `mod + shift + y/x`: Mirror left and right/top and bottom
- `mod + shift + r`: Rotate 270 degrees
- `mod + shift + f`: Toggle fullscreen
- `mod + shift + SPC`: Toggle fullscreen
- `mod + shift + 0`: Balance all window size
- `mod + shift + <>`: Increase or decrease window size horizontally
- `mod + shift + =-`: Increase (`=`, meant to represent `+`) or decrease (`-`) window size vertically
- `mod + shift + 1-6`: Move the window to WS 1-6

Yabai is a fantastic tool, but there is a few limitations due to the nature of macOS.

- Layout not persisting after exiting a full-screen video play in Firefox
- Windows with minimum width (e.g., Apple Calendar, Spotify, Discord) not tiling nicely
- Emacs not tiling (even with `(menu-bar-mode t)`)
- Kitty not tiling (with the window decorations removed)
- Being unable to delete a Mission Control desktop with Yabai running
- High CPU usage of `WindowServer` process

Use `cat /tmp/yabai_$USER.err.log` and `cat /tmp/skhd_$USER.err.log` to view the Yabai and Skhd log messages.


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

| Development     | Fun       | Productivity | System (macOS) | Tools       | Web         |
|-----------------|-----------|--------------|----------------|-------------|-------------|
| Docker          | Discord   | Emacs        | **Aerospace**  | Bitwarden   | Firefox     |
| IntelliJ CE     | Minecraft | **Itsycal**  | App Cleaner    | Cryptomator | Thunderbird |
| kitty           | Spotify   | Notion       | Maccy          | GIMP        | Zen Browser |
| MacTex (no GUI) |           |              | **Stats**      | OBS         |             |
| **Wezterm**     |           |              |                | **Skim**    |             |
|                 |           |              |                | VLC         |             |

### Settings

`./macos/macos-settings.sh` includes some macOS settings like adding a Dock spacer, show hidden files in Finder, changing screenshot format and location, etc.

## Archive

> [!WARNING]
> Configuration files in the archive directory are no longer maintained

See the [list of archived configurations](./archive/README.md).

## Credits

- The macOS wallpaper is by [arsenicxs](https://www.deviantart.com/arsenixc/art/Tokyo-Street-Night-684804497)
- I drew inspirations from a lot of difference places, but I am confident to say that all of the config/code in this repository is mine (besides some default configurations)
    - I dislike having code that I do not understand, especially in my day-to-day development environment.
        I try to understand and rewrite configurations on my own
- I give credit to all the amazing developers and the open-source community who developed all these tools I rely on everyday
- and people in communities like [Dotfyle](https://dotfyle.com/) and [r/unixporn](https://www.reddit.com/r/unixporn/) who share their dotfiles
- If you have any questions, concerns, or suggestions, do not hesitate to let me know through issues or PR!

