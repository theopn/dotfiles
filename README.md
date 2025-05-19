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
# Clone the repository
git clone git@github.com:theopn/dotfiles.git

# Homebrew bootstrap
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew analytics off

# Install formulae
cd $HOME/dotfiles
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
- **Fish**: Main interactive shell
    - Fish writes universal variables to `$XDG_CONFIG_HOME/fish/fish_variables` for better performance.
        Thus, you need to set global variables only once for your system.
        I created a script to set all the universal environment variables at once; run: `fish $HOME/dotfiles/fish/.config/fish/set-universal.fish`
    - Abbreviations in `conf.d/abbreviations.fish`
    - Functions in `functions/`
    - Other modular configurations in `conf.d` and `config.fish` (in the order of source priority)

### Terminal emulators and multiplexers

- Tmux - things different from the defaults:
    - Settings: Some terminal information, all indexing starts from 1, increased history limit, and reduced escape time
    - `C-a` is the prefix, and `PFX + C-a` sends `C-a` to the terminal
    - `PFX + C-r` reloads the config
    - `PFX + c` creates a new window *with the same CWD as the current pane*
    - `PFX + %/"` creates a new pane *with the same CWD as the current pane*
    - `PFX + hjkl` navigates panes (the default `l` is unbound which is `:last-window`)
    - `PFX + C-s` prompts you to send the current pane to a selected window
    - Vi-copy-mode is set, `y` in copy mode yanks the selection -- I do not know what this is not the default since `y` is not bound to anything by default
    - Dracula themed status bar displaying the current session & window information, CWD, and current command running
    - Use `PFX + ?` (list keys) and `PFX + / + <key>` (describe key) for more help
- **Wezterm**: My favorite terminal emulator. Watch my YouTube video [Configure Wezterm terminal emulator in Lua with me [ASMR Coding]](https://youtu.be/I3ipo8NxsjY) :)
    - Many keybindings in the video now has been changed to match the Tmux keybindings, but the overall functionality remains the same
    - `LDR` = `C-a`
    - `LDR [`: enters the copy mode
    - `LDR :`: opens the command palette
    - `LDR s`: opens the fuzzy picker for workspace (similar to sessions in Tmux)
    - Tabs (similar to windows in Tmux)
        - `LDR w`: opens the picker for tabs
        - `LDR c`: creates a new tab
        - `LDR p/n`: navigates previous/next tabs
        - `LDR ,`: renames the tab
        - `LDR .`: enters the `move_tab` mode where you can use `hj`/`kl` to order tabs and `ESC` or `RET` to confirm
        - `LDR 1-9`: moves to the tab with the given index
    - Panes
        - `LDR %/"`: creates a new pane vertically/horizontally
        - `LDR hjkl`: navigates pane
        - `LDR SPC`: zooms the current pane
        - `LDR x`: closes the current pane
        - `LDR !`: breaks the current pane into a new tab
        - `LDR r`: enters the `resize_pane` mode where you can resize the pane with `<>-+` and `ESC` or `RET` to confirm
    - `$ wezterm show-keys --lua` to get the Lua table of all keybindings available
- Kitty: The secondary terminal
    - Colorscheme can be changed with `kitten themes`

### Text editors:

- **Neovim**: My IDE, to-do list, and software engineering project. Reference [`:h theovim`](./nvim/.config/nvim/doc/theovim.txt), a 700-lines of help documentation I wrote on custom functions, plugin configurations, and Vim tips I have accumulated over the years
- Neovide: Neovim GUI frontend with pretty animations
- Vim: Because Neovim is my main text editor, Vim is kept as a simple Vim config with Vimwiki for my journal writing
    - My Vimrc is built using [Kickstart.vim](https://github.com/theopn/kickstart.vim), features [40-line Vimscript bufferline](https://theopark.me/i-made-a-bufferline-with-40-lines-of-vimscript/)

### Others

- **LF**: Simple and fast terminal file manager
    - For my custom `preview` script support: install `bat` and Poppler (for `pdftotext` command);
        make sure your terminal has either (1) [Sixel](https://www.arewesixelyet.com/) support and has `chafa` installed OR (2) Kitty's `icat` protocol support
    - If preview breaks, which might happen while displaying images in Tmux, use `C-l` to reset the screen
    - `ee`: Open a file in `$EDITOR`
    - `ec`: You choose what editor you want to open a file in
    - `DD`: Move a file to `~/.theoshell/trash`
    - `gs`: [g]it [s]tatus
    - `ml`, `mr`, `ms`: [m]ark [l]oad, [m]ark [r]emove, [m]ark [s]ave
    - `md`: mkdir
    - `mf`: Create a file and open in `$EDITOR`
    - `mo`: chmod
- Git: Some Git settings and global `.gitignore` file; `editor` and `pager` are set to require Neovim
- Fastfetch: essential

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
**Do not use `brew services start sketchybar` to launch Sketchybar** as Sketchybar needs to be launched after Aerospace has been launched (via `after-startup-command` in `aerospace.toml`) in order to correctly render workspaces.

The configuration and keybindings are very close to the default with a couple of tweaks (the `opt`/`alt`/`⌥` key is the modifier):

- `alt-shift-space`: Toggle between floating and tiling for the current window
- `alt-enter`: Open Wezterm
- `alt-d`: Open Terminal with a command to pick a window to focus
    This feature is a workaround and should be updated when it is implemented natively, see: https://github.com/nikitabobko/AeroSpace/discussions/1371
- `alt-slash`: Toggle between horizontal and vertical tiling
- `alt-comma`: Toggle between horizontal and vertical accordion mode
- `alt-h/j/k/l`: Focus windows
- `alt-shift-h/j/k/l`: Move windows
- `alt-minus/equal` (`-/+`): Resize window
- `alt-1 - 9`: Move to workspace 1 - 9
- `alt-shift-1 - 9`: Move the current tree to workspace 1 - 9
- `alt-tab`: Switch between previously used workspaces
- `alt-shift-tab`: Move current workspace to a different monitor
- `alt-shift-space`: Enter the service mode
    - `esc`: Reload the config and exit the service mode
    - `r`: Reset layout and exit the service mode
    - `alt-shift-h/j/k/l`: Join windows into different trees

### Homebrew

Bolded items are in `Brewfile_core`, and other items are in `Brewfile_optional`, either because I don't want them to be installed on every machine or are too large.

Formulae:

- bat: `cat` with syntax highlighting (LF preview script dependency)
- chafa: convert images to Sixel format (Lf preview script dependency)
- fastfetch
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
- **neovim**: Purpose of my life
- **node**
- poppler: `pdftotext in.pdf -` (LF preview script dependency)
- ripgrep: faster alternative to `grep` (Neovim Telescope dependency)
- **rust**
- **tmux**: Universal terminal multiplexer
- tree: Tree-like directory view
- **wget**: Be careful with what you download

Casks:

| Development     | Fun       | Productivity | Sync        | System        | Tools         | Web         |
|-----------------|-----------|--------------|-------------|---------------|---------------|-------------|
| Docker          | Discord   | **Itsycal**  | Cryptomator | **Aerospace** | **Bitwarden** | Chromium    |
| IntelliJ CE     | Minecraft | Notion       | Filen       | **Ice**       | GIMP          | **Firefox** |
| kitty           | Spotify   |              | Syncthing   | **Maccy**     | OBS           | Thunderbird |
| MacTex (no GUI) |           |              |             | **Stats**     | **Skim**      |             |
| **Macvim**      |           |              |             |               | VLC           |             |
| Neovide         |           |              |             |               |               |             |
| **Wezterm**     |           |              |             |               |               |             |

### macOS Settings

`./misc/macos-settings.sh` includes some macOS settings like adding a Dock spacer, show hidden files in Finder, changing screenshot format and location, etc.

## Archive

> [!WARNING]
> Configuration files in the archive directory are no longer maintained

See the [list of archived configurations](./archive/README.md).

## Inspirations and Shameless Plug

- macOS wallpaper: "Firewatch 2" in [nordic-wallpapers](https://github.com/linuxdotexe/nordic-wallpapers)
- Aerospace: [`default-config.toml`](https://nikitabobko.github.io/AeroSpace/guide.html#default-config) and [DevOps Toolbox's Dotfiles](https://github.com/omerxx/dotfiles)
- LF: [example `lfrc`](https://github.com/gokcehan/lf/tree/master/etc), [Brodie Robertson's Dotfiles](https://github.com/BrodieRobertson/dotfiles/blob/master/config/lf/lfrc), and [`lfimg` for `batorcat` function](https://github.com/thimc/lfimg/blob/master/preview)
- Neovim: Spiritual successor of [Theovim](https://github.com/theopn/theovim)
- Sketchybar: [default plugins](https://github.com/FelixKratz/SketchyBar/tree/master/plugins)
- Vim: [kickstart.vim](https://github.com/theopn/kickstart.vim)
- Wezterm: [my Wezterm config video](https://www.youtube.com/watch?v=I3ipo8NxsjY)
- [Haunted Tiles](https://github.com/theopn/haunted-tiles/) has dotfiles for my minimal, Dracula-themed Fedora i3/Sway Spin environment.

