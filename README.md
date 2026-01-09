# Theo's dotfiles

| ![macos-sc](./assets/macos-sc.jpg) |
|:--:|
| my macos |

Here are dotfiles for my systems:

- [Framework 13](https://theopark.me/blog/2025-11-06-framework/): runs the latest Fedora KDE Plasma + Sway (see [Haunted Tiles](https://github.com/theopn/haunted-tiles/), my Sway configuration)
- M4 Mac Mini: runs the latest macOS
- ThinkPad X270: currently running Fedora Server, TBD for how I am going to use this
- M1 MacBook Air: served me well since 2021, TBD for what I am going to do with this

The tools in this repository are primarily open-source utilities for development.

> [!IMPORTANT]
> **Read the code before using them!**

## Installation

### Generating SSH Key for GitHub Authentication

If you are not me, skip this section and use HTTPS cloning.

```sh
ssh-keygen -t ed25519 -C "Theo's ED25519 key @ $(hostname) for GitHub Auth"
# Followed by RET to accept the default path + passphrase
# Add the public key to GitHub Settings -> SSH and GPG Keys
cat ~/.ssh/id_ed25519.pub

# Linux: my Fish config automatically launches keychain and ssh-agent
# macOS: execute the following commands
eval "$(ssh-agent -s)"
ssh-add --apple-use-keychain ~/.ssh/id_ed25519
# Did you know that the Heredoc End-of-Transmission delimiter could be anything?
# Double quote means no parameter expansion
cat <<"HI" >> ~/.ssh/config
Host github.com
    IdentityFile ~/.ssh/id_ed25519
    AddKeysToAgent yes
    UseKeychain yes
HI
```

### Cloning & Deploying Dotfiles - macOS

```sh
# Change the hostname
HOSTNAME=<my computer>; sudo scutil --set ComputerName "$HOSTNAME" && sudo scutil --set HostName "$HOSTNAME"

# Install dev tools (including Git)
xcode-select --install

# Homebrew bootstrap
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew analytics off

cd $HOME
git clone git@github.com:theopn/dotfiles.git
cd $HOME/dotfiles

# Install formulae
brew bundle --file ./homebrew/Brewfile_core
brew bundle --file ./homebrew/Brewfile_optional
# Install Nerd Fonts
brew install --cask font-fantasque-sans-mono-nerd-font font-proggy-clean-tt-nerd-font

# Deploy dotfiles using Stow
./bootstrap.sh

# macOS settings
./misc/macos-settings.sh
```

### Cloning & Deploying Dotfiles - Fedora

```sh
# Change the hostname
hostnamectl set-hostname --static <my computer>

# Install packages
sudo dnf upgrade
# TODO: Make a DNF installation script
sudo dnf install git keychain python3-pip stow zsh

cd $HOME
git clone git@github.com:theopn/dotfiles.git
cd $HOME/dotfiles

# Move Fedora default bashrc
mv ~/.bashrc ~/.bashrc-fedora

# Deploy dotfiles using Stow
./bootstrap.sh

# Install fonts with font-cache
mkdir -p ~/.local/share/fonts && cd ~/.local/share/fonts/
# TODO: Check that this link is up-to-date before you proceed
wget -O tmp.zip https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/FantasqueSansMono.zip
unzip tmp.zip && rm tmp.zip
# -v: verbose
# -f: force generation
# -r: erase existing cache
fc-cache -vfr
cd -
```

### Post-installation

Now is a good time to reopen the shell.

```sh
# Fish writes universal variables to `$XDG_CONFIG_HOME/fish/fish_variables` for performance
# I created a script to set global variables only once
fish ~/dotfiles/fish/.config/fish/set-universal.fish

# Create and add directories to the directory bookmark/favorites list for my `cdf` fish/zsh function
mkdir -p $XDG_DATA_HOME/theoshell && touch $XDG_DATA_HOME/theoshell/cd-fav.txt
cat <<AHH >> $XDG_DATA_HOME/theoshell/cd-fav.txt
$XDG_CONFIG_HOME
$XDG_DATA_HOME
AHH

# Neovim Treesitter and LSP servers
nvim -c "TSInstall bash c cpp fish html java javascript latex lua luadoc markdown markdown_inline python sql vim vimdoc"
nvim -c "MasonInstall bash-language-server clangd lua-language-server python-lsp-server texlab"

# Generate and copy public SSH key to my school's remote server
ssh-keygen -t rsa -b 4096 -C "Theo's RSA Key @ $(hostname) for Purdue CS servers auth"
ssh-copy-id -i ~/.ssh/id_rsa.pub <my username>@data.cs.purdue.edu

# Add SSH alias
# The last two lines are only for macOS
# In Linux, my Fish config handles launching Keychain and ssh-agent
cat <<"BYE" >> ~/.ssh/config
Host data
    Hostname data.cs.purdue.edu
    User <my username>
    IdentityFile ~/.ssh/id_rsa
    AddKeysToAgent yes
    UseKeychain yes
BYE
```

### Few More Things (for myself)

- Linux systems: install Sway and follow the instructions in the [Haunted Tiles repository](https://github.com/theopn/haunted-tiles)
- Also Linux: follow the instructions for the private configurations and scripts in [my Framework 13 repository (private)](https://github.com/theopn/framework-13)
- macOS: follow the instructions for the private configurations and scripts in [my script repository (private)](https://github.com/theopn/scripts)

-----

## Utilities

### Shells

- Bash: Basic configuration for my school SSH server
- ZSH: My `$SHELL`
    - Minimal plugin manager (`plug`, `plug_update`) to download and source [zsh-autocompletion](https://github.com/marlonrichert/zsh-autocomplete)
    - Rudimentary implementation of trash-cli (`trash`)
    - Vim mode & Git info in the prompt
- **Fish**: Main interactive shell
    - Cool handmade prompt
    - multicd: `..` to `cd ..`, `...` to `cd../..`, ...
    - CD favorites/bookmark: `cdf_add` to add current directory to the list, `cdf` to use FZF to search the directory and CD into it, and `cdf_edit` to edit the list with `$EDITOR`

### Terminal emulators and multiplexers

- **Wezterm**: My favorite terminal emulator. Watch my YouTube video [Configure Wezterm terminal emulator in Lua with me [ASMR Coding]](https://youtu.be/I3ipo8NxsjY) :)
    - Many keybindings in the video now been changed to match the Tmux keybindings, but the overall functionality remains the same
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
- Kitty: The secondary terminal, but its speed as well as the ability to set scrollback pager to Neovim is slowly winning me over.
    As Kitty's window/tab management is vastly different from Tmux and Wezterm, so are keybindings.
    When I use Kitty, I tend to rely on tilign WM and Neovim for multiplexing.
    - Scrollback history
        - `ctrl+a > up/down`: scroll to previous/next prompt
        - `ctrl+a > [`: open scrollback history in Neovim with special settings
    - Windows (pane in Tmux)
        - `ctrl+a > enter`: create a new window 
        - `ctrl+a > x`: close a window
        - `ctrl+a > h/j/p`: move to a previous window
        - `ctrl+a > k/l/n`: move to a next window
        - `ctrl+a > shift+h/j/p`: swap current window with the previous window
        - `ctrl+a > shift+k/l/n`: swap current window with the next window
        - `ctrl+a > shift+t`: make current window to be the "master window"
        - `ctrl+a > r`: enter resizing mode
        - `ctrl+a > q`: Like "Display Pane Numbers" in Tmux
        - `ctrl+a > shift+q`: Like "Display Pane Numbers" in Tmux but for swapping windows
    - Tab (windows in Tmux)
        - `ctrl+a > c`: create a new tab
        - `ctrl+a >,`: set tab title
        - `ctrl+a > 1-9`: move to tabs 1-9
    - Window Layout
        - `ctrl+a > o`: cycle between tall, horizontal, vertical, and stack layouts
        - `ctrl+a > shift+o`: reverse cycle
        - `ctrl+a > z`: toggle stack (full screen) layout
    - Reference the config for more
- Tmux: Most of the time, I try to use a terminal emulator without Tmux.
    But Tmux still comes in handy, especially when working in a remote environment.
    The following are things that are different from the defaults:
    - Settings: Some terminal information, all indexing starts from 1, increased history limit, and reduced escape time
    - `C-a` is the prefix, and `PFX + C-a` sends `C-a` to the application
    - `PFX + C-r` reloads the config
    - `PFX + c` creates a new window *with the same CWD as the current pane*
    - `PFX + %/"` creates a new pane *with the same CWD as the current pane*
    - `PFX + hjkl` navigates panes (the default `l` is unbound which is `:last-window`)
    - `PFX + C-s` prompts you to send the current pane to a selected window
    - Vi-copy-mode is set, `y` in copy mode yanks the selection -- I do not know what this is not the default since `y` is not bound to anything by default
    - Status bar displaying the current session & window information, CWD, and the current command
    - Use `PFX + ?` (list keys) and `PFX + / + <key>` (describe key) for more help

### Text editors:

- **Neovim**: My IDE, to-do list, Pomodoro timer, and a life-long software engineering project. Read [`:h theovim`](./nvim/.config/nvim/doc/theovim.txt), a ~700~ 1080 lines of help documentation I wrote on custom functions, plugin configurations, and Vim tips I have accumulated over the years
    - Read [Neovim related posts](https://theopark.me/tags/neovim/) in my blog!
- Neovide: Neovim GUI frontend with pretty animations
- Vim: Simple config with Vimwiki for journal writing
    - My Vimrc is built with [Kickstart.vim](https://github.com/theopn/kickstart.vim), featuring
    - `.vim/plugin` directory features [40-line Vimscript bufferline](https://theopark.me/blog/2023-03-17-vimscript-bufferline/) and ["Bufferpanel"](https://theopark.me/blog/2025-06-01-tabpanel/)
    - Choose between `(Bufferpanel + Tabline) || (Tabpanel + Bufferpanel)` by setting `g:theoline_buflist` and `g:theopanel:buflist` variables

### Others

- **LF**: Simple and fast terminal file manager
    - For my custom `preview` script support: install `bat` and Poppler (for `pdftotext` command);
        make sure your terminal has either (1) [Sixel](https://www.arewesixelyet.com/) support and has `chafa` installed OR (2) Kitty's `icat` protocol support
    - If preview breaks for whatever reason, use `C-l` to reset the screen
    - `ee`: Open a file in `$EDITOR`
    - `ec`: You choose what editor you want to open a file in
    - `DD`: Move a file to `$XDG_DATA_HOME/theoshell/trash`
    - `gs`: [g]it [s]tatus
    - `ml`, `mr`, `ms`: [m]ark [l]oad, [m]ark [r]emove, [m]ark [s]ave
    - `md`: mkdir
    - `mf`: Create a file and open in `$EDITOR`
    - `mo`: chmod
- Git: Some Git settings and global `.gitignore` file
- Fastfetch: essential

## macOS

### macOS Tiling WM Setup

> ![NOTE]
> I no longer use Sketchybar, but the config works just fine.
> Please reference the [archive readme](./archive/README.md) for more information.

My Aerospace configuration is very close to the default (the `opt`/`alt`/`⌥` key is the modifier):

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
- chafa: convert images to Sixel format (Lf preview script dependency for certain terminals)
- exiftool: read and modify EXIF data in image files
- fastfetch
- fd: faster alternative to `find` (Neovim Telescope dependency)
- ffmpeg: `ffmpeg -i in.xxx out.yyy`
- figlet: ASCII art generator
- **fish**: de facto default shell
- **fzf**: command line fuzzy finder
- git-filter-repo: a tool to rewrite Git history
- **htop**: system monitor
- hugo: static website generator
- imagemagick: image manipulation
- **lf**: my favorite CLI file manager
- **lua**
- **neovim**: what more can I say
- **node**
- poppler: `pdftotext in.pdf -` (LF preview script dependency)
- r
- ripgrep: faster alternative to `grep` (Neovim Telescope dependency)
- **rust**
- **stow**: managing Dotfiles (used in `./bootstrap.sh`)
- **tmux**: terminal multiplexer
- tree: tree-like directory view
- **wget**: be careful with what you download

Casks:

| Development     | Fun       | Productivity | Sync        | System        | Tools         | Web                |
|-----------------|-----------|--------------|-------------|---------------|---------------|--------------------|
| Docker          | Discord   | **Itsycal**  | Cryptomator | **Aerospace** | **Bitwarden** | **Firefox**        |
| IntelliJ CE     | Minecraft | Notion       | Filen       | **Ice**       | CEmu          | Tailscale          |
| kitty           | Spotify   |              | Nextcloud   | **Maccy**     | GIMP          | Thunderbird        |
| MacTex (no GUI) |           |              | Syncthing   | **Stats**     | KeyCastr      | Ungoogled Chromium |
| **Macvim**      |           |              |             |               | OBS           |                    |
| Neovide         |           |              |             |               | **Skim**      |                    |
| RStudio         |           |              |             |               | VLC           |                    |
| UTM             |           |              |             |               |               |                    |
| **Wezterm**     |           |              |             |               |               |                    |

### macOS Settings

`./misc/macos-settings.sh` includes some macOS settings like adding a Dock spacer, show hidden files in Finder, changing screenshot format and location, etc.

## Archive

> [!WARNING]
> Configuration files in the archive directory are no longer maintained

See the [list of archived configurations](./archive/README.md).

## Inspirations and Shameless Plug

- Font(s): I use [Comic Code](https://tosche.net/fonts/comic-code) (yes, [I paid $30 for the monospaced Comic Sans](http://theopark.me/blog/2025-12-27-comic-code/)), [Fantasque Sans](https://github.com/belluzj/fantasque-sans), and [Ubuntu Sans Mono](https://github.com/canonical/Ubuntu-Sans-Mono-fonts); all [Nerd Font](https://www.nerdfonts.com/) patched
- macOS wallpaper: "Firewatch 2" in [nordic-wallpapers](https://github.com/linuxdotexe/nordic-wallpapers)
- Aerospace: [`default-config.toml`](https://nikitabobko.github.io/AeroSpace/guide.html#default-config) and [DevOps Toolbox's Dotfiles](https://github.com/omerxx/dotfiles)
- LF: [example `lfrc`](https://github.com/gokcehan/lf/tree/master/etc), [Brodie Robertson's Dotfiles](https://github.com/BrodieRobertson/dotfiles/blob/master/config/lf/lfrc), and [`lfimg` for `batorcat` function](https://github.com/thimc/lfimg/blob/master/preview)
- Neovim: Spiritual successor of [Theovim](https://github.com/theopn/theovim)
- Sketchybar: [default plugins](https://github.com/FelixKratz/SketchyBar/tree/master/plugins)
- Vim: [kickstart.vim](https://github.com/theopn/kickstart.vim)
- Wezterm: [my Wezterm config video](https://www.youtube.com/watch?v=I3ipo8NxsjY)
- [Haunted Tiles](https://github.com/theopn/haunted-tiles/) has dotfiles for my minimal, Dracula-themed i3/Sway environment.

