# Theo's dotfiles

> Collection of my configuration files. The reason why this README is so long is that I forget things often, definitely not because I like wasting time writing documentation that no one will ever read instead of doing actual work.

![macos-sc](./assets/2023-03-18-macos-rice-sc.jpg)
![fedora-sc](./assets/2023-03-12-fedora-rice-sc.png)

Here are dotfiles for my systems, the 2020 MacBook Air with M1 processor and Lenovo ThinkPad X270 (6th-gen i5).
MBA runs the latest version of macOS, and X270 runs the latest version of Fedora i3 Spin with different tiling WM (primarily i3 and Awesome, I used to use Sway window manager. Actually, Sway is a Wayland compositor :nerd-emoji:. I'm planning on testing out Qtile and XMonad).

You are welcome to take inspiration from any file in this repository, but I do not take any responsibility for any content of the configurations. **Read the code before you use them!**

## Installation

- Configure cross-platform utilities:

```bash
git clone https://github.com/theopn/dotfiles.git ~/dotfiles
~/dotfiles/dotfiles-util.sh --install
~/dotfiles/dotfiles-util.sh --delete-backup # Optional
```

- Configure macOS-specific utilities and settings:

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
2. Right-click on the font download and copy the link
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

### Other Projects

> Shameless plugs

- [Theovim](https://github.com/theopn/theovim) "is a somewhat minimal, somewhat opinionated, totally stable, and totally functional IDE layer for Neovim." Neovim is my IDE as well as my primary text editor, and Theovim is a great configuration to make my Neovim experience richer
- [Hunted Tiles](https://github.com/theopn/hunted-tiles/) is a collection of my Wayland compositor configurations (Sway and Hyprland) along with scripts to provide a better Wayland experience

### Cross-platform

#### Bash

> For school SSH server

Basic shell settings with some aliases and prompts.

#### Doom Emacs

> Everything you could think of but a text editor

My setup is focused on making Emacs the ultimate "Second Brain." Org-capture captures my to-do item, writing drafts, and project ideas, Org-agenda displays everything I need to do, and Org-roam is my knowledge database. I tried using Emacs as my text editor a couple of times, but Neovim's simplicity won me over every time.

#### Fish

> The shell part 2

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

Very simple (as I prefer) but a complete file manager for my use. Some keybindings other than default Vim-style bindings:

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

#### Neofetch

> Essential

Happy ricing!

#### Qutebrowser

> Modern keyboard-based browser

It's based on the Chromium engine, uses Vim keybindings, and is configured through Python. What more can I ask?

#### tmux

> Actual terminal emulator

Keybindings are drastically different from stock bindings so use the `<C-a> ?` key to read the help documentation. Statusbar with various information without using any external plugins.

#### Vim

> Focused note taker

- Very simple configurations (almost everything is built without a plugin, including TabLine and StatusLine!) for [Vimwiki](https://github.com/vimwiki/vimwiki) and other note-taking related plug-ins managed by [vim-plug](https://github.com/junegunn/vim-plug)
- `pastelcula.vim`, a custom-made [base16](https://github.com/chriskempson/base16-vim) theme loosely based on Dracula is included - thanks [Jonathan](https://github.com/JonathanOppenheimer) for helping your colorblind friend

#### Wezterm

> Lua lover's dream

It's a very performant GPU-accelerated term emulator with a built-in multiplexer. Until the day I use Neovim running on Wezterm running on Awesome WM...

#### Zsh

> The shell

- Greeting message
- `theoshell_plug` and `theoshell_upgrade` automatically download and load ZSH plug-ins I need (currently only zsh-autocomplete), eliminating the need for a bloated shell plug-in manager
- `trash`, `trash_print`, and `trash_empty` commands
- Aliases utilizing Tmux pop-up window (`nvif`, `note`, `wiki`) and miscellaneous functions like `trash()` and `updater()`
- Set-theory-themed prompt:

```bash
echo "
 ⦰ [ parktheo0 ϵ ~/dotfiles ] {main *} $                                              41ms
"
```

### Linux

Fedora Fedora Fedora.

#### Dunst

> Notification daemon

Dracula theme, that's it.

#### i3 (i3-gap) - Now merged!

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
- figlet: ASCII art generator
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

| Type              | Casks                                                                                  |
|-------------------|----------------------------------------------------------------------------------------|
| Anti-Productivity | - Discord<br> - Minecraft<br> - Spotify                                                |
| Development       | - Docker<br> - **kitty**<br> - IntelliJ CE<br> - MacTex (No GUI)<br> - **MacVim**<br>  |
| Productivity      | - **Emacs**<br> - Notion<br> - **Obsidian**                                            |
| System (MacOS)    | - AppCleaner<br> - **Raycast**<br> - Stats<br> - **Spaceman**                          |
| Tools             | - **Bitwarden**<br> - Cryptomator<br> - GIMP<br> - OBS<br> - **Skim**<br> - VLC        |
| Web               | - **Firefox**<br> - Qutebrowser<br> - Thunderbird                                      |

#### Settings

Remove Dock unhide animation, add a Dock spacer, Finder hidden file, screenshot format and location, etc.

