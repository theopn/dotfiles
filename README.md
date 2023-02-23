# Theo's dotfiles

> Collection of my configuration files. The reason why this README is so long is that I forget things often, definitely not because I like wasting time writing documentation that no one will ever read instead of doing actual work.

![macos-sc](./assets/macos-rice-sc-2023-02-15.jpg)
![fedora-sc](./assets/fedora-rice-sc-2022-07-25.png)

Here are dotfiles for my systems, the 2020 MacBook Air with M1 processor and Lenovo ThinkPad X270 (6th-gen i5).
MBA runs the latest version of macOS, and X270 runs the latest version of Fedora i3 Spin with i3 and Sway window manager (actually, Sway is a Wayland compositor :nerd-emoji:).

You are welcome to take inspiration from any file in this repository, but I do not take any responsibility for any content of the configurations. **Read the code before you use them!**

## Installation

Install Homebrew:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew analytics off # Sorry Brew but I don't want Google to know what packages I installed
```

Install packages and make symlinks using `dotfiles-util.sh`:

```bash
git clone https://github.com/theopn/dotfiles.git ~/dotfiles
~/dotfiles/dotfiles-util.sh install
~/dotfiles/dotfiles-util.sh delete_backup # Optional
```

If you are on Linux machine and has dependencies needed for i3 WM setup:

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
fc-list <font-name> # to verify the installation
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

> I love you Neovim

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
echo "||"
echo "
 ⦰ [ parktheo0 ϵ ~/dotfiles ] {main *} $                                              41ms
"
```

### Linux

Fedora Fedora Fedora.

#### Awesome

Migrated to [separate repository](https://github.com/theopn/hunted-tiles). Configuring Awesome is a Lua software engineering project.

#### Dunst

Notification daemon for X11. Also used to grep the result of the `cal` command and weather information to display them as a pop-up.

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
- setxkbmap: Swap caps lock and control, your pinky will thank you
- xinput: Enabling trackpad natural scrolling and tap-to-click
- xss-lock: Calling i3lock before suspending

Below is a list of keybindings that are unique to my setup.

- Instead of `jkl;`, Vim keybinding of `hjkl`.
- Instead of `$mod+h`, `$mod+z` toggles horiZontal split.
- In addition to the default binding of `$mod+d`, `$mod+Space` launches the app launcher.
- `$mod+Shift+c, r, e` (originally config reload, restart, exit) all redirect to `system_mode`, from which you can choose all the above actions and `systemctl` actions.
- `Passthrough mode ($mod+Shift+p)` lets you use keybindings that are bound to both i3 and another program (e.g: Kitty uses `super + 0` to reset the font adjustment, which overlaps with i3's "move to workspace number 10." This can be solved by executing `super + 0` in the passthrough mode).
- `$mod+Shift+s` to execute a screenshot tool.

Below are packages that you might want to install as well.

- Blueman: GUI Bluetooth manager
- network-manager-applet: GUI network manager
- Pavucontrol: Volume control for Pulseaudio (or Pipewire that pretends to be Pulseaudio)
- xrandr: External display output control, should be a dependency for X11 server

#### Polybar

Two configurations. Both are semi-transparent utilizing a very similar set of modules, but one is more simplified with just one bar and the other has top and bottom bars.

#### Sway

Wayland will be the norm one day, but today is not that day. Wayland is just too buggy for me to use every day. My Sway setup is almost a copy of the i3 setup.

- Clipman & wl-clipboard: Wayland clipboard utility (wl-clipboard) and terminal command-line clipboard history manager (clipman).
- fzf: Fuzzy finder is needed to launch the [sway-launcher-desktop](https://github.com/Biont/sway-launcher-desktop).
- Gammastep: Redshift replacement
- Grim & Slurp: Select a region in Wayland compositor (Slurp) and take a screenshot (Grim).
- Waybar: Polybar replacement, arguably better

#### Waybar

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
- mutt: TUI email client
- **node**
- **neofetch**
- **neovim**
- **ranger**: TUI file explorer
- **tmux**
- w3m: TUI web browser

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

