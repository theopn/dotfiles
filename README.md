# Theo's dotfiles

```php
  _____ _              ___      _    __ _ _
 |_   _| |_  ___ ___  |   \ ___| |_ / _(_) |___
   | | | ' \/ -_) _ \ | |) / _ \  _|  _| | / -_)
   |_| |_||_\___\___/ |___/\___/\__|_| |_|_\___|
```

![fedora_sc](./pictures/2022-07-05_Fedora_Screenshot.png)
![macos_sc](./pictures/2022-07-02_macOS_Screenshot.jpg)

Here are dotfiles for my systems, the 2020 MacBook Air with M1 processor and Lenovo ThinkPad T460s with 6th generation Intel i5.
MBA runs the latest version of macOS, and T460s runs the latest version of Fedora Workstation (Gnome DE) with i3 window manager.
You are free to use all or some of the dotfiles in your system, but

1. My dotfiles are tailored toward me, and they might not suit your taste.
2. The installation script, Homebrew formulae, and some other components are developed on macOS. Cross-platform dotfiles work on my Linux machine, but be cautious when utilizing them on a Linux machine (or any machine).
3. If you are beginning to customize your \*nix system, I highly recommend you explore what each file does before copying them over to your system. I once started my dotfiles and \*nix customization journey by referencing other people's systems (and I still do), but you always should try to understand and learn from them.

Dots in front of the files in the repo are removed for readability reasons, which is ironic.
Don't worry, assuming my script works, it should create symbolic links with dots in front of the file names.

## Pre-requisites (in order of importance)

- \*nix system (Yeah not you Microsoft)
- Git and internet connection (to clone the repo)
- Bash

## Usage

Clone this repository in the home directory.

`git clone https://github.com/theopn/dotfiles.git ~/dotfiles`

Run the installation script (do not run with sudo as Homebrew will not like that).

`bash ~/dotfiles/dotfiles_install.sh`

## After installation

- NeoVim: `:PackerSync`, then `:COQdeps`.
- Emacs: `list-packages` then `all-the-icons-install-fonts`
- Firefox:
  - `about:config` and set `toolkit.legacyUserProfileCustomizations.stylesheets` to true.
  - `about:profiles`, spot your default-release profile or the profile in use, and create symlinks for the `chrome` folder within the profile folder.
- Install fonts. You can utilize the included `fonts/font_test.sh` to test if your terminal emulator correctly displays the fonts.
- Dotfiles for Linux are not automatically deployed. Manually make symlinks for them if needed.

## Highlights

### Cross-platform

#### Bash

Basic shell setting with just ls alias and prompt.

#### Emacs

I'm currently learning Emacs for its Org mode. Popular opinion: Vim is a better "text editor."

#### Firefox

userChrome files based on [minimal-functional-fox](https://github.com/mut-ex/minimal-functional-fox) and HTML for custom startup page based on [Prismatic-Night Firefox theme](https://github.com/3r3bu5x9/Prismatic-Night/) are included. Just be aware that user profile customization using CSS is deprecated and might break anytime with future update of Firefox.

#### Fonts

I have an exquisite taste in fonts.

- [Comic Mono](https://dtinth.github.io/comic-mono-font/): Mono-spaced version of Comic Sans. The best font in the world. Just trust me and use it, you'll love it.
- [NERD font version](https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/FantasqueSansMono) of [Fantasque Sans Mono](https://github.com/belluzj/fantasque-sans). Formally known as *Cosmic Sans Neue Mono*, the name "comes from [author's] realization that at some point it looked like the mutant child of Comic Sans and Helvetica Neue."

As you can tell, I like rather childish fonts, but why serious fonts? When I'm stressed out with work, the last thing I want is to stare at the screen filled with Consolas (bashing MS once again) or other generic fonts and 56 error messages. I highly recommend you explore fonts, especially if you have dyslexia.
I also recommend [DaddyTimeMono Nerd Font](https://www.programmingfonts.org/#daddytimemono) and [Hermit Nerd Font](https://www.programmingfonts.org/#hermit), available to download at [Nerd Fonts website](https://www.nerdfonts.com/font-downloads).

#### Git

Basic gitignore and name/email config.

#### kitty

kitty is my choice of terminal emulator, it is fast and cross-platform. Other than Dracula colorscheme, there's nothing out of ordinary.`color_test.sh` is also included for testing colors.

#### Neofetch

Happy ricing!

#### Neovim

NeoVim is my IDE for C, Lua, Python, and other languages. All the configurations are written in Lua, and Plug-ins are managed by Packer!

#### tmux

Tmux is very usable out of the box. I changed the prefix to `ctrl + a`, use Vim keybinding for navigation, and configured the status bar.

#### vim

I use vanilla Vim as a light text editor, and I have moved away from having plugins (theme, NERDTree, statusline, etc) to utilize built-in features.
`drakai.vim`, which is a custom-made colorscheme of mix between Dracula and Monokai is included. I probably will make this a separate project.

Notable settings include

- 2 spaces as a tab. > for the \t, ␣ for the trailing whitespace, + for the non-breaking space
- Automatic bracket closers, HJKL to navigate split panes, ESC to escape terminal, and jj as an ESC in insert mode.

#### Zsh

Vim keybinding, list, clear, Neovim alias, and prompt setting inspired by "fino-time" theme in oh-my-zsh.
zsh-autocomplete plug-in included.

### Linux

I use the latest version of Fedora Workstation and primarily use a tiling window manager and Gnome as a back up.

#### Dunst

Notification daemon. This is also used to grep result of `cal` command and display calendar as a notification, simulating pop-up calendar of other OS.

#### i3 WM (i3-gap)

i3 is integral in my workflow, and I tried to keep as many stock keybindings as possible. Below is a list of the packages that get mentioned in my configuration.

| Packages | Description |
| -------- | ----------- |
| Brightnessctl | Controls backlight |
| CopyQ | Clipboard manager |
| Feh | Wallpaper |
| Flameshot | Screenshot program |
| i3lock | Simple lock program |
| ImageMagick | Used to make lock screen background for i3lock |
| network-manager-applet | GUI network manager |
| Polybar | Top status bar. Replacement of i3bar |
| Redshift | Blue light filter |
| Rofi | App launcher and more |
| setxkbmap | Everyone needs to swap caps lock and control |
| xinput | Enables trackpad natural scrolling and tab to click |
| xrandr | Controls external display output |
| xss-lock | Can automatically call i3lock before suspend |
| Scripts | All the custom scripts that are included in the dotfiles folder |

Below is a list of keybindings that are unique to my setup.

- Instead of `jkl;`, I use Vim keybinding of `hjkl`.
- Instead of `$mod+h`, `$mod+z` toggles horiZontal split.
- Instead of `$mod+d`, `$mod+Space` launches rofi. Habit from using macOS Spotlight.
- `$mod+d` launches `rofi_mode`, which you can choose various rofi modes like filebrowser, full app list (run), window list, etc
- `$mod+Shift+c, r, e` (originally config reload, restart, exit) all redirects to `system_mode`, which you can choose all the above actions and other system-related actions.
- `Passthrough mode ($mod+Shift+p)` lets you use keybindings that are bound to both i3 and other program (e.g: kitty uses `super + 0` to reset the font adjustment, which you cannot normally use since i3 uses `super + 0` as a shortcut to workspace 0. This can be solved by executing `super + 0` in the passthrough mode).
- `$mod+Shift+v` to execute the clipboard manager.
- `$mod+Shift+s` to execute screenshot tool.

#### Polybar

There are two configurations utilizing essentially the same modules - one with two top and bottom bars and one with just one bar. I honestly don't like either of them, but one bar is better in terms of screen utilization (duh). There's a launch script that can take `onebar` or `twobar` argument and launch bar(s) accordingly.

#### Rofi

Just the Dracula colorscheme.

#### Sway

I am experiencing with Sway. It's a big buggy and I don't like the notion of having one program be WM + compositor + input/output manager + etc, but so far it is the best tiling WM in Wayland. Keybindings are practically the mirror of my i3 setup, and below are programs needs to run my configuration.

| Packages | Description |
| -------- | ----------- |
| Brightnessctl | Backlight control |
| CopyQ | Clipboard manager |
| Dunst | Notification daemon |
| Gammastep | Redshift replacement |
| nm-applet | Network manager |
| Waybar | Polybar replacement |
| Wl-clipboard | Enables `+` clipboard so that I can copy from Neovim |
| Wofi | Rofi replacement |

#### Waybar

Unlike my Polybar setup, I went with a simpler route with one top bar, and detailed information can be accessed by hovering the moues over to modules.

### macOS

#### Homebrew

Bolded items are in `Brewfile_core`, and other items are in `Brewfile_optional`, either because I don't want them to be installed on every machine or is too large.

| Formulae | Description |
| ---- | ----------- |
| Bat| Fancier `cat` replacement (though I don't do alias cat="bat") |
| ffmpeg | Great media type converter. `ffmpeg -i in.xxx out.yyy` |
| hugo | Static website generator |
| htop | System monitor |
| **Lua** | Programming language |
| **node** | Node Js |
| **neofetch** | |
| **NeoVim** | |
| ranger | TUI file explorer
| **tmux** | |

| Type | Casks |
| ---- | ----- |
| Anti-Productivity | - Spotify <br> - Minecraft <br> - Discord: Ugh.
| Development | - Docker <br> - **kitty** <br> - IntelliJ CE: Ugh Java. <br> - **MacVim** <br> - **VSCode**
| Productivity| - Emacs (Cask version): Vim is better, but it doesn't hurt to have it. <br> - **Obsidian: Knowledge base based on local markdown files.** <br> - Notion <br> - Libre Office |
| System (MacOS) | - Alfred: Spotlight replacement. <br> - AppCleaner: App remover for MacOS. <br> - **Amethyst: "Tiling window manager" for MacOS.** <br> - iStat Menus: System monitoring tool.
| Tools | - Bitwarden: The best password manager. <br> - Cryptomator: File encryption before uploading to cloud storage <br> - Syncthing: File synchronization across multiple devices. <br> - Nextcloud: I host a personal Nextcloud server on VPS.
| Web | - Brave: I like to keep a Chromium based browser just in case. <br> - **Firefox: My main browser.** <br> - Thunderbird |

#### Settings

Shows hidden file and full path on the Finder window. Puts screenshot as .jpg file in ~/Downloads by default.

