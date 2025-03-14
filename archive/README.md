# Archives

These configuration files are no longer maintained.

## Mutt

> Last updated: 2022-07-23

Why do modern emails essentially have to be embedded HTML websites, ugh.

## Qutebrowser

> Last updated: 2023-05-19

Firefox + Vimium add-on is good enough for me.

## Neofetch

> Last updated: 2023-08-24

Neofetch is deprecated, so I tried to look for a replacement.
Then I realized that all *fetch programs are essentially the same thing and that I really do not need any of them.
The included theme is inspired by "insert name" from [Neofetch Themes](https://github.com/Chick2D/neofetch-themes).

## Yabai & SKHD

> Last Updated: 2024-10-24

I now use Aerospace, which feels much snappier and intuitive for me.
Below are the instructions.

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

## Installation Script

> Last Updated: 2024-10-23

Ridiculously over-engineered and crappy installation script, I now use `stow` and individual commands to deploy my dotfiles and install required software.
Below is the instruction on how I set up dotfiles with the script.

- Configure cross-platform utilities using the following commands:
    ```bash
    git clone https://github.com/theopn/dotfiles.git ~/dotfiles
    ~/dotfiles/dotfiles-util.sh --install
    ~/dotfiles/dotfiles-util.sh --delete-backup # Optional
    ```

- Install Homebrew and configure macOS-specific utilities and settings using the following commands:
    ```bash
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    brew analytics off

    ~/dotfiles/dotfiles-util.sh --macos-install
    ```

- Install Doom Emacs:
    ```bash
    git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
    ~/.config/emacs/bin/doom install
    ```
    - I had the best experience running natively-compiled [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus) in macOS
    ```bash
    brew tap d12frosted/emacs-plus
    brew install emacs-plus --with-native-comp --with-modern-doom3-icon
    ~/.emacs.d/bin/doom sync # if you have already initialized Doom with other version of Emacs
    ```

- Add SSH shortcut for frequently used servers:
    ```bash
    ~/dotfiles/dotfiles-util.sh --add-ssh-shortcut
    # Follow the prompt
    ```

- To install fonts via `fontconfig` and the included function in `dotfiles-util.sh`:
    1. Navigate to [NERD Fonts download](https://www.nerdfonts.com/font-downloads) website
    2. Right-click on the font download and copy the link
    3. Execute the following
        ```bash
        $FONT_URL=thing-you-just-copied
        ~/dotfiles/dotfiles-util.sh --install-font $FONT_URL
        ```

- To install Iosevka Nerd Font (for terminal emulators and text editors) and FantasqueSansM Nerd Fonts (for window managers) using Homebrew:
    ```bash
    brew tap homebrew/cask-fonts &&
    brew install --cask font-iosevka-nerd-font font-fantasque-sans-mono-nerd-font
    ```

