# Deprecated Dotfiles

These are dotfiles that are no longer used, either because I do not use the utility or made a new configuration.

## Mutt

> Last updated: 2022-07-23

It's the best CLI email client.
But that doesn't say a lot these days where all emails are essentially embedded HTML websites.
Even with `w3m` integration, I found myself not using Mutt at all as I always have Thunderbird open on my computer.

## Qutebrowser

> Last updated: 2023-05-19

Firefox + Vimium add-on is good enough for me.

## Midnight Commander

> Last updated: 2023-08-08

It's a decent file manager, especially when dealing with a large number of files.
However, it's slow to launch, keybindings are atrocious, and features are generally not intuitive to use.
For browsing directories quickly, I use [lf](../lf/lfrc), and when I need to bulk-rename files/directories, I prefer [Emacs Dired](https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html) or [oil.nvim](https://github.com/stevearc/oil.nvim).

## macOS Tiling WM Setup (Yabai, Skhd, and Sketchybar)

> Last updated: 2024-02-26

I ~~use~~ used to use [Yabai](https://github.com/koekeishiya/yabai), [Skhd](https://github.com/koekeishiya/skhd), and [Sketchybar](https://github.com/FelixKratz/SketchyBar) to make a Tokyo-Night-themed tiling WM setup for my macOS environment.

To begin, modify the macOS settings as follows:

- "Desktop & Dock" (Mission Control) -> "Displays have separate Spaces" -> On
- "Desktop & Dock" (Menu Bar) -> "Automatically hide and show the menu bar" -> "Always"
- Make shortcuts for switching desktops using a built-in macOS key modifier (if you are to use Skhd for this, it requires disabling SIP)
    - Create 5 Mission Control desktops
    - "Keyboard" -> "Keyboard Shortcuts" -> "Mission Control" -> "Mission Control" -> Turn on "Switch to Desktop n" (where "n" is the number 1 - 5)
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
- `mod + f`: Toggle fullscreen
- `mod + shift + r`: Rotate tree
- `mod + shift + y/x`: Mirror x-axis/y-axis
- `mod + shift + SPC`: Toggle floating
- `mod + shift + e`: Balance all window size
- `mod + shift + hjkl`: ~~Resize window (h to shrink left, j to grow above, k to shrink below, l to grow right)~~ Swap window (use mouse for resizing)
- `mod + ctrl + hjkl`: Move window and tile with what was already there
- `mod + shift + 1-5`: Move to WS 1-5

Yabai is a fantastic tool, but because it's running on top of Aqua (macOS default WM), there are a few limitations.
Here are some bugs I encountered, all to blame Apple for not letting users change Aqua.

- Layout not persisting after exiting a full-screen video play in Firefox
- Windows with minimum width (e.g., Apple Calendar, Spotify, Discord) not tiling nicely
- Emacs not tiling (even with `(menu-bar-mode t)`)
- Kitty not tiling (with the window decorations removed)
- Being unable to delete a Mission Control desktop with Yabai running
- High CPU usage of `WindowServer` process

Use `cat /tmp/yabai_$USER.err.log` and `cat /tmp/skhd_$USER.err.log` to view the Yabai and Skhd log messages.

\+ After the macOS Sonoma update, Sketchybar's WiFi plug-in can no longer directly obtain SSID since Apple disabled it for a security reason.
The workaround is to `awk` the SSID from Apple's Airport Framework file (https://github.com/FelixKratz/SketchyBar/issues/407#issuecomment-1755765318).

\+ I am deprecating my tiling WM setup since I feel like too many "hacks" are involved to make things work.
Also, I have grown to not mind manually tiling windows using Rectangle.

