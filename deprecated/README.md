# Deprecated Dotfiles

These are dotfiles that are no longer used, either because I do not use the utility or made a new configuration.

## Mutt

It's the best CLI email client. But that doesn't say a lot these days where all emails are essentially embedded HTML websites. Even with `w3m` integration, I found myself not using Mutt at all as I always have Thunderbird open on my computer.

## Skhd & Yabai

Trying to use a fully featured tiling WM emulator in macOS is like having a chihuahua and a grumpy cat in the same house (and yes, I have done that). They constantly argue about who should take priority, and I am tired of window layout breaking and distrusting my workflow. I wish I can disable Quartz and give Yabai full control, but for now, I will have to settle for Raycast's "Maximize," "Left Half," and "First Two Thirds" commands.

## Sway & Waybar

My experience with Wayland was okay. I did not experience any bugs, and I found Sway to be a great replacement for the i3. However, using a tiling WM means you not only need a great tiling WM (a compositor for Wayland, I guess) but also tools that go with it, such as an app launcher, a status bar, a screenshot tool, etc. And it's the area where Wayland falls behind. Below are the tools I used with Sway, and frankly, I found none of them to be better than the X11 counterparts.

- Clipman & wl-clipboard: Wayland clipboard utility (wl-clipboard) and terminal command-line clipboard history manager (clipman).
- fzf: Fuzzy finder is needed to launch the [sway-launcher-desktop](https://github.com/Biont/sway-launcher-desktop).
- Gammastep: Redshift replacement
- Grim & Slurp: Select a region in Wayland compositor (Slurp) and take a screenshot (Grim).
- Waybar: Polybar replacement

