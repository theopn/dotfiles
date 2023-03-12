# Deprecated Dotfiles

These are dotfiles that are no longer used, either because I do not use the utility or made a new configuration.

## Sway & Waybar

My experience with Wayland was okay. I did not experience any bug, and I found Sway to be a great replacement for i3. However, using a tiling WM means you not only need a great tiling WM (a compositor for Wayland, I guess) but also a tools surrounding it, such as app launcher, bar, screenshot tool, etc. And it's the area where Wayland falls behind. Below are the tools I use along with Sway, and frankly, I found none of them to be better than X11 counterparts.

- Clipman & wl-clipboard: Wayland clipboard utility (wl-clipboard) and terminal command-line clipboard history manager (clipman).
- fzf: Fuzzy finder is needed to launch the [sway-launcher-desktop](https://github.com/Biont/sway-launcher-desktop).
- Gammastep: Redshift replacement
- Grim & Slurp: Select a region in Wayland compositor (Slurp) and take a screenshot (Grim).
- Waybar: Polybar replacement

