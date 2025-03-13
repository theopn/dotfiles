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

