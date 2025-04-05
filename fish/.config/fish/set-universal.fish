# These variables will be written in ~/.config/fish/fish_variables
# Run them once when you set up your system
set -Ux EDITOR nvim
set -Ux LESSHISTFILE "-"

set -q XDG_CACHE_HOME || set -Ux XDG_CACHE_HOME "$HOME/.cache"
set -q XDG_CONFIG_HOME || set -Ux XDG_CONFIG_HOME "$HOME/.config"
set -q XDG_DATA_HOME || set -Ux XDG_DATA_HOME "$HOME/.local/share"
set -q XDG_STATE_HOME || set -Ux XDG_STATE_HOME "$HOME/.local/state"

set -Ux DOT ~/dotfiles
set -Ux THEOSHELL_TRASH_DIR "$XDG_DATA_HOME/theoshell-trash"
