# These variables will be written in ~/.config/fish/fish_variables
# Run them once when you set up your system
set -Ux EDITOR nvim
set -Ux MANPAGER "nvim +Man! +'set nocursorcolumn scrolloff=999'"
set -Ux LESSHISTFILE '-'

set -q XDG_CACHE_HOME || set -Ux XDG_CACHE_HOME "$HOME/.cache"
set -q XDG_CONFIG_HOME || set -Ux XDG_CONFIG_HOME "$HOME/.config"
set -q XDG_DATA_HOME || set -Ux XDG_DATA_HOME "$HOME/.local/share"
set -q XDG_STATE_HOME || set -Ux XDG_STATE_HOME "$HOME/.local/state"

set -Ux DOT_DIR ~/dotfiles
set -Ux THEOSHELL_TRASH_DIR "$XDG_DATA_HOME/theoshell/trash"
set -Ux THEOSHELL_CD_BOOKMARK_DIR "$XDG_DATA_HOME/theoshell/cd-bookmark.txt"

# Personal cloud drive path
set -l cloud_dir "$HOME/My Drive"
set -Ux CLOUD_DIR "$cloud_dir"
set -Ux ARCHIVE_DIR "$cloud_dir/archive"
set -Ux CACHE_DIR "$cloud_dir/l1-cache"
set -Ux CAOS_DIR "$cloud_dir/l1-cache/caos"
