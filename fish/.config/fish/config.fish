# $figlet -f fuzzy fish
#  .--. _       .-.
# : .-':_;      : :
# : `; .-. .--. : `-.
# : :  : :`._-.': .. :
# :_;  :_;`.__.':_;:_;
#
# Theo's Fish config
# Most of them are in conf.d

if status is-interactive
  # Enable Vi keybinding
  fish_vi_key_bindings
  set fish_cursor_default block
  set fish_cursor_insert line
  set fish_vi_force_cursor
end

# Fish does offer "universal" variables,
# which is essentially a global, exported variable that is written to
# $XDG_CONFIG_HOME/fish/fish_variables (auto generated file) and persists across all Fish instances.
# But there are debates about removing it: https://github.com/fish-shell/fish-shell/issues/7379
# And I personally think it's very anti-declarative design, so I try not to use any.
# For example, if I accidentally execute `set -u VAR oopsie`, $VAR will hunt me down until I notice it
# and run `unset`.
# So yeah, keep $XDG_CONFIG_HOME/fish/fish_variables clean (other than auto-generated variables),
# use "global" (for Fish functions) and "export" (for child processes) instead.
# There is barely any performance trade-off anyway.
set -gx EDITOR nvim
set -gx MANPAGER "nvim +Man! +'set nocursorcolumn scrolloff=999'"
set -gx LESSHISTFILE '-'

set -q XDG_CACHE_HOME   ||  set -gx XDG_CACHE_HOME   "$HOME/.cache"
set -q XDG_CONFIG_HOME  ||  set -gx XDG_CONFIG_HOME  "$HOME/.config"
set -q XDG_DATA_HOME    ||  set -gx XDG_DATA_HOME    "$HOME/.local/share"
set -q XDG_STATE_HOME   ||  set -gx XDG_STATE_HOME   "$HOME/.local/state"

set -gx THEOSHELL_TRASH_DIR "$XDG_DATA_HOME/theoshell/trash"
set -gx THEOSHELL_CDF_DIR "$XDG_DATA_HOME/theoshell/cd-fav.txt"

fish_add_path ~/.local/bin

