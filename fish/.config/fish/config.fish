# $figlet -f fuzzy fish
#  .--. _       .-.
# : .-':_;      : :
# : `; .-. .--. : `-.
# : :  : :`._-.': .. :
# :_;  :_;`.__.':_;:_;
#
# Theo's Fish config

# Env var
set -gx EDITOR nvim
set -gx XDG_CONFIG_HOME "$HOME/.config"

# Personal variables
# tilde expansion in quote doesn't work, so either use ~/My\ Drive or $HOME
set -gx CACHE_DIR "$HOME/My Drive/l1-cache"
set -gx CLOUD_DIR "$HOME/My Drive"
set -gx DOT_DIR ~/dotfiles

set -gx QUICK_NOTE_PATH "$CACHE_DIR/quick-note.md"
set -gx DAILY_WRITING_DIR "$CACHE_DIR/dw-$(date +'%Y')"

if status is-interactive
  # source personal alias
  set -l alias_dir $__fish_config_dir/alias.fish
  [ -f $alias_dir ] && source $alias_dir || echo -e (set_color -o red) "[ERR] $alias_dir does not exist!"

  # Enable Vi keybinding
  fish_vi_key_bindings
  set fish_cursor_default block
  set fish_cursor_insert line
  set fish_vi_force_cursor
end

# determine the OS
switch $(uname)
  case "Linux"
    set -x OSTYPE 'Linux'
  case "Darwin"
    set -x OSTYPE 'macOS'
  case '*BSD' 'DragonFly'
    set -x OSTYPE 'BSD'
  case '*'
    set -x OSTYPE 'UNKNOWN'
end

# Homebrew
if [ $OSTYPE = 'macOS' ]
  fish_add_path /opt/homebrew/bin/
end
