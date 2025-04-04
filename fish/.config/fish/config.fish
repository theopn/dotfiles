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

