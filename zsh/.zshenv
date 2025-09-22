#              8
#              8
#.oooo. .oPYo. 8oPYo. .oPYo. odYo. o    o
#  .dP  Yb..   8    8 8oooo8 8' `8 Y.  .P
# oP'     'Yb. 8    8 8.     8   8 `b..d'
#`Yooo' `YooP' 8    8 `Yooo' 8   8  `YP'

export EDITOR=nvim
export MANPAGER='nvim +Man!'
export LESSHISTFILE=-

export HISTFILE=~/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000

export XDG_CACHE_HOME=${XDG_CACHE_HOME:="$HOME/.cache"}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="$HOME/.config"}
export XDG_DATA_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}
export XDG_STATE_HOME=${XDG_DATA_HOME:="$HOME/.local/state"}

export DOT_DIR="$HOME/dotfiles"

export FZF_DEFAULT_COMMAND='fd --hidden --strip-cwd-prefix --exclude ".git"'
export FZF_DEFAULT_OPTS='--layout=reverse --cycle --height=50% --margin=5% --border=double'

export THEOSHELL_TRASH_DIR="$XDG_DATA_HOME/theoshell/trash"
export THEOSHELL_CDF_DIR="$XDG_DATA_HOME/theoshell/cd-fav.txt"
export ZSH_PLUGIN_DIR="$XDG_DATA_HOME/theoshell/zsh-plugins"

