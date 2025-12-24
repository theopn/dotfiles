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

export MUMBOSHELL_TRASH_DIR="$XDG_DATA_HOME/mumbo/trash"
export MUMBOSHELL_CDF_DIR="$XDG_DATA_HOME/mumbo/cd-fav.txt"
export ZSH_PLUGIN_DIR="$XDG_DATA_HOME/mumbo/zsh-plugins"

eval "$(/opt/homebrew/bin/brew shellenv)"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# export XDG_DATA_HOME="$HOME/.local/share"
# export XDG_CONFIG_HOME="$HOME/.config"
#
# Java installations (using Homebrew paths)
export JAVA21_HOME="$(brew --prefix openjdk@21)/libexec/openjdk.jdk/Contents/Home"
# export JAVA17_HOME="$(brew --prefix openjdk@17)/libexec/openjdk.jdk/Contents/Home"

# Default Java
export JAVA_HOME="$JAVA21_HOME"
export PATH="$JAVA_HOME/bin:$PATH"

# Quick switching aliases
# alias use-java17='export JAVA_HOME=$JAVA17_HOME && export PATH=$JAVA_HOME/bin:$PATH'
alias use-java21='export JAVA_HOME=$JAVA21_HOME && export PATH=$JAVA_HOME/bin:$PATH'
. "$HOME/.cargo/env"
