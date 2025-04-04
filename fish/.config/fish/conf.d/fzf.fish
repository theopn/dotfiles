set -gx FZF_DEFAULT_COMMAND 'fd --hidden --strip-cwd-prefix --exclude ".git"'
set -gx FZF_DEFAULT_OPTS '--layout=reverse --cycle --height=50% --margin=5% --border=double'

fzf --fish | source

