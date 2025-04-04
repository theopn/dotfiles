set -gx FZF_DEFAULT_COMMAND 'fd --hidden --strip-cwd-prefix --exclude ".git"'
set -gx FZF_DEFAULT_OPTS '--layout=reverse --cycle --height=50% --margin=5% --border=double'

fzf --fish | source

function fssh -d "Fuzzy-find ssh host via ag and ssh into it"
  ag --ignore-case '^host [^*]' ~/.ssh/config | cut -d ' ' -f 2 | fzf | read -l result; and ssh "$result"
end

