set -gx FZF_DEFAULT_COMMAND 'fd --hidden --strip-cwd-prefix --exclude ".git"'
set -gx FZF_DEFAULT_OPTS '--layout=reverse --cycle --height=50% --margin=5% --border=double'

fzf --fish | source

function fssh -d "Fuzzy-find ssh host via ag and ssh into it"
  ag --ignore-case '^host [^*]' ~/.ssh/config | cut -d ' ' -f 2 | fzf | read -l result; and ssh "$result"
end

function fav_add -d "Add PWD to the directory bookmark"
  if not set -q THEOSHELL_CD_BOOKMARK_DIR
    echo 'Set THEOSHELL_CD_BOOKMARK_DIR universal variable'
    return
  end

  if not test -e $THEOSHELL_CD_BOOKMARK_DIR
    mkdir -p (dirname $THEOSHELL_CD_BOOKMARK_DIR)
    touch $THEOSHELL_CD_BOOKMARK_DIR
  end

  pwd >> $THEOSHELL_CD_BOOKMARK_DIR
end

function fav -d "CD from the directory bookmark"
  if not set -q THEOSHELL_CD_BOOKMARK_DIR
    echo 'Set THEOSHELL_CD_BOOKMARK_DIR universal variable'
    return
  end

  set -l dir (fzf --header="Choose from the directory bookmark" < $THEOSHELL_CD_BOOKMARK_DIR)
  not test -z $dir; and cd "$dir"
end

abbr fav_open $EDITOR $THEOSHELL_CD_BOOKMARK_DIR
