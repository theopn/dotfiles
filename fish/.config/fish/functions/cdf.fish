# Custom "cd favorite" commands.
# - `cdf_add` : adds the CWD to $THEOSHELL_CDF_DIR (~/.local/share/theoshell/cd-fav.txt),
# - `cdf`     : executes a fzf to search and CD to the directory in the file
# - `cdf_edit`: edits the file
# not really used anymore since I started using zoxide.

function cdf -d "[CDF] Directory Favorite/Bookmark using FZF"
  if not set -q THEOSHELL_CDF_DIR
      echo 'You must provide THEOSHELL_CDF_DIR'
    return 1
  end

  set -l dir (fzf --header="Favorite Directories" < $THEOSHELL_CDF_DIR)
  not test -z $dir; and cd "$dir"
end

function cdf_add -d "[CDF] Add CWD to the directory list"
  if not set -q THEOSHELL_CDF_DIR
      echo 'You must provide THEOSHELL_CDF_DIR'
    return 1
  end

  if not test -e $THEOSHELL_CDF_DIR
    mkdir -p (dirname $THEOSHELL_CDF_DIR)
    touch $THEOSHELL_CDF_DIR
  end

  pwd >> $THEOSHELL_CDF_DIR
end

function cdf_edit -d "[CDF] Edit $THEOSHELL_CDF_DIR file"
  if set -q EDITOR
    $EDITOR $THEOSHELL_CDF_DIR
  else
    nvim $THEOSHELL_CDF_DIR
  end
end

