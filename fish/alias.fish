# $ fish figlet -f fourtops fish
#  /~\'  |
# -|- |(~|/~\
#  |  |_)|   |
#
# Functions and aliases (which are just a wrapper for functions in Fish)

alias cl "clear"
alias l "ls -Alh" # [A]lmost all (except . && ..), [l]ist, [h]: display unit
alias weather "curl 'https://wttr.in'"

alias l1="cd \"$CACHE_DIR\""
alias dw="vim \"$DAILY_WRITING_DIR/index.md\""
alias dot="cd \"$DOT_DIR\""

function cdf -d "Navigate to directories using fzf"
  set selected $(find * -maxdepth 1 -type d 2>/dev/null | fzf \
    --reverse --border=rounded --cycle --height=50% \
    --header='Pick a directory to navigate to')
  [ -z $selected ]; and echo 'Nothing was selected :('; or cd "$selected"
end

function fav -d "Navigate to my favorite directories using fzf"
  set -l fav_dir "$CLOUD_DIR" "$CACHE_DIR" "$DOT_DIR"
  set selected $(printf "%s\n" $fav_dir | fzf \
    --reverse --border=rounded --cycle --height=50% \
    --header='Pick a directory to navigate to')
  [ -z $selected ]; and echo 'Nothing was selected :('; or cd "$selected"
end

function mkcd -d "Create a directory and set CWD"
  command mkdir $argv
  if [ $status = 0 ]
    switch $argv[(count $argv)]
      case '-*'

      case '*'
        cd $argv[(count $argv)]
        return
    end
  end
end

function note -d "A place to quickly write something down"
  set -l note "$QUICK_NOTE_PATH"
  if [ ! -e $note ]
    echo "# Quick Note

> A place to quickly write something down.
> Theo, please do not store info here long-term
> Move to other sources like Org-roam" > $note
  end
  nvim $note
end

function numfiles -d "Count the number of file in the directory"
  set -l num $(ls -A $argv | wc -l)
  [ -n $num ]; and echo "$num files in $argv"
end

function updater -d "Place to update all the different stuff"
  # Construct a lsit of stuff to update
  set -l stuff 'dotfiles' 'doom emacs' 'theovim'
  if [ $OSTYPE = 'Linux' ]
    set -a stuff 'dnf' 'flatpak'
  else if [ $OSTYPE = 'macOS' ]
    set -a stuff 'homebrew'
  end

  while true
    # Prompt user and check for C-c and no input
    set selected $(printf "%s\n" $stuff | fzf \
      --reverse --border=rounded --cycle --height=50% \
      --header='[Updater] What are you updating today?')
    [ -z $selected ]; and echo '[Updater] Ending the updater...'; and return

    echo '[Updater] Updating' $selected '...'
    switch $selected
      case 'dotfiles'
        cd ~/dotfiles/ && git pull && cd - &> /dev/null
      case 'doom emacs'
        ~/.emacs.d/bin/doom upgrade
      case 'theovim'
        cd ~/.config/nvim && git pull && cd - &> /dev/null
      case 'dnf'
        sudo dnf upgrade
      case 'flatpak'
        flatpak upgrade
      case 'homebrew'
        brew upgrade
      case '*'
        echo "[Updater] Error. Switch couldn't match anything -- this shoudln't happen"
        return
    end # End switch
  end # End while true
end

