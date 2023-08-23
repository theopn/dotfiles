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

function numfiles -d "Count the number of file in the directory"
  set -l num $(ls -A $argv | wc -l)
  [ -z $num ] && echo "$num files in $argv"
end

function updater -d "Place to update all the different stuff"
  echo "what's up"
end


function note -d "Quick note"
  
end

