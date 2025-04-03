#!/bin/zsh

alias dot="cd \"$DOT\""

alias cl='clear'
alias histgrep='echo "[Tip] Use !number to execute the command" && history -i | grep' # -i for the timestamp
alias l='ls -A -l -h --color=auto' # All file except . and .., list view, display unit suffix for the size

mkcd() { mkdir -p $1; cd $1 }

numfiles() {
  num=$(ls -A $1 | wc -l)
  echo "$num files in $1"
}

# c for archive, z for gzip, v for verbose, f for file
tarmake() { tar -czvf ${1}.tar.gz $1 }

# x for extracting, v for verbose, f for file
tarunmake() { tar -zxvf $1 }

