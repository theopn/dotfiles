##### PURDUE CS CONFIG #####
# this file is processed on each interactive invocation of bash

# avoid problems with scp -- don't process the rest of the file if non-interactive
[[ $- != *i* ]] && return

#PS1="`shorthostname` \! $ "
#HISTSIZE=50

alias mail=mailx

# Strip permissions from group and others when creating new files
umask 077

# Add cs240/bin to PATH
export PATH=$PATH:~cs240/bin

# path of cs-console, cs-status
export PATH=${PATH}:/p/xinu/bin
# architecture
export CS_CLASS="cortex"
# download filename
export CS_FILENAME="xinu"
# name of server csconsole contacts
export CS_SERVERS="xinuserver"


##### MY CONFIG BEGINS #####

export PATH=$PATH:~/.local/bin

# Env var
export EDITOR=vi
export LESSHISTFILE=-

export XDG_CACHE_HOME=${XDG_CACHE_HOME:="$HOME/.cache"}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="$HOME/.config"}
export XDG_DATA_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}
export XDG_STATE_HOME=${XDG_DATA_HOME:="$HOME/.local/state"}

# alias
alias ga='git add'
alias gcm='git commit -m'
alias gss='git status'
alias histgrep='echo "[Tip] Use !number to execute the command" && history | grep'
alias l='ls -A -l -h --color=auto' # All file except . and .., list view, display unit suffix for the size
alias v='vim'

#alias nvim=~/apps/nvim.appimage
# 2022-02-27, Shriansh emailed ScienceHelp and they actually updated package
# Today is a good day

mkcd() {
  mkdir -p $1
  cd $1
}

numfiles() {
  num=$(ls -A $1 | wc -l)
  echo "$num files in $1"
}

# c for archive, z for gzip, v for verbose, f for file
tarmake() {
  tar -czvf ${1}.tar.gz $1
}

# x for extracting, v for verbose, f for file
tarunmake() {
  tar -zxvf $1
}

# Set prompt
PROMPT_DIRTRIM=3
PS1="\[\e[1;36m\] >Jobs:\j< \[\e[0;34m\][\u\[\e[0;36m\]@\[\e[0;35m\]\h]:\[\e[1;31m\]\w \[\e[0m\]\$ "

