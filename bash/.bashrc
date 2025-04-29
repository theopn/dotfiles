# Strip permissions from group and others when creating new files
umask 077
# Add cs240/bin to PATH
export PATH=$PATH:~cs240/bin

# Env var
export EDITOR=vi
export LESSHISTFILE=-

export XDG_CACHE_HOME=${XDG_CACHE_HOME:="$HOME/.cache"}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="$HOME/.config"}
export XDG_DATA_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}
export XDG_STATE_HOME=${XDG_DATA_HOME:="$HOME/.local/state"}

# alias
alias cl='clear'
alias ga='git add'
alias gcm='git commit -m'
alias gss='git status'
alias histgrep='echo "[Tip] Use !number to execute the command" && history | grep'
alias l='ls -A -l -h --color=auto' # All file except . and .., list view, display unit suffix for the size

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

# My cute greeting func
greeting() {
  my_hostname=$(hostname -s)
  timestamp="$(date -I) $(date +"%T")"
  uptime=$(uptime | grep -ohe 'up .*' | sed 's/,//g' | awk '{ print $2" "$3 " " }')
  echo -e "  " "\033[0;32m" "Welcome back $USER!"                       "\033[0m"
  echo -e "\033[0;31m" \
      '
                             _
            |\      _-``---,) )
      ZZZzz /,`.-```    -.   /
           |,4-  ) )-,_. ,\ (
          `---``(_/--`  `-`\_)
      ' \
      "\033[0;0m"
  echo -e "  " "\033[0;33m" "Bash Open:\t" "$timestamp"   "\033[0m"
  echo -e "  " "\033[0;34m" "Hostname :\t" "$my_hostname" "\033[0m"
  echo -e "  " "\033[0;35m" "Uptime   :\t" "$uptime"      "\033[0m"
}
[[ -t 0 ]] && greeting # check if file descriptor 0 (stdin) is attached to tty

# Set prompt
PS1="\[\e[0;36m\]> \e[0;34m\]\u \[\e[0;36m\]@ \e[0;35m\h \[\e[36m\]in \e[1;31m\]\w \[\e[0m\]$ "

