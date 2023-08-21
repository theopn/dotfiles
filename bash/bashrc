# Strip permissions from group and others when creating new files
umask 077

# Add cs240/bin to PATH
export PATH=$PATH:~cs240/bin

# Custom exports - for Kitty terminal and lf
export TERM=xterm-256color
export EDITOR=nvim
export XDG_CONFIG_HOME="$HOME/.config"

# Not very special alias
alias cl="clear"
alias l="ls -A -l -h --color=auto" # All file except . and .., list view, display unit suffix for the size

#Alias for those who prefer nvim appimage - Theovim
#alias nvim=~/apps/nvim.appimage
# 2022-02-27, Shriansh emailed ScienceHelp and they actually updated package
# Today is a good day

alias lf=~/apps/lf
alias neofetch=~/apps/neofetch-7.1.0/neofetch

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
PS1="\[\e[0;36m\]-> \e[0;34m\]\u \[\e[0;36m\]@ \e[0;35m\h \[\e[36m\]in \e[1;31m\]\w \[\e[0m\]) "

