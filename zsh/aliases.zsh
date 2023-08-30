#!/bin/zsh

######### Alias ##########
alias cl='clear'
alias histgrep='echo "[Tip] Use !number to execute the command" && history -i | grep' # -i for the timestamp
alias l='ls -A -l -h --color=auto' # All file except . and .., list view, display unit suffix for the size
alias weather="curl 'https://wttr.in'"

# These personal aliases require various other env var from .zshrc
alias l1="cd \"$CACHE_DIR\""
alias dw="vim \"${DAILY_WRITING_DIR}/index.md\""
alias dot="cd \"$DOT_DIR\""

########## Small Functions ##########

cdf() {
  selected=$(find * -maxdepth 1 -type d 2>/dev/null | fzf \
    --reverse --border=rounded --cycle --height=50% \
    --header='Pick a directory to navigate to')
  [[ -z $selected ]] && echo 'Nothing was selected :(' || cd "$selected"
}

fav() {
  dir=("$CLOUD_DIR" "$CACHE_DIR" "$DOT_DIR" "$THEOSHELL_TRASH_DIR")
  selected=$(printf "%s\n" "${dir[@]}" | fzf \
    --reverse --border=rounded --cycle --height=30% \
    --header='Pick a directory to navigate to')
  [[ -z $selected ]] && echo 'Nothing was selected :(' || cd "$selected"
}

mkcd() { mkdir -p $1; cd $1 }

numfiles() {
  num=$(ls -A $1 | wc -l)
  echo "$num files in $1"
}

# c for archive, z for gzip, v for verbose, f for file
tarmake() { tar -czvf ${1}.tar.gz $1 }

# x for extracting, v for verbose, f for file
tarunmake() { tar -zxvf $1 }

########## Big Functions ###########

# A place to quickly write something down
note() {
  note="$QUICK_NOTE_PATH"
  if [[ ! -e $note ]]; then
    echo "# Quick Note

> A place to quickly write something down.
> Theo, please do not store info here long-term
> Move to other sources like Org-roam" > $note
  fi
  nvim $note
}

# Using fzf to prompt hosts in ~/.ssh/config
sshf() {
  [[ ! -e ~/.ssh/config ]] && echo 'There are is SSH config file!'
  hostnames=$(awk ' $1 == "Host" { print $2 } ' ~/.ssh/config )
  [[ -z "${hostnames}" ]] && echo 'There are no host param in SSH config file'
  selected=$(printf "%s\n" "${hostnames[@]}" | fzf \
    --reverse --border=rounded --cycle --height=30% \
    --header='pick a host')
  [[ -z "${selected}" ]] && echo 'Nothing was selected :(' && return
  echo "SSH to ${selected}..." && ssh "$selected"
}

# Hub for updating bunch of stuff at once using fzf
updater () {
  # Make list of things to update
  stuff=('dotfiles' 'zsh plugins' 'doom emacs' 'theovim')
  if [[ "${OSTYPE}" == "linux-gnu"* ]]; then
    stuff+=('dnf' 'flatpak')
  elif [[ "${OSTYPE}" == "darwin"* ]]; then
    stuff+=('homebrew')
  fi
  # Repeat until user uses C-c
  while true; do
    # Prompt and check for the existence of $selected
    selected=$(printf "%s\n" "${stuff[@]}" | fzf --reverse --border=rounded --cycle --height=30% --header='[Updater] What are you going to update?')
    [[ -z $selected ]] && echo '[Updater] No selection registered -- ending the updater' && return
    printf "[Updater] Updating %s\n" $selected
        echo -e "-----\n"
    case "${selected}" in
      'dotfiles')
        cd ~/dotfiles/ && git pull && cd - &> /dev/null ;;
      'zsh plugins')
        theoshell_upgrade
        ;;
      'doom emacs')
        ~/.emacs.d/bin/doom upgrade ;;
      'theovim')
        cd ~/.config/nvim && git pull && cd - &> /dev/null ;;
      'homebrew')
        brew upgrade ;;
      'dnf')
        sudo dnf upgrade ;;
      'flatpak')
        flatpak upgrade ;;
      *)
        echo -e "\n[DEBUG] This shouldn't happen bc -z should've checked for no selection -- debug time Theo\n"
        return
        ;;
    esac
    echo -e "\n-----"
    echo '[Updater] Done updating'
  done
}

# Quickly switching external display on/off using xrandr
xrandr_external_on() {
  if [[ "$OSTYPE" != "linux-gnu"* ]]; then
    echo -e "\033[0;31m[Fatal] You are on ${OSTYPE}\033[0m"
    return 1
  fi
  if [[ $# != 1 ]]; then
    echo -e "\033[0;32m[Usage] xrandr_external_on <output-name>\033[0m"
    echo -e "\033[0;32m\tRetreive the output name using xrandr command\033[0m"
  else
    xrandr --output $1 --auto --same-as eDP-1
  fi
}

xrandr_external_off() {
  if [[ "$OSTYPE" != "linux-gnu"* ]]; then
    echo -e "\033[0;31m[Fatal] You are on ${OSTYPE}\033[0m"
    return 1
  fi
  if [[ $# != 1 ]]; then
    echo -e "\033[0;32m[Usage] xrandr_external_off <output-name>\033[0m"
    echo -e "\033[0;32m\tRetreive the output name using xrandr command\033[0m"
  else
    xrandr --output $1 --off
  fi
}

