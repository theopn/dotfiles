#############################
#              __
#  ____  _____/ /_  __________
# /_  / / ___/ __ \/ ___/ ___/
#  / /_(__  ) / / / /  / /__
# /___/____/_/ /_/_/   \___/
##############################

# .zshenv -> .zprofile -> .zshrc -> .zlogin
#
# Interactive shell config

# Vim mode
bindkey -v


##### Alias #####
alias cl='clear'
alias ga='git add'
alias gcm='git commit -m'
alias gss='git status'
alias histgrep='echo "[Tip] Use !number to execute the command" && history -i | grep' # -i for the timestamp
alias l='ls -A -l -h --color=auto' # All file except . and .., list view, display unit suffix for the size
alias nv='neovide --fork'
alias v=nvim

alias dot="cd \"$DOT_DIR\""


##### Functions #####
mkcd() { mkdir -p $1; cd $1 }

numfiles() {
  num=$(ls -A $1 | wc -l)
  echo "$num files in $1"
}

# c for archive, z for gzip, v for verbose, f for file
tarmake() { tar -czvf ${1}.tar.gz $1 }

# x for extracting, v for verbose, f for file
tarunmake() { tar -zxvf $1 }


##### Simple Trash Function #####
function trash() {
  if [[ -z "$THEOSHELL_TRASH_DIR" ]]; then
    echo "You must provide THEOSHELL_TRASH_DIR"
    return 1
  fi

  [[ ! -d ${THEOSHELL_TRASH_DIR} ]] && mkdir -p ${THEOSHELL_TRASH_DIR}

  if [[ -z $@ ]]; then
    echo 'Select file(s) to trash!'
    return 2
  fi

  for file in $@; do
    mv ${file} ${THEOSHELL_TRASH_DIR} && echo ":) ${file} moved to trash!" || echo ":( Failed to move ${file} to trash"
  done
}


##### Minimal Plugin Manager #####

# Double check double check
function source_file() {
  [ -f ${ZSH_PLUGIN_DIR}/$1 ] && source ${ZSH_PLUGIN_DIR}/$1
}

# Function to source or load a plugin
function plug() {
  if [[ -z "$ZSH_PLUGIN_DIR" ]]; then
    echo "You must provide ZSH_PLUGIN_DIR"
    return 1
  fi

  PLUGIN_NAME=$(echo $1 | cut -d "/" -f 2)
  if [[ -d ${ZSH_PLUGIN_DIR}/${PLUGIN_NAME} ]]; then
    source_file ${PLUGIN_NAME}/${PLUGIN_NAME}.plugin.zsh || \
    source_file ${PLUGIN_NAME}/${PLUGIN_NAME}.zsh
  else
    git clone --depth 1 "https://github.com/${1}.git" ${ZSH_PLUGIN_DIR}/${PLUGIN_NAME}
  fi
}

function plug_update() {
  if [[ -z "$ZSH_PLUGIN_DIR" ]]; then
    echo "You must provide ZSH_PLUGIN_DIR"
    return 1
  fi

  for repo in ${ZSH_PLUGIN_DIR}/*
  do
    echo "refreshing ${repo}:"
    cd ${repo} && git pull && cd - > /dev/null
  done
}

# Add zsh-autocomplete
plug marlonrichert/zsh-autocomplete


##### FZF #####
# I mean it is my machine so I would assume fd is installed
#type -P fd &> /dev/null && export FZF_DEFAULT_COMMAND='fd --hidden --strip-cwd-prefix --exclude ".git"'
#whence -p fd &> /dev/null && export FZF_DEFAULT_COMMAND='fd --hidden --strip-cwd-prefix --exclude ".git"'

source <(fzf --zsh)

cdf() {
  selected=$(find * -maxdepth 1 -type d 2>/dev/null | fzf \
    --reverse --border=rounded --cycle --height=50% \
    --header='Pick a directory to navigate to')
  [[ -z $selected ]] && echo 'Nothing was selected :(' || cd "$selected"
}

alias manf="compgen -c | fzf | xargs man"

sshf() {
  [[ ! -e ~/.ssh/config ]] && echo 'There are no SSH config file!'
  hostnames=$(awk ' $1 == "Host" { print $2 } ' ~/.ssh/config )
  [[ -z "${hostnames}" ]] && echo 'There are no host param in the SSH config file'
  selected=$(printf "%s\n" "${hostnames[@]}" | fzf \
    --reverse --border=rounded --cycle --height=30% \
    --header='pick a host')
  [[ -z "${selected}" ]] && echo 'Nothing was selected :(' && return
  echo "SSHing to ${selected}..." && ssh "$selected"
}


##### Directory Bookmark using FZF #####

cdf() {
  if [[ -z "$THEOSHELL_CDF_DIR" ]]; then
    echo "You must provide THEOSHELL_CDF_DIR"
    return 1
  fi

  dir=$(fzf --header="Favorite Directories" < $THEOSHELL_CDF_DIR)
  [[ ! -z "$dir" ]] && cd "$dir"
}

cdf_add() {
  if [[ -z "$THEOSHELL_CDF_DIR" ]]; then
    echo "You must provide THEOSHELL_CDF_DIR"
    return 1
  fi

  if [[ ! -e $THEOSHELL_CDF_DIR ]]; then
    mkdir -p $(dirname $THEOSHELL_CDF_DIR)
    touch $THEOSHELL_CDF_DIR
  fi

  pwd >> $THEOSHELL_CDF_DIR
}

alias cdf_edit="$EDITOR $THEOSHELL_CDF_DIR"


##### Git Information #####
autoload -Uz vcs_info

zstyle ':vcs_info:*' enable git
# Hook before every commands
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst

zstyle ':vcs_info:*' check-for-changes true

zstyle ':vcs_info:*' unstagedstr '*'
zstyle ':vcs_info:*' stagedstr '+'
zstyle ':vcs_info:git:*' formats '%b%u%c'
# Only displayed in Git action like rebase, merge, cherry-pick
zstyle ':vcs_info:git:*' actionformats '[%b | %a%u%c]'


##### Vim mode indicator #####
# https://superuser.com/questions/151803/how-do-i-customize-zshs-vim-mode
# perform parameter expansion/command substitution in prompt
setopt PROMPT_SUBST

ins_mode_indicator="%F{yellow}[I]%f"
norm_mode_indicator="%F{magenta}[N]%f"
# Initial mode
vi_mode_indicator=$ins_mode_indicator

# on keymap change, define the mode and redraw prompt
zle-keymap-select() {
  if [[ "$KEYMAP" == 'vicmd' ]]; then
    vi_mode_indicator=$norm_mode_indicator
  else
    vi_mode_indicator=$ins_mode_indicator
  fi
  zle reset-prompt
}
zle -N zle-keymap-select

# reset to default mode at the end of line input reading
zle-line-finish() {
  vi_mode_indicator=$ins_mode_indicator
}
zle -N zle-line-finish

# When C-c in [N], the prompt becomes [N] even though you are in [I]
# Fix by catching SIGNIT and set the prompt to int again, and resend SIGINT
TRAPINT() {
  vi_mode_indicator=$ins_mode_indicator
  return $(( 128 + $1 ))
}


##### PROMPT #####

# %(5~|%-1~/…/%3~|%4~) - IF path_len > 5 THEN print 1st element; print /.../; print last 3 elem; ELSE print 4 elem;
PROMPT=" \$vi_mode_indicator %F{blue}%(5~|%-1~/.../%3~|%4~)%f %F{cyan}\$vcs_info_msg_0_%f %F{white}❱%f "

RPROMPT="%(?|%F{green}|%F{red})[%?]%f "


##### Greeting #####
function zsh_greeting() {
  # Colors
  normal='\033[0m'

  red='\033[0;31m'
  brred='\033[1;31m'
  green='\033[0;32m'
  brgreen='\033[1;32m'
  yellow='\033[0;33m'
  bryellow='\033[1;33m'
  blue='\033[0;34m'
  brblue='\033[1;34m'
  magenta='\033[0;35m'
  brmagenta='\033[1;35m'
  cyan='\033[0;36m'
  brcyan='\033[1;36m'

  # Collection of Oliver ASCII arts
  olivers=(
    '
       \/   \/
       |\__/,|     _
     _.|o o  |_   ) )
    -(((---(((--------
    ' \
    '
                           _
          |\      _-``---,) )
    ZZZzz /,`.-```    -.   /
         |,4-  ) )-,_. ,\ (
        `---``(_/--`  `-`\_)
    ' \
    '
       \/   \/
       |\__/,|        _
       |_ _  |.-----.) )
       ( T   ))        )
      (((^_(((/___(((_/
    '
  )
  # 1. RANDOM is biased toward the lower index
  # 2. Array index in ZSH starts at 1
  oliver=${olivers[ $(( RANDOM % ${#olivers[@]} + 1 )) ]}

  # Other information
  zsh_ver="$(zsh --version)"
  uptime=$(uptime | grep -ohe 'up .*' | sed 's/,//g' | awk '{ print $2" "$3 " " }')

  # Greeting msg
  echo
  echo -e "  " "$brgreen" "Meow"                             "$normal"
  echo -e "  " "$brred"   "$oliver"                          "$normal"
  echo -e "  " "$cyan"    "  Shell:\t"   "$brcyan$zsh_ver"  "$normal"
  echo -e "  " "$blue"    "  Uptime:\t"  "$brblue$uptime"   "$normal"
  echo
}

zsh_greeting

