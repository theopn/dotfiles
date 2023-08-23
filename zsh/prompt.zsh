#!/bin/zsh

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

########## Prompt Setting ##########

# Left prompt
# %(5~|%-1~/…/%3~|%4~) - IF path_len > 5 THEN print 1st element; print /.../; print last 3 elem; ELSE print 4 elem;
PROMPT="➜ %F{075}%(5~|%-1~/.../%3~|%4~)%f %F{013}\$vcs_info_msg_0_%f %F{245}?:%f%F{069}%?%f %F{245}❱%f "

# Right prompt with execution time
# https://gist.github.com/knadh/123bca5cfdae8645db750bfb49cb44b0 - comment by sudocurse
function preexec() {
  timer=$(($(print -P %D{%s%6.})/1000))
}

function precmd() {
  if [ $timer ]; then
    now=$(($(print -P %D{%s%6.})/1000))
    elapsed=$(($now-$timer))
    if [ $elapsed -lt 1000 ]; then
      export RPROMPT="%F{245}${elapsed}ms %f"
    else
      elapsed=$(($elapsed / 1000))
      export RPROMPT="%F{245}${elapsed}s %f"
    fi
    unset timer
  fi
}