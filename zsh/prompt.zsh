#!/bin/zsh

## Git

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

## Vi mode
setopt PROMPT_SUBST

ins_mode_indicator='%F{green}[I]%f'
norm_mode_indicator='%F{red}[N]%f'

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

## Setting Prompt

# %(5~|%-1~/…/%3~|%4~) - IF path_len > 5 THEN print 1st element; print /.../; print last 3 elem; ELSE print 4 elem;
PROMPT="\$vi_mode_indicator ➜  %F{cyan}%(5~|%-1~/.../%3~|%4~)%f %F{blue}\$vcs_info_msg_0_%f | %(?|%F{green}|%F{red})%? ❱ %f"

