# $ fish figlet -f fourtops fish
#  /~\'  |
# -|- |(~|/~\
#  |  |_)|   |
#

abbr -a cl clear
abbr -a ga git add
abbr -a gcm git commit -m
abbr -a gss git status
abbr -a l ls -Alh --color=auto # [A]lmost all (except . && ..), [l]ist, [h]: display unit
abbr -a nv neovide --fork
abbr -a v nvim
abbr -a weather curl 'https://wttr.in'

# .. to cd .., ... to cd ../.., etc.
function multicd
    echo cd (string repeat -n (math (string length -- $argv[1]) - 1) ../)
end
abbr --add dotdot --regex '^\.\.+$' --function multicd

# Dotfiles directory
abbr -a dot cd "$DOT_DIR"

# personal journal entry for the day
# - If I just pass the subshell without escaping (i.e., `+/$(date)`),
#   then Fish will evaluate in the shell startup and the fixed date.
# - I do not want to overcomplicate the command so I keep the first one as-is
#   (since year changing does not happen very often),
#   but I do not want to reopen the shell every 24 hours
abbr -a dw vim \"$CACHE_DIR/dw-$(date +%Y)/index.md\" +/\$\(date +%Y-%m-%d\)

