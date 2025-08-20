# $ fish figlet -f fourtops fish
#  /~\'  |
# -|- |(~|/~\
#  |  |_)|   |
#

set -x DOT_DIR ~/dotfiles
set -x CACHE_DIR ~/.cache

# abbr -a cl clear
# abbr -a ga git add
# abbr -a gcm git commit -m
# abbr -a gss git status
# abbr -a l ls -Alh --color=auto # [A]lmost all (except . && ..), [l]ist, [h]: display unit
abbr -a nv neovide --fork
abbr -a v nvim
abbr -a weather curl 'https://wttr.in'

# .. to cd .., ... to cd ../.., etc.
function multicd
    echo cd (string repeat -n (math (string length -- $argv[1]) - 1) ../)
end

abbr --add dotdot --regex '^\.\.+$' --function multicd

abbr -a dot cd "$DOT_DIR"

abbr -a dw vim "\"$CACHE_DIR/dw-$(date +'%Y')/index.md\""

abbr -a dockerps docker ps
