# $ fish figlet -f fourtops fish
#  /~\'  |
# -|- |(~|/~\
#  |  |_)|   |
#

abbr -a cl clear

abbr -a ga git add
abbr -a gcm git commit -m
abbr -a gss git status

abbr -a nv neovide --fork
abbr -a v nvim
abbr -a weather curl 'https://wttr.in'

alias l="eza -a -l --header --git --total-size --time-style iso --icons auto --color auto"

# .. to cd .., ... to cd ../.., etc.
function multicd
    echo cd (string repeat -n (math (string length -- $argv[1]) - 1) ../)
end
abbr --add dotdot --regex '^\.\.+$' --function multicd

