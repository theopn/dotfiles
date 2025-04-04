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
abbr -a weather curl 'https://wttr.in'

# .. to cd .., ... to cd ../.., etc.
function multicd
    echo cd (string repeat -n (math (string length -- $argv[1]) - 1) ../)
end
abbr --add dotdot --regex '^\.\.+$' --function multicd

abbr -a dot cd "$DOT"

# Personal productivity system and journal shortcuts
abbr -a caos cd "$HOME/My\ Drive/l1-cache/caos"
abbr -a dw vim "$HOME/My\ Drive/l1-cache/dw-$(date +'%Y')/index.md"
abbr -a note $EDITOR "$HOME/My\ Drive/l1-cache/note.md"

# Homebrew
abbr -a brup "brew update && brew upgrade"
abbr -a brcl "brew autoremove && brew cleanup && brew doctor"

