switch $(uname)
  case "Linux"
    set -gx OSTYPE 'Linux'
  case "Darwin"
    set -gx OSTYPE 'macOS'
  case '*BSD' 'DragonFly'
    set -gx OSTYPE 'BSD'
  case '*'
    set -gx OSTYPE 'UNKNOWN'
end

if [ $OSTYPE = 'macOS' ]
  fish_add_path /opt/homebrew/bin/
  abbr -a aero "aerospace list-windows --all | fzf --bind 'enter:execute($SHELL -c \"aerospace focus --window-id {1}\")+abort'"
end
