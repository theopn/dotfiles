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
  fish_add_path /opt/homebrew/sbin/
  abbr -a aero "aerospace list-windows --all | fzf --bind 'enter:execute($SHELL -c \"aerospace focus --window-id {1}\")+abort'"
else if [ $OSTYPE = 'Linux' ]
  begin
    set -l KEYS id_rsa id_ed25519
    if status --is-interactive
      keychain --eval $KEYS
    end

    set -l HOSTNAME (hostname)
    if test -f ~/.keychain/$HOSTNAME-fish
      source ~/.keychain/$HOSTNAME-fish
    end
  end
end
