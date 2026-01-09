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
  abbr -a purdue_vpn sudo openconnect webvpn.purdue.edu
else if [ $OSTYPE = 'Linux' ]
  if status --is-interactive
    # Isolation for setting up keychain
    begin
      # `man keychain` gives two options for loading SSH key into the shell
      # (1) is to use --eval.
      #     However, it uses $SHELL to find the script to source,
      #     so for me, it will try to source ~/.keychain/${hostname}-sh
      #     and cause syntax error in Fish
      # (2) is to manually load the key and the sockets stored in ~/.keychain
      #     This is the method I will be using, though I will add `--quick` flag

      set -l KEYS id_rsa id_ed25519
      ## you can also add --quiet if you wish
      keychain --quick $KEYS
      test -z "$hostname"; and set hostname (uname -n)
      if test -f "$HOME/.keychain/$hostname-fish"
        source $HOME/.keychain/$hostname-fish
      end
    end
  end
end
