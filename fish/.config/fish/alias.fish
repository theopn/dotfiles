# Functions and aliases (which are just a wrapper for functions in Fish)


function fav -d "Navigate to my favorite directories using fzf"
  set -l fav_dir "$CLOUD_DIR" "$CACHE_DIR" "$DOT_DIR"
  set selected $(printf "%s\n" $fav_dir | fzf \
    --reverse --border=rounded --cycle --height=50% \
    --header='Pick a directory to navigate to')
  [ -z $selected ]; and echo 'Nothing was selected :('; or cd "$selected"
end

function updater -d "Place to update all the different stuff"
  # Construct a lsit of stuff to update
  set -l stuff 'dotfiles' 'doom emacs' 'theovim'
  if [ $OSTYPE = 'Linux' ]
    set -a stuff 'dnf' 'flatpak'
  else if [ $OSTYPE = 'macOS' ]
    set -a stuff 'homebrew'
  end

  while true
    # Prompt user and check for C-c and no input
    set selected $(printf "%s\n" $stuff | fzf \
      --reverse --border=rounded --cycle --height=50% \
      --header='[Updater] What are you updating today?')
    [ -z $selected ]; and echo '[Updater] Ending the updater...'; and return

    echo '[Updater] Updating' $selected '...'
    switch $selected
      case 'dotfiles'
        cd ~/dotfiles/ && git pull && cd - &> /dev/null
      case 'doom emacs'
        ~/.config/emacs/bin/doom upgrade
      case 'theovim'
        cd ~/.config/nvim && git pull && cd - &> /dev/null
      case 'dnf'
        sudo dnf upgrade
      case 'flatpak'
        flatpak upgrade
      case 'homebrew'
        brew update && brew upgrade
        and echo "[Updater] Brew update && upgrade successful"
        or echo "[Updater] Brew update && upgrade failed"

        brew autoremove && brew cleanup
        and echo "[Updater] Brew autoremove && cleanup successful"
        or echo "[Updater] Brew autoremove && cleanup failed"

        echo "[Updater] Running brew doctor..."
        brew doctor; echo "[Updater] TIP: Perodically 'brew untap' unnecessary sources"
      case '*'
        echo "[Updater] Error. Switch couldn't match anything -- this shoudln't happen"
        return
    end # End switch
  end # End while true
end

