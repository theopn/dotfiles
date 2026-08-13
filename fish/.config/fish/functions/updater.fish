function updater -d "Using fzf to pick and run update commands"
  switch $(uname)
    case "Linux"
      set -l OSTYPE 'Linux'
    case "Darwin"
      set -l OSTYPE 'macOS'
    case '*BSD' 'DragonFly'
      set -l OSTYPE 'BSD'
    case '*'
      set -l OSTYPE 'UNKNOWN'
  end

  # Construct a lsit of stuff to update
  set -l stuff 'dotfiles' 'nvim: vim.pack' 'Vim: Plug'
  if [ $OSTYPE = 'Linux' ]
    # assumes Fedora
    set -a stuff 'Fedora: dnf' 'Linux: flatpak' 'Linux: Firmware Upgrade'
  else if [ $OSTYPE = 'macOS' ]
    set -a stuff 'Homebrew: Update' 'Homebrew: Cleanup'
  end

  while true
    # Prompt user and check for C-c and no input
    set selected $(printf "%s\n" $stuff | fzf \
      --reverse --border=rounded --cycle --height=50% \
      --header='[Updater] Choose the update command')
    [ -z $selected ]; and echo '[Updater] Ending the updater...'; and return

    echo '[Updater] ' $selected '...'
    switch $selected
      case 'dotfiles'
        cd ~/dotfiles/ && git pull && cd - &> /dev/null
      case 'nvim: vim.pack'
        nvim --headless "+lua vim.pack.update()" +wqa
      case 'Vim: Plug'
        vim +PlugUpdate
      case 'Homebrew: Update'
        brew update && brew upgrade
        and echo '[Updater] Brew update && upgrade successful'
        or echo '[Updater] Brew update && upgrade failed'
      case 'Homebrew: Cleanup'
        brew bundle cleanup --file=~/dotfiles/homebrew/Brewfile  # zapping undeclared packages
        and echo '[Updater] Undeclared package zapped'
        or echo '[Updater] brew zap skipped, removing orphan dependencies instead' && brew autoremove

        brew cleanup   # remove cache
        echo '[Updater] Running brew doctor...'
        brew doctor
      case 'Fedora: dnf'
        sudo dnf upgrade
      case 'Linux: flatpak'
        flatpak upgrade
      case 'Linux: Firmware Upgrade'
        fwupdmgr refresh && fwupdmgr get-updates
        echo '[Updater] After confirming the updates, run `fwupdmgr update` manually'
      case '*'
        echo "[Updater] Error. Switch couldn't match anything -- this shoudln't happen"
        return
    end # End switch
  end # End while true
end # End function
