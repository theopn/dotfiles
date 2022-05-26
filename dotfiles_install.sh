home=~/
dotfiles_backup=~/dotfiles_backup
printf "
  _____ _              ___      _    __ _ _
 |_   _| |_  ___ ___  |   \ ___| |_ / _(_) |___
   | | | ' \/ -_) _ \ | |) / _ \  _|  _| | / -_)
   |_| |_||_\___\___/ |___/\___/\__|_| |_|_\___|
\n"

printf "1. Homebrew. 
All existing formulae will be uninstalled.
Some formulae might compitable with non-MacOS System. 
Do you want to proceed? y/n: \n"
read -n1 homebrew_input # read -sk1 in zsh
case $homebrew_input in
  y|Y)
    brew remove --force $(brew list --formula)
    brew remove --cask --force $(brew list)
    brew bundle --file ~/dotfiles/homebrew/Brewfile
    ;;
  *)
    printf "Skipping Homebrew installation...\n" ;;
esac

printf "2. Dotfiles. Do you wish to replace dotfiles? y/n: \n"
read -n1 dotfile_input
case $dotfile_input in  
  y|Y)
    git=("gitignore" "gitconfig")
    for v in ${git[@]}; do
      if [[ -e "$home.$v" ]]; then
        mkdir -p $dotfiles_backup/git
        echo $home.$v exists. Moving to $dotfiles_backup
        mv $home.$v $dotfiles_backup/git/
      else
        sudo rm -rf ~/.$v
      fi
      ln -sf ~/dotfiles/git/$v ~/.$v
      echo .$v modified
    done
    vim=("vimrc")
    for v in ${vim[@]}; do
      if [[ -e "$home.$v" ]]; then
        mkdir -p $dotfiles_backup/vim
        echo $home.$v exists. Moving to $dotfiles_backup
        mv $home.$v $dotfiles_backup/vim/
      else
        sudo rm -rf ~/.$v
      fi
      ln -sf ~/dotfiles/vim/$v ~/.$v
      echo .$v modified
    done
    bash=("bashrc")
    for v in ${bash[@]}; do
      if [[ -e "$home.$v" ]]; then
        mkdir -p $dotfiles_backup/bash
        echo $home.$v exists. Moving to $dotfiles_backup
        mv $home.$v $dotfiles_backup/bash/
      else
        sudo rm -rf ~/.$v
      fi
      ln -sf ~/dotfiles/bash/$v ~/.$v
      echo .$v modified
    done
    zsh=("zshrc" "zsh_plugins")
    for v in ${zsh[@]}; do
      if [[ -e "$home.$v" ]]; then
        mkdir -p $dotfiles_backup/zsh
        echo $home.$v exists. Moving to $dotfiles_backup
        mv $home.$v $dotfiles_backup/zsh/
      else
        sudo rm -rf ~/.$v
      fi
      ln -sf ~/dotfiles/zsh/$v ~/.$v
      echo .$v modified
    done
    tmux=("tmux.conf")
    for v in ${tmux[@]}; do
      if [[ -e "$home.$v" ]]; then
        mkdir -p $dotfiles_backup/tmux
        echo $home.$v exists. Moving to $dotfiles_backup
        mv $home.$v $dotfiles_backup/tmux/
      else
        sudo rm -rf ~/.$v
      fi
      ln -sf ~/dotfiles/tmux/$v ~/.$v
      echo .$v modified
    done
    ;;
  *) 
    printf "Skipping dotfile setups...\n" ;; 
esac

printf "3. MacOS specific Settings. Do you want to proceed? y/n: \n"
read -n1 macos_input # read -n1 in bash
case $macos_input in
  y|Y)
    source ~/dotfiles/scripts/macos_settings.sh ;;
  *)
    printf "Skipping MacOS specific settings... \n" ;;
esac

printf "Ending the dotfile installation...\n"
printf "
   ____   __   __U _____ u
U | __')u \ \ / /\| ___'|/
 \|  _ \/  \ V /  |  _|'
  | |_) | U_|'|_u | |___
  |____/    |_|   |_____|
 _|| \\_.-,//|(_  <<   >>
(__) (__)\_) (__)(__) (__)\n"