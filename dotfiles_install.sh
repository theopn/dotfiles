home=~/
dotfiles_backup=~/dotfiles_backup
printf "
  _____ _              ___      _    __ _ _
 |_   _| |_  ___ ___  |   \ ___| |_ / _(_) |___
   | | | ' \/ -_) _ \ | |) / _ \  _|  _| | / -_)
   |_| |_||_\___\___/ |___/\___/\__|_| |_|_\___|
\n"

printf "\n1. Homebrew. 
All existing formulae will be uninstalled.
Some formulae might compitable with non-MacOS System. 
Do you want to proceed? y/n: "
read -n1 homebrew_input # read -sk1 in zsh
case $homebrew_input in
  y|Y)
    brew remove --force $(brew list --formula)
    brew remove --cask --force $(brew list)
    brew bundle --file ~/dotfiles/homebrew/Brewfile
    ;;
  *)
    printf "\nSkipping Homebrew installation...\n" ;;
esac

printf "\n2. Dotfiles. Do you wish to replace dotfiles? y/n: "
read -n1 dotfile_input
case $dotfile_input in  
  y|Y)
    # Git related dotfiles
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
    # Zsh related dotfiles
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
    # Vim related dotfiles
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
    # Tmux related dotfiles
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
    # Bash related dotfiles
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
    ;;
  *) 
    printf "\nSkipping dotfile setups...\n" ;; 
esac

printf "\n3. MacOS specific Settings. Do you want to proceed? y/n: "
read -n1 macos_input # read -n1 in bash
case $macos_input in
  y|Y)
    source ~/dotfiles/scripts/macos_settings.sh ;;
  *)
    printf "\nSkipping MacOS specific settings... \n" ;;
esac

printf "\nEnding the dotfile installation..."
printf "
   ____   __   __U _____ u
U | __')u \ \ / /\| ___'|/
 \|  _ \/  \ V /  |  _|'
  | |_) | U_|'|_u | |___
  |____/    |_|   |_____|
 _|| \\_.-,//|(_  <<   >>
(__) (__)\_) (__)(__) (__)\n"