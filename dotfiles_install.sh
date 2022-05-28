home=~/
printf "
  _____ _              ___      _    __ _ _
 |_   _| |_  ___ ___  |   \ ___| |_ / _(_) |___
   | | | ' \/ -_) _ \ | |) / _ \  _|  _| | / -_)
   |_| |_||_\___\___/ |___/\___/\__|_| |_|_\___|
\n"

printf "\n1. Homebrew Core Formulae/Casks. 
All existing formulae will be uninstalled.
Some formulae might compitable with non-MacOS System. 
Do you want to proceed? y/n: "
read -n1 homebrew_input # read -sk1 in zsh
case $homebrew_input in
  y|Y)
    brew remove --force $(brew list --formula)
    brew remove --cask --force $(brew list)
    brew bundle --file ~/dotfiles/homebrew/Brewfile_core
    ;;
  *)
    printf "\nSkipping Homebrew core file installation...\n" ;;
esac

printf "\n2. Dotfiles. Do you wish to replace dotfiles? y/n: "
read -n1 dotfile_input
case $dotfile_input in  
  y|Y)
    # Git related dotfiles
    current="git"
    git_files=("gitignore_global" "gitconfig")
    for v in ${git_files[@]}; do
      if [[ -e "$home.$v" ]]; then
        mkdir -p ~/dotfiles_backup/$current
        echo ~/.$v exists. Moving to ~/dotfiles_backup
        mv ~/.$v ~/dotfiles_backup/$current/
      fi
      sudo rm -rf ~/.$v
      ln -sf ~/dotfiles/$current/$v ~/.$v
      echo .$v modified
    done
    # Zsh related dotfiles
    current="zsh"
    zsh_files=("zshrc" "zsh_plugins")
    for v in ${zsh_files[@]}; do
      if [[ -e "$home.$v" ]]; then
        mkdir -p ~/dotfiles_backup/$current
        echo ~/.$v exists. Moving to ~/dotfiles_backup
        mv ~/.$v ~/dotfiles_backup/$current/
      fi
      sudo rm -rf ~/.$v
      ln -sf ~/dotfiles/$current/$v ~/.$v
      echo .$v modified
    done
    # Tmux related dotfiles
    current="tmux"
    tmux_files=("tmux.conf")
    for v in ${tmux_files[@]}; do
      if [[ -e "$home.$v" ]]; then
        mkdir -p ~/dotfiles_backup/$current
        echo ~/.$v exists. Moving to ~/dotfiles_backup
        mv ~/.$v ~/dotfiles_backup/$current/
      fi
      sudo rm -rf ~/.$v
      ln -sf ~/dotfiles/$current/$v ~/.$v
      echo .$v modified
    done
    # Vim related dotfiles
    current="vim"
    vim_files=("vimrc")
    for v in ${vim_files[@]}; do
      if [[ -e "$home.$v" ]]; then
        mkdir -p ~/dotfiles_backup/$current
        echo ~/.$v exists. Moving to ~/dotfiles_backup
        mv ~/.$v ~/dotfiles_backup/$current/
      fi
      sudo rm -rf ~/.$v
      ln -sf ~/dotfiles/$current/$v ~/.$v
      echo .$v modified
    done
    # Manual deployment for tokyonight theme
    mkdir -p ~/.vim/
    sudo rm -rf ~/.vim/colors
    ln -sf ~/dotfiles/vim/colors ~/.vim/colors
    printf "Modified version of tokyonight theme for Vim installed\n"
    # Modified deployment for neovim settings
    current="nvim"
    nvim_files=("init.lua")
    for v in ${nvim_files[@]}; do
      if [[ -e "$home.config/nvim/$v" ]]; then
        mkdir -p ~/dotfiles_backup/$current
        echo ~/.config/nvim/$v exists. Moving to ~/dotfiles_backup
        mv ~/.config/nvim/$v ~/dotfiles_backup/$current/
      fi
      mkdir -p ~/.config/nvim
      sudo rm -rf ~/.config/nvim/$v
      ln -sf ~/dotfiles/$current/$v ~/.config/nvim/$v
      echo $v modified
    done
    # Manual deployment for extra lua settings
    mkdir -p ~/.config/nvim
    sudo rm -rf ~/.config/nvim/lua
    ln -sf ~/dotfiles/nvim/lua ~/.config/nvim/lua
    printf "Extra lua config for NeoVim installed\n" 
    # Bash related dotfiles
    current="bash"
    bash_files=("bashrc")
    for v in ${bash_files[@]}; do
      if [[ -e "$home.$v" ]]; then
        mkdir -p ~/dotfiles_backup/$current
        echo ~/.$v exists. Moving to ~/dotfiles_backup
        mv ~/.$v ~/dotfiles_backup/$current/
      fi
      ln -sf ~/dotfiles/$current/$v ~/.$v
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

printf "\n4. Homebrew Optional Formulae/Casks.
Some formulae might compitable with non-MacOS System.
This might take a while
Do you want to proceed? y/n: "
read -n1 homebrew_opt_input
case $homebrew_opt_input in
  y|Y)
    brew bundle --file ~/dotfiles/homebrew/Brewfile_optional
    ;;
  *)
    printf "\nSkipping Homebrew optonal file installation...\n" ;;
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