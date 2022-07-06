home=~/
bu_dir=~/dotfiles_backup

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
    # Git
    current="git"
    git_files=("gitignore_global" "gitconfig")
    for v in ${git_files[@]}; do
      if [[ -e "$home.$v" ]]; then
        mkdir -p $bu_dir/$current
        echo .$v exists. Backing up.
        mv ~/.$v $bu_dir/$current/
      fi
      sudo rm -rf ~/.$v
      ln -sf ~/dotfiles/$current/$v ~/.$v
      echo .$v modified
    done
    
    # Zsh
    current="zsh"
    zsh_files=("zshrc" "zsh_plugins")
    for v in ${zsh_files[@]}; do
      if [[ -e "$home.$v" ]]; then
        mkdir -p $bu_dir/$current
        echo .$v exists. Backing up.
        mv ~/.$v $bu_dir/$current/
      fi
      sudo rm -rf ~/.$v
      ln -sf ~/dotfiles/$current/$v ~/.$v
      echo .$v modified
    done
    
    # Tmux
    current="tmux"
    tmux_files=("tmux.conf")
    for v in ${tmux_files[@]}; do
      if [[ -e "$home.$v" ]]; then
        mkdir -p $bu_dir/$current
        echo .$v exists. Backing up.
        mv ~/.$v $bu_dir/$current/
      fi
      sudo rm -rf ~/.$v
      ln -sf ~/dotfiles/$current/$v ~/.$v
      echo .$v modified
    done
    
    # Vim
    current="vim"
    vim_files=("vimrc")
    for v in ${vim_files[@]}; do
      if [[ -e "$home.$v" ]]; then
        mkdir -p $bu_dir/$current
        echo .$v exists. Backing up.
        mv ~/.$v $bu_dir/$current/
      fi
      sudo rm -rf ~/.$v
      ln -sf ~/dotfiles/$current/$v ~/.$v
      echo .$v modified
    done
    # Vim theme
    mkdir -p ~/.vim/colors
    ln -sf ~/dotfiles/vim/colors ~/.vim/colors
    printf "Vim themes installed"
    
    # kitty
    current="kitty"
    kitty_files=("kitty.conf")
    for v in ${kitty_files[@]}; do
      if [[ -e "$home.config/kitty/$v" ]]; then
        mkdir -p $bu_dir/$current
        echo $v exists. Backing up.
        mv ~/.config/kitty/$v $bu_dir/$current/
      fi
      mkdir -p ~/.config/kitty
      sudo rm -rf ~/.config/kitty/$v
      ln -sf ~/dotfiles/$current/$v ~/.config/kitty/$v
      echo $v modified
    done

    # neofetch
    current="neofetch"
    neofetch_files=("config.conf")
    for v in ${neofetch_files[@]}; do
      if [[ -e "$home.config/neofetch/$v" ]]; then
        mkdir -p $bu_dir/$current
        echo $v exists. Backing up.
        mv ~/.config/neofetch/$v $bu_dir/$current/
      fi
      mkdir -p ~/.config/neofetch
      sudo rm -rf ~/.config/neofetch/$v
      ln -sf ~/dotfiles/$current/$v ~/.config/neofetch/$v
      echo $v modified
    done

    # Neovim
    current="nvim"
    nvim_files=("init.lua")
    for v in ${nvim_files[@]}; do
      if [[ -e "$home.config/nvim/$v" ]]; then
        mkdir -p $bu_dir/$current
        echo $v exists. Backing up.
        mv ~/.config/nvim/$v $bu_dir/$current/
      fi
      mkdir -p ~/.config/nvim
      sudo rm -rf ~/.config/nvim/$v
      ln -sf ~/dotfiles/$current/$v ~/.config/nvim/$v
      echo $v modified
    done
    # Neovim Lua settings
    mkdir -p ~/.config/nvim/lua
    ln -sf ~/dotfiles/nvim/lua ~/.config/nvim/lua
    printf "Extra lua config for NeoVim installed\n" 

    # Emacs
    current="emacs"
    emacs_files=("init.el")
    for v in ${emacs_files[@]}; do
      mkdir -p ~/.emacs.d
      ln -sf ~/dotfiles/$current/$v ~/.emacs.d/$v
      echo $v modified
    done

    # Bash
    current="bash"
    bash_files=("bashrc")
    for v in ${bash_files[@]}; do
      if [[ -e "$home.$v" ]]; then
        mkdir -p $bu_dir/$current
        echo .$v exists. Backing up.
        mv ~/.$v $bu_dir/$current/
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
