printf "
 ________              ___       __  _____ __      
/_  __/ /  ___ ___    / _ \___  / /_/ __(_) /__ ___
 / / / _ \/ -_) _ \  / // / _ \/ __/ _// / / -_|_-<
/_/ /_//_/\__/\___/ /____/\___/\__/_/ /_/_/\__/___/
\n"

printf "1. Dotfiles; If previous file existed, it will be moved to ~/dotfiles_backup.
Do you wish to replace dotfiles? Type 'yes': "
read -r dotfile_input
case $dotfile_input in  
  y|Y|yes|Yes)
    git=("gitignore" "gitconfig")
    for v in ${git[@]}; do
      sudo rm -rf ~/.$v > /dev/null 2>&1
      ln -sf ~/dotfiles/git/$v ~/.$v
      echo .$v modified
    done
    vim=("vimrc")
    for v in ${vim[@]}; do
      sudo rm -rf ~/.$v # > /dev/null 2>&1
      ln -sf ~/dotfiles/vim/$v ~/.$v
      echo .$v modified
    done
    ;;
  *) 
    printf "Skipping the dotfiles...\n";; 
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