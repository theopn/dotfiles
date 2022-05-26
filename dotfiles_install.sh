printf "This script will remove and replace dotfiles with ones in the repository.
Do you wish to continue? Type 'yes': "
read -r initial_input
case $initial_input in  
  yes)
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
    printf "Ending the script...";; 
esac