printf "This script will remove and replace dotfiles with ones in the repository.
Do you wish to continue? Type 'yes': "
read -r initial_input
case $initial_input in  
  yes)
    sudo rm -rf ~/.gitignore > /dev/null 2>&1
    SYMLINKS=()
    ln -sf ~/dotfiles/gitignore ~/.gitignore
    SYMLINKS+=('.gitignore')
    echo ${SYMLINKS[@]} modified.
    ;;
  *) 
    printf "Ending the script...";; 
esac