printf "This script will remove and replace dotfiles with ones in the repository.
Do you wish to continue? Type 'yes': "
read -r initial_input
case $initial_input in  
  yes)
    git=( "gitignore" "gitconfig" )
    for t in ${git[@]}; do
      sudo rm -rf ~/.$t > /dev/null 2>&1
      ln -sf ~/dotfiles/git/$t ~/.$t
      echo .$t modified
    done
    ;;
  *) 
    printf "Ending the script...";; 
esac