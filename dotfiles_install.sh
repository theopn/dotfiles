sudo rm -rf ~/.gitignore > /dev/null 2>&1

SYMLINKS=()
ln -sf ~/dotfiles/gitignore ~/.gitignore
SYMLINKS+=('.gitignore')

echo ${SYMLINKS[@]}