# Miscellaneous Configurations

These are single-file, simple configurations that do not change very often.
These are meant to be manually deployed as needed.
Use the following commands to create symlink for these files

```bash
ln -sf ~/dotfiles/misc/bashrc ~/.bashrc
mkdir -p ~/.config/kitty && ln -sf ~/dotfiles/misc/kitty.conf ~/.config/kitty/kitty.conf
mkdir -p ~/.config/neofetch && ln -sf ~/dotfiles/misc/neofetch.conf ~/.config/neofetch/config.conf
ln -sf ~/dotfiles/misc/ideavimrc ~/.ideavimrc
```

