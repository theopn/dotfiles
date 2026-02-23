# Some keybindings from Bash

# up arrow + alt+e/v
abbr -a fc "up-or-search; and edit_command_buffer"

# ctrl-d and h already exists
bind -M insert ctrl-a beginning-of-buffer
bind -M insert ctrl-e end-of-buffer
bind -M insert ctrl-b backward-char
bind -M insert ctrl-f forward-char

