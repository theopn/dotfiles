# Tmux

Tmux is a highly customizable terminal multiplexer.

Terminologies:

- Window : Think of different workspaces in tiling WM
- Pane   : Windows within the workspaces

## Bindings

- `<C-a>`  : Prefix; in this document, unless specified, every binding starts with the prefix (OG: `<C-b>`)
- `<C-c`   : Exit out of the Tmux help, command, etc
- `<C-r>`  : Reload the configuration file (OG: `r`)
- `:`      : Prompt for Tmux commands
- `?`      : Open a pop-up terminal with my custom keybindings (originally a help menu for default binding that are not unbound)

### Pane Related Bindings/Commands

Used:

- `h/j/k/l` : Navigation
- `<Arrow>` : Resizing
- `<C-o>`   : Move panes
- `|`       : Split pane vertically (I know the command says -h) (OG: `"`)
- `-`       : Split pane horizontally (OG: `%`)
- `x`       : Kill the current pane
- `<space>` : Cycle through layout
- `f`       : Maximize the current pane (OG: `z`)
- `<C-s>`   : Send the current focused pane to window index of prompted number
- `<C-j>`   : Join the last focused pane from the window index of prompted number

Unused:

- `o`       : Navigate through pane and sometimes switch? I don't know
- `t`       : Get the time on the current pane
- `q`       : Get current pane numbers displayed
- `</>`     : Gets a comprehensive menu for the pane

### Window Related Bindings/Commands

Used:

- `<number>` : Move to a window
- `n`        : Create a new window (OG: `c`)
- `r`        : Rename a window (OG: `,`)
- `m`        : Re-index current window. Remove ":" and supply non-numeric session name to move the window to another session (OG: `.`)
- `<C-m>`    : Swap (re-index) current window with existing window index
- `<C-p>`    : Swap window index with previous window
- `<C-n>`    : Swap window index with next window

Unused:

- `NIL`      : Kill a window. Use kill pane (`x`) instead (OG: `&`)
- `!`        : Break the current pane out of the window (it acts as re-indexing with one-pane-window)
- `p`        : Go to previous window
- `NIL`      : Go to next window (OG: `n`)

### Session Related Bindings/Commands

- `$`                                       : Rename the current session
- `d`                                       : Detach the current session
- `!tmux attach-session -t<target-session>` : Attach the target session
- `w`                                       : Get a tree view of all panes
- `s`                                       : Get a tree view of all sessions
- `i`                                       : Get an information about the current session

## Copy Mode

- `[`            : Takes you to the copy mode
- `<ESC>, <C-c>` : Exit copy mode
- `v`            : Start copying
- `y`            : Yank

