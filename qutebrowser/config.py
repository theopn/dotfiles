#                __        ___.
#   ________ ___/  |_  ____\_ |_________  ______  _  ________ ___________
#  / ____/  |  \   __\/ __ \| __ \_  __ \/  _ \ \/ \/ /  ___// __ \_  __ \
# < <_|  |  |  /|  | \  ___/| \_\ \  | \(  <_> )     /\___ \\  ___/|  | \/
#  \__   |____/ |__|  \___  >___  /__|   \____/ \/\_//____  >\___  >__|
#     |__|                \/    \/                        \/     \/
# figlet -f graffiti qutebrowser

"""
Resources:
    - https: // qutebrowser.org/doc/help/settings.html
    - https: // gist.github.com/Gavinok/f9c310a66576dc00329dd7bef2b122a1
    - https: // gist.github.com/Ape/d0c48b3f7ec9c8efaecf48eaa1e75d0d
"""

c.tabs.position = "top"
c.completion.shrink = True

config.load_autoconfig(False)  # Do not load from yml file

# - always: Always show a confirmation.
# - multiple-tabs: Show a confirmation if multiple tabs are opened.
# - downloads: Show a confirmation if downloads are running
# - never: Never show a confirmation.
c.confirm_quit = ["always"]

# Keybindings
config.unbind("d")         # tab-close
config.unbind("<ctrl+q>")  # tab-close
config.unbind("D")         # tab-close -o (-o for selecting opposite tab)
config.bind("<space>q", "tab-close")

config.bind("t", "set-cmd-text :open -t {url:pretty}")

config.bind("<space>1", "tab-focus 1")
config.bind("<space>2", "tab-focus 2")
config.bind("<space>3", "tab-focus 3")
config.bind("<space>4", "tab-focus 4")
config.bind("<space>5", "tab-focus 5")
config.bind("<space>6", "tab-focus 6")
config.bind("<space>7", "tab-focus 7")
config.bind("<space>8", "tab-focus 8")
config.bind("<space>9", "tab-focus 9")
config.bind("<space>0", "tab-focus 0")

# Colors
"""
Dracula theme: Heavily modified from
https://github.com/dracula/qutebrowser/blob/master/draw.py
"""
def dracula_theme(c):
    palette = {
        'background': '#282a36',
        'current-line': '#44475a',
        'selection': '#44475a',
        'foreground': '#f8f8f2',
        'comment': '#6272a4',
        'cyan': '#8be9fd',
        'green': '#50fa7b',
        'orange': '#ffb86c',
        'pink': '#ff79c6',
        'purple': '#bd93f9',
        'red': '#ff5555',
        'yellow': '#f1fa8c'
    }

    ## Background color of the completion widget category headers.
    # c.colors.completion.category.bg = palette['background']
    #
    # ## Bottom border color of the completion widget category headers.
    # c.colors.completion.category.border.bottom = palette['border']
    #
    # ## Top border color of the completion widget category headers.
    # c.colors.completion.category.border.top = palette['border']
    #
    # ## Foreground color of completion widget category headers.
    # c.colors.completion.category.fg = palette['foreground']
    #
    # ## Background color of the completion widget for even rows.
    # c.colors.completion.even.bg = palette['background']
    #
    # ## Background color of the completion widget for odd rows.
    # c.colors.completion.odd.bg = palette['background-alt']
    #
    # ## Text color of the completion widget.
    # c.colors.completion.fg = palette['foreground']
    #
    # ## Background color of the selected completion item.
    # c.colors.completion.item.selected.bg = palette['selection']
    #
    # ## Bottom border color of the selected completion item.
    # c.colors.completion.item.selected.border.bottom = palette['selection']
    #
    # ## Top border color of the completion widget category headers.
    # c.colors.completion.item.selected.border.top = palette['selection']
    #
    # ## Foreground color of the selected completion item.
    # c.colors.completion.item.selected.fg = palette['foreground']
    #
    # ## Foreground color of the matched text in the completion.
    # c.colors.completion.match.fg = palette['orange']
    #
    # ## Color of the scrollbar in completion view
    # c.colors.completion.scrollbar.bg = palette['background']
    #
    # ## Color of the scrollbar handle in completion view.
    # c.colors.completion.scrollbar.fg = palette['foreground']
    #
    # ## Background color for the download bar.
    # c.colors.downloads.bar.bg = palette['background']
    #
    # ## Background color for downloads with errors.
    # c.colors.downloads.error.bg = palette['background']
    #
    # ## Foreground color for downloads with errors.
    # c.colors.downloads.error.fg = palette['red']
    #
    # ## Color gradient stop for download backgrounds.
    # c.colors.downloads.stop.bg = palette['background']
    #
    # ## Color gradient interpolation system for download backgrounds.
    # ## Type: ColorSystem
    # ## Valid values:
    # ##   - rgb: Interpolate in the RGB color system.
    # ##   - hsv: Interpolate in the HSV color system.
    # ##   - hsl: Interpolate in the HSL color system.
    # ##   - none: Don't show a gradient.
    # c.colors.downloads.system.bg = 'none'
    #
    # ## Background color for hints. Note that you can use a `rgba(...)` value
    # ## for transparency.
    # c.colors.hints.bg = palette['background']
    #
    # ## Font color for hints.
    # c.colors.hints.fg = palette['foreground']
    #
    # ## Hints
    # c.hints.border = '1px solid ' + palette['background-alt']
    #
    # ## Font color for the matched part of hints.
    # c.colors.hints.match.fg = palette['foreground-alt']
    #
    # ## Background color of the keyhint widget.
    # c.colors.keyhint.bg = palette['background']
    #
    # ## Text color for the keyhint widget.
    # c.colors.keyhint.fg = palette['purple']
    #
    # ## Highlight color for keys to complete the current keychain.
    # c.colors.keyhint.suffix.fg = palette['selection']
    #
    # ## Background color of an error message.
    # c.colors.messages.error.bg = palette['background']
    #
    # ## Border color of an error message.
    # c.colors.messages.error.border = palette['background-alt']
    #
    # ## Foreground color of an error message.
    # c.colors.messages.error.fg = palette['red']
    #
    # ## Background color of an info message.
    # c.colors.messages.info.bg = palette['background']
    #
    # ## Border color of an info message.
    # c.colors.messages.info.border = palette['background-alt']
    #
    # ## Foreground color an info message.
    # c.colors.messages.info.fg = palette['comment']
    #
    # ## Background color of a warning message.
    # c.colors.messages.warning.bg = palette['background']
    #
    # ## Border color of a warning message.
    # c.colors.messages.warning.border = palette['background-alt']
    #
    # ## Foreground color a warning message.
    # c.colors.messages.warning.fg = palette['red']
    #
    # ## Background color for prompts.
    # c.colors.prompts.bg = palette['background']
    #
    # # ## Border used around UI elements in prompts.
    # c.colors.prompts.border = '1px solid ' + palette['background-alt']
    #
    # ## Foreground color for prompts.
    # c.colors.prompts.fg = palette['cyan']
    #
    # ## Background color for the selected item in filename prompts.
    # c.colors.prompts.selected.bg = palette['selection']
    #

    ########## SETTINGS THAT I HAVE MODIFIED ##########

    ########## COMPLETION SETTINGS ##########

    ########## DOWNLOADS SETTINGS ##########

    ########## HINTS SETTINGS ##########

    ########## MESSAGES SETTINGS ##########

    ########## PROMPTS SETTINGS ##########

    ########## STATUS BAR SETTINGS ##########
    # Caret mode (v)
    c.colors.statusbar.caret.bg = palette['background']
    c.colors.statusbar.caret.fg = palette['green']
    # Command mode
    c.colors.statusbar.command.bg = palette['background']
    c.colors.statusbar.command.fg = palette['cyan']
    # Private mode
    c.colors.statusbar.private.bg = palette['red']
    c.colors.statusbar.private.fg = palette['foreground']
    # Insert mode
    c.colors.statusbar.insert.bg = palette['background']
    c.colors.statusbar.insert.fg = palette['yellow']
    # Normal mode
    c.colors.statusbar.normal.bg = palette['background']
    c.colors.statusbar.normal.fg = palette['foreground']
    # Passthrough mode
    c.colors.statusbar.passthrough.bg = palette['background']
    c.colors.statusbar.passthrough.fg = palette['orange']
    # Progress bar
    c.colors.statusbar.progress.bg = palette['background']
    # URL color
    c.colors.statusbar.url.fg = palette['foreground']
    c.colors.statusbar.url.error.fg = palette['red']
    c.colors.statusbar.url.hover.fg = palette['cyan']  # URL color when hovering
    c.colors.statusbar.url.success.http.fg = palette['green']
    c.colors.statusbar.url.success.https.fg = palette['green']
    c.colors.statusbar.url.warn.fg = palette['yellow']
    c.statusbar.padding = {
        'top': 6,
        'right': 10,
        'bottom': 10,
        'left': 6
    }

    ########## TAB SETTINGS ###########
    c.colors.tabs.bar.bg = palette['selection']  # not sure if this even matters
    # Tab status indicator
    c.colors.tabs.indicator.error = palette['red']
    c.colors.tabs.indicator.start = palette['orange']
    c.colors.tabs.indicator.stop = palette['green']
    c.colors.tabs.indicator.system = 'none'  # Gradient; rgb, hsv, hsl, or none
    # Colors for selected and unselected tabs
    c.colors.tabs.even.bg = palette['background']
    c.colors.tabs.even.fg = palette['foreground']
    c.colors.tabs.odd.bg = palette['background']
    c.colors.tabs.odd.fg = palette['foreground']
    c.colors.tabs.selected.even.bg = palette['pink']
    c.colors.tabs.selected.even.fg = palette['foreground']
    c.colors.tabs.selected.odd.bg = palette['pink']
    c.colors.tabs.selected.odd.fg = palette['foreground']
    # Componenets size
    c.tabs.padding = {
        'top': 8,
        'right': 10,
        'bottom': 8,
        'left': 10
    }
    c.tabs.indicator.width = 3  # Tab status indicator (error, loading, etc)
    c.tabs.favicons.scale = 2


# Setting the theme
dracula_theme(c)

