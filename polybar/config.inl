;=======================================
; ▄▄▄·By    ▄▄▌T  ▄· ▄▌▄▄▄▄·  ▄▄▄· ▄▄▄
;▐█ ▄█▪     ██• h▐█▪██▌▐█ ▀█▪▐█ ▀█ ▀▄ █·
; ██▀· ▄█▀▄ ██▪  e█▌▐█▪▐█▀▀█▄▄█▀▀█ ▐▀▀▄
;▐█▪·•▐█▌.▐▌▐█▌▐▌ o█▀·.██▄▪▐█▐█ ▪▐▌▐█•█▌
;.▀    ▀█▄▀▪.▀▀▀   ▀ • ·▀▀▀▀  ▀  ▀ .▀  ▀
;=======================================

[colors]
; Prefix aa is for the transparent, ranging from 0 (fully transparent), 1, ... E, F
; Dracula Background
background = #77282a36
background-alt = #282a36
; Dracula Foreground
foreground = #f8f8f2
; Dracula Cyan
primary = #8be9fd
; Dracula Comment
secondary = #6272a4
; Dracula Red
alert = #ff5555
; Dracula Orange
disabled = #ffb86c
transparent = #00000000

[bar/hamilton]
background = ${colors.background}
foreground = ${colors.foreground}
border-color = ${colors.transparent}
separator = |
separator-foreground = ${colors.disabled}
font-0 = FantasqueSansMono Nerd Font:size=12;1
cursor-click = pointer
cursor-scroll = ns-resize
width = 100%
height = 18pt
radius = 6
line-size = 3pt
border-size = 3pt
padding-left = 1
padding-right = 1
module-margin = 1
module-margin-left = 1
module-margin-right = 1

modules-left = powermenu display_settings do_not_disturb backlight pulseaudio
modules-center = xwindow
modules-right = battery0 battery1 network date weather

[bar/bottas]
enable-ipc = true
background = ${colors.background}
foreground = ${colors.foreground}
border-color = ${colors.transparent}
font-0 = FantasqueSansMono Nerd Font:size=12;1
cursor-click = pointer
cursor-scroll = ns-resize
width = 100%
height = 18pt
radius = 6
line-size = 3pt
border-size = 3pt
padding-left = 1
padding-right = 1
module-margin = 1
module-margin-left = 1
module-margin-right = 1
bottom = true

modules-left = i3
modules-center = spotify
modules-right = temperature filesystem memory cpu
tray-position = right

; ---[[ Hamilton modules
[module/powermenu]
type = custom/menu
expand-right = true
format-spacing = 1
label-open = "  "
label-open-foreground = ${colors.primary}
label-close = "  "
label-close-foreground = ${colors.secondary}
label-separator = |
label-separator-foreground = ${colors.primary}
menu-0-0 = Lock
menu-0-0-exec = $HOME/.config/i3/i3_system_mode.sh lock
menu-0-1 = Exit i3 (Logout)
menu-0-1-exec = #powermenu.open.1
menu-0-2 = Reboot
menu-0-2-exec = #powermenu.open.2
menu-0-3 = Shutdown
menu-0-3-exec = #powermenu.open.3

menu-1-0 = Exit i3 Confirm
menu-1-0-exec = i3-msg exit
menu-1-1 = Cancel
menu-1-1-exec = #powermenu.open.0
menu-2-0 = Reboot Confirm
menu-2-0-exec = $HOME/.config/i3/i3_system_mode.sh reboot
menu-2-1 = Cancel
menu-2-1-exec = #powermenu.open.0
menu-3-0 = Shutdown Confirm
menu-3-0-exec = $HOME/.config/i3/i3_system_mode.sh shutdown
menu-3-1 = Cancel
menu-3-1-exec = #powermenu.open.0

[module/display_settings]
type = custom/menu
expand-right = true
format-spacing = 1
label-open = " "
label-open-foreground = ${colors.primary}
label-close = " "
label-close-foreground = ${colors.secondary}
label-separator = |
label-separator-foreground = ${colors.primary}
menu-0-0 = "  Compositor"
menu-0-0-exec = ~/dotfiles/polybar/polybar_display_tools_toggle.sh compositor
menu-0-1 = "盛  Nightlight/Nightshift/Bluelight filter/whatever it's called"
menu-0-1-exec = ~/dotfiles/polybar/polybar_display_tools_toggle.sh nightlight

[module/do_not_disturb]
type = custom/text
content = " "
content-foreground = ${colors.primary}
click-left = ~/dotfiles/polybar/polybar_dunst_toggle.sh

[module/backlight]
type = internal/backlight
card = intel_backlight
use-actual-brightness = true
label = %percentage%%
bar-width = 11
bar-indicator = |
bar-indicator-foreground = ${colors.primary}
bar-fill = ─
bar-fill-font = 4
bar-fill-foreground = ${colors.primary}
bar-empty = ─
bar-empty-font = 4
bar-empty-foreground = ${colors.secondary}
format-prefix = "  "
format-prefix-foreground = ${colors.primary}
format = <label> <bar>

[module/pulseaudio]
type = internal/pulseaudio
use-ui-max = true
click-right = pavucontrol

label-muted = 婢 muted
label-muted-foreground = ${colors.disabled}

ramp-volume-0 = 奄
ramp-volume-1 = 奔
ramp-volume-2 = 墳
label-volume = %percentage%%
bar-volume-width = 11
bar-volume-indicator = |
bar-volume-indicator-foreground = ${colors.secondary}
bar-volume-fill = ─
bar-volume-fill-font = 4
bar-volume-fill-foreground = ${colors.secondary}
bar-volume-empty = ─
bar-volume-empty-font = 4
bar-volume-empty-foregorund = ${colors.primary}
format-volume = <ramp-volume> <label-volume> <bar-volume>

[module/xwindow]
type = internal/xwindow
label = %title:0:60:...%

[module/battery0]
type = internal/battery
battery = BAT0
adapter = ADP1
full-at = 100
poll-interval = 5

ramp-capacity-0 = 
ramp-capacity-0-foreground = ${colors.alert}
ramp-capacity-1 = 
ramp-capacity-1-foreground = ${colors.primary}
ramp-capacity-2 = 
ramp-capacity-3 = 
format-charging =  BAT0(Int) <label-charging>
format-discharging = <ramp-capacity> BAT0(Int) <label-discharging>
format-full =  BAT0(Int) <label-full>

[module/battery1]
type = internal/battery
battery = BAT1
adapter = ADP1
full-at = 100
poll-interval = 5

ramp-capacity-0 = 
ramp-capacity-0-foreground = ${colors.alert}
ramp-capacity-1 = 
ramp-capacity-1-foreground = ${colors.primary}
ramp-capacity-2 = 
ramp-capacity-3 = 
format-charging =  BAT1(Ext) <label-charging>
format-discharging = <ramp-capacity> BAT1(Ext) <label-discharging>
format-full =  BAT1(Ext) <label-full>

[module/date]
type = internal/date
interval = 1
date = "%a %m-%d"
time = "%H:%M:%S"
label = %date% %time%
; A1 Left click, A2 middle, A3 right click, A4 Scroll up, A5 scroll down, etc
format = %{A1:$HOME/.config/polybar/polybar_calendar.sh curr:}%{A3:$HOME/.config/polybar/polybar_calendar.sh next:}  <label>%{A}%{A}

[module/weather]
type = custom/text
content = "摒 "
click-left = ~/dotfiles/polybar/polybar_weather.sh
; --]]

; ---[[ Bottas modules
[module/i3]
type = internal/i3
pin-workspaces = true
strip-wsnumbers = true
index-sort = true
enable-click = true
enable-scroll = false
wrapping-scroll = false
reverse-scroll = false
fuzzy-match = false

label-mode = %mode%
label-mode-padding = 2
label-mode-background = ${colors.alert}

label-focused = %name%
label-focused-background = ${colors.background}
label-focused-underline = ${colors.primary}
label-focused-padding = 2

label-unfocused = %index%: %name%
label-unfocused-padding = 2

label-urgent = %index%: %name%
label-urgent-background = ${colors.alert}
label-urgent-padding = 2

[module/spotify]
type = custom/script
interval = 15
label = %output:0:30:...%
format-prefix = " ﱘ  "
format = <label>
exec = $HOME/.config/polybar/polybar_spotify.py

[module/temperature]
type = internal/temperature
interval = 5
; $ for i in /sys/class/thermal/thermal_zone*; do echo "$i: $(<$i/type)"; done
thermal-zone = 0
base-temperature = 0
units = true
format-prefix = "   "
label = %temperature-c%

[module/filesystem]
type = internal/fs
interval = 25
mount-0 = /
label-mounted =  %mountpoint%: %used% of %total%
label-unmounted =  %mountpoint%: NOT MOUNTED
label-unmounted-foreground = ${colors.disabled}

[module/memory]
type = internal/memory
interval = 5
label = %gb_used%/%gb_total%
format-prefix = "  "
format-prefix-foreground = ${colors.primary}

[module/cpu]
type = internal/cpu
interval = 5
label = %percentage:2%%
format-prefix = " "
format-prefix-foreground = ${colors.primary}

[module/network]
type = internal/network
interface-type = wireless
interval = 5.0
ramp-signal-0 = x
ramp-signal-1 = .
ramp-signal-2 = :
ramp-signal-3 = .:
ramp-signal-4 = ::
ramp-signal-5 = .::
ramp-signal-foreground = ${colors.primary}
label-connected = 直  %essid%
label-disconnected = 睊  no wifi?
format-connected = <label-connected> <ramp-signal>
format-disconnected = <label-disconnected>
; --]]

[settings]
screenchange-reload = true
pseudo-transparency = true

; ---[[ Unused modules
[module/xworkspaces]
type = internal/xworkspaces
label-active = %name%
label-active-background = ${colors.background}
label-active-underline= ${colors.primary}
label-active-padding = 1
label-occupied = %name%
label-occupied-padding = 1
label-urgent = %name%
label-urgent-background = ${colors.alert}
label-urgent-padding = 1
label-empty = %name%
label-empty-foreground = ${colors.disabled}
label-empty-padding = 1
[network-base]
type = internal/network
interval = 5
format-connected = <label-connected>
format-disconnected = <label-disconnected>
label-disconnected = %{F#F0C674}%ifname%%{F#707880} disconnected
[module/wlan]
inherit = network-base
interface-type = wireless
label-connected = %{F#F0C674}%ifname%%{F-} %essid% %local_ip%
[module/eth]
inherit = network-base
interface-type = wired
label-connected = %{F#F0C674}%ifname%%{F-} %local_ip%
[module/xkeyboard]
type = internal/xkeyboard
blacklist-0 = num lock
label-layout = %layout%
label-layout-foreground = ${colors.primary}
label-indicator-padding = 2
label-indicator-margin = 1
label-indicator-foreground = ${colors.background}
label-indicator-background = ${colors.secondary}
; --]]

; vim:ft=dosini
