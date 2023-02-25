;=======================================
; ▄▄▄·By    ▄▄▌T  ▄· ▄▌▄▄▄▄·  ▄▄▄· ▄▄▄
;▐█ ▄█▪     ██• h▐█▪██▌▐█ ▀█▪▐█ ▀█ ▀▄ █·
; ██▀· ▄█▀▄ ██▪  e█▌▐█▪▐█▀▀█▄▄█▀▀█ ▐▀▀▄
;▐█▪·•▐█▌.▐▌▐█▌▐▌ o█▀·.██▄▪▐█▐█ ▪▐▌▐█•█▌
;.▀    ▀█▄▀▪.▀▀▀   ▀ • ·▀▀▀▀  ▀  ▀ .▀  ▀
;=======================================

[colors]
transparent = #00000000
; Prefix 00 is for the transparent, ranging from 0 (fully transparent), 1, ... E, F
; Dracula Background
background = #00282a36
; Dracula Cyan
foreground = #8be9fd
; Dracula Red
alert = #ff5555
; Dracula Comment
disabled = #6272A4

[bar/hamilton]
background = ${colors.background}
foreground = ${colors.foreground}
border-color = ${colors.transparent}
separator =
font-0 = CaskaydiaCove Nerd Font:size=11;1
cursor-click = pointer
cursor-scroll = ns-resize
width = 100%
height = 22pt
radius = 0
line-size = 3pt
border-size = 3pt
padding-left = 1
padding-right = 1
module-margin = 1

modules-left = display_settings do_not_disturb temperature memory cpu network
modules-center = i3
modules-right = pulseaudio backlight battery1 date spotify-simple weather

tray-position = left
tray-padding = 1

[module/display_settings]
type = custom/menu
expand-right = true
format-spacing = 1
label-open = " "
label-close = " "
label-close-foreground = ${colors.disabled}
label-separator = |
label-separator-foreground = ${colors.disabled}
menu-0-0 = "  Compositor"
menu-0-0-exec = ~/dotfiles/polybar/polybar-display-tools-toggle.sh compositor
menu-0-1 = "盛  Bluelight filter"
menu-0-1-exec = ~/dotfiles/polybar/polybar-display-tools-toggle.sh nightlight

[module/do_not_disturb]
type = custom/text
content = " "
click-left = ~/dotfiles/polybar/polybar-dunst-toggle.sh

[module/temperature]
type = internal/temperature
interval = 5
; for i in /sys/class/thermal/thermal_zone*; do echo "$i: $(<$i/type)"; done
thermal-zone = 0
base-temperature = 0
units = true
format-prefix = "  "
label = %temperature-c%

[module/memory]
type = internal/memory
interval = 5
label = %gb_used%
format-prefix = "  "

[module/cpu]
type = internal/cpu
interval = 5
label = %percentage:2%%
format-prefix = " "

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
label-connected = 直  %essid%
label-disconnected = 睊  no wifi?
format-connected = <label-connected> <ramp-signal>
format-disconnected = <label-disconnected>

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
label-focused-underline = ${colors.foreground}
label-focused-padding = 2

label-unfocused = %index%: %name%
label-unfocused-padding = 2

label-urgent = %index%: %name%
label-urgent-background = ${colors.alert}
label-urgent-padding = 2

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
format-volume = <ramp-volume> <label-volume>

[module/backlight]
type = internal/backlight
card = intel_backlight
use-actual-brightness = true
label = %percentage%%
format-prefix = "  "
format = <label>

[module/battery0]
; Internal (bottom) battery of X270/T460
type = internal/battery
battery = BAT0
adapter = ADP1
full-at = 100
poll-interval = 5

ramp-capacity-0 = 
ramp-capacity-0-foreground = ${colors.alert}
ramp-capacity-1 = 
ramp-capacity-2 = 
ramp-capacity-3 = 
format-charging =  <label-charging>
format-discharging = <ramp-capacity> <label-discharging>
format-full =  <label-full>

[module/battery1]
; External (top) battery of X270/T460
type = internal/battery
battery = BAT1
adapter = ADP1
full-at = 100
poll-interval = 5

ramp-capacity-0 = 
ramp-capacity-0-foreground = ${colors.alert}
ramp-capacity-1 = 
ramp-capacity-2 = 
ramp-capacity-3 = 
format-charging =  <label-charging>
format-discharging = <ramp-capacity> <label-discharging>
format-full =  <label-full>

[module/date]
type = internal/date
interval = 1
date = "%a %m-%d"
time = "%H:%M"
label = %date% %time%
; A1 Left click, A2 middle, A3 right click, A4 Scroll up, A5 scroll down, etc
format = %{A1:~/dotfiles/polybar/polybar-calendar.sh curr:}%{A3:~/dotfiles/polybar/polybar-calendar.sh next:}  <label>%{A}%{A}

[module/spotify-simple]
type = custom/text
content = " ﱘ "
click-left = notify-send "$(~/dotfiles/polybar/polybar-spotify.py)"

[module/weather]
type = custom/text
content = "摒 "
click-left = ~/dotfiles/polybar/polybar-weather.sh

[settings]
screenchange-reload = true
pseudo-transparency = true

; vim:ft=dosini
