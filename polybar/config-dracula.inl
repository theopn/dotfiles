;=======================================
; ▄▄▄·By    ▄▄▌T  ▄· ▄▌▄▄▄▄·  ▄▄▄· ▄▄▄
;▐█ ▄█▪     ██• h▐█▪██▌▐█ ▀█▪▐█ ▀█ ▀▄ █·
; ██▀· ▄█▀▄ ██▪  e█▌▐█▪▐█▀▀█▄▄█▀▀█ ▐▀▀▄
;▐█▪·•▐█▌.▐▌▐█▌▐▌ o█▀·.██▄▪▐█▐█ ▪▐▌▐█•█▌
;.▀    ▀█▄▀▪.▀▀▀   ▀ • ·▀▀▀▀  ▀  ▀ .▀  ▀
;
;=======================================

[colors]
background = #00282A36
current-line = #44475A
foreground = #F8F8F2
comment = #6272A4
cyan = #8BE9FD
green = #50FA7B
orange = #FFB86C
pink = #FF79C6
purple = #BD93F9
red = #FF5555
yellow = #F1FA8C

[bar/dracula]
width = 100%
height = 24pt
radius = 6

background = ${colors.background}
foreground = ${colors.foreground}

line-size = 3pt

border-size = 2pt
border-color = ${colors.pink}

padding-left = 0
padding-right = 1

module-margin = 1

separator =
separator-foreground = ${colors.comment}

font-0 = CaskaydiaCove Nerd Font:size=12;1

modules-left = stuff temperature memory cpu network
modules-center = i3 xwindow
modules-right = pulseaudio backlight battery0 battery1 date

cursor-click = pointer
cursor-scroll = ns-resize

enable-ipc = true

tray-position = right
tray-padding = 1

; wm-restack = i3

;;;;;;;;;; Right  Modules ;;;;;;;;;;
[module/stuff]
type = custom/menu
expand-right = true
format-spacing = 1

; Icons for the module itself
label-open = " 󰄛 >"
label-open-foreground = ${colors.purple}
label-close = " 󰄛 >"
label-close-foreground = ${colors.comment}

; Icons for the menu within
label-separator = |
label-separator-foreground = ${colors.comment}

; Fun stuff
menu-0-0 = "ﱘ"
menu-0-0-exec = notify-send "$(~/dotfiles/polybar/polybar-spotify.py)"
menu-0-1 = "摒"
menu-0-1-exec = ~/dotfiles/polybar/polybar-weather.sh
menu-0-2 = ""
menu-0-2-exec = ~/dotfiles/polybar/polybar-dunst-toggle.sh

; Display options
menu-0-3 = " Picom"
menu-0-3-exec = ~/dotfiles/polybar/polybar-display-tools-toggle.sh compositor
menu-0-4 = "盛 Redshift"
menu-0-4-exec = ~/dotfiles/polybar/polybar-display-tools-toggle.sh nightlight

[module/temperature]
type = internal/temperature
interval = 5
; for i in /sys/class/thermal/thermal_zone*; do echo "$i: $(<$i/type)"; done
thermal-zone = 0
base-temperature = 0
units = true

label = %temperature-c%
format-prefix = "  "
format-prefix-foreground = ${colors.orange}

[module/memory]
type = internal/memory
interval = 5

label = %gb_used%
format-prefix = "  "
format-prefix-foreground = ${colors.orange}

[module/cpu]
type = internal/cpu
interval = 5

label = %percentage:2%%
format-prefix = " "
format-prefix-foreground = ${colors.orange}

[module/network]
type = internal/network
interface-type = wireless
interval = 5.0

; Icons for wifi on
ramp-signal-0 = x
ramp-signal-1 = .
ramp-signal-2 = :
ramp-signal-3 = .:
ramp-signal-4 = ::
ramp-signal-5 = .::
label-connected = %essid%
format-connected = <label-connected> <ramp-signal>
format-connected-prefix = "直  "
format-connected-prefix-foreground = ${colors.orange}

; Icons for wifi off
label-disconnected = 睊  no wifi?
label-disconnected-foreground = ${colors.comment}

;;;;;;;;;; Middle Modules ;;;;;;;;;;
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

; i3 mode
label-mode = %mode%
label-mode-padding = 2
label-mode-background = ${colors.current-line}
label-mode-foreground = ${colors.foreground}

; Current WS
label-focused = %index%: %name%
label-focused-background = ${colors.background}
label-focused-underline = ${colors.orange}
label-focused-padding = 2

; Other WS
label-unfocused = %index%: %name%
label-unfocused-padding = 2

; Urgent WS
label-urgent = %index%: %name%
label-urgent-background = ${colors.red}
label-urgent-padding = 2

[module/xwindow]
type = internal/xwindow
label = %title:0:40:...%
format = 󱂬  <label>

;;;;;;;;;; Left   Modules ;;;;;;;;;;
[module/pulseaudio]
type = internal/pulseaudio
use-ui-max = true
click-right = pavucontrol

; Icons for volume on
ramp-volume-0 = 奄
ramp-volume-1 = 奔
ramp-volume-2 = 墳
ramp-volume-foreground = ${colors.orange}
label-volume = %percentage%%
format-volume = <ramp-volume> <label-volume>

; Icon for volume mute
label-muted = 婢 muted
label-muted-foreground = ${colors.comment}

[module/backlight]
type = internal/backlight
card = intel_backlight
use-actual-brightness = true

label = %percentage%%
format-prefix = "  "
format-prefix-foreground = ${colors.orange}

[module/battery0]
; Internal (bottom) battery of X270/T460
type = internal/battery
battery = BAT0
adapter = ADP1
full-at = 100
poll-interval = 5

; Icons for normal battery operation
ramp-capacity-0 = 
ramp-capacity-0-foreground = ${colors.red}
ramp-capacity-1 = 
ramp-capacity-1-foreground = ${colors.orange}
ramp-capacity-2 = 
ramp-capacity-2-foreground = ${colors.orange}
ramp-capacity-3 = 
ramp-capacity-3-foreground = ${colors.orange}
format-discharging = <ramp-capacity> <label-discharging>

; Icons for full and charging
format-full =  <label-full>
format-full-foreground = ${colors.yellow}
format-charging =  <label-charging>

[module/battery1]
; External (top) battery of X270/T460
type = internal/battery
battery = BAT1
adapter = ADP1
full-at = 100
poll-interval = 5

; Icons for normal battery operation
ramp-capacity-0 = 
ramp-capacity-0-foreground = ${colors.red}
ramp-capacity-1 = 
ramp-capacity-1-foreground = ${colors.orange}
ramp-capacity-2 = 
ramp-capacity-2-foreground = ${colors.orange}
ramp-capacity-3 = 
ramp-capacity-3-foreground = ${colors.orange}
format-discharging = <ramp-capacity> <label-discharging>

; Icons for full and charging
format-full =  <label-full>
format-full-foreground = ${colors.yellow}
format-charging =  <label-charging>

[module/date]
type = internal/date
interval = 1

date = %a %m-%d
time = %H:%M
label = %date% %time%
format-prefix = "  "
format-prefix-foreground = ${colors.orange}
; A1 Left click, A2 middle, A3 right click, A4 Scroll up, A5 scroll down, etc
format = %{A1:~/dotfiles/polybar/polybar-calendar.sh curr:}%{A3:~/dotfiles/polybar/polybar-calendar.sh next:}<label>%{A}%{A}

;;;;;;;;;; Unused Modules ;;;;;;;;;;
[module/xworkspaces]
type = internal/xworkspaces

label-active = %name%
label-active-background = ${colors.current-line}
label-active-underline= ${colors.orange}
label-active-padding = 1

label-occupied = %name%
label-occupied-padding = 1

label-urgent = %name%
label-urgent-background = ${colors.red}
label-urgent-padding = 1

label-empty = %name%
label-empty-foreground = ${colors.comment}
label-empty-padding = 1

[module/filesystem]
type = internal/fs
interval = 25
mount-0 = /
label-mounted = %{F#FFB86C}%mountpoint%%{F-} %used% / %free%
label-unmounted = %mountpoint% not mounted
label-unmounted-foreground = ${colors.comment}

[module/xkeyboard]
type = internal/xkeyboard
blacklist-0 = num lock

label-layout = %layout%
label-layout-foreground = ${colors.orange}

label-indicator-padding = 2
label-indicator-margin = 1
label-indicator-foreground = ${colors.background}
label-indicator-background = ${colors.pink}

;;;;;;;;;; Settings ;;;;;;;;;;
[settings]
screenchange-reload = true
pseudo-transparency = true

; vim:ft=dosini
