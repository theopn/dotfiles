;=======================================
; ▄▄▄·By    ▄▄▌T  ▄· ▄▌▄▄▄▄·  ▄▄▄· ▄▄▄
;▐█ ▄█▪     ██• h▐█▪██▌▐█ ▀█▪▐█ ▀█ ▀▄ █·
; ██▀· ▄█▀▄ ██▪  e█▌▐█▪▐█▀▀█▄▄█▀▀█ ▐▀▀▄
;▐█▪·•▐█▌.▐▌▐█▌▐▌ o█▀·.██▄▪▐█▐█ ▪▐▌▐█•█▌
;.▀    ▀█▄▀▪.▀▀▀   ▀ • ·▀▀▀▀  ▀  ▀ .▀  ▀
;
; Configuration based on the default
; + all the modules I made
; + Dracula theme
;=======================================

[colors]
background = #282A36
current-line = #44475A
foreground = #F8F8F2
orange = #FFB86C
pink = #FF79C6
red = #FF5555
comment = #6272A4

[bar/dracula]
width = 100%
height = 24pt
radius = 6

background = ${colors.background}
foreground = ${colors.foreground}

line-size = 3pt

border-size = 3pt
border-color = ${colors.comment}

padding-left = 0
padding-right = 1

module-margin = 1

separator = |
separator-foreground = ${colors.comment}

font-0 = CaskaydiaCove Nerd Font:size=11;1

modules-left = i3 xwindow
modules-right = pulseaudio backlight memory cpu wlan eth date

cursor-click = pointer
cursor-scroll = ns-resize

enable-ipc = true

;tray-position = right
tray-padding = 1

wm-restack = i3

;;;;;;;;;; Right Modules ;;;;;;;;;;
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
label-mode-background = ${colors.current-line}
label-mode-foreground = ${colors.current-line}

label-focused = %index%: %name%
label-focused-background = ${colors.background}
label-focused-underline = ${colors.orange}
label-focused-padding = 2

label-unfocused = %index%: %name%
label-unfocused-padding = 2

label-urgent = %index%: %name%
label-urgent-background = ${colors.red}
label-urgent-padding = 2

[module/xwindow]
type = internal/xwindow
; 0 to 60, then ...
label = %title:0:60:...%
format = 󱂬  <label>

;;;;;;;;;; Left  Modules ;;;;;;;;;;
[module/pulseaudio]
type = internal/pulseaudio
use-ui-max = true
click-right = pavucontrol

ramp-volume-0 = 奄
ramp-volume-1 = 奔
ramp-volume-2 = 墳
ramp-volume-foreground = ${colors.orange}
label-volume = %percentage%%
format-volume = <ramp-volume> <label-volume>

label-muted = 婢 off
label-muted-foreground = ${colors.comment}

[module/backlight]
type = internal/backlight
card = intel_backlight
use-actual-brightness = true
format-prefix = "  "
format-prefix-foreground = ${colors.orange}
label = %percentage%%
format = <label>

[module/memory]
type = internal/memory
interval = 5
label = %gb_used:0%
format-prefix = "  "
format-prefix-foreground = ${colors.orange}

[module/cpu]
type = internal/cpu
interval = 5
label = %percentage:2%%
format-prefix = " "
format-prefix-foreground = ${colors.orange}

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

[module/date]
type = internal/date
interval = 1

date = %H:%M
date-alt = %Y-%m-%d %H:%M:%S

label = %date%
label-foreground = ${colors.orange}

[settings]
screenchange-reload = true
pseudo-transparency = true

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

; vim:ft=dosini
