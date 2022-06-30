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
background = #aa282a36
background-alt = #282a36
; Dracula Foreground
foreground = #f8f8f2
; Dracula Cyan
primary = #8be9fd
; Dracula Comment
secondary = #6272a4
; Dracula Red
alert = #ff5555
; Nord 14
disabled = #a3be8c
transparent = #00000000

[bar/myBarTop]
background = ${colors.background}
foreground = ${colors.foreground}
border-color = ${colors.transparent}
separator = |
separator-foreground = ${colors.disabled}
font-0 = FantasqueSansMono Nerd Font:size20;1
cursor-click = pointer
cursor-scroll = ns-resize
width = 100%
height = 24pt
radius = 6
line-size = 3pt
border-size = 3pt
padding-left = 1
padding-right = 1
module-margin = 1
module-margin-left = 1
module-margin-right = 1

modules-left = xworkspaces
modules-center = xwindow
modules-right = backlight pulseaudio battery0 battery1 date
enable-ipc = true

[bar/myBarBottom]
background = ${colors.background}
foreground = ${colors.foreground}
border-color = ${colors.transparent}
font-0 = FantasqueSansMono Nerd Font:size20;1
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

modules-left = powermenu
modules-right = filesystem memory cpu network

; ---[[ Top modules
[module/xworkspaces]
type = internal/xworkspaces
label-active = %name%
label-active-background = ${colors.background-alt}
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

[module/xwindow]
type = internal/xwindow
label = %title:0:60:...%

[module/backlight]
type = internal/backlight
card = intel_backlight
use-actual-brightness = true
label = %percentage%%
format-prefix = "  "
format-prefix-foreground = ${colors.primary}

[module/pulseaudio]
type = internal/pulseaudio
use-ui-max = true

label-volume = %percentage%%
ramp-volume-0 = 奄
ramp-volume-1 = 奔
ramp-volume-2 = 墳
label-muted = 婢 muted
label-muted-foreground = ${colors.disabled}
format-volume = <ramp-volume> <label-volume>

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
interval = 5
date = "%a %m-%d"
time = "%H:%M:%S"
label = %date% %time%
format = %{A1:$HOME/.config/polybar/polybar_calendar.sh curr:}%{A4:$HOME/.config/polybar/polybar_calendar.sh next:}%{A5:$HOME/.config/polybar/polybar_calendar.sh prev:}   <label>%{A}%{A}%{A}
; --]]

; ---[[ Bottom modules
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

[module/filesystem]
type = internal/fs
interval = 25
mount-0 = /
label-mounted =  %mountpoint%: %used% of %total%
label-mounted-foreground = ${colors.primary}
label-unmounted =  %mountpoint%: NOT MOUNTED
label-unmounted-foreground = ${colors.disabled}

[module/memory]
type = internal/memory
interval = 2
label = %gb_used%/%gb_total%
format-prefix = " "
format-prefix-foreground = ${colors.primary}

[module/cpu]
type = internal/cpu
interval = 2
label = %percentage:2%%
format-prefix = " "
format-prefix-foreground = ${colors.primary}

[module/network]
type = internal/network
interface-type = wireless
interval = 3.0
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
