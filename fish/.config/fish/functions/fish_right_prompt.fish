function fish_right_prompt -d "Theo's custom right prompt displaying command duration"
  set -l __last_command_duration $CMD_DURATION

  set -l colo (set_color -o magenta)

  if test $__last_command_duration -gt 1000
    set __last_command_duration (math $__last_command_duration / 1000) 's'
    set colo (set_color -o red)
  else
    set __last_command_duration $__last_command_duration 'ms'
  end
  echo $colo $__last_command_duration (set_color normal)
end

