function fish_right_prompt -d "Mumbo's custom right prompt displaying command duration"
  # has to be on the top of the function
  set -l __last_command_exit_status $status

  # Nordfox
  set -l bg0 232831
  set -l bg2 39404F
  set -l fg2 ABB1BB
  set -l fg3 7E8188
  set -l green A3BE8C
  set -l red BF616A

  set -l normal (set_color normal)

  # Exit status code
  set -l exit_status_color (set_color --bold $green --background $bg2)
  if test $__last_command_exit_status != 0
      set exit_status_color (set_color --bold $red --background $bg2)
  end
  set -l exit_stat $exit_status_color $__last_command_exit_status

  # last command duration
  set -l __last_command_duration $CMD_DURATION
  set -l cmd_duration_color (set_color --bold $fg3 --background $bg0)
  if test $__last_command_duration -gt 1000
    set __last_command_duration (math $__last_command_duration / 1000) 's'
    set cmd_duration_color (set_color --bold $fg2 --background $bg0)
  else
    set __last_command_duration $__last_command_duration 'ms'
  end
  set -l cmd_duration_stat $cmd_duration_color $__last_command_duration


  echo $normal $exit_stat $cmd_duration_stat $normal
end

