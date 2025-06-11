function fish_mode_prompt -d "Theo's Vim statusline esque mode indicator"
  # Nordfox
  set -l bg0 232831
  set -l cyan 88C0D0
  set -l green A3BE8C
  set -l red BF616A
  set -l purple B48EAD
  set -l blue 81A1C1

  set -l normal (set_color normal)

  set -l mode_prompt
  switch $fish_bind_mode
    case default
      set mode_prompt (set_color --bold $bg0 --background $cyan) "N"
    case insert
      set mode_prompt (set_color --bold $bg0 --background $green) "I"
    case replace_one
      set mode_prompt (set_color --bold $bg0 --background $red) "R"
    case visual
      set mode_prompt (set_color --bold $bg0 --background $purple) "V"
    case '*'
      set mode_prompt (set_color --bold $bg0 --background $blue) "?"
  end

  echo $mode_prompt $normal
end
