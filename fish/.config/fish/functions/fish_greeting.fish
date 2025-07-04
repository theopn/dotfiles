function fish_greeting -d "GREETINGS"
  # Colors
  set -l normal (set_color normal)
  set -l cyan (set_color -o cyan)
  set -l brcyan (set_color -o brcyan)
  set -l green (set_color -o green)
  set -l brgreen (set_color -o brgreen)
  set -l red (set_color -o red)
  set -l brred (set_color -o brred)

  set -l blue (set_color -o blue)
  set -l brblue (set_color -o brblue)
  set -l magenta (set_color -o magenta)
  set -l brmagenta (set_color -o brmagenta)
  set -l yellow (set_color -o yellow)
  set -l bryellow (set_color -o bryellow)

  set -l beige (set_color -o bba592)

  # Collection of Oliver ASCII arts
  set -l olivers \
  '
       \/   \/
       |\__/,|     _
     _.|o o  |_   ) )
    -(((---(((--------
    ' \
    '
       \/       \/
       /\_______/\
      /   o   o   \
     (  ==  ^  ==  )
      )           (
     (             )
     ( (  )   (  ) )
    (__(__)___(__)__)
    ' \
    '
                           _
          |\      _-``---,) )
    ZZZzz /,`.-```    -.   /
         |,4-  ) )-,_. ,\ (
        ---``(_/--`  `-`\_)
    ' \
    # Thanks Jonathan for the one below
    '
          \/ \/
          /\_/\ _______
         = o_o =  _ _  \     _
         (__^__)   __(  \.__) )
      (@)<_____>__(_____)____/
        ♡ ~~ ♡ OLIVER ♡ ~~ ♡
    ' \
    '
       \/   \/
       |\__/,|        _
       |_ _  |.-----.) )
       ( T   ))        )
      (((^_(((/___(((_/
    ' \
    '
    You found the only "fish" that Oliver could not eat!
           .
          ":"
        ___:____     |"\/"|
      ,`        `.    \  /
      |  O        \___/  |
    ~^~^~^~^~^~^~^~^~^~^~^~^~
    '
  set -l oliver "$(random choice $olivers)" # will break new line without the quotes

  # Other information
  set -l fish_ver $(fish --version)
  set -l uptime $(uptime | grep -ohe 'up .*' | sed 's/,//g' | awk '{ print $2" "$3 " " }')

  # Print the msg
  echo
  echo -e "  " "$brgreen"  "Meow"                              "$normal"
  echo -e "  " "$beige"    "$oliver"                           "$normal"
  echo -e "  " "$cyan"     "  Shell:\t"   "$brcyan$fish_ver"  "$normal"
  echo -e "  " "$blue"     "  Uptime:\t"  "$brblue$uptime"    "$normal"
  echo
end
