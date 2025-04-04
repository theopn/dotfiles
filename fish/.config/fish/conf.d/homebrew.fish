switch $(uname)
  case "Linux"
    set -x OSTYPE 'Linux'
  case "Darwin"
    set -x OSTYPE 'macOS'
  case '*BSD' 'DragonFly'
    set -x OSTYPE 'BSD'
  case '*'
    set -x OSTYPE 'UNKNOWN'
end

# Homebrew
if [ $OSTYPE = 'macOS' ]
  fish_add_path /opt/homebrew/bin/
end
