function numfiles -d "Count the number of file in the directory"
  set -l num $(ls -A $argv | wc -l)
  [ -n $num ]; and echo "$num files in $argv"
end

