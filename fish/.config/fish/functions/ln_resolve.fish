function ln_resolve -d "Create a symlink using absolute path"
  if test (count $argv) -ne 2
    echo "Usage: ln_resolve <source> <target>"
    return 1
  end
  set -l source (realpath $argv[1])
  set -l target (realpath $argv[2])

  ln -s "$source" "$target"

  if test $status -eq 0
    echo "Symlink created: $target -> $source"
  end
end
