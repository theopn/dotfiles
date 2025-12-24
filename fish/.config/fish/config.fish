# $figlet -f fuzzy fish
#  .--. _       .-.
# : .-':_;      : :
# : `; .-. .--. : `-.
# : :  : :`._-.': .. :
# :_;  :_;`.__.':_;:_;
#
# Mumbo's Fish config
# Most of them are in conf.d

if status is-interactive
  # Enable Vi keybinding
  fish_vi_key_bindings
  set fish_cursor_default block
  set fish_cursor_insert line
  set fish_vi_force_cursor
end

# pyenv path start
set -x PYENV_ROOT $HOME/.pyenv

if test -d $PYENV_ROOT/bin
    set -x PATH $PYENV_ROOT/bin $PATH
end

if status --is-interactive
    pyenv init - | source
    pyenv virtualenv-init - | source
end
# pyenv path end

# nvm start
# function nvm
#   bass source (brew --prefix nvm)/nvm.sh --no-use ';' nvm $argv
# end
#
set -x NVM_DIR ~/.nvm
nvm use latest --silent
# nvm end


set -x GPG_TTY (tty)



# Java installations (using Homebrew paths)
set -x JAVA21_HOME (brew --prefix openjdk@21)/libexec/openjdk.jdk/Contents/Home
# set -x JAVA17_HOME (brew --prefix openjdk@17)/libexec/openjdk.jdk/Contents/Home

# Default Java
set -x JAVA_HOME $JAVA21_HOME
set -x PATH $JAVA_HOME/bin $PATH

# Quick switching functions
# function use-java17
#     set -x JAVA_HOME $JAVA17_HOME
#     set -x PATH $JAVA_HOME/bin $PATH
#     echo "Switched to Java 17"
# end

function use-java21
    set -x JAVA_HOME $JAVA21_HOME
    set -x PATH $JAVA_HOME/bin $PATH
    echo "Switched to Java 21"
end
