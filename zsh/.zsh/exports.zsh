export TZ="Europe/London"

# Setup terminal, and turn on colours
export TERM=xterm-256color
export CLICOLOR=1
export LSCOLORS=Gxfxcxdxbxegedabagacad

# Enable color in grep
export GREP_COLOR='3;33'

export LESS='--ignore-case --raw-control-chars'
export PAGER='less'

# Start emacs in server mode then connects to it
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"

# Ruby
export RBENV_ROOT=/usr/local/rbenv
eval "$($RBENV_ROOT/bin/rbenv init -)"

# Node
export NVM_DIR=/usr/local/nvm
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# Go
export GOROOT=/usr/local/go
export GOPATH=$HOME/dev/go

# Java
export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64

# Set up paths for my stuff
export ALEXANDRIA_LIBRARY=$HOME/Books/alexandria
export MIRROR_ROOT=$HOME/mirrors/_data
export MUSIC_ROOT=$HOME/Music
export SOCKET_DIR=$XDG_RUNTIME_DIR/socketandserve

# XDG
export XDG_CONFIG_HOME=$HOME/.config

# PATH
export PATH=$PATH:$GOROOT/bin:$RBENV_ROOT/bin
export PATH=$PATH:$HOME/dev/bin:$HOME/dev/dotfiles/bin:$GOPATH/bin
