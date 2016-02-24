export PATH=/bin:/usr/bin:/usr/local/bin:$PATH
export PATH=/usr/local/go/bin:$PATH
export PATH=/usr/local/rbenv/bin:$PATH
export PATH=$HOME/dev/go/bin:$PATH
export PATH=$HOME/dev/dotfiles/bin:$PATH
export PATH=$HOME/dev/bin:$PATH

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
eval "$(rbenv init -)"

# Node
export NVM_DIR=/usr/local/nvm
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# Go
export GOPATH=$HOME/dev/go

# Set up paths for my stuff
export ALEXANDRIA_LIBRARY=$HOME/Books/alexandria
export MIRROR_ROOT=$HOME/mirrors/_data
export MUSIC_ROOT=$HOME/Music
export SOCKET_DIR=$XDG_RUNTIME_DIR/socketandserve
