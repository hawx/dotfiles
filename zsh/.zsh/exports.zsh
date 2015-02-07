export PATH=/bin:/usr/bin:/usr/local/bin:$PATH
export PATH=/usr/local/go/bin:$PATH
export PATH=/usr/local/heroku/bin:$PATH
export PATH=$HOME/.anaconda/bin:$PATH
export PATH=$HOME/.rbenv/shims:$HOME/.rbenv/bin:$PATH
export PATH=$HOME/dev/go/bin:$PATH
export PATH=$HOME/dev/dotfiles/bin:$PATH
export PATH=$HOME/dev/bin:$PATH

export TZ="Europe/London"

# Setup terminal, and turn on colours
export TERM=xterm-256color
export CLICOLOR=1
export LSCOLORS=Gxfxcxdxbxegedabagacad

# Enable color in grep
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='3;33'

export LESS='--ignore-case --raw-control-chars'
export PAGER='less'

# Start emacs in server mode then connects to it
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"

# This path is for node.js
export NODE_PATH=/usr/local/lib/node:$NODE_PATH

# Get rid of annoying .rbc files
export RBXOPT="-Xrbc.db=/tmp/rbc -X19"

# Set up paths for Go
export GOPATH=$HOME/dev/go

# Set up paths for Java
export CLASSPATH=$HOME/dev/java/mysql-connector-java-5.1.34/mysql-connector-java-5.1.34-bin.jar:$CLASSPATH
export CLASSPATH=$HOME/dev/java/hamcrest-core-1.3.jar:$CLASSPATH
export CLASSPATH=$HOME/dev/java/junit-4.11.jar:$CLASSPATH

# Set up paths for my stuff
export ALEXANDRIA_LIBRARY=$HOME/Books/alexandria
export MIRROR_ROOT=$HOME/mirrors/_data
export MUSIC_ROOT=$HOME/Music
