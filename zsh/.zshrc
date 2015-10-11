# Mostly stolen from https://github.com/spicycode/ze-best-zsh-config

source ~/.zsh/exports.zsh
source ~/.zsh/colors.zsh
source ~/.zsh/setopt.zsh
source ~/.zsh/prompt.zsh
source ~/.zsh/completion.zsh
source ~/.zsh/aliases.zsh
source ~/.zsh/functions.zsh
source ~/.zsh/history.zsh
source ~/.zsh/bindkeys.zsh
source ~/.zsh/zsh_hooks.zsh
source ~/.zsh/secure.zsh             # git ignored, for secure stuff only!

eval "$(rbenv init -)"                     # rbenv

# For uni network
[[ -z $SSH_TTY ]] && umask 007

source ~/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/completion/pass.zsh

export NVM_DIR="/home/hawx/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
