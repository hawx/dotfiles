# Mostly stolen from https://github.com/spicycode/ze-best-zsh-config

fpath+=("$(brew --prefix)/share/zsh/site-functions")
if [ -s "$HOME/.asdf/asdf.sh" ]; then
    source ~/.asdf/asdf.sh
fi

source ~/.zsh/exports.zsh
source ~/.zsh/colors.zsh
source ~/.zsh/setopt.zsh
source ~/.zsh/completion.zsh
source ~/.zsh/aliases.zsh
source ~/.zsh/functions.zsh
source ~/.zsh/history.zsh
source ~/.zsh/bindkeys.zsh
source ~/.zsh/zsh_hooks.zsh
source ~/.zsh/secure.zsh             # git ignored, for secure stuff only!

source ~/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# npm i -g pure-prompt
autoload -U promptinit; promptinit
prompt pure

which fortune &> /dev/null
if [ $? -eq 0 ]; then
    fortune clippings
fi
