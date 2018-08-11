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

source ~/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/completion/pass.zsh

if [ -s "$HOME/.asdf/asdf.sh" ]; then
  source ~/.asdf/asdf.sh
  source ~/.asdf/completions/asdf.bash
fi

if [ `which fortune` ]; then
  echo ""
  fortune clippings
  echo ""
fi
