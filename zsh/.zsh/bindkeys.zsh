# Disable r rerunning last command
disable r

# To see the key combo you want to use just do:
# cat > /dev/null
# And press it

bindkey "^K"      kill-whole-line                      # ctrl-k
bindkey "^R"      history-incremental-search-backward  # ctrl-r
bindkey "^A"      beginning-of-line                    # ctrl-a
bindkey "^E"      end-of-line                          # ctrl-e

# bindkey "[B"      history-search-forward               # down arrow
# bindkey "[A"      history-search-backward              # up arrow
# History search with up and down arrow keys

if [[ -n "${key[Up]}"   ]]
then
  bindkey  "${key[Up]}" history-beginning-search-backward
else
  bindkey "^[[A" history-beginning-search-backward
fi

if [[ -n "${key[Down]}" ]]
then
  bindkey  "${key[Down]}" history-beginning-search-forward
else
  bindkey "^[[B" history-beginning-search-forward
fi

bindkey "^D"      delete-char                          # ctrl-d
bindkey "^F"      forward-char                         # ctrl-f
bindkey "^B"      backward-char                        # ctrl-b

bindkey -e   # Default to standard emacs bindings, regardless of editor string
