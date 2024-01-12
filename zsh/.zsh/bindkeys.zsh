# Disable r rerunning last command
disable r

# To see the key combo you want to use just do:
# cat > /dev/null
# And press it

bindkey "^K"      kill-whole-line                      # ctrl-k
bindkey "^R"      history-incremental-search-backward  # ctrl-r
bindkey "^A"      beginning-of-line                    # ctrl-a
bindkey "^E"      end-of-line                          # ctrl-e
bindkey "^D"      delete-char                          # ctrl-d
bindkey "^F"      forward-char                         # ctrl-f
bindkey "^B"      backward-char                        # ctrl-b

# History search with up and down arrow keys
bindkey "^[[A"    history-beginning-search-backward
bindkey "^[[B"    history-beginning-search-forward

bindkey -e   # Default to standard emacs bindings, regardless of editor string
