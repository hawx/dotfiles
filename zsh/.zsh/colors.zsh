autoload colors; colors

# The variables are wrapped in %{%}. This should be the case for every
# variable that does not contain space.
for COLOR in RED GREEN YELLOW BLUE MAGENTA CYAN BLACK WHITE; do
  eval PR_$COLOR='%{$fg_no_bold[${(L)COLOR}]%}'
  eval PR_BOLD_$COLOR='%{$fg_bold[${(L)COLOR}]%}'
done

eval RESET='$reset_color'
export PR_RED PR_GREEN PR_YELLOW PR_BLUE PR_WHITE PR_BLACK
export PR_BOLD_RED PR_BOLD_GREEN PR_BOLD_YELLOW PR_BOLD_BLUE 
export PR_BOLD_WHITE PR_BOLD_BLACK

# Clear LSCOLORS
unset LSCOLORS

# Main change, you can see directories on a dark background
#expor tLSCOLORS=gxfxcxdxbxegedabagacad

export CLICOLOR=1
export LS_COLORS=exfxcxdxbxegedabagacad


# Colours for my ir_black mod are
#
# black: 0, 0, 0 = #000000
# light black: 124, 124, 124 = #7c7c7c
# red: 255, 108, 96 = #ff6c60
# light red: 255, 182, 176 = #ffb6b0
# green: 168, 255, 96 = #a8ff60
# light green: 206, 255, 172 = #ceffac
# yellow: 255, 255, 182 = #ffffb6
# light yellow: 255, 255, 204 = #ffffcc
# blue: 150, 203, 254 = #96cbfe
# light blue: 182, 220, 255 = #b6dcff
# magenta: 255, 115, 253 = #ff73fd
# light magenta: 255, 156, 254 = #ff9cfe
# cyan: 198, 197, 254 = #c6c5fe
# light cyan: 223, 223, 254 = #dfdffe
# white: 238, 238, 238 = #eeeeee
# light white: 255, 255, 255 = #ffffff
# 
# text: 242, 242, 242 = #f2f2f2
# bold text: 255, 255, 255 = #ffffff
# selection: 18, 19, 43 = #12132b
# cursor: 255, 165, 96 = #ffa560
# background: 0, 0, 0 = #000000
