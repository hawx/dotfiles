# Usage: echo "def my_method\nputs 'hi'\nend" | htmlize ruby
function htmlize {
  pygmentize -f html -l $1
}

function git(){hub "$@"}

function reset-permissions() {
  find . -type f -exec chmod 644 {} +;
  find . -type d -exec chmod 755 {} +;
}

function swap_emacs() {
  mv ~/.emacs.d ~/.emacs.d.tmp
  mv ~/.emacs.d.other ~/.emacs.d
  mv ~/.emacs.d.tmp ~/.emacs.d.other
}
