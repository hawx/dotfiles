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

function uplogtail() {
  tail -f $XDG_CACHE_HOME/upstart/$1.log
}

__git_files () {
  _wanted files expl 'local files' _files
}

eg(){
  MAN_KEEP_FORMATTING=1 man "$@" 2>/dev/null \
    | sed --quiet --expression='/^E\(\x08.\)X\(\x08.\)\?A\(\x08.\)\?M\(\x08.\)\?P\(\x08.\)\?L\(\x08.\)\?E/{:a;p;n;/^[^ ]/q;ba}' \
    | ${MANPAGER:-${PAGER:-pager -s}}
}
