# Usage: echo "def my_method\nputs 'hi'\nend" | htmlize ruby
function htmlize {
  pygmentize -f html -l $1
}

function reset-permissions() {
  find . -type f -exec chmod 644 {} +;
  find . -type d -exec chmod 755 {} +;
}

__git_files () {
  _wanted files expl 'local files' _files
}

eg(){
  MAN_KEEP_FORMATTING=1 man "$@" 2>/dev/null \
    | sed --quiet --expression='/^E\(\x08.\)X\(\x08.\)\?A\(\x08.\)\?M\(\x08.\)\?P\(\x08.\)\?L\(\x08.\)\?E/{:a;p;n;/^[^ ]/q;ba}' \
    | ${MANPAGER:-${PAGER:-pager -s}}
}

function dir-to-mp3() {
  for f in *.flac; do
    ffmpeg -i "$f" -codec:a libmp3lame -qscale:a 2 -map_metadata 0 "${f%.*}".mp3
  done
}

npx() {
  node_modules/.bin/$1 "${@:2}"
}
