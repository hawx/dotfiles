# Also see http://www.acm.uiuc.edu/workshops/zsh/prompt/escapes.html

function parse_git_branch {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\/git:\1/'
}

function in_git {
  git branch >/dev/null 2> /dev/null && return true
  false
}

function git_prompt_info {
  local ok=$?
  local gitst="$(git status 2> /dev/null)"

  if [[ -f .git/MERGE_HEAD ]]; then
    if [[ ${gitst} =~ "Unmerged" ]]; then
      gitstatus="%{$fg[red]%}~%{$reset_color%} "
    else
      gitstatus="%{$fg[green]%}~%{$reset_color%} "
    fi
  elif [[ ${gitst} =~ "Changes to be committed" ]]; then
    gitstatus="⚡ "
  elif [[ ${gitst} =~ "use \"git add" ]]; then
    gitstatus="+ "
  elif [[ -n `git checkout HEAD 2> /dev/null | grep ahead` ]]; then
    gitstatus="↑ "
    # down: ↓  up: ↑  both: ↕
  else
    gitstatus='> '
  fi

  if $(in_git); then
    #echo "%{$fg_bold[green]%}/${ref#refs/heads/}%{$reset_color%}$gitstatus$pairname"
    echo "$gitstatus"
  else
    # echo "> "
    if [ $ok -eq 0 ]; then
      echo '\xe2\x86\x92 '
    else
      echo '\xc3\x97 '
    fi
  fi
}

function git_branch_name {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* //'
}

function git_branch_info {
  if $(in_git); then
    echo "%{$fg[cyan]%}$(git_branch_name)%{$reset_color%}"
  else
    echo ""
  fi
}

PROMPT='$(git_prompt_info)'
RPROMPT='$(git_branch_info)'
