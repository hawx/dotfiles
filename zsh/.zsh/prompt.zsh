# Also see http://www.acm.uiuc.edu/workshops/zsh/prompt/escapes.html

PROMPT_ARROW='\xe2\x86\x92'
PROMPT_CROSS='\xc3\x97'
PROMPT_PUSH='\xe2\x86\x91'
PROMPT_PULL='\xe2\x86\x93'
PROMPT_SYNC='\xe2\x86\x95'
PROMPT_ADD='+'
PROMPT_CHANGES='\xe2\x9a\xa1'
PROMPT_MERGE='~'

function in_git {
  git branch >/dev/null 2> /dev/null && return true
  false
}

function git_prompt_info {
  local ok=$?
  local prompt="$PROMPT_ARROW "

  if [ $ok -ne 0 ]; then
    prompt="$PROMPT_CROSS "
  fi

  if $(in_git); then
    local gitst="$(git status 2> /dev/null)"

    if [[ -f .git/MERGE_HEAD ]]; then
      if [[ ${gitst} =~ "Unmerged" ]]; then
        prompt="%{$fg[red]%}$PROMPT_MERGE%{$reset_color%} "
      else
        prompt="%{$fg[green]%}$PROMPT_MERGE%{$reset_color%} "
      fi
    elif [[ ${gitst} =~ "Changes to be committed" ]]; then
      prompt="$PROMPT_CHANGES "
    elif [[ ${gitst} =~ "use \"git add" ]]; then
      prompt="$PROMPT_ADD "
    else
      local gitstsb="$(git status -sb 2> /dev/null)"

      if [[ ${gitstsb} =~ "ahead" ]]; then
        if [[ ${gitstsb} =~ "behind" ]]; then
          prompt="$PROMPT_SYNC "
        else
          prompt="$PROMPT_PUSH "
        fi
      elif [[ ${gitstsb} =~ "behind" ]]; then
        prompt="$PROMPT_PULL "
      fi
    fi
  fi

  echo "$prompt"
}

function git_branch_info {
  if $(in_git); then
    local name=`git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* //'`
    echo "%{$fg[cyan]%}$name%{$reset_color%}"
  else
    echo ""
  fi
}

PROMPT='$(git_prompt_info)'
RPROMPT='$(git_branch_info)'
