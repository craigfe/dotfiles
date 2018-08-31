#!/bin/zsh

ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[green]%}["
ZSH_THEME_GIT_PROMPT_SUFFIX="]%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}*%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""

# Customized git status, oh-my-zsh currently does not allow render dirty status before branch
git_custom_status() {
  local cb=$(git_current_branch)
  if [ -n "$cb" ]; then
    echo "$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_PREFIX$(git_current_branch)$ZSH_THEME_GIT_PROMPT_SUFFIX"
  fi
}

# Calculate the length of a string post colour parsing
parsed_length () {
	local zero='%([BSUbfksu]|([FB]|){*})'
  echo ${#${(S%%)1//$~zero/}}
}

# The precmd function is invoked before the zsh promt is printed
precmd () {
	local LEFT=$'%B%{$fg[red]%}[ %{$fg[cyan]%}%B%n%{$fg[red]%}%B@%{$fg[green]%}%m%{$reset_color%}%B%{$fg[red]%} ] %{$fg[magenta]%}%B%~%{$reset_color%}'
	local RIGHT=$(git_custom_status)' '

	# Calculate the number of spaces to print between LEFT and RIGHT (accounting for multiline left)
	local SPACES=$(($COLUMNS-$(parsed_length $RIGHT) - ($(parsed_length $LEFT)) % $COLUMNS))

	# Don't show right prompt if it must wrap to a new line
	if [ $SPACES -gt 1 ]; then
		print -rP $LEFT${(l:$SPACES:)}$RIGHT
	else
		print -rP $LEFT
	fi
}

# Return status
function vi_mode_prompt_info() {
	local INSERT_COLOUR="%(?:%{$fg_bold[green]%}:%{$fg_bold[red]%})"
	local NORMAL_COLOUR="%{$fg_bold[magenta]%}"

	echo "${${${KEYMAP/#%/$INSERT_COLOUR}/vicmd/$NORMAL_COLOUR}/main/${INSERT_COLOUR}}"
#	echo "${${KEYMAP/vicmdmain/$RETURN_COLOUR}/(main|viins)/}"
}

function zle-keymap-select zle-line-init zle-line-finish {

	# Change the cursor shape to correspond to vi mode
	case $KEYMAP in
		vicmd)      print -n "\e[1 q";; # block cursor
		viins|main) print -n "\e[6 q";; # line cursor
	esac

	zle reset-prompt
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select

export KEYTIMEOUT=1
PROMPT='$(vi_mode_prompt_info)${ret_status}λ%b '
RPROMPT=' ' # Prevent vi-mode indicator from showing up
