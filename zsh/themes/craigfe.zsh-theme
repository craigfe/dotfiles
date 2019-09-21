#!/bin/zsh

export KEYTIMEOUT=8
bindkey -M viins 'jk' vi-cmd-mode
bindkey -M viins 'kj' vi-cmd-mode

ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[green]%}["
ZSH_THEME_GIT_PROMPT_SUFFIX="]%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}*%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""

# Customized git status, oh-my-zsh currently does not allow render dirty status before branch
function git_custom_status {
    local cb=$(git_current_branch)
    if [ -n "$cb" ]; then
        echo "$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_PREFIX$(git_current_branch)$ZSH_THEME_GIT_PROMPT_SUFFIX"
    fi
}

function is_ocaml_project {
    test -d "$1/_opam" -o -f "$1/dune-project"
}

ZSH_THEME_OPAM_PROMPT_PREFIX="%{$reset_color%}%{$fg[cyan]%}["
ZSH_THEME_OPAM_PROMPT_SUFFIX="]%{$reset_color%}"
ZSH_THEME_OPAM_PROMPT_GLOBAL="%{$fg[red]%}*%{$reset_color%}"

# State the opam version if there is a local switch
function opam_status {
    git_toplevel="$(git rev-parse --show-toplevel 2>/dev/null)"
    if is_ocaml_project "$git_toplevel"; then
        if [ -d "$git_toplevel/_opam" ]; then
            local compiler_version="$(cat "$git_toplevel/_opam/.opam-switch/config/ocaml.config" | grep --color=never -oP 'compiler: "\K.*(?=")')"
            echo "$ZSH_THEME_OPAM_PROMPT_PREFIX$compiler_version$ZSH_THEME_OPAM_PROMPT_SUFFIX"
        else
            # There is no local opam switch in <git_home>/_opam, so get the currently active one
            local compiler_version="$(opam switch --color=never | grep --color=never -- '->' | awk '{print $2}')"
            echo "$ZSH_THEME_OPAM_PROMPT_GLOBAL$ZSH_THEME_OPAM_PROMPT_PREFIX$compiler_version$ZSH_THEME_OPAM_PROMPT_SUFFIX"
        fi
    fi
}

# Calculate the length of a string post colour parsing
function parsed_length {
	local zero='%([BSUbfksu]|([FB]|){*})'
  echo ${#${(S%%)1//$~zero/}}
}

# The precmd function is invoked before the zsh promt is printed
function precmd {
	local LEFT=$'%B%{$fg[red]%}[ %{$fg[cyan]%}%B%n%{$fg[red]%}%B@%{$fg[green]%}%m%{$reset_color%}%B%{$fg[red]%} ] %{$fg[magenta]%}%B%~%{$reset_color%}'
	local RIGHT="$(opam_status) $(git_custom_status)"

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
		vicmd)      print -n "\e[2 q";; # block cursor
		viins|main) print -n "\e[6 q";; # line cursor
	esac

	zle reset-prompt
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select

PROMPT='$(vi_mode_prompt_info)${ret_status}ᐅ%b '
RPROMPT=' ' # Prevent vi-mode indicator from showing up
