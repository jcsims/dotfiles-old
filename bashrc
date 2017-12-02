#!/usr/bin/env bash

## Basic bash config
export CLICOLOR=true
export EDITOR='emacsclient'
export TERM=xterm-256color

if [[ -x /usr/bin/dircolors ]] ; then
    eval "$(dircolors)"
fi

# Source global definitions
if [[ -f /etc/bashrc ]]; then
	. /etc/bashrc
fi

# use .localrc for SUPER SECRET CRAP that you don't
# want in your public, versioned repo.
if [[ -a ~/.localrc ]]
then
    source "$HOME/.localrc"
fi

## Path
export PATH=$HOME/bin:$PATH

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    # enable programmable completion features (you don't need to enable
    # this, if it's already enabled in /etc/bash.bashrc and /etc/profile
    # sources /etc/bash.bashrc).
    if ! shopt -oq posix; then
        if [ -f /usr/share/bash-completion/bash_completion ]; then
            . /usr/share/bash-completion/bash_completion
        elif [ -f /etc/bash_completion ]; then
            . /etc/bash_completion
        fi
    fi

    # Add an "alert" alias for long running commands.  Use like so:
    #   sleep 10; alert
    alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

    ## Pacman aliases
    alias pac-list-orphans='pacman -Qdt'
    alias pac-list-files-in-package='pacman -Ql'
    alias pac-leaves='pacman -Qet'

elif [[ "$OSTYPE" == "darwin"* ]]; then
    # Add GOROOT bin path to PATH
    export PATH=/usr/local/go/bin:$PATH
    [ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

    # Brew-installed paths
    export PATH=/usr/local/bin:/usr/local/sbin:$PATH
fi

export GOPATH=$HOME/code/go:$HOME/code/tg/sandcastle:$HOME/code/tg/ops
export GOBIN=$HOME/bin

# Sensible options, borrowed from
# https://github.com/mrzool/bash-sensible
# Update window size after every command
shopt -s checkwinsize

# Automatically trim long paths in the prompt (requires Bash 4.x)
PROMPT_DIRTRIM=2

# Enable history expansion with space
# E.g. typing !!<space> will replace the !! with your last command
bind Space:magic-space

# Turn on recursive globbing (enables ** to recurse all directories)
shopt -s globstar 2> /dev/null

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

# Perform file completion in a case insensitive fashion
bind "set completion-ignore-case on"

# Treat hyphens and underscores as equivalent
bind "set completion-map-case on"

# Display matches for ambiguous patterns at first tab press
bind "set show-all-if-ambiguous on"

# Immediately add a trailing slash when autocompleting symlinks to directories
bind "set mark-symlinked-directories on"

# Append to the history file, don't overwrite it
shopt -s histappend

# Save multi-line commands as one command
shopt -s cmdhist

PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'history -a'
PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'

# Huge history. Doesn't appear to slow things down, so why not?
HISTSIZE=500000
HISTFILESIZE=100000

# Avoid duplicate entries
HISTCONTROL="erasedups:ignoreboth"

# Don't record some commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

# Use standard ISO 8601 timestamp
# %F equivalent to %Y-%m-%d
# %T equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='%F %T '

# Enable incremental history search with up/down arrows (also Readline goodness)
# Learn more about this here: http://codeinthehole.com/writing/the-most-important-command-line-tip-incremental-history-searching-with-inputrc/
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
bind '"\e[C": forward-char'
bind '"\e[D": backward-char'
bind '"\e\e[D": backward-word'
bind '"\e\e[C": forward-word'

# Prepend cd to directory names automatically
shopt -s autocd 2> /dev/null
# Correct spelling errors during tab-completion
shopt -s dirspell 2> /dev/null
# Correct spelling errors in arguments supplied to cd
shopt -s cdspell 2> /dev/null

# This defines where cd looks for targets
# Add the directories you want to have fast access to, separated by colon
# Ex: CDPATH=".:~:~/projects" will look for targets in the current working directory, in home and in the ~/projects folder
# CDPATH=".:~/code"

alias grep='grep --color=auto'
if [[ -x /usr/local/bin/gls ]] ; then
    alias ls='gls'
else
    alias ls='ls --color=auto'
fi

alias reload!='. ~/.bashrc'
alias tree='tree -C'

alias e='emacsclient -t -a ""'
alias ec='emacsclient -c -a ""'

## Git aliases
alias gl="git log --graph --abbrev-commit --date=relative --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'"
alias gp='git push origin HEAD'
alias gpl='git pull --rebase --prune'
alias gd='git diff'
alias gc='git commit'
alias gco='git checkout'
alias gs='git status -sb' # upgrade your git if -sb breaks for # you. it's fun.

for f in $HOME/.functions/*; do source "$f"; done

## Colorized man pages!
## http://boredzo.org/blog/archives/2016-08-15/colorized-man-pages-understood-and-customized
man() {
    env \
    LESS_TERMCAP_md=$'\e[1;36m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[1;40;92m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[1;32m' \
    man "$@"
}

## Prompt
#PS1="[\u@\h \W]\$ " # Default prompt

PS1="[\u@\h \w]\$ "
