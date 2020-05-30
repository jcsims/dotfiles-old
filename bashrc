#!/usr/bin/env bash

## Basic bash config
export CLICOLOR=1
export EDITOR='emacsclient -t -a ""'
export VISUAL='emacsclient -c -a ""'
export TERM=xterm-256color
export LSCOLORS='exfxcxdxbxegedabagacad'

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
appendpath () {
    case ":$PATH:" in
        *:"$1":*)
        ;;
        *)
            PATH="${PATH:+$PATH:}$1"
    esac
}

appendpath $HOME/bin

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

    ## eval "$(keychain --eval --quiet --agents gpg,ssh id_rsa 98662236EE64EFAF0BE9973025FF041622DE3AFB)"

    # Add an "alert" alias for long running commands.  Use like so:
    #   sleep 10; alert
    alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

    alias ls='ls --color=auto'

    alias open='xdg-open'

    # For Fedora
    #alias docker='podman' <-- this is already handled by the
    #                          podman-docker package
    alias docker-compose='podman-compose'

    [ -f /usr/share/fzf/shell/key-bindings.bash ] && source /usr/share/fzf/shell/key-bindings.bash
    [ -f /usr/share/fzf/shell/completion.bash ] && source /usr/share/fzf/shell/completion.bash

    export GOPATH=$HOME/code/go:$HOME/code/tg/sandcastle
    export GOBIN=$HOME/bin

    # Base16 Shell
    # https://github.com/chriskempson/base16-shell
    BASE16_SHELL="$HOME/.config/base16-shell/"
    [ -n "$PS1" ] && \
        [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"

    ## Set the terminal theme
    [ -f $HOME/.base16_theme ] && source $HOME/.base16_theme

    # export XDG_DESKTOP_DIR="$HOME"
    # export XDG_DOWNLOAD_DIR="$HOME/downloads"
    # export XDG_DOCUMENTS_DIR="$HOME/documents"
    # export XDG_MUSIC_DIR="$HOME/music"
    # export XDG_PICTURES_DIR="$HOME/pictures"
    # export XDG_VIDEOS_DIR="$HOME/videos"

    ## Nix support
    [ -f /etc/profile.d/nix.sh ] && source /etc/profile.d/nix.sh

    if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
        export _JAVA_AWT_WM_NONREPARENTING=1
        export XDG_CURRENT_DESKTOP=sway
        export XDG_SESSION_TYPE=wayland
        export CLUTTER_BACKEND=wayland
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
        export SDL_VIDEODRIVER=wayland
        exec sway
    fi

elif [[ "$OSTYPE" == "darwin"* ]]; then
    # Add GOROOT bin path to PATH
    appendpath /usr/local/go/bin

    export BASH_COMPLETION_COMPAT_DIR="/usr/local/etc/bash_completion.d"

    [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

    # Brew-installed paths
    appendpath /usr/local/bin
    appendpath /usr/local/sbin

    # Add installed-from-source Postgres binary and manpage paths
    appendpath /usr/local/pgsql/bin
    export MANPATH=/usr/local/pgsql/share/man:$MANPATH

    alias stay-awake='caffeinate -di'

    alias alert='terminal-notifier -activate "com.googlecode.iterm2" -message "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

    [ -f ~/.fzf.bash ] && source ~/.fzf.bash

    export GOPATH=$HOME/dev/go:$HOME/dev/tg/sandcastle:$HOME/dev/tg/ops
    export GOBIN=$HOME/bin

fi

# Set the theme for `bat`
export BAT_THEME="Nord"

# Cargo's bin path
appendpath $HOME/.cargo/bin

# Sensible options, borrowed from
# https://github.com/mrzool/bash-sensible
# Update window size after every command
shopt -s checkwinsize

# Automatically trim long paths in the prompt (requires Bash 4.x)
PROMPT_DIRTRIM=2

# Set the terminal title
PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'

## Bind a few things in interactive shells only
if [[ $- == *i* ]]; then
    # Enable history expansion with space
    # E.g. typing !!<space> will replace the !! with your last command
    bind Space:magic-space

    # Perform file completion in a case insensitive fashion
    bind "set completion-ignore-case on"

    # Treat hyphens and underscores as equivalent
    bind "set completion-map-case on"

    # Display matches for ambiguous patterns at first tab press
    bind "set show-all-if-ambiguous on"

    # Immediately add a trailing slash when autocompleting symlinks to directories
    bind "set mark-symlinked-directories on"

    # Enable incremental history search with up/down arrows (also Readline goodness)
    # Learn more about this here: http://codeinthehole.com/writing/the-most-important-command-line-tip-incremental-history-searching-with-inputrc/
    bind '"\e[A": history-search-backward'
    bind '"\e[B": history-search-forward'
    bind '"\e[C": forward-char'
    bind '"\e[D": backward-char'
    bind '"\e\e[D": backward-word'
    bind '"\e\e[C": forward-word'

fi

# Turn on recursive globbing (enables ** to recurse all directories)
shopt -s globstar 2> /dev/null

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

# Append to the history file, don't overwrite it
shopt -s histappend

# Save multi-line commands as one command
shopt -s cmdhist

# Huge history. Doesn't appear to slow things down, so why not?
HISTSIZE=500000
HISTFILESIZE=100000

# Avoid duplicate entries
HISTCONTROL="erasedups:ignoreboth"

# Don't record some commands
HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

# Use standard ISO 8601 timestamp
# %F equivalent to %Y-%m-%d
# %T equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='%FT%T '

# Prepend cd to directory names automatically
shopt -s autocd 2> /dev/null
# Correct spelling errors during tab-completion
shopt -s dirspell 2> /dev/null
# Correct spelling errors in arguments supplied to cd
shopt -s cdspell 2> /dev/null

# This defines where cd looks for targets
# Add the directories you want to have fast access to, separated by colon
# Ex: CDPATH=".:~:~/projects" will look for targets in the current
# working directory, in home and in the ~/projects folder
# CDPATH=".:~/code"

alias grep='grep --color=auto'

alias reload!='. ~/.bashrc'
alias tree='tree -C'

alias e='emacsclient -t -a ""'
alias ec='emacsclient -c -a ""'

alias cat='bat'

## Git aliases
alias gl="git log --graph --abbrev-commit --date=relative --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'"
alias gp='git push origin HEAD'
alias gpl='git pull --rebase --prune'
alias gd='git diff'
alias gdc='git diff --cached'
alias gc='git commit'
alias gco='git checkout'
alias ga='git add'
alias gs='git status -sb'

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

unset appendpath
export PATH

## Prompt
#PS1="[\u@\h \W]\$ " # Default prompt

PS1="[\u@\h \w]\$ "
