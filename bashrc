#!/usr/bin/env bash

## Basic bash config
export CLICOLOR=1
export EDITOR=$HOME/bin/e
export VISUAL=$HOME/bin/ec
export TERM=xterm-256color

if [[ -x /usr/bin/dircolors ]] ; then
    eval "$(dircolors)"
fi

# use .localrc for SUPER SECRET CRAP that you don't
# want in your public, versioned repo.
if [[ -a ~/.localrc ]]
then
    source "$HOME/.localrc"
fi

## Path
append_path () {
    case ":$PATH:" in
        *:"$1":*)
        ;;
        *)
            PATH="${PATH:+$PATH:}$1"
    esac
}

prepend_path () {
    case ":$PATH:" in
        *:"$1":*)
        ;;
        *)
            PATH="$1:$PATH"
    esac
}

prepend_path $HOME/bin

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    eval "$(keychain --eval --quiet --agents gpg,ssh id_rsa 98662236EE64EFAF0BE9973025FF041622DE3AFB)"

    # Add an "alert" alias for long running commands.  Use like so:
    #   sleep 10; alert
    alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

    alias ls='ls --color=auto'

    alias open='xdg-open'

    [ -f /usr/share/skim/key-bindings.bash ] && source /usr/share/skim/key-bindings.bash
    [ -f /usr/share/skim/completion.bash ] && source /usr/share/skim/completion.bash

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

    ## Nix support
    [ -f /etc/profile.d/nix.sh ] && source /etc/profile.d/nix.sh

    export XDG_DESKTOP_DIR="$HOME"
    export XDG_DOWNLOAD_DIR="$HOME/downloads"
    export XDG_DOCUMENTS_DIR="$HOME/documents"
    export XDG_MUSIC_DIR="$HOME/music"
    export XDG_PICTURES_DIR="$HOME/pictures"
    export XDG_VIDEOS_DIR="$HOME/videos"

    if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
	export MOZ_ENABLE_WAYLAND=1
	export GDK_SCALE=2
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
    append_path /usr/local/go/bin

    export BASH_COMPLETION_COMPAT_DIR="/opt/homebrew/etc/bash_completion.d"

    [[ -r "/opt/homebrew/etc/profile.d/bash_completion.sh" ]] && . "/opt/homebrew/etc/profile.d/bash_completion.sh"

    # Brew-installed paths, arm64 variant
    prepend_path /opt/homebrew/bin
    prepend_path /opt/homebrew/sbin

    #export JAVA_HOME=$(brew --prefix openjdk)

    ## Intel-version paths get added towards the end, so we favor arm64 variants when possible
    append_path /usr/local/bin
    append_path /usr/local/sbin

    # Add an explicit alias here, so that we can call the intel variant when needed
    alias ibrew='arch -x86_64 /usr/local/bin/brew'

    # Add installed-from-source Postgres binary and manpage paths
    append_path /usr/local/pgsql/bin
    export MANPATH=/usr/local/pgsql/share/man:$MANPATH

    alias stay-awake='caffeinate -di'

    alias alert='terminal-notifier -activate "com.googlecode.iterm2" -message "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

    source "$HOME/.cargo/env"

    export GOPATH=$HOME/dev/go:$HOME/dev/tg/sandcastle:$HOME/dev/tg/ops
    export GOBIN=$HOME/bin

    # Base16 Shell
    BASE16_SHELL="$HOME/.config/base16-shell/"
    [ -n "$PS1" ] && \
	[ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"

fi

# Set the theme for `bat`
export BAT_THEME="Nord"

# Cargo's bin path
append_path $HOME/.cargo/bin

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
alias gl="git log --graph --abbrev-commit --date=relative --pretty=format:'%C(bold blue)%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'"
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

unset append_path
unset prepend_path
export PATH

## Prompt
#PS1="[\u@\h \W]\$ " # Default prompt

PS1="[\u@\h \w]\$ "
#eval "$(starship init bash)"

## TODO: Add some macos settings
## defaults write NSGlobalDomain NSWindowResizeTime .001 # Speed up animations on window resizes
## defaults write com.apple.finder DisableAllAnimations -bool true # Disable Finder animations
# Enable subpixel font rendering on non-Apple LCDs
# Reference: https://github.com/kevinSuttle/macOS-Defaults/issues/17#issuecomment-266633501
## defaults write NSGlobalDomain AppleFontSmoothing -int 1

