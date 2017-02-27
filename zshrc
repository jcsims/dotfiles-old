#!/usr/bin/env zsh

## Basic zsh config
export LSCOLORS="exfxcxdxbxegedabagacad"
export CLICOLOR=true
export EDITOR='emacsclient'
export TERM=xterm-256color

# your project folder that we can `c [tab]` to
export PROJECTS=~/code

# use .localrc for SUPER SECRET CRAP that you don't
# want in your public, versioned repo.
if [[ -a ~/.localrc ]]
then
    source ~/.localrc
fi

## Path
typeset -U PATH=/usr/local/bin:/usr/local/sbin:$PATH
typeset -U PATH=$HOME/bin:$PATH

# Add any user-installed python binaries to the path
typeset -U PATH=$HOME/Library/Python/2.7/bin:$PATH

# Add GOROOT bin path to PATH
typeset -U PATH=/usr/local/opt/go/libexec/bin:$PATH

typeset -U GOPATH=$HOME/code/go

# Setup paths for pkgsrc
typeset -U PATH=/opt/pkg/sbin:/opt/pkg/bin:$PATH
typeset -U MANPATH=/opt/pkg/man:$MANPATH

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

setopt autocd
setopt NO_BG_NICE # don't nice background tasks
setopt NO_HUP
setopt NO_LIST_BEEP
setopt LOCAL_OPTIONS # allow functions to have local options
setopt LOCAL_TRAPS # allow functions to have local traps
setopt HIST_VERIFY
setopt EXTENDED_HISTORY # add timestamps to history
setopt PROMPT_SUBST
setopt CORRECT
setopt COMPLETE_IN_WORD
setopt IGNORE_EOF

setopt APPEND_HISTORY # adds history
# adds history incrementally and share it across sessions
setopt INC_APPEND_HISTORY SHARE_HISTORY
setopt HIST_IGNORE_ALL_DUPS  # don't record dupes in history
setopt HIST_REDUCE_BLANKS

# don't expand aliases _before_ completion has finished
#   like: git comm-[tab]
setopt complete_aliases

zle -N newtab

bindkey '^[^[[D' backward-word
bindkey '^[^[[C' forward-word
bindkey '^[[5D' beginning-of-line
bindkey '^[[5C' end-of-line
bindkey '^[[3~' delete-char
bindkey '^[^N' newtab
bindkey '^?' backward-delete-char

## Functions
fpath=(~/.functions $fpath)
if [[ -d /usr/local/share/zsh/site-functions ]]
then
    fpath=(/usr/local/share/zsh/site-functions $fpath)
fi

autoload -U ~/.functions/*(:t)

## Completion
# Details taken from:
# https://scottlinux.com/2011/08/19/quick-intro-to-zsh-auto-complete/

# Use modern completion system
autoload -Uz compinit && compinit
autoload -Uz bashcompinit && bashcompinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
if [[ $OSTYPE == darwin* ]]
then
    zstyle ':completion:*' menu select=2 eval "$(gdircolors -b)"
else
    zstyle ':completion:*' menu select=2 eval "$(dircolors -b)"
fi
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# pasting with tabs doesn't perform completion
zstyle ':completion:*' insert-tab pending

## Antigen
source ~/.antigen/antigen.zsh

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search
antigen bundle zsh-users/zsh-autosuggestions

antigen apply

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

# setup history substring search
# bind UP and DOWN arrow keys
zmodload zsh/terminfo
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# bind P and N for EMACS mode
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

#rbenv
# if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# Dynamically set JAVA_HOME
typeset -U JAVA_HOME=$(/usr/libexec/java_home)

## Aliases
# grc overides for ls
#   Made possible through contributions from generous benefactors like
#   `sudo pkgin install coreutils`
if $(gls &>/dev/null)
then
    lscom="gls"
else
    lscom="ls"
fi

alias ls="$lscom -F --color"
alias l="$lscom -lAh --color"
alias ll="$lscom -l --color"
alias la='$lscom -A --color'
alias grep='grep --color=auto'

# I think this is the only thing that needs reattach-to-user-namespace anymore..
alias terminal-notifier='reattach-to-user-namespace terminal-notifier'

alias reload!='. ~/.zshrc'
alias tree='tree -C'
alias upgrade='sudo apt-get update && sudo apt-get upgrade'

alias e='emacsclient -t -a ""'
alias ec='emacsclient -c -a ""'

## Git aliases
alias gl="git log --graph --abbrev-commit --date=relative --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'"
alias gp='git push origin HEAD'
alias gpl='git pull --rebase --prune'
alias gd='git diff'
alias gc='git commit'
alias gca='git commit -a'
alias gco='git checkout'
alias gb='git branch'
alias gs='git status -sb' # upgrade your git if -sb breaks for you. it's fun.
alias grm="git status | grep deleted | awk '{print \$3}' | xargs git rm"
alias ga='git add'
alias grs='git reset'

## Set up git-flow-completion
## Clone from https://github.com/petervanderdoes/git-flow-completion.git
source ~/.git-flow-completion/git-flow-completion.zsh

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

#alias hclean="ghc-pkg check --simple-output | xargs -n 1 ghc-pkg
#unregister --force"

alias quiet!='osascript -e "set Volume 0.01"'

## Prompt
autoload -U colors && colors
# cheers, @ehrenmurdick
# http://github.com/ehrenmurdick/config/blob/master/zsh/prompt.zsh

git_branch() {
    echo $(git symbolic-ref HEAD 2>/dev/null | awk -F/ {'print $NF'})
}

git_dirty() {
    if $(! git status -s &> /dev/null)
    then
        echo ""
    else
        if [[ $(git status --porcelain) == "" ]]
        then
            echo "%{$fg[green]%}$(git_prompt_info)%{$reset_color%}"
        else
            echo "%{$fg[red]%}$(git_prompt_info)%{$reset_color%}"
        fi
    fi
}

git_prompt_info () {
    ref=$(git symbolic-ref HEAD 2>/dev/null) || return
    echo "${ref#refs/heads/}"
}

unpushed () {
    git cherry -v @{upstream} 2>/dev/null
}

need_push () {
    if [[ $(unpushed) == "" ]]
    then
        echo " "
    else
        echo " with %{$fg[magenta]%}unpushed%{$reset_color%} "
    fi
}

local cdir="%{$fg[cyan]%}%~ %{$reset_color%}"
local lambda="%(?,%{$fg[green]%}λ%{$reset_color%},%{$fg[red]%}λ%{$reset_color%}) "

PROMPT='$cdir%{$fg[black]%}$(git_dirty)$(need_push)%{$reset_color%}${lambda}'
