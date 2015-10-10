## Basic zsh config
export LSCOLORS="exfxcxdxbxegedabagacad"
export CLICOLOR=true
export EDITOR='emacsclient'
export TERM=xterm-256color

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


# your project folder that we can `c [tab]` to
export PROJECTS=~/code

# use .localrc for SUPER SECRET CRAP that you don't
# want in your public, versioned repo.
if [[ -a ~/.localrc ]]
then
    source ~/.localrc
fi

## Functions
fpath=($(brew --prefix)/share/zsh/site-functions ~/.functions $fpath)
autoload -U ~/.functions/*(:t)


## Completion
# Details taken from:
# https://scottlinux.com/2011/08/19/quick-intro-to-zsh-auto-complete/

# Use modern completion system
autoload -Uz compinit
compinit

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

# GRC colorizes nifty unix tools all over the place
GRC=`which grc`
if [ "$TERM" != dumb ] && [ -n "$GRC" ]
then
    alias colourify="$GRC -es --colour=auto"
    alias configure='colourify ./configure'
    alias diff='colourify diff'
    alias make='colourify make'
    alias gcc='colourify gcc'
    alias g++='colourify g++'
    alias as='colourify as'
    alias gas='colourify gas'
    alias ld='colourify ld'
    alias netstat='colourify netstat'
    alias ping='colourify ping'
    alias traceroute='colourify /usr/sbin/traceroute'
fi


## Antigen
source ~/.antigen/antigen.zsh

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search

antigen apply

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

#setup history substring search
# bind UP and DOWN arrow keys
zmodload zsh/terminfo
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# bind P and N for EMACS mode
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down


## Path
typeset -U PATH=/usr/local/bin:/usr/local/sbin:$PATH
# Add GHC and cabal-install installer to path
typeset -U PATH=/Applications/ghc-7.8.3.app/Contents/bin:$PATH
# Add cask to path
typeset -U PATH=$HOME/.cask/bin:$PATH
# Add any cabal-installed executables to the path
typeset -U PATH=$HOME/.cabal/bin:$PATH
typeset -U PATH=$HOME/bin:$PATH
typeset -U PATH=$HOME/Library/Haskell/bin:$PATH

#Heroku toolbelt
typeset -U PATH=/usr/local/heroku/bin:$PATH

#rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

## Aliases
# grc overides for ls
#   Made possible through contributions from generous benefactors like
#   `brew install coreutils`
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

alias reload!='. ~/.zshrc'
alias fact="elinks -dump randomfunfacts.com | sed -n '/^| /p' | tr -d \|"
alias tree='tree -C'
alias upgrade='sudo apt-get update && sudo apt-get upgrade'

alias e='emacsclient -t -a ""'
alias ec='emacsclient -c -a ""'

## Git aliases
alias gl="git log --graph --abbrev-commit --date=relative --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'"
alias gp='git push origin HEAD'
alias gpl='fact && git pull --rebase --prune'
alias gd='git diff'
alias gc='git commit'
alias gca='git commit -a'
alias gco='git checkout'
alias gb='git branch'
alias gs='git status -sb' # upgrade your git if -sb breaks for you. it's fun.
alias grm="git status | grep deleted | awk '{print \$3}' | xargs git rm"
alias ga='git add'
alias grs='git reset'

alias hclean="ghc-pkg check --simple-output | xargs -n 1 ghc-pkg unregister --force"

if [[  $OSTYPE == darwin* ]]
then
    alias mvim='mvim --remote-silent'
fi

## Prompt
autoload -U colors && colors
# cheers, @ehrenmurdick
# http://github.com/ehrenmurdick/config/blob/master/zsh/prompt.zsh


git=`which git`

git_branch() {
    echo $($git symbolic-ref HEAD 2>/dev/null | awk -F/ {'print $NF'})
}

git_dirty() {
    if $(! $git status -s &> /dev/null)
    then
        echo ""
    else
        if [[ $($git status --porcelain) == "" ]]
        then
            echo "on %{$fg[green]%}$(git_prompt_info)%{$reset_color%}"
        else
            echo "on %{$fg[red]%}$(git_prompt_info)%{$reset_color%}"
        fi
    fi
}

git_prompt_info () {
    ref=$($git symbolic-ref HEAD 2>/dev/null) || return
    echo "${ref#refs/heads/}"
}

unpushed () {
    $git cherry -v @{upstream} 2>/dev/null
}

need_push () {
    if [[ $(unpushed) == "" ]]
    then
        echo " "
    else
        echo " with %{$fg[magenta]%}unpushed%{$reset_color%} "
    fi
}

local ctime="%{$fg[magenta]%}%T%{$reset_color%}"
local mname="%{$fg[green]%}%m%{$reset_color%}"
local cdir="%{$fg[cyan]%}%~ %{$reset_color%}"
local lambda="%(?,%{$fg[green]%}λ%{$reset_color%},%{$fg[red]%}λ%{$reset_color%})"


PROMPT='$ctime on $mname in $cdir
${lambda}  '

RPROMPT='%{$fg[white]%} $(git_dirty)$(need_push) %{$reset_color%}'

HBASE_HOME="/${HOME}/code/hbase-0.94.6-cdh4.4.0"
export HBASE_HOME
PATH="${PATH}:${HOME}/bin"
export PATH

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export FRIENDLY_NAME="chris"
