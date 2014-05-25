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
setopt SHARE_HISTORY # share history between sessions ???
setopt EXTENDED_HISTORY # add timestamps to history
setopt PROMPT_SUBST
setopt CORRECT
setopt COMPLETE_IN_WORD
setopt IGNORE_EOF

setopt APPEND_HISTORY # adds history
setopt INC_APPEND_HISTORY SHARE_HISTORY  # adds history incrementally and share it across sessions
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

#setup history substring search
# bind UP and DOWN arrow keys
zmodload zsh/terminfo
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down

# bind P and N for EMACS mode
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

# bind k and j for VI mode
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# your project folder that we can `c [tab]` to
export PROJECTS=~/code

# use .localrc for SUPER SECRET CRAP that you don't
# want in your public, versioned repo.
if [[ -a ~/.localrc ]]
then
    source ~/.localrc
fi

## Functions
fpath=(~/.functions $fpath)
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
zstyle ':completion:*' menu select=2 eval "$(gdircolors -b)"
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

completion='$(brew --prefix)/share/zsh/site-functions/_git'

if test -f $completion
then
    source $completion
fi


## Colors
# Base16 Shell
BASE16_SCHEME="eighties"
BASE16_SHELL="$HOME/.config/base16-shell/base16-$BASE16_SCHEME.dark.sh"
[[ -s $BASE16_SHELL ]] && . $BASE16_SHELL

# GRC colorizes nifty unix tools all over the place
if (( $+commands[grc] )) && (( $+commands[brew] ))
then
  source `brew --prefix`/etc/grc.bashrc
fi


## Antigen
source ~/.antigen/antigen.zsh

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search

antigen apply

## Path
# Add cask to path
export PATH=/usr/local/bin:/usr/local/sbin:$PATH
export PATH=$HOME/.cask/bin:$PATH
# Add any cabal-installed executables to the path
export PATH=$HOME/.cabal/bin:$PATH
export PATH=$HOME/bin:$PATH 

#Heroku toolbelt
export PATH=/usr/local/heroku/bin:$PATH

# Brew-installed-npm binaries
export PATH=/usr/local/share/npm/bin:$PATH

# Haskell
export PATH=~/.cabal/bin:$PATH
export PATH=$HOME/Library/Haskell/bin:$PATH


## Rbenv
eval "$(rbenv init -)"

## Aliases
# grc overides for ls
#   Made possible through contributions from generous benefactors like
#   `brew install coreutils`
if $(gls &>/dev/null)
then
  alias ls="gls -F --color"
  alias l="gls -lAh --color"
  alias ll="gls -l --color"
  alias la='gls -A --color'
fi
alias reload!='. ~/.zshrc'
alias fact="elinks -dump randomfunfacts.com | sed -n '/^| /p' | tr -d \|"
alias tree='tree -C'
      
alias e='emacsclient -t -a ""'
alias ec='emacsclient -c -a ""'

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

case $OSTYPE in darwin*)
  alias mvim='mvim --remote-silent'
esac


## Prompt
autoload colors && colors
# cheers, @ehrenmurdick
# http://github.com/ehrenmurdick/config/blob/master/zsh/prompt.zsh

if (( $+commands[git] ))
then
    git="$commands[git]"
else
    git="/usr/bin/git"
fi


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
    # echo "(%{\e[0;33m%}${ref#refs/heads/}%{\e[0m%})"
    echo "${ref#refs/heads/}"
}

unpushed () {
    /usr/bin/git cherry -v @{upstream} 2>/dev/null
}

need_push () {
    if [[ $(unpushed) == "" ]]
    then
        echo " "
    else
        echo " with %{$fg[magenta]%}unpushed%{$reset_color%} "
    fi
}

# directory_name(){
#   echo "%{$fg_bold[cyan]%}%1/%\/%{$reset_color%}"
# }

# machine_name(){
#   echo "%{$fg_bold[green]%}$(echo $HOST | sed 's/\..*$//')%{$reset_color%}"
# }

local lambda="%(?,%{$fg[green]%}λ%{$reset_color%},%{$fg[red]%}λ%{$reset_color%})"

# Show the relative path on one line, then the smiley.
PROMPT='%{$fg[magenta]%}%T%{$reset_color%} on %{$fg[green]%}%m%{$reset_color%} in %{$fg[cyan]%}%~ %{$reset_color%}
${lambda}  %{$reset_color%}'

RPROMPT='%{$fg[white]%} $(git_dirty)$(need_push) %{$reset_color%}'

## Completion
# Uses git's autocompletion for inner commands. Assumes an install of git's
# bash `git-completion` script at $completion below (this is where Homebrew
# tosses it, at least).
completion='$(brew --prefix)/share/zsh/site-functions/_git'

if test -f $completion
then
  source $completion
fi

## Functions
fpath=(~/.functions $fpath)
autoload -U ~/.functions/*(:t)
