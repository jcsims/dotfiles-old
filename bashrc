## Basic bash config
export LSCOLORS="exfxcxdxbxegedabagacad"
export CLICOLOR=true
export EDITOR='emacsclient'
export TERM=xterm-256color

# use .localrc for SUPER SECRET CRAP that you don't
# want in your public, versioned repo.
if [[ -a ~/.localrc ]]
then
    source ~/.localrc
fi

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

## Path
export PATH=/usr/local/bin:/usr/local/sbin:$PATH
export PATH=$HOME/bin:$PATH

# Add any user-installed python binaries to the path
export PATH=$HOME/Library/Python/2.7/bin:$PATH

# Add GOROOT bin path to PATH
export PATH=/usr/local/opt/go/libexec/bin:$PATH

export GOPATH=$HOME/code/go:$HOME/code/sandcastle:$HOME/code/ops

# Setup paths for pkgsrc
export PATH=/opt/pkg/sbin:/opt/pkg/bin:$PATH
export MANPATH=/opt/pkg/man:$MANPATH

# Sensible options, borrowed from
# https://github.com/mrzool/bash-sensible
# Prevent file overwrite on stdout redirection
# Use `>|` to force redirection to an existing file
set -o noclobber

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

# Record each line as it gets issued
PROMPT_COMMAND='history -a'

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

# Prepend cd to directory names automatically
shopt -s autocd 2> /dev/null
# Correct spelling errors during tab-completion
shopt -s dirspell 2> /dev/null
# Correct spelling errors in arguments supplied to cd
shopt -s cdspell 2> /dev/null

# This defines where cd looks for targets
# Add the directories you want to have fast access to, separated by colon
# Ex: CDPATH=".:~:~/projects" will look for targets in the current working directory, in home and in the ~/projects folder
CDPATH=".:~/code"

#rbenv
# if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

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

alias reload!='. ~/.bashrc'
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

alias quiet!='osascript -e "set Volume 0.01"'

## Prompt
source ~/.functions/powerline-prompt
