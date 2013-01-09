autoload colors && colors
# cheers, @ehrenmurdick
# http://github.com/ehrenmurdick/config/blob/master/zsh/prompt.zsh

git_branch() {
  echo $(/usr/bin/git symbolic-ref HEAD 2>/dev/null | awk -F/ {'print $NF'})
}

git_dirty() {
  st=$(/usr/bin/git status 2>/dev/null | tail -n 1)
  if [[ $st == "" ]]
  then
    echo ""
  else
    if [[ $st == "nothing to commit (working directory clean)" ]]
    then
      echo "on %{$fg_bold[green]%}$(git_prompt_info)%{$reset_color%}"
    else
      echo "on %{$fg_bold[red]%}$(git_prompt_info)%{$reset_color%}"
    fi
  fi
}

git_prompt_info () {
 ref=$(/usr/bin/git symbolic-ref HEAD 2>/dev/null) || return
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
    echo " with %{$fg_bold[magenta]%}unpushed%{$reset_color%} "
  fi
}

rb_prompt(){
  if $(which rbenv &> /dev/null)
  then
	  echo "%{$fg_bold[yellow]%}$(rbenv version | awk '{print $1}')%{$reset_color%}"
	else
	  echo ""
  fi
}

directory_name(){
  echo "%{$fg_bold[cyan]%}%1/%\/%{$reset_color%}"
}

machine_name(){
  echo "%{$fg_bold[green]%}$(echo $HOST | sed 's/\..*$//')%{$reset_color%}"
}

# Show count from TODOs, etc in the code when in a directory
# Pulled from https://github.com/pengwynn/dotfiles/blob/master/zsh/prompt.zsh
function notes_count() {
if [[ -z $1 ]]; then
  local NOTES_PATTERN="TODO|FIXME|HACK";
else
  local NOTES_PATTERN=$1;
fi
grep -ERn "\b($NOTES_PATTERN)\b" {app,config,lib,spec,test,src,} 2>/dev/null | wc -l | sed 's/ //g'
}

function notes_prompt() {
local COUNT=$(notes_count $1);
if [ $COUNT != 0 ]; then
  echo "$1: $COUNT";
else
  echo "";
fi
}

export PROMPT=$'\n$(rb_prompt) in $(machine_name):$(directory_name) $(git_dirty)$(need_push)\n› '
set_prompt () {
  export RPROMPT="$(notes_prompt TODO) %{$fg_bold[yellow]%}$(notes_prompt HACK)%{$reset_color%} %{$fg_bold[red]%}$(notes_prompt FIXME)%{$reset_color%}"
}

precmd() {
  title "zsh" "%m" "%55<...<%~"
  set_prompt
}
