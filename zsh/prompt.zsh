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
