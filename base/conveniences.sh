cdl(){ cd $1; ls; }
cdll(){ cd $1; ls -l; }
cdlla(){ cd $1; ls -la; }
alias cdt1='cd "$(ls -t | head -n 1)"'
alias lsl="ls -l"
alias lsa="ls -a"
alias lsla="ls -la"
alias lslh="ls -lh"
alias lslrt="ls -lrt"
alias wcl="wc -l"
alias rgrep="grep -r"
alias gerp=grep
alias today="date +%Y-%m-%d"
alias jqu="jq --unbuffered"
alias jqr="jq -r"
alias k91="kill -9 %1"
alias p3="python3"
alias emc="emacsclient"
alias pd="pushd"
alias dp="popd"

_find_root_of_git_dir(){
    git rev-parse --show-toplevel 2>/dev/null
}
gcd(){
    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "Not in a git repo"\!
        return 1
    fi

    local cdup
    cdup=$(git rev-parse --show-cdup)

    cd "${cdup:-.}" || return
    if [[ -n "$1" ]]; then
        cd "$1" || return
    fi
}

_gcd_completion() {
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"
    local repo_root
    repo_root=$(_find_root_of_git_dir)
    if [ -n "$repo_root" ]; then
        local IFS=$'\n'
        COMPREPLY=($(cd "$repo_root" && compgen -S "/" -d -- "$cur"))
    fi
}

complete -o nospace -F _gcd_completion gcd


# git shortcuts
alias gs="git status"
alias gl="git lg"
alias gc="git commit"
alias ga="git add"
alias gco="git checkout"
alias gd="git diff"
alias gr="git reset"
alias grh="git reset HEAD"
alias gpp="git pull && git push"
alias gpddraagp="git pull --rebase && git push"


# Navigation
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias .......='cd ../../../../../..'
alias ........='cd ../../../../../../..'
alias .........='cd ../../../../../../../..'
alias cd-="cd -"


alias head1='head -n $(( $(tput lines) - 3 ))'
alias tail1='tail -n $(( $(tput lines) - 3 ))'
alias head2='head -n $(( $(tput lines) / 2 ))'
alias tail2='tail -n $(( $(tput lines) / 2 ))'
alias head3='head -n $(( $(tput lines) / 3 ))'
alias tail3='tail -n $(( $(tput lines) / 3 ))'

mkcd(){ mkdir -p "$1"; cd "$1"; }
tmp(){
    if [[ -z "$1" ]]; then
        local D="$HOME/tmp/$(date -u +"%Y-%m-%dT%H%M%S")"
    else
        local D="$HOME/tmp/$(date -u +"%Y-%m-%d")-$1"
        local INCR=1
        while [[ -e "$D" ]]; do
            D="$HOME/tmp/$(date -u +"%Y-%m-%d")-$1-$INCR"
            INCR=$((INCR+1))
        done
    fi
    mkdir "$D" && pushd "$D"
}
tmpshm(){ pushd `mktemp -d --tmpdir=/dev/shm`; }


-cd(){
  ( cd "$1"; bash; )
}

cdgwt() {
    local current_root
    current_root=$(git rev-parse --show-toplevel 2>/dev/null) || { echo "Not in a git repo"; return 1; }

    local rel_path
    rel_path=$(git rev-parse --show-prefix)
    rel_path="${rel_path%/}"

    local worktree_list="" is_first=1
    local path="" head_sha="" branch="" is_detached=0

    _cdgwt_emit() {
        [[ -z "$path" ]] && return
        [[ ! -d "$path" ]] && { path="" head_sha="" branch="" is_detached=0; return; }
        local wt_name branch_display
        if [[ $is_first == 1 ]]; then
            wt_name="(root)"
            is_first=0
        else
            wt_name=$(basename "$path")
        fi
        if [[ $is_detached == 1 ]]; then
            branch_display=$(git -C "$path" log -1 --format="%h %s" 2>/dev/null)
        else
            branch_display="$branch"
        fi
        worktree_list+="${wt_name}: ${branch_display}"$'\t'"${path}"$'\n'
        path="" head_sha="" branch="" is_detached=0
    }

    while IFS= read -r line; do
        case "$line" in
            "worktree "*) path="${line#worktree }" ;;
            "HEAD "*)     head_sha="${line#HEAD }" ;;
            "branch "*)   branch="${line#branch refs/heads/}" ;;
            "detached")   is_detached=1 ;;
            "")           _cdgwt_emit ;;
        esac
    done < <(git worktree list --porcelain)
    _cdgwt_emit
    unset -f _cdgwt_emit

    local selection
    selection=$(printf '%s' "$worktree_list" | fzf --delimiter=$'\t' --with-nth=1) || return 0
    [[ -z "$selection" ]] && return 0

    local target_root
    target_root=$(printf '%s' "$selection" | cut -f2)

    local target_dir="$target_root${rel_path:+/$rel_path}"
    while [[ "$target_dir" != "$target_root" ]] && [[ ! -d "$target_dir" ]]; do
        target_dir="${target_dir%/*}"
    done
    cd "$target_dir"
}
