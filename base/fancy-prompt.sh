# Showing the runtime of the last command; adapted from
# http://jakemccrary.com/blog/2015/05/03/put-the-last-commands-run-time-in-your-bash-prompt/

IN_PROMPT=1

function timer_start {
    if [ $IN_PROMPT -eq "1" ]; then
        timer=${timer:-$SECONDS}
    fi
}

function timer_stop {
    # NOCOMMIT: does this (:-0) stop our prompt from getting into the weird
    # syntax error thing?
    the_seconds=$(($SECONDS - ${timer:-0}))

    __last_stopped_at=$SECONDS

    # Hide results for <2sec to reduce noise
    if [[ $the_seconds > 1 ]]; then
        timer_show="`format-duration seconds $the_seconds`"
    else
        timer_show=""
    fi

    unset timer
}

delete_ansi_codes() {
    echo "$1" | sed -r "s/\x1B\[([0-9]{1,3}(;[0-9]{1,2})?)?[mGK]//g"
}

_escape_prompt_string(){
    # Backticks have special meaning in PS1/PS2/PS3 apparently, and
    # this (prepending a backslash) should suffice to address that
    echo "${1//\`/\\\`}"
}

# Prompt
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}

if [ "$NO_FANCY_PROMPT" == "1" ]; then
  PS1="$ "
  PROMPT_COMMAND=
else
  trap 'timer_start' DEBUG
  PROMPT_COMMAND=__prompt_command
fi

__prompt_command() {
    local EXIT="$?"
    IN_PROMPT=0
    timer_stop
    if [[ -n "$EXTRA_PROMPT_COMMAND" ]]; then
        eval "$EXTRA_PROMPT_COMMAND"
    fi
    PS1=""

    local RCol='\[\e[0m\]'

    local Yellow='\[\e[0;33m\]'
    local Orange='\[\e[01;31m\]'
    local Red='\[\e[0;31m\]'
    local Gre='\[\e[0;32m\]'
    local Grey='\[\e[01;32m\]'
    local BYel='\[\e[1;33m\]'
    local BBlu='\[\e[1;34m\]'
    local LightGrey='\[\e[01;34m\]'
    local Pur='\[\e[0;35m\]'
    local Theme=${FANCY_PROMPT_THEME:-$Yellow}
    local CIRCLE="$Theme●$RCol"
    local INPUT_COLOR=$'\[\e[0;36m\]'

    declare -A PROMPT_COMPONENTS

    if [[ -n "${PROMPT_LABEL:-}" ]]; then
        PROMPT_COMPONENTS["0-0label"]=$'\u001b[34;1m'"$PROMPT_LABEL"$'\u001b[0m'
    fi

    GIT_BRANCH=$(parse_git_branch)
    if [[ "$GIT_BRANCH" ]]; then
        local V
        if [[ "$GIT_BRANCH" == "master" ]]; then
            V=$'\u001b[34;1mgit\u001b[0m'
        else
            V=$'\u001b[34;1mgit:\u001b[0m'"$GIT_BRANCH"
        fi
        PROMPT_COMPONENTS["3-git-branch"]="$V"
    fi

    local JOBS="$(jobs -p -r | wc -l)"
    if [ $JOBS -gt "0" ]; then
        if [ $JOBS -eq "1" ]; then
            PROMPT_COMPONENTS["2-jobs"]="1 job"
        else
            PROMPT_COMPONENTS["2-jobs"]="$JOBS jobs"
        fi
    fi

    if [ $EXIT -gt "0" ]; then
        PROMPT_COMPONENTS["4-exit"]="${Red}$EXIT${RCol}"
    fi

    THE_TIMER=${timer_show}
    if [[ "$THE_TIMER" ]]; then
        PROMPT_COMPONENTS["5-timer"]="$THE_TIMER"
    fi

    local H;
    if [ ! -z "$BASH_PROMPT_HOSTNAME" ]; then
      H="$BASH_PROMPT_HOSTNAME"
    else
      H="$HOSTNAME"
    fi
    PROMPT_COMPONENTS["0-hostname"]="$H"
    THEWD="$PWD"
    [[ "$THEWD" =~ ^"$HOME"(/|$) ]] && THEWD="~${THEWD#$HOME}"
    local v
    for k in "${!PROMPT_WD_SUBSTITUTIONS[@]}"; do
        v="${PROMPT_WD_SUBSTITUTIONS["$k"]}"
        THEWD="${THEWD/$k/$v}"
    done
    DIRSLEN=${#DIRSTACK[@]}
    if [[ $DIRSLEN -gt 1 ]]; then
        DIRS_PREFIX=$'\u001b[1;32m'"[$DIRSLEN] "$'\u001b[0m'
    else
        DIRS_PREFIX=""
    fi
    PROMPT_COMPONENTS["1-pwd"]="$DIRS_PREFIX$THEWD"

    for k in "${!EXTRA_PROMPT_COMPONENTS[@]}"; do
        v="${EXTRA_PROMPT_COMPONENTS[$k]}"
        PROMPT_COMPONENTS["$k"]="$v"
    done

    # Python venv
    if [[ -n "$VIRTUAL_ENV" ]]; then
        PATH_TO_ENV="$(realpath --relative-to="$(pwd)" "$VIRTUAL_ENV")"
        PROMPT_COMPONENTS["7-venv"]=$'\u001b[34;1mvenv:\u001b[0m'"$PATH_TO_ENV"
    fi

    local EXTRA_PROMPT_COMPONENTS_DIR="$HOME/tmp/.prompt-components"
    if [[ -d $EXTRA_PROMPT_COMPONENTS_DIR ]]; then
        {
            local X=0
            while read line; do
                X=$((X+=1))
                if [[ $X -gt 5 ]]; then
                  PROMPT_COMPONENTS["8-err-too-many"]="${Red}Too many lines in ~/tmp/.prompt-components${RCol}"
                  break
                fi
                PROMPT_COMPONENTS["9-extra-$line-$RANDOM$RANDOM"]="$line"
            done < <(find "$EXTRA_PROMPT_COMPONENTS_DIR" -type f -print0 | xargs -0 cat)
        } &> /dev/null
    fi

    unset EXTRA_PROMPT_COMPONENTS
    declare -A EXTRA_PROMPT_COMPONENTS

    local MAIN_LINE="$Theme◀$RCol"
    local KEYS=()
    while read line; do
        KEYS+=("$line")
    done <<< "$(for k in "${!PROMPT_COMPONENTS[@]}"; do echo "$k"; done | sort)"

    local PROMPT_LINES=()



    while [[ ${#KEYS[@]} -gt 0 ]]; do
        local TOTAL=0
        local LINE_LEN=3
        local MAIN_LINE="$Theme◀$RCol"
        while [[ ${#KEYS[@]} -gt 0 ]]; do
            OLD_MAIN_LINE="$MAIN_LINE"
            if [[ $TOTAL -gt 0 ]]; then
                MAIN_LINE="$MAIN_LINE $CIRCLE"
            fi
            TOTAL=$((TOTAL+1))
            K="${KEYS[0]}"
            COMP="${PROMPT_COMPONENTS["$K"]}"
            # counting the line length manually because the color codes
            # make the length of the string and the actual display length
            # different; the color codes seem escaped at this point, which
            # is surprising
            COMP_NO_COLORS="$(delete_ansi_codes "$COMP")"
            LINE_LEN=$(( $LINE_LEN + 3 + ${#COMP_NO_COLORS} ))
            MAIN_LINE="$MAIN_LINE $COMP"
            if [[ "$LINE_LEN" -gt $COLUMNS ]] && [[ $TOTAL -gt 1 ]]; then
                # too long, roll back
                PROMPT_LINES+=("$OLD_MAIN_LINE $Theme▶ $RCol")
                break
            else
                KEYS=("${KEYS[@]:1}")
            fi
        done
    done
    PROMPT_LINES+=("$MAIN_LINE $Theme▶ $RCol")


    # I believe bash is designed to let you have a separate
    # font for the input text, but emacs' term-mode has
    # some disturbing bug when doing this, so I don't by
    # default :/
    if [[ -n "${COLORIZED_BASH_INPUT:-}" ]]; then
      PROMPT_LINES+=("${Theme}\$${RCol} $INPUT_COLOR")
    else
      PROMPT_LINES+=("${Theme}\$${RCol} ")
    fi

    local WALCOM=""
    if [[ "$HAS_DONE_WALCOM" == "0" ]]; then
      if [[ $SHLVL == 1 ]]; then
        HAS_DONE_WALCOM=1
        WALCOM="\n${Theme}▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰\n▰                      ▰\n▰  WALCOM!!1 TO BASH!  ▰\n▰                      ▰\n▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰\n$RCol"
      fi
    fi

    local PREFIX=""
    if [[ "$SHLVL" -gt 1 ]]; then
      local X="$SHLVL"
      PREFIX="${Theme}"
      while [[ "$X" -gt 1 ]]; do
        PREFIX+="┃"
        X=$((X - 1))
      done
      PREFIX+="$RCol "
    fi

    WITH_NEWLINES="$(printf $'\n'"$PREFIX"'%s' "${PROMPT_LINES[@]}")"
    PS1+="$WALCOM$WITH_NEWLINES"
    if [[ -n "${COLORIZED_BASH_INPUT:-}" ]]; then
      PS0="$RCol"
      PS2="${Theme}> $INPUT_COLOR"
    fi

    PS1="$(_escape_prompt_string "$PS1")"
    PS2="$(_escape_prompt_string "$PS2")"
    PS3="$(_escape_prompt_string "$PS3")"

    # So we can tell how long since the last command finished
    SECONDS=0

    IN_PROMPT=1
}
