#!/usr/bin/env bash

# Some boilerplate for making more reliable bash scripts

function emit-stack-trace {
    local -i x
    local -i stack_frames="${#FUNCNAME[@]}"

    {
        echo "ERROR in ${FUNCNAME[1]}() at ${BASH_SOURCE[1]}:${BASH_LINENO[0]}"

        for (( x=2; x < stack_frames; x++ ))
        do
            echo "    called by ${FUNCNAME[x]}() at ${BASH_SOURCE[x]}:${BASH_LINENO[x - 1]}"
        done
    } >&2
}

function be-strict {
    # credit to Elliot Shank
    set -o errexit    # Die on non-0 exit codes other than inside conditions.
    set -o noclobber  # Do not overwrite files without being explicit about it.
    set -o nounset    # Do not allow using unset variables.
    set -o pipefail   # Cause errors in the middle of pipelines to be caught.
    set -o errtrace   # ERR trap works in functions, subshells, etc.

    trap emit-stack-trace ERR
}

FINALLIES=()

function finally {
    for CODE in "${FINALLIES[@]}"; do
        eval "$CODE" || echo >&2 "Error running $CODE"
    done
}

be-strict
trap finally EXIT
