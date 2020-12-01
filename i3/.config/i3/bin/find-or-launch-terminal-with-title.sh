#!/bin/bash

set -o pipefail
set -o errexit

function main
{
    local title="${1}"
    shift

    xdotool search --name "${title}" windowactivate	\
	|| urxvt256c -title "${title}" "${@}"
}

main "${@}"
