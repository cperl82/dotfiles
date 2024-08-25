#!/bin/bash

set -o pipefail
set -o errexit

function main
{
    local title="${1}"
    shift

    local here=""

    here=$(dirname "${0}")
    xdotool search --name "${title}" windowmap windowactivate \
	|| "${here}/sensible-terminal.sh" --title "${title}" "${@}"
}

main "${@}"
