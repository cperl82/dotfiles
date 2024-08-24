#!/bin/bash

set -o pipefail
set -o errexit

function main
{
    local title="${1}"
    shift

    xdotool search --name "${title}" windowmap windowactivate \
	|| alacritty --title "${title}" "${@}"
}

main "${@}"
