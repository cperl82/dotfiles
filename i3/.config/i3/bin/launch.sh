#!/bin/bash

set -o pipefail
set -o errexit

main () {
    local cmd=""
    cmd=$(compgen -c | sort | fzf)

    if [[ -n "${cmd}" ]]
    then
	${cmd}
    fi
}

main "${@}"
