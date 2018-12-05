#!/bin/bash

set -o errexit
set -o pipefail
set -o nounset

function main ()
{
    local local_conf="stow.local"
    local stow_dir=""

    stow_dir=$(dirname ${0})
    cd "${stow_dir}" && \
	{
	    if [[ -f "${local_conf}" ]]
	    then
		# Stow just what we've been asked to stow
		cat "${local_conf}" \
		    | xargs -t stow -R
	    else
		# Stow everything
		find . -mindepth 1 -maxdepth 1 -type d \
		     | xargs -t stow -R
	    fi
	}
}

main "${@}"
