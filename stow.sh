#!/bin/bash

set -o errexit
set -o pipefail
set -o nounset

function main ()
{
    local local_conf="stow.local"
    local stow_dir=""
    stow_dir=$(dirname "${0}")

    cd "${stow_dir}" && \
	{
	    if [[ -f "${local_conf}" ]]
	    then
		# Stow just what we've been asked to stow
		xargs -r -n1 -t stow -Rvv < "${local_conf}"
	    else
		# Stow everything
		find . -mindepth 1 -maxdepth 1 \! -name ".*" -type d	\
		     | sed -e 's#^./##'					\
		     | xargs -r -n1 -t stow -Rvv
	    fi
	}
}

main "${@}"
