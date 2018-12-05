#!/bin/bash

set -o errexit
set -o pipefail
set -o nounset

function main ()
{
    local tmp=""
    local stow_dir=""
    local stow_basename=""

    stow_dir=$(dirname $(realpath -m ${0}))
    cd "${stow_dir}/.." && \
	{
	    tmp=$(mktemp -t)
	    stow_basename=$(basename "${stow_dir}")
	    echo "${stow_basename}" > "${tmp}"
	    # enumerate all links that point into us
	    find . -maxdepth 4 -lname "*${stow_basename}*" >> "${tmp}"
	    rsync -av				\
		  --delete			\
		  --recursive			\
		  --itemize-changes		\
		  --files-from="${tmp}"		\
		  --exclude="./"		\
		  --dry-run			\
		  "${PWD}/"			\
		  "${*}"
	}
}

main "${@}"
