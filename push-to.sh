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
	    stow_basename=$(basename "${stow_dir}")

	    tmp=$(mktemp -t)
	    printf "${stow_basename}\000" > "${tmp}"
	    # enumerate all links that point into us
	    find .					\
		 -maxdepth 3				\
		 -name "${stow_basename}" -prune -o	\
		 -lname "*${stow_basename}*"		\
		 -print0 >> "${tmp}"
	    for dst in "${@}"
	    do
		if [[ "${dst}" =~ : ]]
		then
		    h=$(awk -F: '{print $1}' <<< "${dst}")
		    p=$(awk -F: '{print $2}' <<< "${dst}")
		    prefix="ssh ${h} -- "
		    dst="${p}"
		else
		    prefix=""
		fi

		eval				\
		    ${prefix}			\
		    cd "${dst}" &&		\
		    {
			find .					\
			     -maxdepth 3			\
			     -name "${stow_basename}" -prune -o \
			     -lname "*${stow_basename}*"	\
			     -print0
		    } >> "${tmp}"
		rsync -av			\
		      --delete			\
		      --recursive		\
		      --itemize-changes		\
		      --files-from="${tmp}"	\
		      --from0			\
		      --exclude="./"		\
		      "${PWD}/"			\
		      "${dst}"
	    done
	}
}

main "${@}"
