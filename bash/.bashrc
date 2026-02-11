# -*- mode: Shell-script; -*-

export PS1='[\u@\h \w]\$ '
export PAGER=less
export HISTIGNORE=' *'

function _path-append-prepend-uniq {
    local append="${1}"
    local dir="${2}"
    local components=()
    local path=""

    if [[ "${#}" -ne 2 ]]
    then
	echo 1>&2 "Usage: ${FUNCNAME[1]} path"
	return 1
    fi

    if [[ -d "${dir}" ]]
    then
	dir=${dir//\/\//\/}
	dir=${dir%/.}

	while IFS=$'\n' read -r line
	do
	    components+=("${line}")
	done < <(tr ':' '\n' <<< "${PATH}" | uniq)

	for p in "${components[@]}"
	do
	    if [[ "${p}" == "${dir}" ]]
	    then
		# if the thing we've been asked to appened or prepend
		# to path already exists in PATH, remove it
		continue
	    fi

	    if [[ -z "${path}" ]]
	    then
		path="${p}"
	    else
		path="${path}:${p}"
	    fi
	done

	if (( append ))
	then
	    export PATH="${path}:${dir}"
	else
	    export PATH="${dir}:${path}"
	fi
    fi
}

function path-prepend {
    _path-append-prepend-uniq 0 "${@}"
}

function path-append {
    _path-append-prepend-uniq 1 "${@}"
}

function tmpmkcd {
    local p

    p="${HOME}/tmp/$(date '+%Y-%m-%d')"
    if [[ ! -d "${p}" ]]
    then
	mkdir -p "${p}"
    fi
    cd "${p}" || return
}

# xt - xterm title setter
function xt {
    local name=""

    if [[ -z "${1}" ]]
    then
	name=${HOSTNAME}
    else
	name=${1}
    fi
    printf '\033]0;%s\007' "${name}"
}

# st - screen window title setter
function st {
    local name=""

    if [[ -z "${1}" ]]
    then
	name=${HOSTNAME}
    else
	name=${1}
    fi
    printf '\033k%s\033\134' "${name}"
}

# Bash path canonicalization
# Copied from comment at
# http://blog.publicobject.com/2006/06/canonical-path-of-file-in-bash.html
# and adapted to further fit my needs and be more cross platform compatible

# Resolves symlinks for path components, but will not resolve if the last
# component, file or directory is a symlink

# path-canonical-simple
function path-canonical-simple {
    local dst="${1}"
    local d
    local b

    if [[ -z "${dst}" ]]
    then
	dst="${PWD}"
    fi

    if [[ -d "${dst}" ]]
    then
	cd -- "${dst}" > /dev/null 2>&1 && pwd -P
    else
	d=$(dirname -- "${dst}")
	b=$(basename -- "${dst}")
	cd -- "${d}" >/dev/null 2>&1 && echo "$(pwd -P)/${b}"
    fi
    cd -- - >/dev/null 2>&1 || return
}

# path-canonical
# Resolves symlinks for all path components, including the final component
function path-canonical {
    local dst="${1}"
    if [[ -z "${dst}" ]]; then
	dst="${PWD}"
    fi

    dst=$(path-canonical-simple "${1}")

    # Strip a potential trailing slash as it can cause `test -h' to fail
    while [[ -h "${dst%/}" ]]; do
	local link_dst=$(command ls -l "${dst}" | sed -e 's/^.*[ \t]*->[ \t]*\(.*\)[ \t]*$/\1/g')
	if   [[ "${link_dst}" =~ ^..$ ]]; then
	    # special case
	    dst=$(dirname $(dirname "${dst}"))
	elif [[ "${link_dst}" =~ ^.$  ]]; then
	    # special case
	    :
	elif [[ "${link_dst}" =~ ^/   ]]; then
	    # absolute symlink
	    dst="${link_dst}"
	else
	    # relative symlink
	    dst=$(dirname "${dst}")/"${link_dst}"
	fi
    done
    # This call IS necessary as the traversal of symlinks above in the while
    # loop may have introduced additional symlinks into the path where the
    # symlink is NOT the last path component.
    dst=$(path-canonical-simple "${dst}")
    # Remove duplicate // and a trailing /.
    dst=${dst//\/\//\/}
    dst=${dst%/.}
    echo "${dst}"
}

# AES Encryption via command line
# This is meant to be simplified interface to the aes-256-cbc encryption
# available in openssl.  Note that output is always "ascii armored" as that just
# makes life easier.
function aes-256-cbc { 
	verb="${1}"
	shift
	case "${verb}" in
	e|enc|encrypt)
		encrypt_decrypt=""
		;;
	d|dec|decrypt)
		encrypt_decrypt="-d"
		;;
	*)
		echo "${FUNCNAME} encrypt|decrypt string|file str|filename"
		return
		;;
	esac	

	noun="${1}"
	shift
	case "${noun}" in 
	s|str|string)
		if [[ -z "${1}" ]]
		then
			# No data on command line, prompt for it
			pre_cmd="read -e -p 'input string: ' data"
		else
			# Just use the data from the command line
			pre_cmd="data=${1}"
		fi
		openssl_cmd="openssl aes-256-cbc \${encrypt_decrypt} -a -in /dev/stdin -out /dev/stdout <<< \${data}"
		post_cmd=""
		;;
	f|fil|file)
		file="${1}"
		infile="${file}"
		outfile="${file}.aes.$$"
		pre_cmd=""
		openssl_cmd="openssl aes-256-cbc ${encrypt_decrypt} -a -in ${infile} -out ${outfile}"
		post_cmd="mv \${outfile} \${file}"
		;;
	*)
		echo "${FUNCNAME} encrypt|decrypt string|file str|filename"
		return
		;;
	esac

	#echo ${openssl_cmd}
	eval ${pre_cmd} && eval ${openssl_cmd} && eval ${post_cmd}

}

# function wrapper for screen to try to cover some common cases of
# window title setting
function scr {
    local parnt=""
    local child=""
    local i=0
    local components=()

    while IFS=$'\n' read -r line
    do
	components+=("${line}")
    done < <(path-canonical "${PWD}" | sed -e 's#^/##' -e 's#/$##' | tr '/' '\n')

    for ((i=0; i<$(( ${#components[@]} - 1)); i++))
    do
	parnt="${components[$((i  ))]}"
	child="${components[$((i+1))]}"
	if [[ "${parnt}" =~ rpmbuild|git|src ]]
	then
	    break
	fi
	parnt=""
	child=""
    done

    if [[ -n "${parnt}" && -n "${child}" ]]
    then
	xt "${parnt} ${child}"
	screen -S "${parnt}-${child}" emacs -nw
    else
	screen emacs -nw
    fi
}

function setup-misc {
    local f

    path-prepend "${HOME}/bin"
    path-prepend "${HOME}/.local/bin"

    # Interactive python session setup
    f="${HOME}/.python_startup.py" 
    if [[ -f "${f}" ]];
    then
	export PYTHONSTARTUP="${f}"
    fi

    if command -v cargo >/dev/null 2>&1
    then
	path-prepend "${HOME}/.cargo/bin"
    fi

    if command -v fzf >/dev/null 2>&1
    then
	path-prepend "${HOME}/.fzf/bin"
    fi

    if command -v rg >/dev/null 2>&1
    then
	RIPGREP_CONFIG_PATH="${HOME}/.ripgreprc"
	export RIPGREP_CONFIG_PATH
    fi

    if command -v opam >/dev/null 2>&1
    then
	eval "$(opam config env)"
    fi
}

function setup-os-specific {
    local n
    local f 

    n=$(uname -s | tr '[:upper:]' '[:lower:]')
    f="${BASHFILES}/.bashrc.${n}"
    if [[ -f "${f}" ]]
    then
	# shellcheck disable=SC1090
	source "${f}"
    fi
}

function setup-local {
    local f
    # Local overrides
    f="${HOME}/.bashrc.local"
    if [[ -f "${f}" ]]
    then
	source "${f}"
    fi
}

function main
{
    setup-misc
    setup-os-specific
    setup-local
}

main "${@}"
