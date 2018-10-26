# -*- mode: Shell-script; -*-

export PS1='[\u@\h \w]\$ '
export PAGER=less
export MYSQL_PS1="\u@\h [\d]> "
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
	components=( $(echo "${PATH}" | tr ':' '\n' | uniq) )
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
    else
	# We expect to be called as a helper, not directly
	echo 1>&2 "${FUNCNAME[1]} called with non-directory"
	return 1
    fi
}

function path-prepend {
    _path-append-prepend-uniq 0 "${@}"
}

function path-append {
    _path-append-prepend-uniq 1 "${@}"
}

function tmpmkcd {
    today=$(date '+%Y-%m-%d')
    pathname="${HOME}/tmp/${today}"
    if [[ ! -d "${pathname}" ]]
    then
	mkdir -p ${pathname} && cd ${pathname}
    else
	cd ${pathname}
    fi
}

# xt - xterm title setter
function xt {
    if [[ -z "${1}" ]]
    then
	NAME=${HOSTNAME}
    else
	NAME=${1}
    fi
    printf "\033]0;${NAME}\007"
}

# st - screen window title setter
function st {
    if [[ -z "${1}" ]]
    then
	NAME=${HOSTNAME}
    else
	NAME=${1}
    fi
    printf "\033k${NAME}\033\\"
}

# cl - reset all attributes
function cl {
    printf '\033[;0m'
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
    local target=""

    if [[ -z "${dst}" ]]
    then
	dst="${PWD}"
    fi

    if [[ -d "${dst}" ]]
    then
	cd -- "${dst}" > /dev/null 2>&1 && pwd -P
    else
	cd -- $(dirname -- "${dst}") > /dev/null 2>&1 &&	\
	echo $(pwd -P)/$(basename "${dst}")
    fi
    cd -- - >/dev/null 2>&1
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
	    dst="${dst}"
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

# NFSv3 Capture Filter
# Generate tcpdump capture filter for host running nfsv3
function nfs3-capture-filter-for-host {
	local host="${1}"
	if [[ -z "${host}" ]]
	then
		echo "nfs3-capture-filter-for-host hostname"
		return
	fi
	OFS="${IFS}"
	IFS=$'\n'
	ports=(
		$(rpcinfo -p "${host}" | \
		  awk '
			$4 ~ /[0-9]+/ {
				a[$4] = 1;
			}
			END {
				for (n in a)
					printf("%d\n", n)
			}
		  ' | \
		  sed -e 's/^/port /')
	)
	IFS="|"
	printf "host %s and (%s)\n" "${host}" "${ports[*]}" | sed -e 's/|/ or /g'
	IFS="${OFS}"
}

function nfs3-capture-filter-for-hosts {
    hosts="${@}"
    result=""
    for host in ${hosts}
    do
	s=$(nfs3-capture-filter-for-host "${host}")
	if [[ -z "${result}" ]]
	then
	    result="(${s})"
	else
	    result="${result} or (${s})"
	fi
    done
    printf "${result}\n"
}

function maybe-add-fzf-to-path {
    local fzf="${HOME}/.fzf/bin/fzf"

    if [[ -e "${fzf}" ]]
    then
	path-append $(dirname "${fzf}")
    fi
}

function with-fzf {
    local fzf=""
    local input_cmd=()
    local action_cmd=()
    local fzf_options=()
    local action_single=0
    local parsing_input=0
    local parsing_action=0
    local parsing_fzf_options=0
    local substitution=0

    function usage {
	echo "Usage: ${FUNCNAME[0]} --input INPUTGENCMD --action ACTION [--fzf-options OPTIONS] [--single]" 1>&2 
    }

    fzf=$(which fzf 2>/dev/null)
    if [[ -z "${fzf}" ]]
    then
	echo "Unable to find fzf, please make sure it is installed" 1>&2 
	return 1
    fi

    while [[ "${#}" -gt 0 ]]
    do
	case "${1}" in
	    -input|--input)
		input_cmd+=("${2}")
		shift
		shift
		parsing_input=1
		parsing_action=0
		parsing_fzf_options=0
		;;

	    -action|--action)
		action_cmd+=("${2}")
		shift
		shift
		parsing_input=0
		parsing_action=1
		parsing_fzf_options=0
		;;

	    -fzf-options|--fzf-options)
		fzf_options+=("${2}")
		shift
		shift
		parsing_input=0
		parsing_action=0
		parsing_fzf_options=1
		;;

	    -single|--single)
		action_single=1
		shift
		parsing_input=0
		parsing_action=0
		parsing_fzf_options=0
		;;

	    *)
		if (( "${parsing_input}" ))
		then
		    input_cmd+=("${1}")
		    shift
		elif (( "${parsing_action}" ))
		then
		    action_cmd+=("${1}")
		    shift
		elif (( "${parsing_fzf_options}" ))
		then
		    fzf_options+=("${1}")
		    shift
		else
		    usage
		    return 1
		fi

		;;
	esac
    done

    if [[ "${#input_cmd[@]}" -eq 0 ]] || [[ "${#action_cmd[@]}" -eq 0 ]]
    then
	usage
	return 1
    fi

    selected_input=$(eval "${input_cmd[@]}" 2>/dev/null | eval "${fzf}" "${fzf_options[@]}")
    rc="${?}"
    if [[ "${rc}" -eq 0 ]]
    then
	for ((i=0; i<"${#action_cmd[@]}"; i++))
	do
	    if [[ "${action_cmd[${i}]}" == "{}" ]]
	    then
		action_cmd[${i}] = "${selected_input}"
		subsitution=1
	    fi
	done

	if ! (( "${subsitution}" ))
	then
	    action_cmd+=("${selected_input}")
	fi

	eval "${action_cmd[@]}"
    else
	return "${rc}"
    fi
}

function centos-vault-url-for-sprm {
    local output=""
    local base="http://vault.centos.org"
    local urls=""
    local cache_dir=""

    function urls_for_version {
	# For each version, get
	# ${root}/${version}/{os,updates}/Source/SPackages

	for version in "${@}"
	do
	    printf "%s/%s/os/Source/SPackages\n" "${base}" "${version}"
	    printf "%s/%s/updates/Source/SPackages\n" "${base}" "${version}"
	done
    }

    output=$(curl -vs -q "${base}")
    versions=(
	$(echo "${output}"				\
	      | tidy -quiet -asxml -numeric -utf8	\
	      | xmlstarlet sel -T -t -v "//_:a"		\
	      | egrep '^[[:digit:]\.]+/?$'		\
	      | sed -e 's#/$##'))

    urls=$(urls_for_version "${versions[@]}")
    cache_dir="${HOME}/.centos-vault-url-for-srpm"
    { mkdir -p "${cache_dir}" && \
      cd "${cache_dir}"       && \
      xargs -n1 -P 6 curl -qs <<< "${urls}"
    }
}

# screen: function wrapper for screen to try to cover some common cases
function scr-with-title {
    local p=""
    local b=""
    local d=""
    local dirent=""
    local components=()
    local idx=0

    p=$(path-canonical "${PWD}")
    b=$(basename "${p}")
    components+=("${b}")

    p=$(path-canonical "${p}/..")
    b=$(basename "${p}")
    components+=("${b}")

    p=$(path-canonical "${p}/..")
    b=$(basename "${p}")
    while [[ "${p}" != "/" ]]
    do
	for dirent in "rpmbuild" "git" "src"
	do
	    test -e "${p}/${dirent}" && break 2
	done
	dirent=""
	components+=("${b}")
	p=$(path-canonical "${p}/..")
    done

    if [[ -n "${dirent}" ]]
    then
	idx=$(( ${#components[@]}-2 ))
	xt "${dirent} ${components[${idx}]}"
	screen -S "${dirent}-${components[${idx}]}" "${@}"
    else
	screen "${@}"
    fi
}

function setup-misc {
    local osname=""
    local osfile=""
    local localfile=""

    # vim: function wrapper for use with screen
    # A conditional function definition to work around the fact that when screen
    # switches to the alternate screen ("\E[?1049h" and "\E[?1049l") and back, it
    # maintains the background color that was set.  This means that after running
    # vim with the xoria256 color scheme, I am left with my prompt having a light
    # greg background color.  This is highly annoying.  I have been manually getting
    # around this by running "cl" after I quit vim from within screen (see function
    # above), but this should do the same thing automatically if I am within screen.
    if test "${TERM}" = "screen" -o \
	    "${TERM}" = "screen-256color"; then
	function vim
	{
	    command vim "$@"
	    tput op
	}
    fi

    # PYTHONSTARTUP Environment variable
    # if the ${HOME}/.python_startup.py file exists, set PYTHONSTARTUP to point to
    # it such that its contents are executed for interactive python sessions
    if [[ -f "${HOME}/.python_startup.py" ]];
    then
	export PYTHONSTARTUP="${HOME}/.python_startup.py"
    fi

    # Important variable setting
    BASHFILES=$(dirname $(path-canonical ${BASH_ARGV[0]}))
    DOTFILES=$(dirname ${BASHFILES})
    export BASHFILES DOTFILES

    path-append "${HOME}/bin"
    maybe-add-fzf-to-path

    # OS Specific bashrc file inclusion
    osname=$(uname -s | tr '[A-Z]' '[a-z]')
    osfile="${BASHFILES}/.bashrc.${osname}"
    if [[ -f "${osfile}" ]]
    then
	source "${osfile}"
    fi

    # Local bashrc file inclustion
    # Allows inclusion of initialization stuff that I don't want to keep in my
    # dotfiles repo
    localfile="${HOME}/.bashrc.local"
    if [[ -f "${localfile}" ]]
    then
	source "${localfile}"
    fi
}

function main
{
    setup-misc
}

main "${@}"
