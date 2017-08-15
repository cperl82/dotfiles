# -*- mode: Shell-script; -*-

export PS1='[\u@\h \w]\$ '
export PAGER=less
export MYSQL_PS1="\u@\h [\d]> "
export HISTIGNORE=' *'

# tmpmkcd
function add-path {
	d="${1}"
	if [[ -d "${d}" ]]
	then
		components=( $(echo ${PATH} | tr ':' ' ') )
		found=0
		for p in "${components[@]}"
		do
			if [[ "${p}" == "${d}" ]]
			then
				found=1
				break
			fi
		done

		if [[ ${found} -eq 0 ]]
		then
			export PATH=${PATH}:${d}
		fi
	fi
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
	if [[ -z "${dst}" ]]; then
		dst="${PWD}"
	fi

	cd -- $(dirname -- "${dst}") > /dev/null 2>&1 &&	\
		echo $(pwd -P)/$(basename "${dst}") &&		\
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

	# Strip an potential trailing slash as it can cause `test -h' to fail
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

function lack {
	ack --pager='less -R' "$@"
}

function maybe-add-fzf-to-path {
	local fzf_home="${HOME}/.fzf"
	local fzf_path=""

	if [[ -e "${fzf_home}/bin/fzf" ]]
	then
		add-path "${HOME}/.fzf/bin"
	fi
}
maybe-add-fzf-to-path

function with-fzf {
	local fzf=""
	local input_cmd=()
	local action_cmd=()
	local fzf_options=()
	local action_single=0
	local parsing_input=0
	local parsing_action=0
	local parsing_fzf_options=0

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

	selected_input=( $(eval "${input_cmd[@]}" 2>/dev/null | eval "${fzf}" "${fzf_options[@]}") )
	rc="${?}"
	if [[ "${rc}" -eq 0 ]]
	then
		if (( "${action_single}" ))
		then
			${action_cmd[@]} "${selected_input[@]}"
		else
			for input in "${selected_input[@]}"
			do
				${action_cmd[@]} "${input}"
			done
		fi
	else
		return "${rc}"
	fi
}


# Misc Stuff

# vim - function wrapper for use with screen
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
ENV_ROOT="$(dirname "$(dirname "$(path-canonical ${BASH_ARGV[0]})")")"
DOTFILES_ROOT="${ENV_ROOT}/dotfiles"
BUNDLE_ROOT="${ENV_ROOT}/bundles"

export ENV_ROOT DOTFILES_ROOT BUNDLE_ROOT

add-path ${ENV_ROOT}/bin
add-path ${HOME}/bin

# OS Specific bashrc file inclusion
osname=$(uname -s | tr '[A-Z]' '[a-z]')
osfile="${DOTFILES_ROOT}/bashrc.${osname}"
if [[ -f "${osfile}" ]]
then
	source "${osfile}"
else
	echo "Unknown Operating System"
fi

# 2017-05-12: default terminal for i3-sensible-terminal
export TERMINAL="${ENV_ROOT}/bin/urxvt256cc"

# Local bashrc file inclustion
# Allows inclusion of initialization stuff that I don't want to keep in my
# dotfiles repo
localrc="${HOME}/.bashrc.local"
if [[ -f "${localrc}" ]]
then
	source "${localrc}"
fi
