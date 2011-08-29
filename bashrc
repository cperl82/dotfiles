# Variables {{{1
export PS1='[\u@\h \w]\$ '
export PAGER=less

# Functions {{{1
# tmpmkcd {{{2
function tmpmkcd
{
	today=$(date '+%Y-%m-%d')
	pathname="${HOME}/tmp/${today}"
	if [[ ! -d "${pathname}" ]]
	then
		mkdir -p ${pathname} && cd ${pathname}
	else
		cd ${pathname}
	fi
}

# xt - xterm title setter {{{2
function xt
{
	if [[ -z "${1}" ]]
	then
		NAME=${HOSTNAME}
	else
		NAME=${1}
	fi
	printf "\033]0;${NAME}\007"
}

# st - screen window title setter {{{2
function st
{
	if [[ -z "${1}" ]]
	then
		NAME=${HOSTNAME}
	else
		NAME=${1}
	fi
	printf "\033k${NAME}\033\\"
}

# cl - reset all attributes {{{2
function cl
{
	printf '\033[;0m'
}


# Bash path canonicalization {{{2
# Copied from comment at
# http://blog.publicobject.com/2006/06/canonical-path-of-file-in-bash.html
# and adapted to further fit my needs and be more cross platform compatible

# Resolves symlinks for path components, but will not resolve if the last
# component, file or directory is a symlink

# path-canonical-simple {{{3
function path-canonical-simple() {
	local dst="${1}"
	if [[ -z "${dst}" ]]; then
		dst="${PWD}"
	fi

	cd -- "$(dirname -- "${dst}")" > /dev/null 2>&1 && \
		echo "$(pwd -P)/$(basename "${dst}")" && \
		cd -- - >/dev/null 2>&1
}

# path-canonical {{{3
# Resolves symlinks for all path components, including the final component
function path-canonical() {
	local dst="${1}"
	if [[ -z "${dst}" ]]; then
		dst="${PWD}"
	fi

	dst="$(path-canonical-simple "${1}")"

	while [[ -h "${dst}" ]]; do
		local link_dst="$(ls -l "${dst}" | sed -e 's/^.*[ \t]*->[ \t]*\(.*\)[ \t]*$/\1/g')"
		if   [[ "${link_dst}" =~ ^..$ ]]; then
			# special case
			dst="$(dirname -- "$(dirname -- "${dst}")")"
		elif [[ "${link_dst}" =~ ^.$  ]]; then
			# special case
			dst="${dst}"
		elif [[ "${link_dst}" =~ ^/   ]]; then
			# absolute symlink
			dst="${link_dst}"
		else
			# relative symlink
			dst="$(dirname "${dst}")/${link_dst}"
		fi
	done
	# This call IS necessary as the traversal of symlinks above in the while
	# loop may have introduced additional symlinks into the path where the
	# symlink is NOT the last path component.
	dst="$(path-canonical-simple "${dst}")"
	echo "${dst}"
}

# AES Encryption via command line {{{2
# This is meant to be simplified interface to the aes-256-cbc encryption
# available in openssl.  Note that output is always "ascii armored" as that just
# makes life easier.
aes-256-cbc() { 
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
		data="${1}"
		openssl_cmd="openssl aes-256-cbc \${encrypt_decrypt} -a -in /dev/stdin -out /dev/stdout <<< '${data}'"
		;;
	f|fil|file)
		file="${1}"
		if [[ -z "${encrypt_decrypt}" ]]; then
			# encryption
			infile="${file}"
			outfile="${file}.aes"
		else
			# decryption
			# Check to make sure input file ends with .aes
			if [[ ! ("${file}" =~ \.aes$) ]]; then
				echo "File specified for decryption must have an 'aes' extension"
				return
			fi
			infile="${file}"
			outfile="${file%.aes}"
		fi
		openssl_cmd="openssl aes-256-cbc ${encrypt_decrypt} -a -in ${infile} -out ${outfile}"
		;;
	*)
		echo "${FUNCNAME} encrypt|decrypt string|file str|filename"
		return
		;;
	esac

	#echo ${openssl_cmd}
	eval ${openssl_cmd}

}

# Misc Stuff {{{1

# vim - function wrapper for use with screen {{{2
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

# PYTHONSTARTUP Environment variable {{{2
# if the ${HOME}/.python_startup.py file exists, set PYTHONSTARTUP to point to
# it such that its contents are executed for interactive python sessions
if [[ -f "${HOME}/.python_startup.py" ]];
then
	export PYTHONSTARTUP="${HOME}/.python_startup.py"
fi

function vman
{
	vim -c ":Man $*" -c ":only"
}

# OS Specific bashrc file inclusion {{{1
DIRPATH="$(dirname "$(path-canonical ${BASH_ARGV[0]})")"
OSNAME=$(uname -s)
OSFILE="${DIRPATH}/bashrc.${OSNAME}"
if   [[ -f "${OSFILE}" ]]
then
	source "${OSFILE}"
else
	echo "Unknown Operating System"
fi

# vim: tw=80 fdm=marker
