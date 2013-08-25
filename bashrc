# Variables {{{1
export PS1='[\u@\h \w]\$ '
export PAGER=less
export MYSQL_PS1="\u@\h [\d]> "
export HISTIGNORE=' *'

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

	# Strip an potential trailing slash as it can cause `test -h' to fail
	while [[ -h "${dst%/}" ]]; do
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

# NHL Schedule scraping function {{{2
# argument is a regex that is matched against the est node from the xml
nhl-schedule () {
	TMPFILE="/tmp/nhl-sched-by-date.$$"
	SOURCE="http://www.nhl.com/feeds/public/SeasonSchedule.xml"

	what="${1}"
	if [[ -z "${what}" ]]
	then
		what=$(date '+%Y-%m-%d')
	fi
	curl -qs -o "${TMPFILE}" "${SOURCE}"
	local i=0
	while read gameId est awayTeam homeTeam
	do
		est=${est/T/ }
		line="${est}  ${gameId}  ${awayTeam} @ ${homeTeam}"
		if [[ "${line}" =~ ${what} ]];
		then
			echo "${line}"
			i=$((i+1))
		fi
	done < <(xmlstarlet sel -t -m "schedule/game" -v "concat(gameId,' ',est,' ',awayTeam,' ',homeTeam)" -n "${TMPFILE}")
	echo ${i}
	rm -f "${TMPFILE}"
}

# NFSv3 Capture Filter {{{2
# Generate tcpdump capture filter for host running nfsv3
nfs3-capture-filter-for-host () {
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

# lack - function wrapper for use with ack (http://www.betterthangrep.com/)
# to output to less -R.  I don't always want this, but want it easily available
# when I do.
function lack
{
	ack --pager='less -R' "$@"
}

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

# add-path {{{2
# function to add something to PATH, skipping duplicates
function add-path
{
	d="${1}"
	if [[ -d "${d}" ]]
	then
		components=( $(echo ${PATH} | /bin/tr ':' ' ') )
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

# Important variable setting {{{1
ENV_ROOT="$(dirname "$(dirname "$(path-canonical ${BASH_ARGV[0]})")")"
DOTFILES_ROOT="${ENV_ROOT}/dotfiles"
HG_EXT_ROOT="${ENV_ROOT}/hgext"
BUNDLE_ROOT="${ENV_ROOT}/bundles"

export ENV_ROOT DOTFILES_ROOT HG_EXT_ROOT BUNDLE_ROOT

add-path ${ENV_ROOT}/bin
add-path ${HOME}/bin

# OS Specific bashrc file inclusion {{{1
OSNAME=$(uname -s | tr '[A-Z]' '[a-z]')
OSFILE="${DOTFILES_ROOT}/bashrc.${OSNAME}"
if [[ -f "${OSFILE}" ]]
then
	source "${OSFILE}"
else
	echo "Unknown Operating System"
fi

# Local bashrc file inclustion {{{1
# Allows inclusion of initialization stuff that I don't want to keep in my
# dotfiles repo
LOCALRC="${HOME}/.bashrc.local"
if [[ -f "${LOCALRC}" ]]
then
	source "${LOCALRC}"
fi

# vim: tw=80 fdm=marker
