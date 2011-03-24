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
	cd -P -- "$(dirname -- "${dst}")" > /dev/null 2>&1 && \
		echo "$(pwd -P)/$(basename "${dst}")"
}

# path-canonical {{{3
# Resolves symlinks for all path components, including the final component
function path-canonical() {
	local dst="$(path-canonical-simple "${1}")"

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

## OS Specific bashrc file inclusion {{{2
OSFILE="$(dirname "$(path-canonical ${BASH_ARGV[0]})")"
OSNAME=$(uname -s)
if   [[ "${OSNAME}" == "Darwin" ]]
then
	source "${OSFILE}/bashrc.darwin"
elif [[ "${OSNAME}" == "SunOS" ]]
then
	source "${OSFILE}/bashrc.SunOS"
elif [[ "${OSNAME}" == "Linux" ]]
then
	source "${OSFILE}/bashrc.linux"
else
	echo "Unknown Operating System"
fi

# vim: set fdm=marker
