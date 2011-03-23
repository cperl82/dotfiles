# Common setup

## Variables
export PS1='[\u@\h \w]\$ '
export PAGER=less

## Functions
# Function to make temporary scratch workspaces
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

# Function to set xterm window title
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

# Function to set screen window title
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

# Function to reset terminal colors in case something got left in a jacked state
function cl
{
	printf '\033[;0m'
}


# Bash path canonicalization
# Copied from comment at
# http://blog.publicobject.com/2006/06/canonical-path-of-file-in-bash.html
# and adapted to further fit my needs and be more cross platform compatible

# Resolves symlinks for path components, but will not resolve if the last
# component, file or directory is a symlink
function path-canonical-simple() {
	local dst="${1}"
	cd -P -- "$(dirname -- "${dst}")" > /dev/null 2>&1 && \
		echo "$(pwd -P)/$(basename "${dst}")"
}

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
	dst="$(path-canonical-simple "${dst}")"
	echo "${dst}"
}

## OS Specific
OSFILE="$(dirname "$(path-canonical ${BASH_ARGV[0]})")"
OSNAME=$(uname -s)
if   [[ "${OSNAME}" == "Darwin" ]]
then
	source "${OSFILE}/bashrc.darwin"
elif [[ "${OSNAME}" == "SunOS" ]]
then
	source "${OSFILE}/bashrc.SunOS"
else
	echo "Unknown Operating System"
fi
