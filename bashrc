export PS1="[\u@\h \w] \$ "

# alias's for things that I don't like to type too often
alias sshr='ssh -l root'
alias macvim='open -a MacVim'
alias ls='ls -G'

# Setup our PATH and MANPATH to include the stuff for Darwin Ports (or Mac
# Ports)
export PATH=/opt/local/bin:/opt/local/sbin:/usr/local/bin:${PATH}
export MANPATH=/opt/local/man:${MANPATH}

# Specific setup for mysql sandbox
# Installed on or around 2010-09-02
# http://mysqlsandbox.net/
export PATH=${PATH}:${HOME}/local/bin
export PERL5LIB=${PERL5LIB}:${HOME}/local/lib/perl5/site_perl/5.12.3

function nhlsoa
{
	dig @8.20.73.12 ${1} NS +norec +short | sort | while read ns
	do 
		printf "%-20s" ${ns}; dig @${ns} ${1} SOA +norec +short
	done
}

function tmpmkcd
{
	today=$(date '+%Y-%m-%d')
	pathname="${HOME}/tmp/${today}"
	if [[ ! -d "${pathname}" ]]
	then
		mkdir ${pathname} && cd ${pathname}
	else
		cd ${pathname}
	fi
}

function zl
{
	NAME=${1}
	NAME=${NAME%%.*}
	dig +short -t TXT ${NAME}.nhl.com | tr -d '"' | awk '{print $2}'
}

function sshzl
{
	ssh $(zl ${1})
}

# A short little function to set my window title manually
function t
{
	printf "\033]2;${1}\007"
}

# Determine the version of NHL apps currently in qa
function appversion
{
	if [[ "${1}" == "production" ]]
	then
		HOST="www.nhl.com"
	elif [[ "${1}" == "qa" ]]
	then
		HOST="ny-qaweb01.nhl.com"
	else
		echo "Unknown environment"
		return 1
	fi
	curl -s -o /dev/stdout "http://${HOST}/ice/app" | grep '<script src="http://.*/js/_ice-plugins.min.js?v=.*</script>' | sed -e 's/.*?v=\([0-9.][0-9.]*\).*$/\1/'
}

# Function to reset terminal colors in case something got left in a jacked state
function cl
{
	echo -en '\033[;0m'
}
