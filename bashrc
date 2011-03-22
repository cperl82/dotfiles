# Determine which bashrc file to include

OSNAME=$(uname -s)

if [[ "${OSNAME}" == "Darwin" ]]
then
	source .dotfiles/bashrc.Darwin
elif [[ "${OSNAME}" == "SunOS" ]]
then
	source .dotfiles/bashrc.SunOS
else
	echo "Unknown Operating System"
fi
