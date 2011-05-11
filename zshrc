PROMPT='[%n@%m %~]%# '

echo ${0}

function vman
{
	vim -c ":Man $*" -c ":only"
}
