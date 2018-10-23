# 2016-12-11 cperl: just source the xinitrc
xinitrc="${HOME}/.xinitrc}"
if [[ -e "${xinitrc}" ]]
then
	source "${xinitrc}"
fi
