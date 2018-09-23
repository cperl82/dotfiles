#!/bin/bash

xset -b
xsetroot -solid darkslategrey
xsetroot -cursor_name left_ptr

xresources="${HOME}/.Xresources"
if [[ -e "${xresources}" ]]
then
	xrdb -merge -cpp default -I${HOME} "${xresources}"
fi

# local overrides
if [[ -e ${HOME}/.xinitrc.local ]]
then
	source ${HOME}/.xinitrc.local
fi

${HOME}/.config/i3/bin/generate-config.sh
dbus-launch --exit-with-session i3
