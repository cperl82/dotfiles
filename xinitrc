#!/bin/bash

xset -b
xsetroot -solid darkslategrey
xsetroot -cursor_name left_ptr

if [[ -e ${HOME}/.Xresources ]]
then
	xrdb -merge ${HOME}/.Xresources
fi

# local overrides
if [[ -e ${HOME}/.xinitrc.local ]]
then
	source ${HOME}/.xinitrc.local
fi

# exec ck-launch-session fvwm
# exec ck-launch-session openbox
# exec ck-launch-session awesome
# exec gnome-session
# dbus-launch --exit-with-session xmonad
${HOME}/.config/i3/bin/generate-config.sh
dbus-launch --exit-with-session i3
