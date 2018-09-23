#!/bin/bash

xprofile="${HOME}/.xprofile"
if [[ -e "${xprofile}" ]]
then
	source "${xprofile}"
fi

# local overrides
xinitrc_local="${HOME}/.xinitrc.local"
if [[ -e "${xinitrc_local}" ]]
then
	source "${xinitrc_local}" 
fi

dbus-launch --exit-with-session i3
