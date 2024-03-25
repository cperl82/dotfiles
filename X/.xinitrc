#!/bin/bash

function main {
    xset -b
    xsetroot -solid darkslategrey
    xsetroot -cursor_name left_ptr

    # local override for xinitrc
    xinitrc_local="${HOME}/.xinitrc.local"
    if [[ -e "${xinitrc_local}" ]]
    then
	source "${xinitrc_local}"
    fi

    # xresources
    xresources="${HOME}/.Xresources"
    if [[ -e "${xresources}" ]]
    then
	xrdb -merge -cpp /usr/bin/cpp -I"${HOME}" "${xresources}"
    fi

    # xresources
    xresources_local="${HOME}/.Xresources.local"
    if [[ -e "${xresources_local}" ]]
    then
	xrdb -merge -cpp /usr/bin/cpp -I"${HOME}" "${xresources_local}"
    fi

    # Set the terminal to be invoked by i3-sensible-terminal
    export TERMINAL=alacritty

    # regenerate our i3 config
    config=$("${HOME}/.config/i3/bin/generate-config.sh")
    i3 -c "${config}"

}

main "${@}"
