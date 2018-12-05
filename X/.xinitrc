#!/bin/bash

function main {
    xset -b
    xsetroot -solid darkslategrey
    xsetroot -cursor_name left_ptr

    # xresources
    xresources="${HOME}/.Xresources"
    if [[ -e "${xresources}" ]]
    then
	xrdb -merge -cpp /usr/bin/cpp -I"${HOME}" "${xresources}"
    fi

    # local overrides
    xinitrc_local="${HOME}/.xinitrc.local"
    if [[ -e "${xinitrc_local}" ]]
    then
	source "${xinitrc_local}"
    fi

    # start urxvtcd
    urxvt256cd -q -o -f
    export TERMINAL=urxvt256cc

    # regenerate our i3 config
    config=$("${HOME}/.config/i3/bin/generate-config.sh")
    i3 -c "${config}"

}

main "${@}"
