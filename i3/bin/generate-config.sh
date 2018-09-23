#!/bin/bash

main ()
{
    cat "${HOME}/.config/i3/config.base"	\
	"${HOME}/.config/i3/config.local"	\
	2>/dev/null				\
	> "${HOME}/.config/i3/config"
}

main
