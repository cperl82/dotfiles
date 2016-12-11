#!/bin/bash

main ()
{
	cat "${HOME}/.config/i3/config.base"		\
	    "${HOME}/.config/i3/config.local"		\
	    > "${HOME}/.config/i3/config" 2>/dev/null
}

main
