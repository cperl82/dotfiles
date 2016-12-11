
misc_xsettings () {
	xset -b
	xrdb -merge "${HOME}/.dotfiles/Xresources.urxvt"
	xrdb -merge "${HOME}/.dotfiles/Xresources.xterm"
	xsetroot -solid darkslategrey
	xsetroot -cursor_name left_ptr
}

launch-i3 () {
	cat "${HOME}/.config/i3/config.base"	\
	    "${HOME}/.config/i3/config.local"	\
	    > "${HOME}/.config/i3/config"
	exec i3
}

main ()
{
	misc_xsettings

	# local overrides
	if [[ -e ~/.xinitrc.local ]]
	then
		source ~/.xinitrc.local
	fi

	# exec ck-launch-session fvwm
	# exec ck-launch-session openbox
	# exec ck-launch-session awesome
	# exec gnome-session
	# dbus-launch --exit-with-session xmonad
	launch-i3
}


