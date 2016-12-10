xset -b
xrdb -merge ~/.dotfiles/Xresources.urxvt
xrdb -merge ~/.dotfiles/Xresources.xterm
xsetroot -solid darkslategrey
xsetroot -cursor_name left_ptr

# local overrides
if [[ -e ~/.xinitrc.local ]]
then
	source ~/.xinitrc.local
fi

#exec ck-launch-session fvwm
#exec ck-launch-session openbox
#exec ck-launch-session awesome
#exec gnome-session
#dbus-launch --exit-with-session xmonad
exec i3
