xset -b
xrdb -merge ~/.dotfiles/Xresources.urxvt
xrdb -merge ~/.dotfiles/Xresources.xterm
xsetroot -solid darkslategrey
xsetroot -cursor_name left_ptr
#exec ck-launch-session fvwm
#exec ck-launch-session openbox
#exec ck-launch-session awesome
#exec gnome-session
dbus-launch --exit-with-session xmonad
#exec startxfce4
