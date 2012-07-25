xset +fp /usr/share/fonts/local
xset fp rehash
xset -b
xrdb -merge ~/.Xresources
xsetroot -solid darkslategrey
xsetroot -cursor_name left_ptr
#exec ck-launch-session fvwm
#exec ck-launch-session openbox
#exec ck-launch-session awesome
exec ck-launch-session xmonad
