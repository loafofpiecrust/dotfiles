#!/bin/sh

# General
# /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
# ibus controls input methods
ibus-daemon -s -d -n xfce4-session
pkill xfce4-volumed-pulse # sxhkd handles media keys

# Restore last wallpaper
# Starts notification manager as well
~/.config/wpg/wp_init.sh

# Window compositor to make bspwm prettier :cat:
compton &

# Bar
pkill nm-applet # not needed with my polybar modules
polybar main &
