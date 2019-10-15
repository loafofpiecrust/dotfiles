#!/bin/sh

# General
# /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# Restore last wallpaper
# Starts notification manager as well
~/.config/wpg/wp_init.sh

# Bar
pkill nm-applet # not needed with my polybar modules
polybar main &
