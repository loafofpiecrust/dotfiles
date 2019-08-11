#!/bin/sh

# General
# /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# Bar
polybar main &
pkill nm-applet & # not needed with my polybar modules

# Restore last wallpaper
~/.config/wpg/wp_init.sh &

# Notification manager
dunst &