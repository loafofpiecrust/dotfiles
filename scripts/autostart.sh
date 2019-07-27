#!/bin/sh

# General
# /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &


# Bar
polybar -r main &
pkill nm-applet & # not needed with my polybar modules

# Random Wallpaper
wpg -m &

# Windows
dunst &