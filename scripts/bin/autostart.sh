#!/bin/bash

# == Config programs with WM ==
# gtk theme: lxappearance
# display: autorandr
# power mgr: xfce4-power-manager
# mime associations/preferred programs: ???
# authentication: polkit-gnome

# Disable bluetooth at login, we can manually enable if we want to.
# bluetoothctl power off &

# O365 Server for university email
# davmail /home/snead/.config/davmail/.properties & disown

# General
# ibus controls input methods
# ibus-daemon -s -d -n xfce4-session
xsetroot -cursor_name left_ptr

# Enable sticky modifier keys!
# xkbset sticky -twokey -latchlock
# xkbset mousekeys
# xkbset exp 1 =sticky =mousekeys =sticky =twokey =latchlock


xmodmap -e "keycode 133 = F19"
xmodmap -e "clear Mod4"

xfce4-power-manager & disown
xfsettingsd & disown
# Prevent the screen going dark while watching videos.
# caffeine &
~/.config/wpg/wp_init.sh &
# stalonetray & disown
# sleep 0.5 && polybar main & disown

# Restore last wallpaper
# Starts notification manager as well
# sleep 0.5 && /home/snead/.config/wpg/wp_init.sh &
# wpg -m &

# Window compositor to make bspwm prettier :cat:
# picom --experimental-backends &

# polybar main &

# Bar
# polybar main &
# waybar -c ~/.config/waybar/config -s ~/.config/waybar/styles.css

# Start a polkit agent to get GUI password prompts!
exec "$(nix eval --raw nixos.polkit_gnome)/libexec/polkit-gnome-authentication-agent-1" &
