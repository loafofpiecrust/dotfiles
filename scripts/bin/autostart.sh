#!/bin/bash

# == Config programs with WM ==
# gtk theme: lxappearance
# display: autorandr
# power mgr: xfce4-power-manager
# mime associations/preferred programs: ???
# authentication: polkit-gnome

# Disable bluetooth at login, we can manually enable if we want to.
bluetoothctl power off

# O365 Server for university email
davmail ~/.config/davmail/.properties & disown

# General
# ibus controls input methods
# ibus-daemon -s -d -n xfce4-session
# xfce4-power-manager &
# Prevent the screen going dark while watching videos.
# caffeine &

# Restore last wallpaper
# Starts notification manager as well
# ~/.config/wpg/wp_init.sh

# Window compositor to make bspwm prettier :cat:
# picom --experimental-backends &

# Bar
# polybar main &
# waybar -c ~/.config/waybar/config -s ~/.config/waybar/styles.css

# Start a polkit agent to get GUI password prompts!
exec "$(nix eval --raw nixos.polkit_gnome)/libexec/polkit-gnome-authentication-agent-1" &
