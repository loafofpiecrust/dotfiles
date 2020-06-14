#!/bin/sh

# == Config programs with WM ==
# gtk theme: lxappearance
# display: autorandr
# power mgr: xfce4-power-manager
# mime associations/preferred programs: ???
# authentication: polkit-gnome

# Disable bluetooth at login, we can manually enable if we want to.
bluetoothctl power off

# General
# ibus controls input methods
# ibus-daemon -s -d -n xfce4-session
xfce4-power-manager &
# Prevent the screen going dark while watching videos.
caffeine &

# Restore last wallpaper
# Starts notification manager as well
~/.config/wpg/wp_init.sh

# Window compositor to make bspwm prettier :cat:
picom --experimental-backends &

# Bar
polybar main &

# Start a polkit agent to get GUI password prompts!
exec "$(nix eval nixos.polkit_gnome.outPath | tr -d '"')/libexec/polkit-gnome-authentication-agent-1" &
