#!/bin/sh

# wait for formats to output...
sleep 0.5

# Set the wallpaper from cache
# feh --bg-fill ~/.config/wpg/.current &

# Apply colors to bspwm
# ~/.config/bspwm/apply-colors.sh

# TODO: Fix showing in lightdm
# rm -f /usr/share/backgrounds/custom/current
# cp (cat ~/.cache/wal/bglink) /usr/share/backgrounds/custom/current
# rm -f ~/.background-image
# cp (cat ~/.config/wpg/formats/bglink) ~/.background-image
# chmod 555 ~/.background-image

# Restart dunst to apply theme
# ln -sf ~/.cache/wal/dunstrc ~/.config/dunst/dunstrc
ln -sf ~/.cache/wal/mako.conf ~/.config/mako/config
ln -sf ~/.cache/wal/alacritty.yml ~/.config/alacritty/alacritty.yml

# pkill dunst && dunst & disown
pkill waybar && ~/bin/waybar.sh & disown

# Reload theme in emacs!
emacsclient -n -e "(when (string-match \"ewal-\" doom-theme) (doom/reload-theme))"
