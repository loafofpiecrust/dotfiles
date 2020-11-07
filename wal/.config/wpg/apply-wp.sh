#!/bin/bash

# wait for formats to output...
# sleep 0.5

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
# mkdir ~/.config/mako
# ln -sf ~/.cache/wal/mako.conf ~/.config/mako/config
# mkdir ~/.config/alacritty
# ln -sf ~/.cache/wal/alacritty.yml ~/.config/alacritty/alacritty.yml

if pgrep dunst
then
    pkill dunst && dunst & disown
fi
# pkill waybar && ~/bin/waybar.sh & disown

# Reload theme in emacs!
emacsclient -n -e "(progn (ewal-load-colors) (when (string-match \"ewal-\" (symbol-name doom-theme)) (doom/reload-theme)))"
