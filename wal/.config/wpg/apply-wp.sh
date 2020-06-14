#!/usr/bin/env fish

# wait for formats to output...
sleep 0.7

# Set the wallpaper from cache
feh --bg-fill ~/.config/wpg/.current &

# Apply colors to bspwm
~/.config/bspwm/apply-colors.sh

# TODO: Fix showing in lightdm
# rm -f /usr/share/backgrounds/custom/current
# cp (cat ~/.cache/wal/bglink) /usr/share/backgrounds/custom/current
# rm -f ~/.background-image
# cp (cat ~/.config/wpg/formats/bglink) ~/.background-image
# chmod 555 ~/.background-image

# Restart dunst to apply theme
ln -sf ~/.cache/wal/dunstrc ~/.config/dunst/dunstrc
pkill dunst && dunst & disown
