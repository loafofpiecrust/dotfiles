#!/usr/bin/env fish

# wait for formats to output...
sleep 0.25

# Set the wallpaper from cache
feh --bg-fill ~/.config/wpg/.current &

# ...and import its color palette to fish
bass source ~/.config/wpg/formats/colors.sh

# TODO: Fix showing in lightdm
# rm -f /usr/share/backgrounds/custom/current
# cp (cat ~/.cache/wal/bglink) /usr/share/backgrounds/custom/current
# rm -f ~/.background-image
# cp (cat ~/.config/wpg/formats/bglink) ~/.background-image
# chmod 555 ~/.background-image

# Restart dunst to apply theme
ln -sf ~/.config/wpg/formats/dunstrc ~/.config/dunst/dunstrc
pkill dunst && dunst &
