#!/bin/sh

player_status=$(playerctl status 2> /dev/null)
if [ "$player_status" = "Playing" ]; then
    playerctl metadata -f "🎧 {{title}} – {{artist}}" | cut -c 1-80
elif [ "$player_status" = "Paused" ]; then
    playerctl metadata -f "🎧 {{title}} – {{artist}}" | cut -c 1-80
else
    echo "🎧 None"
fi
