#!/bin/bash

player_status=$(playerctl status 2> /dev/null)
if [ "$player_status" = "Playing" ]; then
    echo "{\"text\": \"$(playerctl metadata -f "🎧  {{title}} – {{artist}}" | cut -c 1-80)\"}"
elif [ "$player_status" = "Paused" ]; then
    echo "{\"text\": \"$(playerctl metadata -f "🎧  {{title}} – {{artist}}" | cut -c 1-80)\"}"
else
    echo "{\"text\": \"\"}"
fi
