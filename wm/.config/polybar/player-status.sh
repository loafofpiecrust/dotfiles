#!/bin/sh

echo -n "🎧 "
status=$(playerctl status)
if test $status; then
    playerctl metadata -f "{{title}} – {{artist}}"
else
    echo "$status"
fi
