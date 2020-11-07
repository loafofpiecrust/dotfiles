#!/bin/bash

wifi_conn=$(iwctl station wlan0 show | rg Connected | cut -d' ' -f17- | awk '{$1=$1};1') 
if test "$wifi_conn" = ""; then
    echo "󰖪"
else
    echo "󰖩"
fi
