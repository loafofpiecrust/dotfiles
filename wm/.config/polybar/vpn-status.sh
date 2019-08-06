#!/bin/sh

vpn_conn=$(nmcli c show --active | grep vpn)
if test "$vpn_conn" = ""; then
    echo ""
else
    echo ""
fi