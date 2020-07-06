#!/bin/sh

vpn_conn=$(systemctl list-units --state=active | sed -n "s/^openvpn-\(\w*\).*$/\1/p" | head -n1)
if test "$vpn_conn" = ""; then
    echo '{"text": "", "alt": "none", "tooltip": "No VPN"}'
else
    echo "{\"text\": \"\", \"tooltip\": \"${vpn_conn^}\", \"alt\": \"connected\"}"
fi
