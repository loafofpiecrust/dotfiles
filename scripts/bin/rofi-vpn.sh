#!/usr/bin/env rofi-menugen

#begin main
prompt="VPN"
add_exec 'disconnect' 'systemctl stop "openvpn-*"'
add_exec 'panama'         '~/bin/connect-vpn panama'
#end main
