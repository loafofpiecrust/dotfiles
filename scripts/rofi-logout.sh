#!/usr/bin/env rofi-menugen

#begin main
prompt="Select"
add_exec 'Lock'         'dm-tool lock'
add_exec 'Log Out'      'kill -9 -1'
add_exec 'Sleep'        'systemctl suspend'
add_exec 'Reboot'       'systemctl reboot'
add_exec 'Shutdown'     'systemctl poweroff'
#end main