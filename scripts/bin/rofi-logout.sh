#!/usr/bin/env rofi-menugen

#begin main
prompt="Select"
add_exec 'lock'         'dm-tool lock'
add_exec 'log out'      'kill -9 -1'
add_exec 'sleep'        'systemctl suspend'
add_exec 'reboot'       'systemctl reboot'
add_exec 'shutdown'     'systemctl poweroff'
#end main