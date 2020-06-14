# color names: black 0, red 1, green 2, yellow 3, blue 4, magenta 5, cyan 6, white 7
# bright variant: +8
# set fish_color_normal normal
set fish_color_normal white
set fish_color_command green
set fish_color_param red
set fish_color_redirection $fish_color_param
set fish_color_comment brblack
set fish_color_error brred
set fish_color_escape magenta
set fish_color_operator $fish_color_escape
set fish_color_end blue
set fish_color_quote cyan
set fish_color_autosuggestion brblack brblack
set fish_color_user brgreen
set fish_color_host $fish_color_normal
set fish_color_valid_path --underline
set fish_color_cwd green
set fish_color_cwd_root red
set fish_color_match --background=brblue
set fish_color_search_match bryellow --background=brblack
set fish_color_selection white --bold --background=brblack
set fish_color_cancel -r
set fish_pager_color_prefix white --bold --underline
set fish_pager_color_completion
set fish_pager_color_description $fish_color_quote yellow
set fish_pager_color_progress brwhite --background=cyan
set fish_color_history_current --bold
