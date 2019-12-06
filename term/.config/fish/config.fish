# The line below screws up emacs and may be unnecessary
# cat ~/.config/wpg/sequences &

source ~/.cache/wal/colors.fish 2>/dev/null

# Install all fish packages on new systems!
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

set -x EDITOR "emacsclient -tc"

set -x GOPATH ~/.go
set -g fish_user_paths $GOPATH/bin $HOME/.cargo/bin $HOME/.npm/bin $fish_user_paths
if command -qs gem
    set -g fish_user_paths (gem env | rg "EXECUTABLE DIRECTORY" | cut -d':' -f 2 | cut -c 2-) $fish_user_paths
end
