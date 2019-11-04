
cat ~/.config/wpg/sequences &

source ~/.cache/wal/colors.fish

# Install all fish packages on new systems!
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

set -x EDITOR "emacsclient -tc"

set -x GOPATH ~/.go
set -g fish_user_paths $GOPATH $HOME/.cargo/bin $fish_user_paths
