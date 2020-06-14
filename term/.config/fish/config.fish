# Replace default named colors with our custom scheme.
cat ~/.config/wpg/sequences &
# Tell fish to use named colors for everything.
source ~/.config/fish/custom/colors.fish 2>/dev/null

# Install all fish packages on new systems!
if not functions -q fisher
    set -q XDG_CONFIG_HOME
    or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

# Default to opening files in existing frames
set -x VISUAL "emacsclient"
# If all else fails, use vim
set -x EDITOR "nvim"

# Make go install packages somwehre less annoying.
set -x GOPATH ~/.go
set -g fish_user_paths $HOME/.config/emacs/bin $GOPATH/bin $HOME/.cargo/bin $HOME/.npm/bin $fish_user_paths

# Gives firefox smooth touchpad scrolling.
set -x MOZ_USE_XINPUT2 1

any-nix-shell fish --info-right | source
