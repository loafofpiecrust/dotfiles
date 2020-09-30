if status --is-interactive
    # Replace default named colors with our custom scheme.
    # cat ~/.config/wpg/sequences &
    # Tell fish to use named colors for everything.
    # source ~/.config/fish/custom/colors.fish 2>/dev/null

    # Install all fish packages on new systems!
    if not functions -q fisher
        set -q XDG_CONFIG_HOME
        or set XDG_CONFIG_HOME ~/.config
        curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
        fish -c fisher
    end
end

# Default to opening files in existing frames
set -x VISUAL "emacsclient"
# If all else fails, use vim
set -x EDITOR "nvim"

set -x XDG_CONFIG_HOME $HOME/.config
set -x XDG_CACHE_HOME $HOME/.cache

# Make go install packages somwehre less annoying.
set -x GOPATH ~/.go
set -g fish_user_paths $HOME/.netlify/helper/bin $HOME/.config/emacs/bin $GOPATH/bin $HOME/.cargo/bin $HOME/.npm/bin $fish_user_paths

# Gives firefox smooth touchpad scrolling.
# set -x MOZ_USE_XINPUT2 1
# set -x MOZ_ENABLE_WAYAND 1
set -x _JAVA_AWT_WM_NONREPARENTING 1

# Make some aliases for better cli tools!
alias grep="rg"
# alias find="fd"
alias ls="exa"
alias x="exa"
alias du="dust"
alias htop="ytop"
# alias sed="sd"

# Let programs find the dbus session themselves.
set -e DBUS_SESSION_BUS_ADDRESS

# set -x XDG_DATA_DIRS (nix eval --raw nixpkgs.gsettings-desktop-schemas)/share/gsettings-schemas/(nix eval --raw nixpkgs.gsettings-desktop-schemas.name):$XDG_DATA_DIRS

if status --is-interactive
    any-nix-shell fish | source
    starship init fish | source
end

