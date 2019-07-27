{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    networkmanager
    # desktop environment
    i3-gaps
    polybar
    dunst
    compton-git
    rofi
    rofi-menugen
    networkmanager_dmenu
    pavucontrol
    neofetch
    feh
    wpgtk
    nitrogen
    fortune
    oblogout
    stow
    killall
    gksu
    powertop
    shutter

    # baseline tools
    alacritty
    fish

    # dev
    git
    rustup
    gcc
    python3
    go
    kotlin
    nodejs

    # ide
    jetbrains.idea-ultimate
    insomnia
    vscode
#     libreoffice

    # apps
    firefox-devedition-bin
    spotify
#     zoom-us
#     slack

    # games
#     steam
#     wine

    # gtk themes
    adapta-gtk-theme
    arc-theme
    capitaine-cursors
    bibata-cursors
    paper-icon-theme
  ];

  nixpkgs.config.packageOverrides = pkgs: rec {
    polybar = pkgs.polybar.override {
      i3GapsSupport = true;
      pulseSupport = true;
    };
  };

  fonts.fonts = with pkgs; [
    noto-fonts
    fira-code
    material-design-icons
    hermit
    # Add user fonts to ~/.local/share/fonts
  ];
}
