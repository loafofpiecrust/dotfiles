{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    # system tools
    killall
    neofetch
    stow
    fortune
    powertop
    gksu
    mate.engrampa # provides all archive formats

    # user tools
    networkmanager
    alacritty
    shutter

    # desktop environment
    polybar
    dunst
    rofi
    rofi-menugen
    networkmanager_dmenu
    pavucontrol
    feh
    wpgtk

    # languages
    git
    rustup
    gcc
    python3
    go
    kotlin
    nodejs

    # dev
    jetbrains.idea-ultimate
    insomnia
    vscode

    # apps
    firefox-devedition-bin
    spotify
    calibre # ebook manager
    mate.atril # pdf viewer
    xfce.parole # video player
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
