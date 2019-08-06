{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    # system tools
    binutils
    killall
    powertop
    ripgrep
    htop
    gksu
    networkmanager
    gnome3.file-roller # provides all archive formats
    networkmanager-openvpn
    docker

    # user tools
    alacritty
    stow
    fortune

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
    font-manager
    slack
    deluge

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
    charis-sil # IPA font
    google-fonts
    ubuntu_font_family
    fantasque-sans-mono
    # Add user fonts to ~/.local/share/fonts
  ];

  fonts.fontconfig = {
    defaultFonts.monospace = ["Operator Mono" "Fantasque Sans Mono"];
    defaultFonts.sansSerif = ["Ubuntu"];
    defaultFonts.serif = ["Merriweather"];
  };
}
