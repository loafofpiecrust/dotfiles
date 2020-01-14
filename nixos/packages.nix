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
    networkmanager-openvpn
    unzip
    nnn # file manager
    gnome3.file-roller # provides all archive formats
    xfce.gvfs
    glib

    # user tools
    unstable.alacritty
    stow
    fortune
    lastpass-cli
    gnumake

    # desktop environment
    compton
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
    yarn
    texlive.combined.scheme-medium
    racket

    # Spellcheck
    unstable.aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science

    # dev
    emacsGit
    insomnia
    vscodium

    # apps
    firefox
    calibre # ebook manager
    mate.atril # pdf viewer
    xfce.parole # video player
    font-manager
    deluge
    filezilla
    bleachbit

    # gtk themes
    arc-theme
    paper-icon-theme
    bibata-cursors
  ];

  # TODO: Convert these to overlays where possible.
  nixpkgs.config.packageOverrides = pkgs: rec {
    unstable = import <nixos-unstable> {
      # pass the nixpkgs config to the unstable alias
      # to ensure `allowUnfree = true;` is propagated:
      config = config.nixpkgs.config;
    };
  };

  nixpkgs.overlays = [
    # Build emacs from bleeding-edge source
    (import (builtins.fetchTarball {
      # Pin to a particular commit until I manually upgrade
      url = https://github.com/nix-community/emacs-overlay/archive/987648217c9aacfa5a5fd6925ed3da3bbeb0b3b7.tar.gz;
      sha256 = "07jcy5q79z4yf3y8iw7b8h1n214jrhrrbjpybhbl8hn6lrdkp8w2";
    }))
    (self: super: {
      bspwm = super.bspwm.overrideAttrs(oldAttrs: {
        src = builtins.fetchurl {
          url = "https://github.com/Javyre/bspwm/archive/round_corners.tar.gz";
          sha256 = "0b4a02ami7pa71g4j4an5cfjn3sgrf1mvwm8k90q0j0iqgs7zwii";
        };
      });
      polybar = super.polybar.override {
        pulseSupport = true;
      };
    })
  ];

  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    noto-fonts-extra
    hasklig
    fantasque-sans-mono
    material-design-icons
    charis-sil # IPA font
    google-fonts
    ubuntu_font_family
    # Add user fonts to ~/.local/share/fonts
  ];

  fonts.fontconfig = {
    defaultFonts.monospace = ["Hasklig" "Noto Sans Mono CJK SC" "Noto Emoji"];
    defaultFonts.sansSerif = ["Overpass" "Noto Sans CJK SC" "FreeSans"];
    defaultFonts.serif = ["Merriweather"];
  };
}
