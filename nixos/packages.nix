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
    alacritty
    stow
    fortune
    lastpass-cli
    gnumake

    # desktop environment
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
    polybar = pkgs.polybar.override {
      i3GapsSupport = true;
      pulseSupport = true;
    };
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
    }))
  ];

  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
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
    defaultFonts.monospace = ["SF Mono" "Fantasque Sans Mono" "Noto Sans Mono CJK SC" "Noto Emoji"];
    defaultFonts.sansSerif = ["Overpass" "Noto Sans CJK SC"];
    defaultFonts.serif = ["Merriweather"];
  };
}
