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
    texlive.combined.scheme-full
    racket

    # Spellcheck
    unstable.aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science

    # dev
    emacs
    insomnia

    # apps
    firefox
    calibre # ebook manager
    mate.atril # pdf viewer
    xfce.parole # video player
    font-manager
    deluge
    filezilla
    bleachbit
    libreoffice
    gimp

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
    # Allow use of less reviewed community-developed packages! Pinned on 2020-02-03.
    nur = import (builtins.fetchTarball {
      url = "https://github.com/nix-community/NUR/archive/4a5651cf5f5a46ff5197805b4b10dfd6dd89d28a.tar.gz";
      sha256 = "1h4g76fx8bzrqngwq4ijr063hbf5f3mwfq1vyw305rjmjcs3w3ak";
    }) {
      inherit pkgs;
    };
    # Pin to emacs 27 release branch.
    emacs = nur.repos.kreisys.emacs27.overrideAttrs(oldAttrs: {
      src = builtins.fetchurl {
        url = https://github.com/emacs-mirror/emacs/archive/b2e27d8617ad727c578763445d240962828a872c.tar.gz;
        sha256 = "1ij1f7l20b1npbdy0y354s838c7l7jjxj0rzmw0jqy16p9b0l20i";
      };
    });
  };

  nixpkgs.overlays = [
    # Add wayland packages.
    (self: super: {
      # Add round corners to bspwm
      bspwm = super.bspwm.overrideAttrs(oldAttrs: {
        src = builtins.fetchurl {
          url = https://github.com/Javyre/bspwm/archive/round_corners.tar.gz;
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
    fira-code
    # Add user fonts to ~/.local/share/fonts
  ];

  fonts.enableDefaultFonts = true;
  fonts.fontconfig.defaultFonts = {
    monospace = ["Fira Code" "Noto Sans Mono CJK SC" "Noto Emoji" "Material Design Icons"];
    sansSerif = ["Overpass" "Noto Sans CJK SC" "FreeSans" "Material Design Icons"];
    serif = ["Merriweather"];
  };
}
