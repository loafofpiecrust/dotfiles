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
    unzip
    nnn # file manager
    gnome3.file-roller # provides all archive formats
    networkmanager-openvpn
    docker
    xfce.gvfs
    glib

    # user tools
    alacritty
    stow
    fortune
    lastpass-cli
    unstable.aspell
    gnumake

    # desktop environment
    polybar
    dunst
    rofi
    rofi-menugen
    networkmanager_dmenu
    pavucontrol
    feh
    unstable.wpgtk

    # languages
    git
    rustup
    gcc
    python3
    go
    kotlin
    nodejs-10_x
    yarn
    unstable.nim
    texlive.combined.scheme-full
    racket

    # dev
    emacs
    jetbrains.idea-ultimate
    insomnia
    vscode

    # apps
    firefox-devedition-bin
    chromium # backup
    spotify
    calibre # ebook manager
    mate.atril # pdf viewer
    xfce.parole # video player
    font-manager
    slack
    zoom-us
    deluge
    filezilla
    bleachbit

    # games
#     steam
#     wine

    # gtk themes
    arc-theme
    paper-icon-theme
    bibata-cursors
  ];

  nixpkgs.config.packageOverrides = pkgs: rec {
    polybar = pkgs.polybar.override {
      i3GapsSupport = true;
      pulseSupport = true;
    };
    yarn = pkgs.yarn.override {
      nodejs = pkgs.nodejs-10_x;
    };
    unstable = import <nixos-unstable> {
      # pass the nixpkgs config to the unstable alias
      # to ensure `allowUnfree = true;` is propagated:
      config = config.nixpkgs.config;
    };
  };

  nixpkgs.overlays = [
    (self: super: {
      # Build emacs from bleeding-edge source and customize
      emacs = (super.emacs.override {
        srcRepo = true;
      }).overrideAttrs (oldAttrs:  {
        name = "emacs-27";
        version = "27";
        src = pkgs.fetchurl {
          url = "http://git.savannah.gnu.org/cgit/emacs.git/snapshot/emacs-9027084793831031926c12d3bdfa132ec6ac4e60.tar.gz";
          sha256 = "023m671dmpi501y2qlrwzy5fsfa07cril4miycggk3jba7k963fy";
        };
        # FIXME: nixpkgs emacs requires a few patches for tramp and build env
        patches = [];
        nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ pkgs.git ];
        # Build with json support for faster LSP
        buildInputs = oldAttrs.buildInputs ++ [ pkgs.jansson ];
        configureFlags = oldAttrs.configureFlags ++ [ "--with-json" ];
      });
    })
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
