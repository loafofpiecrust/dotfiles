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
    gnupg

    # user tools
    alacritty
    stow
    fortune
    lastpass-cli
    gnumake

    # desktop environment
    picom
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
    # texlive.combined.scheme-tetex

    # Spellcheck
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science

    # dev
    emacsUnstable

    # apps
    firefox
    calibre # ebook manager
    mate.atril # pdf viewer
    xfce.parole # video player
    font-manager
    deluge
    filezilla
    bleachbit
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
    # nur = import (builtins.fetchTarball {
    #   url = https://github.com/nix-community/NUR/archive/4a5651cf5f5a46ff5197805b4b10dfd6dd89d28a.tar.gz;
    #   sha256 = "1h4g76fx8bzrqngwq4ijr063hbf5f3mwfq1vyw305rjmjcs3w3ak";
    # }) {
    #   inherit pkgs;
    # };
    # Pin to emacs 27 release branch.
    # emacs = nur.repos.kreisys.emacs27.overrideAttrs(oldAttrs: {
    #   src = builtins.fetchurl {
    #     url = https://github.com/emacs-mirror/emacs/archive/90321f595c88324cccaa820add096e5d1c3deac5.tar.gz;
    #     sha256 = "0p2di1h66cbrnmf65gbnj5z7256qq2yn184fm7faz9cglx6fwlji";
    #   };
    # });
    # emacs = pkgs.emacsUnstable;
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
      # iosevka-custom = super.unstable.iosevka.override {
      #   set = "custom";
      #   privateBuildPlan = {
      #     family = "Iosevka Custom";
      #     design = ["sans" "expanded" "ss09"];
      #     upright = ["upright-only" "styles"];
      #     italic = ["italic-only" "styles"];
      #     oblique = ["oblique-only" "styles"];
      #     width = 600;
      #     shape = 500;
      #     menu = 500;
      #     css = 500;
      #   };
      # };
    })
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/40c8a99d93f6a797722362af067a299b58cef84d.tar.gz;
    }))
  ];

  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    noto-fonts-extra
    hasklig
    material-design-icons
    charis-sil # IPA font
    google-fonts
    ubuntu_font_family
    fira-code
    symbola
    dejavu_fonts
    # iosevka-custom
    # Add user fonts to ~/.local/share/fonts
  ];

  fonts.enableDefaultFonts = true;
  fonts.fontconfig.defaultFonts = {
    monospace = ["Fira Code" "Noto Sans Mono CJK SC" "Noto Emoji" "Material Design Icons"];
    sansSerif = ["Overpass" "Noto Sans CJK SC" "FreeSans" "Material Design Icons"];
    serif = ["Merriweather"];
  };
}
