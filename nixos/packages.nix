{ config, pkgs, ... }: {
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    # system tools
    binutils
    killall
    powertop
    ripgrep
    htop
    gksu
    unzip
    nnn # file manager
    gnome3.file-roller # provides all archive formats
    xfce.gvfs
    gnupg

    # user tools
    alacritty
    stow
    fortune
    gnumake

    # desktop environment
    picom # compositor
    polybar
    dunst # notifications
    rofi # MENUS!
    rofi-menugen
    pavucontrol
    feh # wallpapers
    wpgtk
    caffeine-ng # prevent screen from sleeping sometimes
    playerctl
    polkit_gnome

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
    nur = builtins.fetchTarball {
      # Get the revision by choosing a version from https://github.com/nix-community/NUR/commits/master
      url =
        "https://github.com/nix-community/NUR/archive/61eeb89c5553d103b27fc28c0d8eb882049e0dfc.tar.gz";
      # Get the hash by running `nix-prefetch-url --unpack <url>` on the above url
      sha256 = "0qaic8fllwffbxaf3y2yiywlr5pdxr5c39bzfa8k1kbnf3nzp90l";
    };
  };

  nixpkgs.overlays = [
    # Add wayland packages.
    (self: super: {
      # Add round corners to bspwm
      # bspwm = super.bspwm.overrideAttrs(oldAttrs: {
      #   src = builtins.fetchurl {
      #     url = https://github.com/Javyre/bspwm/archive/round_corners.tar.gz;
      #     sha256 = "0b4a02ami7pa71g4j4an5cfjn3sgrf1mvwm8k90q0j0iqgs7zwii";
      #   };
      # });
      picom = super.picom.overrideAttrs (old: {
        src = builtins.fetchurl {
          url =
            "https://github.com/ibhagwan/picom/archive/68c8f1b5729dfd3c0259b3bbb225193c9ecdb526.tar.gz";
          sha256 = "07g8b62s0mxx4599lb46nkzfxjwp2cv2g0f2n1qrxb7cc80yj1nb";
        };
      });
      polybar = super.polybar.override { pulseSupport = true; };
    })
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/40c8a99d93f6a797722362af067a299b58cef84d.tar.gz";
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
    unstable.fira-code
    symbola
    dejavu_fonts
    # Add user fonts to ~/.local/share/fonts
  ];

  fonts.enableDefaultFonts = true;
  fonts.fontconfig = {
    defaultFonts = {
      monospace = [
        "Ubuntu Mono"
        "Hasklig"
        "Noto Sans Mono CJK SC"
        "Noto Emoji"
        "Material Design Icons"
      ];
      sansSerif =
        [ "Overpass" "Noto Sans CJK SC" "FreeSans" "Material Design Icons" ];
      serif = [ "Merriweather" ];
    };
  };
}
