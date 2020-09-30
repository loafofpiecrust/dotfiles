{ config, lib, pkgs, ... }:

{
  # imports = [ ./wayland.nix ];
  environment.systemPackages = with pkgs; [
    gnome3.file-roller # provides all archive formats
    unstable.alacritty
    pavucontrol

    # desktop environment
    polkit_gnome
    picom # compositor
    polybar
    dunst # notifications
    rofi # MENUS!
    rofi-menugen
    feh # wallpapers
    wpgtk
    caffeine-ng # prevent screen from sleeping sometimes
    gsettings-desktop-schemas
    farge # color picker

    # gtk themes
    arc-theme
    paper-icon-theme
    bibata-cursors

    # apps I want everywhere
    chromium
    firefox # backup browser (SAD)
    cmus # music player
    libreoffice
    spotify
    zoom-us

    # system tools
    libnotify
    pkgs.unstable.xdg-desktop-portal
    imagemagick
  ];

  fonts.enableDefaultFonts = true;
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
    migu
    emacs-all-the-icons-fonts
    # unstable.ibm-plex
    # corefonts # sometimes I need Times New Roman
    # Add user fonts to ~/.local/share/fonts
  ];

  fonts.fontconfig = {
    penultimate.enable = true;
    defaultFonts = {
      monospace = [
        "IBM Plex Mono" # Main preference, changes often.
        "Hasklig" # Provides almost all of the IPA symbols.
        "Noto Sans Mono CJK SC"
        "Noto Emoji"
        "Material Design Icons"
      ];
      sansSerif = [ "Overpass" "Noto Sans" "FreeSans" "Material Design Icons" ];
      serif = [ "Merriweather" "Liberation Serif" ];
    };
  };

  nixpkgs.overlays = [
    (self: super: {
      # Rounded corners in any X11 window manager.
      picom = super.picom.overrideAttrs (old: {
        src = builtins.fetchurl {
          url =
            "https://github.com/ibhagwan/picom/archive/68c8f1b5729dfd3c0259b3bbb225193c9ecdb526.tar.gz";
          sha256 = "07g8b62s0mxx4599lb46nkzfxjwp2cv2g0f2n1qrxb7cc80yj1nb";
        };
      });
      polybar = super.polybar.override { pulseSupport = true; };
      waybar = super.waybar.override { pulseSupport = true; };
      # ibus = super.ibus.override { withWayland = true; };
    })
  ];

  # Configure sway if I happen to want it in my setup.
  programs.sway = {
    extraOptions = [ "--my-next-gpu-wont-be-nvidia" ];
    extraPackages = with pkgs; [
      swaylock
      swayidle
      xwayland
      waybar
      mako
      kanshi
      qt5.qtwayland
      grim
      wl-clipboard
      wf-recorder
    ];
    extraSessionCommands = let
      schema = pkgs.gsettings-desktop-schemas;
      datadir = "${schema}/share/gsettings-schemas/${schema.name}";
    in ''
      export XDG_DATA_DIRS=${datadir}:$XDG_DATA_DIRS
      export SDL_VIDEODRIVER=wayland
      export QT_QPA_PLATFORM=wayland
      export MOZ_ENABLE_WAYLAND=1
      export MOZ_DBUS_REMOTE=1
    '';
    wrapperFeatures = {
      base = true;
      gtk = true;
    };
  };

  # Provide default settings for any X11 sessions.
  services.xserver = {
    enable = true;
    layout = "us";
    # FIXME seemingly doesn't work.
    enableCtrlAltBackspace = true;
    autoRepeatDelay = 300;
    autoRepeatInterval = 35; # ms between key repeats
    # I don't use caps lock enough, swap it with escape!
    xkbOptions = "caps:swapescape, compose:ralt";

    # Only applies in X sessions, not wayland AFAICT.
    libinput = {
      enable = true;
      scrollMethod = "twofinger";
      naturalScrolling = true;
      tapping = false;
      clickMethod = "clickfinger";
    };
  };

  # I type in other languages often enough.
  i18n.inputMethod = {
    enabled = null;
    # ibus.engines = with pkgs.ibus-engines; [
    #   libpinyin
    #   anthy
    #   table
    #   table-others
    # ];
    # fcitx.engines = with pkgs.fcitx-engines; [
    #   libpinyin
    #   anthy
    #   table-other
    #   table-extra
    # ];
  };

  # Give Firefox precise touchpad scrolling and wayland support.
  environment.variables = {
    MOZ_USE_XINPUT2 = "1";
    # XDG_CURRENT_DESKTOP = "sway";
  };

  # Enable better XDG integration.
  xdg.portal.enable = true;
  xdg.portal.extraPortals = with pkgs; [
    pkgs.unstable.xdg-desktop-portal-wlr
    xdg-desktop-portal-gtk
  ];
  xdg.portal.gtkUsePortal = true;
}
