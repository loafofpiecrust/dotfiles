{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gnome3.file-roller # provides all archive formats
    alacritty
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

    # apps
    firefox
    calibre # ebook manager
    mate.atril # pdf viewer
    xfce.parole # video player
    font-manager
    deluge
    bleachbit
    gimp

    # gtk themes
    arc-theme
    paper-icon-theme
    bibata-cursors
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
    font-awesome
    # Add user fonts to ~/.local/share/fonts
  ];

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
      unstable.wofi
      qt5.qtwayland
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
    enabled = "ibus";
    ibus.engines = with pkgs.ibus-engines; [
      libpinyin
      anthy
      table
      table-others
    ];
  };

  # Give Firefox precise touchpad scrolling and wayland support.
  environment.variables = { MOZ_USE_XINPUT2 = "1"; };
}
