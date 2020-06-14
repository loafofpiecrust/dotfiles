# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

# Declare a function that takes one destructured argument.
{ config, lib, pkgs, ... }: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./laptop.nix
    ./packages.nix
  ];

  system.autoUpgrade = {
    enable = true;
    channel = "https://nixos.org/channels/nixos-20.03";
  };

  console = {
    # font = "Ubuntu Mono";
    # packages = [ pkgs.ubuntu_font_family ];
    keyMap = "us";
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = let alt = "en_DK.UTF-8";
    in {
      # I prefer ISO time and the metric system.
      LC_TIME = alt;
      LC_MEASUREMENT = alt;
    };
    inputMethod = {
      enabled = "ibus";
      ibus.engines = with pkgs.ibus-engines; [
        libpinyin
        anthy
        table
        table-others
      ];
    };
  };

  # Use pulseaudio for sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable networking. Use connman instead of networkmanager because it has
  # working iwd support. Saves battery and more reliable.
  services.connman = {
    enable = true;
    wifi.backend = "iwd";
  };

  # TODO Add VPN config to another .nix file because they'll have plaintext passwords.
  # TODO Make little rofi menu to pick VPN service to start based on the same list.

  # Display management!
  services = {
    xserver = {
      enable = true;
      layout = "us";
      enableCtrlAltBackspace = true;
      autoRepeatDelay = 300;
      autoRepeatInterval = 35; # ms between key repeats
      # I don't use caps lock enough, swap it with escape!
      xkbOptions = "caps:swapescape, compose:ralt";

      videoDrivers = [ "intel" ]; # TODO: Pick gpu drivers
      libinput = {
        enable = true;
        scrollMethod = "twofinger";
        naturalScrolling = true;
        tapping = false;
        clickMethod = "clickfinger";
      };

      desktopManager = {
        xterm.enable = false;
        xfce = {
          # Bits of xfce that I need: power-manager, session?, xfsettingsd, xfconf
          # Don't need: xfce4-volumed-pulse, nmapplet
          enable = true;
          noDesktop = true;
          enableXfwm = false;
          thunarPlugins = with pkgs; [
            xfce.thunar-archive-plugin
            xfce.thunar-volman
          ];
        };
      };

      windowManager.bspwm.enable = true;

      displayManager.defaultSession = "none+bspwm";
      # displayManager.gdm.enable = true;
      # displayManager.ly.enable = true;
      displayManager.lightdm = {
        enable = true;
        # background = "${pkgs.nixos-artwork.wallpapers.nix-wallpaper-stripes-logo}/share/artwork/gnome/nix-wallpaper-stripes-logo.png";
        greeters.gtk = {
          enable = true;
          cursorTheme.package = pkgs.bibata-cursors;
          cursorTheme.name = "Bibata_Oil";
          theme.package = pkgs.arc-theme;
          theme.name = "Arc";
          # Match the panel as closely as possible to my polybar config.
          indicators = [ "~power" "~host" "~spacer" "~session" "~clock" ];
          clock-format = "%a %I:%M %p";
        };
      };
    };

    # Shared Emacs server for :zap: speedy-macs
    emacs = {
      enable = true;
      defaultEditor = true;
      package = pkgs.emacsUnstable;
    };

    openssh.enable = true;
    printing.enable = true;
    # Allow easy discovery of network devices (like printers).
    avahi = {
      enable = true;
      nssmdns = true;
    };
    tlp.enable = true; # power saving
    autorandr.enable = true; # monitor presets
    # gnome3.gnome-keyring.enable = true;
    # offlineimap = {
    #   enable = true;
    #   path = [pkgs.mu];
    # };

    # Limit journal size
    journald.extraConfig = ''
      SystemMaxUse=512M
    '';
  };

  users = {
    users.snead = {
      isNormalUser = true;
      home = "/home/snead";
      extraGroups = [ "wheel" "docker" "adbusers" ];
      shell = pkgs.fish;
      hashedPassword =
        "$6$PFZjyXdf7W2cu3$55Iw6UjpcdB29fb4RIPcaYFY5Ehtuc9MFZaJBa9wlRbgYxRrDAP0tlApOiIsQY7hoeO9XG7xxiIcsjGYc9QXu1";
    };
  };
  environment.homeBinInPath = true;

  programs = {
    fish.enable = true;
    dconf.enable = true;
    nm-applet.enable = false;
    java.enable = true;
    adb.enable = true;
    gnupg.agent.enable = true;
    gnupg.agent.enableSSHSupport = true;
    # seahorse.enable = true; # GUI to manage keyring passwords.
  };

  virtualisation.libvirtd.enable = false;

  environment.variables = {
    # Give Firefox precise touchpad scrolling.
    MOZ_USE_XINPUT2 = "1";
  };

  powerManagement.powertop.enable = true;
  location = {
    latitude = 42.35843;
    longitude = -71.05977;
  };
  services.redshift = {
    enable = true;
    temperature.night = 3700;
  };

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  # Do we really need docker always running?
  # virtualisation.docker.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
