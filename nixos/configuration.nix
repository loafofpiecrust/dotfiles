# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./laptop.nix
    ./packages.nix
  ];

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Hermit";
    consolePackages = [pkgs.hermit];
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      # I prefer ISO time and the metric system.
      LC_TIME = "en_DK.UTF-8";
      LC_MEASUREMENT = "en_DK.UTF-8";
    };
    inputMethod = {
      enabled = "ibus";
      ibus.engines = with pkgs.ibus-engines; [libpinyin];
    };
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  networking.networkmanager.enable = true;
  
  # Display management!
  services = {
    xserver = {
      enable = true;
      layout = "us";

      videoDrivers = ["intel"]; # TODO: Pick gpu drivers
      libinput = {
        enable = true;
        scrollMethod = "twofinger";
        tapping = false;
        clickMethod = "clickfinger";
      };

      desktopManager = {
        default = "xfce";
        xterm.enable = false;
        xfce.enable = true;
        xfce.noDesktop = true;
        xfce.enableXfwm = false;
        xfce.thunarPlugins = [
          pkgs.xfce.thunar-archive-plugin
          pkgs.xfce.thunar-volman
        ];
      };
      windowManager.i3 = {
        enable = true;
        package = pkgs.i3-gaps;
      };
      
      displayManager.lightdm = {
        enable = true;
        greeters.gtk = {
          indicators = ["~spacer" "~session" "~clock" "~power"];
          cursorTheme.package = pkgs.bibata-cursors;
          cursorTheme.name = "Bibata Oil";
          theme.package = pkgs.arc-theme;
          theme.name = "Arc";
        };
      };
    };

    compton = {
      enable = true;
      package = pkgs.compton-git;
    };
    openssh.enable = true;
    printing.enable = true;
    tlp.enable = true; # power saving
    tzupdate.enable = true; # automatic timezone by IP
    autorandr.enable = true;
  };
  
  users = {
    users.snead = {
      hashedPassword = 
"$6$PFZjyXdf7W2cu3$55Iw6UjpcdB29fb4RIPcaYFY5Ehtuc9MFZaJBa9wlRbgYxRrDAP0tlApOiIsQY7hoeO9XG7xxiIcsjGYc9QXu1";
      isNormalUser = true;
      home = "/home/snead";
      extraGroups = ["wheel" "networkmanager" "docker"];
      shell = pkgs.fish;
    };
  };

  programs = {
    fish.enable = true;
    dconf.enable = true;
    nm-applet.enable = false;
    java.enable = true;
  };

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
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
