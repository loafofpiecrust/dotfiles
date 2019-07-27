# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./packages.nix
  ];

  networking = {
    hostName = "loafofpiecrust";
    networkmanager.enable = true;
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Hermit";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  
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

      # desktopManager.xfce.enable = true;
      desktopManager = {
        default = "xfce";
        xterm.enable = false;
        xfce.enable = true;
        xfce.noDesktop = true;
        xfce.enableXfwm = false;
      };
      windowManager.i3.enable = true;
      windowManager.i3.package = pkgs.i3-gaps;
      
      displayManager.lightdm = {
        enable = true;
        background = "/home/snead/.background-image";
      };
    };

    compton = {
      enable = true;
      package = pkgs.compton-git;
    };
    openssh.enable = true;
    printing.enable = true;
    tlp.enable = true; # power saving
  };

  # Enable the X11 windowing system.
  # services.xserver.xkbOptions = "eurosign:e";
  
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users = {
    users.snead = {
      hashedPassword = 
"$6$PFZjyXdf7W2cu3$55Iw6UjpcdB29fb4RIPcaYFY5Ehtuc9MFZaJBa9wlRbgYxRrDAP0tlApOiIsQY7hoeO9XG7xxiIcsjGYc9QXu1";
      isNormalUser = true;
      home = "/home/snead";
      extraGroups = ["wheel" "networkmanager"];
      shell = "/run/current-system/sw/bin/fish";
    };
  };

  programs = {
    fish.enable = true;
    dconf.enable = true;
    nm-applet.enable = false;
    java.enable = true;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

}
