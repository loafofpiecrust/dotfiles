# Config for Lenovo Ideapad 720s 14-IKB
# Import this file into the main configuration.nix and call it a day.
{ config, lib, pkgs, ... }: {
  imports = [ ./common.nix ./gui.nix ./vpn.nix ./dev.nix ./email.nix ];

  boot = {
    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    # editor defeats the purpose of all security...
    loader.systemd-boot.editor = false;
    loader.efi.canTouchEfiVariables = true;

    initrd.availableKernelModules = [ "usb_storage" "sd_mod" "bbswitch" ];
    initrd.kernelModules = [ "i915" ];

    kernelModules = [ "acpi_call" ];
    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];

    # boot niceties
    cleanTmpDir = true;
    consoleLogLevel = 3;

    # kernel options
    kernelParams =
      [ "pcie_aspm.policy=powersave" "i915.enable_fbc=1" "i915.enable_psr=2" ];
    kernel.sysctl = {
      "kernel.nmi_watchdog" = 0;
      "vm.swappiness" = 1;
    };
  };

  networking.hostName = "loafofpiecrust";
  # FIXME: Open just the ports needed for chromecast.
  networking.firewall.enable = false;

  users.users.snead = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "adbusers" ];
    shell = pkgs.fish;
    hashedPassword =
      "$6$PFZjyXdf7W2cu3$55Iw6UjpcdB29fb4RIPcaYFY5Ehtuc9MFZaJBa9wlRbgYxRrDAP0tlApOiIsQY7hoeO9XG7xxiIcsjGYc9QXu1";
  };

  # Change this to the primary package channel we want to use.
  # system.autoUpgrade = {
  #   enable = true;
  #   channel = "https://nixos.org/channels/nixos-20.03";
  # };

  programs.sway.enable = true;
  # Enables screen sharing on wayland.
  services.pipewire.enable = true;
  services.xserver = {
    # windowManager.exwm.enable = true;
    # windowManager.exwm.enableDefaultConfig = false;
    displayManager.gdm.enable = true;
    displayManager.defaultSession = "sway";
    videoDrivers = [ "intel" ]; # TODO: Pick gpu drivers

    desktopManager = {
      xterm.enable = false;
      xfce = {
        # Bits of xfce that I need: power-manager, session?, xfsettingsd, xfconf
        # Don't need: xfce4-volumed-pulse, nmapplet
        enable = false;
        noDesktop = true;
        enableXfwm = false;
        thunarPlugins = with pkgs; [
          xfce.thunar-archive-plugin
          xfce.thunar-volman
        ];
      };
    };
  };
  services.xserver.windowManager.session = lib.singleton {
    name = "exwm";
    # TODO Try having these IM exports just in Emacs.
    start = ''
      # export XMODIFIERS=@im=exim
      # export GTK_IM_MODULE=xim
      # export QT_IM_MODULE=xim
      # export CLUTTER_IM_MODULE=xim
      # export QT_QPA_PLATFORM=xcb
      export MOZ_ENABLE_WAYLAND=0
      export SDL_VIDEODRIVER=x11
      xrdb ~/.Xdefaults
      ${pkgs.gnome3.gnome-settings-daemon}/libexec/gnome-settings-daemon &
      EMACS_EXWM=t ${pkgs.dbus}/bin/dbus-launch --exit-with-session ${pkgs.emacsCustom}/bin/emacs -mm
    '';
  };
  # displayManager.sessionCommands = "${pkgs.xorg.xhost}/bin/xhost +SI:localuser:$USER";

  # Automatic power saving.
  services.tlp.enable = true;
  powerManagement.powertop.enable = true;
  networking.networkmanager.wifi.powersave = true;

  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
  };
  # virtualisation.docker.enable = true;

  # Let's try out bluetooth.
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Trim SSD for drive health.
  services.fstrim.enable = true;

  environment.systemPackages = with pkgs; [
    xkbset
    # Power management
    powertop
    brightnessctl

    # apps
    gnome3.gnome-settings-daemon
    calibre # ebook manager
    mate.atril # pdf viewer
    xfce.parole # video player
    font-manager
    deluge
    gimp
    krita
    vlc
    inkscape
    # audacity
    xfce.xfce4-power-manager
    xfce.thunar
    xfce.xfce4-session
    xfce.xfce4-settings
    xfce.xfce4-taskmanager

    # communication
    discord
    slack
    teams

    # music
    mpd
    mpc_cli

    # misc
    ledger
  ];

  # Enable NVIDIA GPU
  # hardware.bumblebee.enable = true;
  # hardware.nvidia.prime.offload.enable = true;
  # hardware.nvidia.prime = {
  #   offload.enable = true;
  #   intelBusId = "PCI:0:2:0";
  #   nvidiaBusId = "PCI:60:0:0";
  # };

  # Use newer Intel Iris driver. This fixes screen tearing for me!
  environment.variables = { MESA_LOADER_DRIVER_OVERRIDE = "iris"; };
  hardware.opengl.package = (pkgs.mesa.override {
    galliumDrivers = [ "nouveau" "virgl" "swrast" "iris" ];
  }).drivers;

  # Undervolt to hopefully fix thermal throttling and fan issues.
  services.undervolt = {
    enable = true;
    coreOffset = -130;
    gpuOffset = -130;
  };
  services.throttled.enable = true;

  # Make the screen color warmer at night.
  services.redshift = { enable = true; };
  location.provider = "geoclue2";

  # Use newer intel graphics drivers.
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      # linuxPackages.nvidia_x11.out
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-media-driver
    ];
    # extraPackages32 = [ pkgs.linuxPackages.nvidia_x11.lib32 ];
  };

  # Only log out when the lid is closed with power.
  services.logind.lidSwitchExternalPower = "lock";
}
