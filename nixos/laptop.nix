# Config for Lenovo Ideapad 720s 14-IKB
# Import this file into the main configuration.nix and call it a day.
{ config, lib, pkgs, ... }:
{
  imports = [ ./common.nix ./gui.nix ./vpn.nix ./dev.nix ./email.nix ];

  boot = {
    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    # editor defeats the purpose of all security...
    loader.systemd-boot.editor = false;
    loader.efi.canTouchEfiVariables = true;

    initrd.availableKernelModules = [ "usb_storage" "sd_mod" "bbswitch" ];
    initrd.kernelModules = [ "i915" ];

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
  system.autoUpgrade = {
    enable = true;
    channel = "https://nixos.org/channels/nixos-20.03";
  };

  programs.sway.enable = true;
  # Enables screen sharing on wayland.
  services.pipewire.enable = true;
  services.xserver = {
    windowManager.bspwm.enable = true;
    displayManager.gdm.enable = true;
    displayManager.defaultSession = "sway";
    videoDrivers = [ "intel" ]; # TODO: Pick gpu drivers

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
  };

  # Automatic power saving.
  services.tlp.enable = true;
  powerManagement.powertop.enable = true;
  networking.networkmanager.wifi.powersave = true;

  # Let's try out bluetooth.
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
  # Trim SSD for drive health.
  services.fstrim.enable = true;

  environment.systemPackages = with pkgs; [
    # Power management
    powertop
    brightnessctl

    # apps
    calibre # ebook manager
    mate.atril # pdf viewer
    xfce.parole # video player
    font-manager
    deluge
    bleachbit
    gimp
    discord
    slack
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
    coreOffset = "-120";
    gpuOffset = "-120";
  };
  services.throttled.enable = true;

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
