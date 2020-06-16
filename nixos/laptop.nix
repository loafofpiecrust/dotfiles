{ config, lib, pkgs, ... }: {
  # Add services for any VPN connections we want.
  imports = [ ./vpn.nix ];

  boot = {
    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    # editor defeats the purpose of all security...
    loader.systemd-boot.editor = false;
    loader.efi.canTouchEfiVariables = true;

    initrd.availableKernelModules =
      [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "bbswitch" ];
    # I never use bluetooth
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
  networking.networkmanager.wifi.powersave = true;

  # Let's try out bluetooth.
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
  # Trim SSD for drive health.
  services.fstrim.enable = true;

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

  services.undervolt = {
    enable = true;
    coreOffset = "-120";
    gpuOffset = "-120";
  };
  services.throttled.enable = true;

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      linuxPackages.nvidia_x11.out
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-media-driver
    ];
    extraPackages32 = [ pkgs.linuxPackages.nvidia_x11.lib32 ];
  };

  # Only log out when the lid is closed with power.
  services.logind.lidSwitchExternalPower = "lock";
}
