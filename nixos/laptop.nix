{ config, lib, pkgs, ... }:

{
  boot = {
    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    loader.systemd-boot.editor = false; # editor defeats the purpose of all security...
    loader.efi.canTouchEfiVariables = true;

    initrd.availableKernelModules = ["xhci_pci" "nvme" "usb_storage"
                                     "sd_mod" "bbswitch"];
    # I never use bluetooth
    blacklistedKernelModules = ["btusb" "bluetooth"];

    # boot niceties
    cleanTmpDir = true;
    consoleLogLevel = 3;
  };

  networking.hostName = "loafofpiecrust";

  hardware.nvidiaOptimus.disable = true;
  hardware.opengl.extraPackages = [pkgs.linuxPackages.nvidia_x11.out];
  hardware.opengl.extraPackages32 =
    [pkgs.linuxPackages.nvidia_x11.lib32];

  services.undervolt = {
    enable = true;
    coreOffset = "-110";
    gpuOffset = "-110";
  };

  location = {
    latitude = 42.35843;
    longitude = -71.05977;
  };
  services.redshift = {
    enable = true;
    temperature.night = 3700;
  };
}
