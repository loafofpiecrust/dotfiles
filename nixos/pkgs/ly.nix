{ config, lib, pkgs, ... }:

with lib;

let
  dmcfg = config.services.xserver.displayManager;
  cfg = config.services.xserver.displayManager.ly;
  lyConfig = pkgs.writeText "config.ini" ''
    [box_main]
    x_cmd=${dmcfg.xserverBin}
    x_cmd_setup=${dmcfg.setupCommands}
    shutdown_cmd=${config.systemd.package}/sbin/shutdown
    save=0
    load=0
  '';
in {

  ###### interface

  options = {
    services.xserver.displayManager.ly = {
      enable = mkOption {
        default = false;
        description = ''
          Enable ly.
        '';
      };
    };
  };

  ###### implementation

  config = mkIf cfg.enable {
    services.xserver = {
      exportConfiguration = true;
      displayManager.job.execCmd = "${pkgs.ly}/bin/ly";
      displayManager.lightdm.enable = lib.mkForce false;
    };
    environment.systemPackages = [ pkgs.ly ];
    environment.etc."ly/config.ini".source = lyConfig;
  };
}
