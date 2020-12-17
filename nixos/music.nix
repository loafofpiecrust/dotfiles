{ config, lib, pkgs, ... }:

{
  services.mopidy = {
    enable = true;
    extensionPackages = with pkgs; [
      # sources
      mopidy-local
      mopidy-spotify
      mopidy-youtube
      # extensions
      mopidy-mpris
      mopidy-mpd
    ];
    configuration = ''
      [local]
      media_dir = /home/snead/Music
                '';
  };
}
