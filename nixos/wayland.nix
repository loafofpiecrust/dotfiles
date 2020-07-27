{ config, lib, pkgs, ... }:
let
  rev = "757097f0374fb1654caf3674af8f33a83e1d2dc0";
  url = "https://github.com/colemickens/nixpkgs-wayland/archive/${rev}.tar.gz";
  waylandOverlay = (import (builtins.fetchTarball url));
in {
  nixpkgs.overlays = [ waylandOverlay ];
  environment.systemPackages = with pkgs; [
    wdisplays
    wlogout
    unstable.meson
  ];
}
