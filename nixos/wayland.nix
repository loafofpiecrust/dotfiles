{ config, lib, pkgs, ... }:
let
  rev = "3717212bae16abb2711dcc9fee5ade978e749287";
  url = "https://github.com/colemickens/nixpkgs-wayland/archive/${rev}.tar.gz";
  waylandOverlay = (import (builtins.fetchTarball url));
in {
  nixpkgs.overlays = [ waylandOverlay ];
  environment.systemPackages = with pkgs; [
    mako # notifications
    waybar
    bspwc
    wdisplays
    wlogout
    wofi
  ];
}
