{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # tools of the trade
    git
    gcc
    gnumake

    # languages
    rustup
    go
    nodejs
    yarn
    kotlin
    python3

    # Spellcheck
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science

    # Misc
    filezilla
  ];

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/f426880dd573ed4e7ac39f5d89e4e9a27a78e4d6.tar.gz";
    }))
  ];

  # Shared Emacs server for :zap: speedy-macs
  services.emacs = {
    enable = true;
    defaultEditor = true;
    package = pkgs.emacsUnstable;
  };
  programs.adb.enable = true;
}
