{ config, lib, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # tools of the trade
    git
    git-lfs
    gcc
    gnumake
    tectonic # build latex projects
    pandoc

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

    # fancy tools
    any-nix-shell

    # formatters + language servers
    nixfmt
    html-tidy
    pipenv
    python37Packages.python-language-server
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
