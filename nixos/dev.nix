{ config, lib, pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    # tools of the trade
    git
    git-lfs
    gcc
    gnumake
    cmake
    automake
    autoconf
    libtool
    direnv

    # publishing
    tectonic
    pandoc

    # languages
    rustup
    go
    nodejs
    yarn
    kotlin
    python3
    terraform

    # Spellcheck
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science

    # fancy tools
    any-nix-shell

    # formatters + language servers
    emacsCustom
    nixfmt
    html-tidy
    pipenv
    python37Packages.python-language-server
    nodePackages.typescript-language-server
  ];

  nixpkgs.overlays = [
    (self: super: {
      libgccjit = pkgs.unstable.libgccjit;
      emacs = pkgs.unstable.emacs;
      # emacsCustom = pkgs.unstable.emacs;
      emacsCustom = (pkgs.emacsWithPackagesFromUsePackage {
        config = builtins.readFile /home/snead/.config/emacs/init.el;
        package = pkgs.emacsGcc.override { withXwidgets = true; };
        alwaysEnsure = true;
        # A few packages have native dependencies, so I need to add them here.
        extraEmacsPackages = epkgs: [
          epkgs.emacs-libvterm
          epkgs.fuz
          epkgs.ivy-fuz
        ];
      });
    })
  ];

  # Shared Emacs server for :zap: speedy-macs
  services.emacs = {
    enable = false;
    defaultEditor = true;
    package = pkgs.emacsCustom;
  };
  # Android debugging.
  programs.adb.enable = true;

  # Persistent environment in shell execution!
  services.lorri.enable = true;
}
