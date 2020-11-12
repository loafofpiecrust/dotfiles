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
    neovim # backup editor of choice, after emacs ;)

    # publishing
    unstable.tectonic # lean latex builds
    pandoc

    # languages
    rustup
    go
    nodejs
    yarn
    kotlin
    python3
    terraform # infrastructure as code
    jq # transforms json documents

    # Spellcheck
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))

    # fancy tools
    any-nix-shell
    awscli

    # formatters + language servers
    editorconfig-core-c
    nixfmt
    html-tidy
    pipenv
    python37Packages.python-language-server
    black
    nodePackages.typescript-language-server
    nodePackages.prettier
    unstable.rust-analyzer

    # dev apps
    staruml # diagrams!
    plantuml # plain-text diagrams!

    # editing!
    emacsCustom
    zstd # compression for emacs session files
    pinentry_emacs
    unstable.nyxt
  ];

  nixpkgs.overlays = [
    (self: super: {
      emacsCustom = (pkgs.emacsWithPackagesFromUsePackage {
        config = builtins.readFile /home/snead/.config/emacs/init.el;
        # Use native-comp branch for speed!
        package = (pkgs.emacsGcc.override { withXwidgets = true; });
        alwaysEnsure = true;
        # A few packages have native dependencies, so I need to add them here.
        extraEmacsPackages = epkgs:
          with epkgs; [
            vterm
            fuz
            ivy-fuz
            undo-tree
            pdf-tools
            plantuml-mode
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
