{ config, lib, pkgs, ... }:

{
  # Import my custom package definitions.
  nixpkgs.overlays = [ (import ./pkgs) ];

  # TODO Pick a new TTY font.
  console = { keyMap = "us"; };

  i18n = {
    defaultLocale = "en_US.UTF-8";
    # I prefer ISO time and metric, which come with Danish English.
    extraLocaleSettings = let alt = "en_DK.UTF-8";
    in {
      LC_TIME = alt;
      LC_MEASUREMENT = alt;
    };
  };

  # Use pulseaudio for sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable networking. Use connman instead of networkmanager because it has
  # working iwd support. Saves battery and more reliable.
  services.connman = {
    enable = true;
    wifi.backend = "iwd";
  };

  # Allow other machines to ssh in.
  services.openssh.enable = true;

  # Allow easy discovery of network devices (like printers).
  services = {
    avahi.enable = true;
    avahi.nssmdns = true;
    printing.enable = true;
  };

  # Add ~/bin to PATH for all users.
  environment.homeBinInPath = true;

  programs = {
    # Our two lovely shell choices, fish and zsh.
    fish.enable = true;
    zsh = {
      enable = true;
      # Using alternative highlighting package.
      syntaxHighlighting.enable = false;
      enableCompletion = false;
    };
    dconf.enable = true;
    java.enable = true;
    gnupg.agent.enable = true;
    gnupg.agent.enableSSHSupport = true;
    # seahorse.enable = true; # GUI to manage keyring passwords.
  };

  # Clean up derivations older than a week and any garbage lying around.
  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 7d";
  };

  # Limit journal size
  services.journald.extraConfig = ''
    SystemMaxUse=512M
  '';

  # Hmm... Not sure why I explicitly set this.
  virtualisation.libvirtd.enable = false;

  # TODO: Convert these to overlays where possible.
  nixpkgs.config.packageOverrides = pkgs: rec {
    unstable = import <nixos-unstable> {
      # pass the nixpkgs config to the unstable alias
      # to ensure `allowUnfree = true;` is propagated:
      config = config.nixpkgs.config;
    };
    nur = builtins.fetchTarball {
      # Get the revision by choosing a version from https://github.com/nix-community/NUR/commits/master
      url =
        "https://github.com/nix-community/NUR/archive/61eeb89c5553d103b27fc28c0d8eb882049e0dfc.tar.gz";
      # Get the hash by running `nix-prefetch-url --unpack <url>` on the above url
      sha256 = "0qaic8fllwffbxaf3y2yiywlr5pdxr5c39bzfa8k1kbnf3nzp90l";
    };
  };

  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    # system tools
    binutils
    killall
    ripgrep
    htop
    gksu
    unzip
    nnn # file manager
    xfce.gvfs
    gnupg

    # user tools
    stow
    fortune
    playerctl
  ];
}
