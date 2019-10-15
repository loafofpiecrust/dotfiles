#!/bin/sh

stow scripts
stow fonts
stow git
stow rofi
stow term
stow wal
stow wm
stow firefox

# Link our nixos config files into the system path
gksudo ln -sf $(dirname "$0")/nixos/*.nix /etc/nixos/