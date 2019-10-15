#!/bin/sh

stow scripts
stow fonts
stow git
stow rofi
stow term
stow wal
stow wm
stow firefox

gksudo ln -sf $(dirname "$0")/nixos/*.nix /etc/nixos