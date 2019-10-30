#!/bin/sh

stow scripts
stow fonts
stow git
stow rofi
stow term
stow wal
stow wm
stow firefox
stow system

# Link our nixos config files into the system path
sudo stow -t /etc/nixos nixos