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
stow emacs

# Firefox profile setup.
FIREFOX_PROFILE=$(find ~/.mozilla/firefox -name "*.default")
ln -sf ~/.mozilla/firefox/profile/* $FIREFOX_PROFILE

# Link our nixos config files into the system path
sudo stow -t /etc/nixos nixos
