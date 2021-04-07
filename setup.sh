#!/bin/sh

stow env
stow aspell
stow emacs
stow email
stow firefox
stow fonts
stow git
stow scripts
stow rofi
stow term
stow wal
stow wm

# Firefox profile setup.
FIREFOX_PROFILE=$(find ~/.mozilla/firefox -name "*.default")
ln -sf ~/.mozilla/firefox/profile/* $FIREFOX_PROFILE

# Link our nixos config files into the system path
# sudo stow -t /etc/nixos nixos

# Symlink ld to system folders for binary compatibility.
# sudo mkdir -p /lib64
# sudo ln -s $(which ld) /lib64/
# sudo ln -s $(nix eval nixos.glibc.outPath)/lib64/ld-linux-x86-64.so.2 /lib64/

mu init -m ~/.mail --my-address=taylor@snead.xyz --my-address=snead.t@northeastern.edu --my-address=taylorsnead@gmail.com
