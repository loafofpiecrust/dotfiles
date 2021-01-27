;; -*- no-byte-compile: t; -*-
;;; desktop/exwm/packages.el

(package! xelb :built-in 'prefer)
(package! exwm :built-in 'prefer)
(package! exwm-edit :pin "2fd9426922c8394ec8d21c50dcc20b7d03af21e4")
(package! desktop-environment :pin "dbcf3d9411d53908de09ab0d34932d19c8117144")
(package! exwm-outer-gaps
  :recipe (:host github :repo "lucasgruss/exwm-outer-gaps"))
