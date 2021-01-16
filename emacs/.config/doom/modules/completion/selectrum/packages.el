;; -*- no-byte-compile: t; -*-
;;; completion/selectrum/packages.el

(package! selectrum
  :pin "bea5c62"
  :recipe (:branch "master"))
(package! marginalia
  :pin "327ad58bd69a372dc10dbb5b5d3d47f246e28020"
  :recipe (:branch "main"))
(package! embark
  :pin "296ad8077092ff839029c0bdb53c279e921e5354"
  :recipe (:host github :repo "oantolin/embark" :branch "main"))
(package! consult
  :pin "d11cdb239008903158bf4531c19be5d568d6a86d"
  :recipe (:branch "main"))
(package! selectrum-prescient :pin "42adc802d3ba6c747bed7ea1f6e3ffbbdfc7192d")

(package! mini-frame :pin "0912cf4f500403be32735bc50e331fd06910471f")
