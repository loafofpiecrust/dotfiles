;; -*- no-byte-compile: t; -*-
;;; completion/selectrum/packages.el

(package! selectrum
  :pin "1a5b2855f7944ce43667bebfde3497a231f5fc0d"
  :recipe (:branch "master"))
(package! marginalia
  :pin "86c0461271d407f5676a8af3776e73832458364f"
  :recipe (:branch "main"))
(package! embark
  :pin "75cb2b0fae4c6bcb851224ef8630870d14bdc6f1"
  :recipe (:host github :repo "oantolin/embark" :branch "main"))
(package! consult
  :pin "0bc505b2cac3dd42694b5c74b741a78b27c2429d"
  :recipe (:branch "main"))
(package! selectrum-prescient :pin "42adc802d3ba6c747bed7ea1f6e3ffbbdfc7192d")

(package! mini-frame :pin "0912cf4f500403be32735bc50e331fd06910471f")
