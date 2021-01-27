;; -*- no-byte-compile: t; -*-
;;; completion/selectrum/packages.el

(package! selectrum
  :pin "257224bc7c5e97a52cc56cef2930f548af3a83e9"
  :recipe (:branch "master"))
(package! marginalia
  :pin "5a00dcb9a29f3c8bdb7087b919736045bb845974"
  :recipe (:branch "main"))
(package! embark
  :pin "0fb381caacbadd5c697705e68bee4f5aba7b0c07"
  :recipe (:host github :repo "oantolin/embark" :branch "main"))
(package! consult
  :pin "094e8fcf2c5aaf24183313153a398832f714442d"
  :recipe (:branch "main"))
(package! selectrum-prescient :pin "42adc802d3ba6c747bed7ea1f6e3ffbbdfc7192d")

(package! mini-frame :pin "0912cf4f500403be32735bc50e331fd06910471f")
