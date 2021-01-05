;; -*- no-byte-compile: t; -*-
;;; completion/selectrum/packages.el

(package! selectrum
  :pin "4106b216f9b3ccf8960abe89fb9863c570a6f376"
  :recipe (:branch "main"))
(package! marginalia
  :pin "51b6dbf61f0ed65689e0685da6ab0b59d0220b58"
  :recipe (:branch "main"))
(package! embark
  :pin "f84e11aaa60ede27d562afa314e4a1ee0a1ab5df"
  :recipe (:host github :repo "oantolin/embark" :branch "main"))
(package! consult
  :pin "4fe41a5aea26ca98ef6e1861d88fe6c3a7a77d3b"
  :recipe (:branch "main"))
(package! consult-selectrum
  :pin "4fe41a5aea26ca98ef6e1861d88fe6c3a7a77d3b"
  :recipe (:branch "main"))
(package! selectrum-prescient :pin "42adc802d3ba6c747bed7ea1f6e3ffbbdfc7192d")
(package! ripgrep)

(package! mini-frame :pin "0912cf4f500403be32735bc50e331fd06910471f")
