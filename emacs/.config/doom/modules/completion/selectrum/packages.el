;; -*- no-byte-compile: t; -*-
;;; completion/selectrum/packages.el

(package! selectrum
  :pin "015798542b441d993d53b967d49df3dc0162ca37"
  :recipe (:branch "master"))
(package! marginalia
  :pin "3e061a0fb5305389af5b3da17092f2f09fe92c69"
  :recipe (:branch "main"))
(package! embark
  :pin "26e73117910e78afa209524ecb8f07add45a9ec3"
  :recipe (:host github :repo "oantolin/embark" :branch "main"))
(package! consult
  :pin "b8ab017488b3f04bbfdc1a9610af3224339d943c"
  :recipe (:branch "main"))
(package! selectrum-prescient :pin "42adc802d3ba6c747bed7ea1f6e3ffbbdfc7192d")

(when (featurep! +childframe)
  (package! mini-frame :pin "41afb3d79cd269726e955ef0896dc077562de0f5"))
