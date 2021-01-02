;; -*- no-byte-compile: t; -*-
;;; completion/selectrum/packages.el

(package! selectrum
  :pin "de66b925655c40d6fe08e895b8a4f591e4a97dc6"
  :recipe (:branch "main"))
(package! marginalia
  :pin "56ad689886e618616f9ee282c3d8d57f8270450b"
  :recipe (:branch "main"))
(package! embark
  :pin "f84e11aaa60ede27d562afa314e4a1ee0a1ab5df"
  :recipe (:host github :repo "oantolin/embark" :branch "main"))
(package! consult
  :pin "ccbe9a91a264f8e7517e6096212874d66cc8e070"
  :recipe (:branch "main"))
(package! consult-selectrum
  :pin "ccbe9a91a264f8e7517e6096212874d66cc8e070"
  :recipe (:branch "main"))
(package! selectrum-prescient :pin "5d139e5b1fe03ccaddff8c250ab8e9d795071b95")
(package! ripgrep)

(package! mini-frame)
