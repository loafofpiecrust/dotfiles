;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.
;; (add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/"))

(package! tree-sitter
  :pin "d569763c143fdf4ba8480befbb4b8ce1e49df5e2")

(package! tree-sitter-langs
  :pin "d569763c143fdf4ba8480befbb4b8ce1e49df5e2")

;; Highlight markdown comments in certain modes.
(package! theme-changer :pin "61945695a30d678e6a5d47cbe7c8aff59a8c30ea")
(package! ewal)
(package! ewal-doom-themes)
(package! ewal-evil-cursors)

;; Utilities
(package! string-inflection)
;; Show registers before using them!
(package! evil-owl :pin "ed5a98644a9cf321de213b50df6473de9f3a71ed")
(package! olivetti :pin "b76a020aedb57a6a7d0ae61cde13434f5c802a44")

;; Extra languages
(package! graphql-mode)
(package! polymode)
(package! poly-markdown)
(package! poly-org)
(package! literate-calc-mode)
;; Toggle rendered latex preview when point is over it.
;;(package! org-fragtog :disable t)
;; (package! outshine)
;; Better tool for git merges or any 3-way diff.
;; (package! vdiff :disable t)
;; (package! vdiff-magit :disable t)
;; (package! magit-delta :disable t)
;; Edit markdown comments in an indirect buffer.
;; (package! separedit)

;; Show errors inline so they never overlap with code.
(package! flycheck-inline :pin "0662c314bd819f9b46f8b2b13f0c650445b3a8c5")
(package! flycheck-posframe :disable t)
(package! flycheck-popup-tip :disable t)

;; (package! valign
;;   :disable t
;;   :recipe (:host github :repo "casouri/valign"))

;;; Email
(package! org-msg :disable t)
;; Desktop notifications upon receiving new emails.
(package! mu4e-alert :pin "91f0657c5b245a9de57aa38391221fb5d141d9bd")
(package! mu4e-send-delay
  :recipe (:host github :repo "cacology/mu4e-send-delay"))

(package! app-launcher
  :pin "71fb5a501a646703c81783395ff46cdd043e173a"
  :recipe (:host github :repo "SebastienWae/app-launcher"))

;; Manage citations and references with ease in org-mode.
(package! org-ref)
;; Full elisp citation management solution. TBD on quality.
(package! citeproc-org)
;; (package! org-pretty-table)

(package! transmission :disable t)

;; System stuff!
(package! anzu :pin "7b8688c84d6032300d0c415182c7c1ad6cb7f819")
(package! evil-anzu :pin "d3f6ed4773b48767bd5f4708c7f083336a8a8a86")
(package! posframe :built-in 'prefer)
(package! mount-mode
  :disable t
  :recipe (:host github :repo "zellerin/mount-mode"))

(package! org-cv
  :recipe (:host gitlab :repo "loafofpiecrust/org-cv" :branch "explicit-dates"))

;; I don't use fcitx at all.
(package! fcitx :disable t)

;; Solaire slows down scrolling too much, though I like how it looks.
(package! solaire-mode :disable t)

(package! bitwarden
  :recipe (:host github :repo "seanfarley/emacs-bitwarden"))

(package! zoom :pin "a373e7eed59ad93315e5ae88c816ca70404d2d34")

;; TODO vterm is slightly messed up, I'm not quite sure why.
;; (package! vterm :recipe (:no-native-compile t))
(package! treemacs :recipe (:no-native-compile t))
(package! vterm :built-in 'prefer)
(package! undo-tree :disable t :built-in 'prefer)
(package! plantuml-mode :built-in 'prefer)

(package! highlight-numbers :disable t)

(package! emms)

(package! dired-show-readme
  :recipe (:host gitlab :repo "kisaragi-hiu/dired-show-readme"))

(package! pdf-tools :built-in 'prefer)
(package! pdf-continuous-scroll-mode
  :recipe (:host github :repo "dalanicolai/pdf-continuous-scroll-mode.el"))

(package! oauth2)
(package! spotify
  :recipe (:host github :repo "danielfm/spotify.el"))

(package! calibredb)

(package! ace-window
  :pin "57977baeba15b92c987eb7bf824629a9c746a3c8"
  :recipe (:host github :repo "loafofpiecrust/ace-window" :branch "main"))

(package! doom-modeline :built-in 'prefer)

(package! emacs-webkit
  :recipe (:local-repo "emacs-webkit"))

(package! org-caldav)

(package! mini-modeline
  :recipe (:local-repo "emacs-mini-modeline" :no-native-compile t :no-byte-compile t))

(package! evil-collection
  :recipe (:local-repo "~/pie/evil-collection" :no-native-compile t :no-byte-compile t))

(package! which-key
  :recipe (:local-repo "~/pie/emacs-which-key" :no-native-compile t :no-byte-compile t))

(package! general
  :recipe (:local-repo "~/pie/general.el" :no-native-compile t :no-byte-compile t))

(package! svg-icon
  :recipe (:host github :repo "loafofpiecrust/emacs-svg-icon" :branch "icon-submodules"))

(package! hercules
  :recipe (:host gitlab :repo "jjzmajic/hercules.el"))

(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

(package! ct
  :recipe (:host github
           :repo "neeasade/ct.el"
           :branch "master"))


;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
