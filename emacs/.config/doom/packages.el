;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/"))

(package! tree-sitter
  :recipe (:host github :repo "ubolonton/emacs-tree-sitter"
           :files ("lisp/*.el" "src" "Cargo.toml" "Cargo.lock")))
(package! tree-sitter-langs
  :recipe (:host github :repo "ubolonton/emacs-tree-sitter"
           :files ("langs/*.el" "langs/queries")))

;; Highlight markdown comments in certain modes.
;; (package! typer-mode
;;   :recipe (:host gitlab :repo "monnier/typer" :files ("emacs/*.el")))
(package! emojify)
(package! theme-changer :disable t)
(package! ewal)
(package! ewal-doom-themes)
;; (package! ewal-evil-cursors)

;; Extra languages
(package! bazel-mode :disable t)
(package! graphql-mode)
(package! polymode)
(package! poly-markdown)
(package! poly-org :disable t)
(package! org-sticky-header :disable t)
;; Toggle rendered latex preview when point is over it.
(package! org-fragtog :disable t)
;; (package! outshine)
(package! literate-calc-mode)
;; (package! string-inflection)
;; (package! deadgrep)
;; Better tool for git merges or any 3-way diff.
(package! vdiff :disable t)
(package! vdiff-magit :disable t)
(package! magit-delta :disable t)
;; Edit markdown comments in an indirect buffer.
;; (package! separedit)
;; Show registers before using them!
(package! evil-owl)
;; Show errors inline so they never overlap with code.
(package! flycheck-inline
  :recipe (:host github :repo "loafofpiecrust/flycheck-inline" :branch "moving-point"))

(package! valign
  :disable t
  :recipe (:host github :repo "casouri/valign"))

(package! explain-pause-mode
  :recipe (:host github :repo "lastquestion/explain-pause-mode"))

(package! flycheck-posframe :disable t)
(package! flycheck-popup-tip :disable t)

;; (package! mu4e-conversation)
;; Desktop notifications upon receiving new emails.
(package! org-msg :disable t)
(package! mu4e-alert)
(package! mu4e-send-delay
  :disable t
  :recipe (:host github :repo "bennyandresen/mu4e-send-delay"))

(package! olivetti)

;; Manage citations and references with ease in org-mode.
(package! org-ref)
;; Full elisp citation management solution. TBD on quality.
(package! citeproc-org)
;; (package! org-pretty-table)

(package! transmission :disable t)

;; System stuff!
;; TODO Make a doom module instead of just one custom file.
(package! exwm)
(package! exwm-x)
(package! exwm-edit)
(package! desktop-environment)
(package! mini-modeline :disable t)
(package! mini-frame :disable t)
(package! hide-mode-line)
(package! mount-mode
  :recipe (:host github :repo "zellerin/mount-mode"))

(package! highlight-numbers :disable t)

;; Name buffers relative to project root
;; (package! relative-buffers
;;   :recipe (:host github :repo "emacsmirror/relative-buffers"))

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
