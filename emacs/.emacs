;;; Emacs Configuration
;;;; Bootstrap package manager
;; Fix TLS for emacs 26
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; bootstrap package manager (straight.el)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default 1)
(straight-use-package 'use-package)

;; TODO: Map opposite things to C-? and C-S where possible
;; TODO: Use some similar keybinds from bspwm/firefox/etc. [] for buffer nav,
;; TODO: Bind shorter keys to stuff: dumb jump, last position (currently C-o), next position (currently C-i?), 
;; TODO: Bind swiper to a key in normal state
;; TODO: Make frame background translucent
;; TODO: Fix treemacs unable to open new projects! Has to do with terminal color sequences???
;; TODO: Slight margin between line number and text. Slight vertical margin from window top to first line.
;; TODO: Smoother scrolling?
;; TODO: Custom definitions for vdiff colors. Need to highlight partial line changes and not clear syntax highlighting from lines.

;;;; GUI
;; (use-package better-defaults)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode (if (string-equal system-type "darwin") 1 -1))
(setq-default tab-width 4
              indent-tabs-mode nil      ; use spaces for indentation
              fill-column 80
              display-line-numbers-width 3)
(show-paren-mode 1)
(add-hook 'text-mode-hook 'auto-fill-mode)
;; Attempt to scroll less jarringly
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; (setq mouse-wheel-progressive-speed nil)
;; (setq mouse-wheel-follow-mouse t)
;; (setq scroll-step 1)

;; Scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil)

;; Window focus
(setq focus-follows-mouse t
      mouse-autoselect-window -.1)

(fringe-mode '(10 . 0))

(save-place-mode t)

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;;(use-package smooth-scrolling)
;;(smooth-scrolling-mode 1)

;; Empty scratch buffers
(setq initial-scratch-message "")
;; Easier confirmation
(fset 'yes-or-no-p 'y-or-n-p)

;;;; Backup settings
(setq backup-by-copying t ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.cache/emacs")) ; no clutter!
      auto-save-file-name-transforms '((".*" "~/.cache/emacs" t))
      ;; TODO: Clean out ~/.cache/emacs every so often.
      delete-old-versions t
      create-lockfiles nil ; with emacs server there's no need for lockfiles!
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      delete-by-moving-to-trash t)

;;;; Autofill
;;(setq comment-auto-fill-only-comments t)
;;(setq-default auto-fill-function 'do-auto-fill)

;;; UI Packages
;;;; Icons & Emojis
(use-package all-the-icons)
(use-package emojify
  :config
  (setq emojify-emoji-styles '(unicode github))
  (global-emojify-mode t))

;;;; Fancy looks
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Show marks in fringe for lines past EOF
(setq-default indicate-empty-lines t)

;; TODO: Bind keys to navigate TODOs
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;;;; Dashboard!
(use-package page-break-lines)
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  ;; Load in both independent and client windows.
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-items '((recents . 5)
                          (projects . 5)
                          (agenda . 5)))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-heading-icons t))

;;;; Mode line
(use-package diminish) ; hide minor mode lines

(use-package telephone-line
  :config
  (setq telephone-line-lhs '((evil . (telephone-line-evil-tag-segment))
                             (accent . (telephone-line-vc-segment))
                             (nil . (telephone-line-projectile-segment
                                     telephone-line-erc-modified-channels-segment
                                     telephone-line-buffer-segment)))
        telephone-line-rhs '((nil . (telephone-line-misc-info-segment))
                             (accent . (telephone-line-major-mode-segment))
                             (evil . (telephone-line-airline-position-segment)))
        telephone-line-height 16)
  (telephone-line-mode t))

;;;; Additions to ivy
(use-package counsel
  :hook (after-init . counsel-mode))

(use-package flyspell-correct-ivy
  :commands 'flyspell-correct-wrapper)


;;;; Better help
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h C" . helpful-command)))

;;; Editing
;;;; General
(use-package editorconfig
  :hook (prog-mode . editorconfig-mode))
(use-package dtrt-indent
  :hook (prog-mode . dtrt-indent-mode)) ; auto-detect indentation
(use-package whole-line-or-region)

;; Always indent, no matter what
(use-package aggressive-indent
  :hook ((prog-mode . aggressive-indent-mode)
         (lsp-mode . (lambda () (aggressive-indent-mode -1)))))

(use-package highlight-indent-guides
  :config (setq highlight-indent-guides-method 'fill)
  :hook (prog-mode . highlight-indent-guides-mode))

;;;; Expressions
;; Make expression editing easier everywhere
(use-package smartparens
  :config 
  (sp-local-pair '(go-mode rust-mode java-mode) "{" nil :post-handlers '(("||\n[i]" "RET")))
  (require 'smartparens-config)
  :hook ((prog-mode . smartparens-mode)
         ((emacs-lisp-mode lisp-mode) . smartparens-strict-mode)))

(use-package evil-cleverparens
  :hook (smartparens-enabled . evil-cleverparens-mode))

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode))

;;;; org & outlines
(use-package outshine
  :hook (prog-mode . outshine-mode))

;;;; niceties
;; Highlight color codes in the buffer
;; (use-package rainbow-mode
;;   :hook (after-init . rainbow-mode))

;;; Evil mode
;; Show key combo helpers
(use-package which-key
  :init (setq which-key-enable-extended-define-key t
              which-key-idle-delay 0.5)
  :config (which-key-mode t))

;; Easier binding definitions
(use-package general)

;; vim emulation
(use-package evil
  :init (setq evil-want-keybinding nil        ; let evil-collection bind keys
              evil-move-beyond-eol t)
  :config (evil-mode t))

;; bind keys for many modes with better evil compatibility
(use-package evil-collection :after evil
  :custom
  (evil-collection-outline-bind-tab-p t)
  (evil-collection-company-use-tng nil)
  :config (evil-collection-init))

;; commenting lines with verb 'g'
(use-package evil-commentary :after evil
  :config (evil-commentary-mode t))

;; surround things with verb 'S'
(use-package evil-surround
  :config
  (setq-default evil-surround-pairs-alist
                ;; Allow surrounding with newlines
                (cons '(13 . ("\n" . "")) evil-surround-pairs-alist))
  (global-evil-surround-mode t))

;; add more surroundings by default
(use-package evil-embrace
  :config (evil-embrace-enable-evil-surround-integration)
  :hook (org-mode . embrace-org-mode-hook))

;; gotta learn somehow
(use-package evil-tutor)

;; Default some modes to use emacs bindings
;;(dolist (e '(magit-mode))
;;(add-to-list 'evil-emacs-state-modes e))

;;;; setup prefixes
(general-create-definer global-leader-def
  :prefix "<SPC>"
  :keymaps 'override)

(general-create-definer local-leader-def
  :prefix "\\")

;;; Project management
(use-package projectile
  :config (projectile-mode t))

;; Project tree
(use-package treemacs
  :bind ("C-\\" . treemacs))
(use-package treemacs-evil :after treemacs evil)
(use-package treemacs-projectile :after treemacs evil)
(use-package treemacs-magit :after treemacs magit)

;;; Syntax checking
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;; mini-buffer completion
(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-plus)
                                (ivy-bibtex . ivy--regex-ignore-order)
                                ;; Use fuzzy matching for most cases
                                (t . ivy--regex-fuzzy))
        projectile-completion-system 'ivy))

;; TODO: Figure out how to clump these latex packages
(use-package ivy-bibtex :after ivy)

;;; in-buffer completion
(use-package yasnippet)

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-require-match -1))

;; more fuzzy completion
(use-package company-fuzzy
  :hook (company-mode . company-fuzzy-mode))

;; GUI box to prevent interference with different font sizes
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-math :after company auctex)
(use-package company-auctex
  :after company auctex-math
  :config
  (dolist (e '(company-latex-commands
               company-math-symbols-latex
               company-auctex-environments
               company-auctex-macros))
    (add-to-list 'company-backends e)))

;;; Version Control
;;;; Git
(use-package magit)
;; TODO: Rebind magit file bindings behind C-c

;; Provides evil friendly git bindings
(use-package evil-magit
  :after evil magit
  :commands magit-mode)
(use-package forge
  :commands magit-mode) ; connects to GitHub
(use-package github-review
  ;; Only load GitHub review in magit
  :commands magit-mode)

;; Show changed lines in the margin
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-dired-mode t))

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;;;; Diff configuration
;; Replace ediff with vdiff for synced scrolling and more...
(use-package vdiff
  :config
  (setq vdiff-magit-stage-is-2way t)
  (general-def 'normal vdiff-mode-map
    "\\" vdiff-mode-prefix-map))

(use-package vdiff-magit
  :bind (:map magit-mode-map
              ("e" . vdiff-magit-dwim)
              ("E" . vdiff-magit)))

;; Email!
;; (use-package wanderlust)

;;; Spellcheck
;; Enable spellcheck in comments and strings (requires ispell)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(setq flyspell-issue-message-flag nil)

;;; Enable completion, pair matching, line numbers
(add-hook 'after-init-hook (lambda ()
                             (global-subword-mode t)
                             (global-prettify-symbols-mode t)
                             (global-auto-revert-mode t)
                             (global-visual-line-mode t)
                             (column-number-mode t)))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

;; TODO: Bind multiple cursors to some keys. Follow VSCode/Sublime convention?
(use-package multiple-cursors)
(use-package expand-region
  :bind (("M-=" . 'er/expand-region)
         ("M--" . 'er/contract-region))
  :config (general-def 'visual
            ;; TODO: Rebind evil-indent with TAB
            "=" 'er/expand-region
            "-" 'er/contract-region))

;;; Auxiliary Modes
(use-package request)
(use-package restclient :commands restclient-mode)
(use-package dumb-jump
  :config (setq dumb-jump-selector 'ivy))

;;; Programming Languages
;;;; Language Server Protocol!
;; lsp in conjunction with company and flycheck gives us easy auto-complete and
;; syntax checking on-the-fly.
(use-package lsp-mode
  :config (setq lsp-inhibit-message t)
  :hook (((go-mode rust-mode java-mode) . lsp-deferred)
         ;; Format code on save
         (lsp-mode . (lambda ()
                       (add-hook 'before-save-hook 'lsp-format-buffer))))
  :commands (lsp lsp-deferred))

;; Show contextual code documentation pop-ups
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;; Auto-complete languages with LSP support
(use-package company-lsp
  :after lsp-mode
  :config (push 'company-lsp company-backends))

;;;; One liners
(use-package nix-mode)
(use-package bazel-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package go-mode)
(use-package rust-mode)

;;;; Java
(use-package lsp-java)

;;;; javascript and typescript
(use-package eslint-fix :commands eslint-fix)

(defun custom-tide-setup ()
  (tide-setup)
  (tide-hl-identifier-mode 1))
;; TODO: Use eslint before save

(use-package tide
  :hook (typescript-mode . custom-tide-setup))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode)))

(add-hook 'web-mode-hook
          (lambda ()
            (let ((ext (file-name-extension buffer-file-name)))
              (when (or (string-equal ext "tsx")
                        (string-equal ext "jsx"))
                (custom-tide-setup)))))

;; Use eslint for js/ts
;; FIXME!
;; (flycheck-disable-checker 'typescript-tslint)
;;(flycheck-disable-checker 'javascript-jshint)
;;(flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-mode 'javascript-eslint 'typescript-mode)


;;;; GraphQL
(use-package graphql-mode)
;; (use-package company-graphql)
;; (add-to-list 'company-backends 'company-graphql)

;;;; typesetting
(use-package markdown-mode)
(use-package poly-markdown)
;; org-mode additions
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))
(setq org-fontify-emphasized-text t
      org-agenda-files '("~/Documents/agenda"))
;; (use-package org-plus-contrib)

;;;; Latex
;; latex packages have to go near the top for some reason.
;; otherwise they just mysteriously don't load.
;; Because 'auctex is doodled, must use straight directly here.
(straight-use-package 'auctex)
;; latex additions
(add-hook 'TeX-mode-hook 'TeX-fold-mode)
(setq-default TeX-engine 'xetex) ; enables unicode support

;;; Mode-specific keybindings
;;;; global
;; Cntextual leader key as backslash
;; Gneric leader key as space
(add-hook 'after-init-hook
          (lambda ()
            (global-leader-def
              '(normal motion)
              "SPC" 'counsel-M-x
              "b" '("buffers")
              "bo" 'other-buffer
              "bb" 'counsel-switch-buffer
              "bk" 'kill-buffer 
              "bh" 'home
              "f" '("files")
              "ff" 'counsel-find-file
              "fd" 'dired
              "/" 'comment-line
              "e" '("eval")
              "ee" (kbd "C-x C-e")
              "et" (kbd "C-M-x")
              "w" '("windows")
              "wo" 'other-window
              "wk" 'delete-window
              "wj" 'delete-other-windows
              "w <left>" 'evil-window-left
              "w <right>" 'evil-window-right
              "w <up>" 'evil-window-up
              "w <down>" 'evil-window-down
              "t" '("text")
              "tw" '("words")
              "tws" '("spell-check" . flyspell-correct-wrapper)
              "twc" 'count-words
              "g" '("git")
              "gs" 'magit-status
              "n" '("narrowing")
              "nw" 'widen
              "ns" 'outshine-narrow-to-subtree
              "nn" 'org-narrow-to-element
              "nb" 'org-narrow-to-block
              "p" projectile-command-map
              "m" '("modes")
              "mr" 'restclient-mode
              "mc" 'calc
              "j" '("jump")
              "jd" 'dumb-jump-go
              "jg" 'dumb-jump-go-prompt
              "i" '("input method")
              "is" 'set-input-method
              "it" 'toggle-input-method)))

(use-package hungry-delete
  :config (global-hungry-delete-mode))

;;;; vim
(defun split-line-at-point ()
  (interactive)
  (newline)
  (evil-previous-line)
  (evil-end-of-line))

(general-def 'normal
  "U" 'undo-tree-redo
  "K" (kbd "kJ")
  ;; Useful binding for starting method call chains
  "gs" 'split-line-at-point)

;; Fx outline bindings in non-insert states
(general-def '(normal motion)
  "TAB" 'outshine-kbd-TAB)

;; General evil mode overrides
(general-def '(normal insert)
  "C-s" 'save-buffer
  "C-]" 'tab-to-tab-stop)

(general-def 'insert
  "C-SPC" 'company-complete)

;;;; prog-mode
(local-leader-def 'normal
  "]t" 'hl-todo-next
  "[t" 'hl-todo-previous)

;;;; org-mode
(local-leader-def 'normal org-mode-map
  "c" '("clocking")
  "ci" 'org-clock-in
  "co" 'org-clock-out
  "s" 'org-schedule
  "d" 'org-deadline
  "t" '("tables")
  "th" 'org-table-insert-hline
  "a" 'org-agenda
  "l" '("lists")
  "lb" 'org-cycle-list-bullet)

;; Give org-mode some evil keybindings
(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :config (progn (evil-org-set-key-theme)
                 (require 'evil-org-agenda)
                 (evil-org-agenda-set-keys)))

;;;; lsp-mode
(local-leader-def 'normal lsp-mode-map
  "d" 'lsp-describe-thing-at-point
  "r" 'lsp-rename
  "g" '("goto")
  "gi" 'lsp-goto-implementation
  "gt" 'lsp-goto-type-definition
  "f" '("find")
  "fd" 'lsp-find-definition
  "fr" 'lsp-find-references
  "b" '("buffer")
  "bf" 'lsp-format-buffer
  "i" '("imports")
  "io" 'lsp-organize-imports)

;; TODO: Rebind isearch-forward

;;;; go-mode
(local-leader-def 'normal go-mode-map
  "ia" 'go-import-add)

;;; Custom theme
;; Custom theme to use terminal colors best
;; (load-theme 'terminal-wal t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-icons-alist (quote company-box-icons-all-the-icons))
 '(custom-safe-themes
   (quote
    ("a43cda2f075da1534eb50d7dce3ca559276a49c623321d55f68ad8ee218f420e" "5b77b74104748f954929fa3156201a95af9f0f6beb4860e2435cfd00db0219dc" "dbed1a5cfa6470f7a7338a3d9183c6d9439ea3f03fdd73879f60cd128b5ed05e" "b7388ac03767752ade970303768d65dd5d1b47a860308866a56df30ed1a16c2f" "eabaa2ba26896ab0253f87c1a3ba62fe137a44f22965ccd04f89644bead32e75" "4f87a907299c237ec58c634647b44aca5ee636fb7861da19a9defa0b0658b26e" default)))
 '(evil-collection-company-use-tng nil)
 '(evil-collection-outline-bind-tab-p t)
 '(lsp-rust-server (quote rust-analyzer)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(default ((t (:inherit nil :stipple nil :background "#12131f" :foreground "#cfffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 115 :width normal :foundry "PfEd" :family "SF Mono"))))
 '(org-document-title ((t (:weight bold :height 1.6))))
 '(org-level-1 ((t (:height 1.35))))
 '(org-level-2 ((t (:height 1.2))))
 '(org-level-3 ((t (:height 1.1))))
 '(outline-1 ((t (:inherit org-level-1))))
 '(outline-2 ((t (:inherit org-level-2))))
 '(outline-3 ((t (:inherit org-level-3))))
 '(outline-4 ((t (:inherit org-level-4))))
 '(outline-5 ((t (:inherit org-level-5))))
 '(outline-6 ((t (:inherit org-level-6))))
 '(outline-7 ((t (:inherit org-level-7))))
 '(outline-8 ((t (:inherit org-level-8)))))

(use-package doom-themes)
;; (load-theme 'doom-Iosvkem t)
(use-package gruvbox-theme)
(load-theme 'gruvbox t)
