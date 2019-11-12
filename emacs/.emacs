;;; Bootstrap package manager
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
;; TODO: widen neotree
;; TODO: Use some similar keybinds from bspwm/firefox/etc. [] for buffer nav,
;; arrows for window nav,
;; TODO: Get list-packages to work with straight? At least hook up the same
;; repos so we can use it to find packages. Add evil-mode to *Packages* buffer
;; for better search.

;;; General built-in Configuration
;; (use-package better-defaults)
(tool-bar-mode -1)
(menu-bar-mode (if (string-equal system-type "darwin") 1 -1))
(scroll-bar-mode -1)
(setq-default tab-width 4
              indent-tabs-mode nil ; use spaces for indentation
              fill-column 80
              display-line-numbers-width 3)
(show-paren-mode 1)
(add-hook 'text-mode-hook 'auto-fill-mode)
;; Attempt to scroll less jarringly
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; (setq mouse-wheel-progressive-speed nil)
;; (setq mouse-wheel-follow-mouse t)
;; (setq scroll-step 1)
;; ;;(setq frame-background-mode 'dark)
(setq delete-by-moving-to-trash t)       ; use system trash

(fringe-mode '(12 . 0))

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;;(use-package smooth-scrolling)
;;(smooth-scrolling-mode 1)
;; ;; TODO: Apply some WYSIWYG styling to org-mode text (italics, bolds, etc)

;; Empty scratch buffers
(setq initial-scratch-message "")
;; Easier confirmation
(fset 'yes-or-no-p 'y-or-n-p)

;;; Give me outlines!
(use-package outshine
  :hook (prog-mode . outshine-mode))

;;; UI Packages
(use-package all-the-icons)
;; Show key combo helpers
(use-package which-key
  :init (setq which-key-enable-extended-define-key t
              which-key-idle-delay 0.5)
  :config (which-key-mode t))

;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimeters-mode))

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
  :config (telephone-line-mode t))

;;;; Additions to ivy
(use-package counsel
  :hook (after-init . counsel-mode))

(use-package flyspell-correct-ivy
  :commands 'flyspell-correct-wrapper)


;;; Primary Keybindings (evil)
(use-package general)
(use-package evil
  :init (setq evil-want-keybinding nil        ; let evil-collection bind keys
              evil-move-beyond-eol t)
  :config (evil-mode t))
(use-package evil-collection :after evil
  :custom
  (evil-collection-outline-bind-tab-p t)
  (evil-collection-company-use-tng nil)
  :config (evil-collection-init))
(use-package evil-commentary :after evil
  :config (evil-commentary-mode t))

;; Default some modes to use emacs bindings
;;(dolist (e '(magit-mode))
;;(add-to-list 'evil-emacs-state-modes e))

(general-def 'normal
  "U" 'undo-tree-redo)

;; Fix outline bindings in non-insert states
(general-def '(normal motion)
  "TAB" 'outshine-kbd-TAB)

;; General evil mode overrides
(general-def '(normal insert)
  "M-j" 'move-text-down
  "M-k" 'move-text-up)

(general-create-definer global-leader-def
  :prefix "<SPC>"
  :keymaps 'override)

;; Contextual leader key as comma
;; Generic leader key as space
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
             "w<left>" 'evil-window-left
             "w<right>" 'evil-window-right
             "w<up>" 'evil-window-up
             "w<down>" 'evil-window-down
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
             "jd" 'dumb-jump-go)))

;;;; Mode-local keybindings
(general-create-definer local-leader-def :prefix "\\")
(local-leader-def 'normal org-mode-map
                  "c" '("clocking")
                  "ci" 'org-clock-in
                  "co" 'org-clock-out
                  "s" 'org-schedule
                  "d" 'org-deadline
                  "t" '("tables")
                  "tih" 'org-table-insert-hline
                  "a" 'org-agenda)

;;; Editing Convenience
(use-package editorconfig
  :hook (prog-mode . editorconfig-mode))
(use-package dtrt-indent
  :hook (prog-mode . dtrt-indent-mode)) ; auto-detect indentation
(use-package move-text ; TODO: replace. works weird with selected region.
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))
(use-package whole-line-or-region)

(use-package smartparens
  :config (require 'smartparens-config)
  :hook ((prog-mode . smartparens-mode)
         ((emacs-lisp-mode lisp-mode) . smartparens-strict-mode)))

(use-package evil-cleverparens
  :hook (smartparens-enabled . evil-cleverparens-mode))

;; Make expression editing easier everywhere
;; (use-package lispy
;;   :hook ((emacs-lisp-mode lisp-mode) . lispy-mode))
;; (use-package lispyville
;;   :config (electric-pair-mode t)
;;   :hook (prog-mode . lispyville-mode))

;; Always indent, no matter what
(use-package aggressive-indent
  :config (global-aggressive-indent-mode t))

;;; Project management
(use-package projectile
  :config (projectile-mode t))

;; Project tree
(use-package treemacs
  :bind ("C-\\" . treemacs))
(use-package treemacs-evil :after treemacs evil)
(use-package treemacs-projectile :after treemacs evil)
(use-package treemacs-magit :after treemacs magit)

;; TODO: Rebind 'toggle-input-method (for multilingual input)?

;;; Latex
;; latex packages have to go near the top for some reason.
;; otherwise they just mysteriously don't load.
;; Because 'auctex is doodled, must use straight directly here.
(straight-use-package 'auctex)
;; latex additions
(add-hook 'TeX-mode-hook 'TeX-fold-mode)
(setq-default TeX-engine 'xetex) ; enables unicode support


;;; Essential packages
(use-package yasnippet)
(use-package emojify
  :config
  (setq emojify-emoji-styles '(unicode github))
  (global-emojify-mode t))

;;; Syntax checking
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;; Ivy and mini-buffer completion
(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (ivy-bibtex . ivy--regex-ignore-order)
          ;; Use fuzzy matching for most cases
          (t . ivy--regex-fuzzy)))
  (setq projectile-completion-system 'ivy))

;; TODO: Figure out how to clump these latex packages
(use-package ivy-bibtex :after ivy)

;;; Code Completion (in-buffer)
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-require-match -1))

;; show docs in popup!
;;(use-package company-quickhelp
;;  :hook (company-mode . company-quickhelp-mode))

;; Completion for LaTeX macros
(use-package company-math :after company auctex)
(use-package company-auctex
  :after company auctex-math
  :config
  (dolist (e '(company-latex-commands
               company-math-symbols-latex
               company-auctex-environments
               company-auctex-macros))
    (add-to-list 'company-backends e)))

;;; Language Server Protocol!
;; lsp in conjunction with company and flycheck gives us easy auto-complete and
;; syntax checking on-the-fly.
(use-package lsp-mode
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

;;; Version Control
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
  (general-define-key :states 'normal
                      :keymaps vdiff-mode-map
                      "," vdiff-mode-prefix-map))

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

;;; Backup settings
(setq backup-by-copying t ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.cache/emacs")) ; no clutter!
      ;; TODO: Clean out ~/.cache/emacs every so often.
      delete-old-versions t
      create-lockfiles nil ; with emacs server there's no need for lockfiles!
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;; Enable completion, pair matching, line numbers
(add-hook 'after-init-hook (lambda ()
                             (global-visual-line-mode t)
                             (column-number-mode t)))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package multiple-cursors)
(use-package expand-region
  :bind (("M-=" . 'er/expand-region)
         ("M--" . 'er/contract-region)))

;; Highlight color codes in the buffer
(use-package rainbow-mode
  :hook (after-init . rainbow-mode))

;;; Auxiliary Modes
(use-package request)
(use-package restclient :commands restclient-mode)
(use-package dumb-jump
  :config (setq dumb-jump-selector 'ivy)
  :commands dumb-jump-go)

;;; Programming Languages
;;;; One liners
(use-package nix-mode :commands nix-mode)
(use-package bazel-mode :commands bazel-mode)
(use-package yaml-mode :commands yaml-mode)
(use-package json-mode :commands json-mode)
(use-package go-mode :commands go-mode)
(use-package rust-mode :commands rust-mode)

;;;; Java
(use-package lsp-java :commands java-mode)

;;;; GraphQL
(use-package graphql-mode :commands graphql-mode)
;; (use-package company-graphql)
;; (add-to-list 'company-backends 'company-graphql)

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


;;;; typesetting
(use-package markdown-mode :commands markdown-mode)
(use-package poly-markdown :after markdown-mode)
;; org-mode additions
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))
(setq org-fontify-whole-heading-line t)
;; (use-package org-plus-contrib)
;;; Custom key bindings
(use-package hungry-delete
  :config (global-hungry-delete-mode))

(general-define-key
 "M-SPC" 'company-complete
 "C-s" 'save-buffer
 "C-]" 'tab-to-tab-stop)

;; TODO: Rebind isearch-forward
;; TODO: Backspace works weird in terminal...?!
;; Useful commands to rebind/learn: transpose-words, downcase-word, pop-mark

(cua-mode t) ; normal copy-paste bindings (must go near end)

;;; Considering spacemacs
;; Spacemacs
;; Pros: built-in SPC bindings, all modes consistent bindings via layers,
;; everything built-in, easier config?, layers for new pkgs are optional
;; Cons: layer abstraction, installs lots i don't use, would have to
;; revert default theme, little direct control of config, some obsolete stuff
;; installed, no latex biz, changing default bindings is a pain!, more shit to
;; pull to setup on another machine.

;; Pros of my own config: learning it all, vanilla file structure, only bind the
;; commands i actually use, term colors already
;; Cons: long-ass config file, takes time to get evil mode working with all the
;; aux modes i'll use (but not too many aux modes!)

;;; Autofill
;;(setq comment-auto-fill-only-comments t)
;;(setq-default auto-fill-function 'do-auto-fill)

;;; Custom theme
;; Custom theme to use terminal colors best
;; (load-theme 'terminal-wal t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(default ((t (:inherit nil :stipple nil :background "#12131f" :foreground "#cfffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "PfEd" :family "SF Mono")))))

(use-package doom-themes)
;; (load-theme 'doom-Iosvkem t)
(use-package gruvbox-theme)
(load-theme 'gruvbox t)
