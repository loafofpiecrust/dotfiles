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
(menu-bar-mode -1)
(setq-default tab-width 4
              fill-column 80)
(show-paren-mode 1)
(add-hook 'text-mode-hook 'auto-fill-mode)
(setq-default indent-tabs-mode nil) ; spaces by default
;; Attempt to scroll less jarringly
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)
(setq scroll-step 1)
(setq frame-background-mode 'dark)

;; TODO: Apply some WYSIWYG styling to org-mode text (italics, bolds, etc)

;;; Give me outlines!
(use-package outshine
  :hook (prog-mode . outshine-mode))


;;; UI Packages
(use-package which-key
  :init
  (setq which-key-enable-extended-define-key t)
  (setq which-key-idle-delay 0.5)
  :config (which-key-mode))


;;;; Mode line
(use-package diminish) ; hide minor mode lines
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package counsel
  :config (counsel-mode))

(use-package flyspell-correct-ivy)


;;; Editing
(use-package evil
  :config (progn
            (evil-mode t)))
(use-package evil-leader
  :config (progn
            (setq evil-leader/in-all-states t)
            (global-evil-leader-mode)))

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
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
  "w" '("windows")
  "wo" 'other-window
  "wk" 'delete-window
  "x" '("text")
  "xw" '("words")
  "xws" '("spell-check" . flyspell-correct-wrapper)
  "xwc" 'count-words
  "g" 'magit-status)

(use-package dtrt-indent) ; auto-detect indentation
(use-package move-text ; TODO: replace. works weird with selected region.
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))
(use-package whole-line-or-region)



;;; Project management
(use-package projectile)
(use-package neotree
  :commands neotree-toggle
  :config
  (setq neo-smart-open t)
  ;; Rebind neotree commands for evil mode
  (evil-define-key 'normal neotree-mode-map
    "TAB" 'neotree-enter
    "RET" 'neotree-enter
    "g" 'neotree-refresh
    "." 'neotree-hidden-file-toggle))

;; reload tree automatically on project switch
(setq projectile-switch-project-action 'neotree-projectile-action)

(defun neotree-project-dir ()
    "Open NeoTree at the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

(global-set-key (kbd "C-\\") 'neotree-project-dir)
;; TODO: Rebind 'toggle-input-method (for multilingual input)?


;; latex packages have to go near the top for some reason.
;; otherwise they just mysteriously don't load.
;; Because 'auctex is doodled, must use straight directly here.
(straight-use-package 'auctex)
;; latex additions
(add-hook 'TeX-mode-hook 'TeX-fold-mode)
(setq-default TeX-engine 'xetex) ; enables unicode support


;; Essential packages
(use-package yasnippet)
;;(use-package emojify) ; TODO: Fix unicode emoji font...
  ;; :hook (after-init . global-emojify-mode)
  ;; :init (setq emojify-display-style 'unicode))

;;; Syntax checking
(use-package flycheck
  :hook (after-init . global-flycheck-mode))


;;; Ivy and mini-buffer completion
(use-package ivy :hook (after-init . ivy-mode))
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-re-builders-alist
      '((ivy-switch-buffer . ivy--regex-plus)
        (ivy-bibtex . ivy--regex-ignore-order)
        ;; Use fuzzy matching for most cases
        (t . ivy--regex-fuzzy)))
(setq projectile-completion-system 'ivy)

;; TODO: Figure out how to clump these latex packages
(use-package ivy-bibtex)


;;; Code Completion (in-buffer)
(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-require-match -1))

;; show docs in popup!
(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

;; Completion for LaTeX macros
(use-package company-math)
(use-package company-auctex)
(push 'company-latex-commands company-backends)
(push 'company-math-symbols-latex company-backends)
(push 'company-auctex-environments company-backends)
(push 'company-auctex-macros company-backends)


;;; Language Server Protocol!
;; lsp in conjunction with company and flycheck gives us easy auto-complete and
;; syntax checking on-the-fly.
(use-package lsp-mode
  :hook (((go-mode rust-mode) . lsp-deferred)
         ;; Format code on save
         (lsp-mode . (lambda ()
                       (add-hook 'before-save-hook 'lsp-format-buffer))))
  :commands (lsp lsp-deferred)) ; language server protocol

;; Show contextual code documentation pop-ups
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;; Auto-complete languages with LSP support
(use-package company-lsp)
(push 'company-lsp company-backends)



;;; Version Control
(use-package magit)
(use-package forge :after magit) ; connects to GitHub
;; Show changed lines in the margin
(use-package diff-hl
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode))


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
                             (electric-pair-mode)
							 (global-display-line-numbers-mode)
                             (column-number-mode)))

(use-package smart-hungry-delete)
(smart-hungry-delete-add-default-hooks)

(use-package multiple-cursors)
(use-package expand-region
  :bind (("M-=" . 'er/expand-region)
         ("M--" . 'er/contract-region)))

;; (use-package paredit
;;   :bind (:map paredit-mode-map
;;               ("M-[" . 'paredit-splice-sexp-killing-backward)
;;               ("M-]" . 'paredit-splice-sexp-killing-forward)
;;               ("ESC <up>" . nil)
;;               ("ESC <down>" . nil)
;;               ("M-<up>" . nil)
;;               ("M-<down>" . nil))
;;   :hook ((emacs-lisp-mode lisp-mode eval-expression-minibuffer-setup) . paredit-mode))
;; Highlight color codes in the buffer
(use-package rainbow-mode
  :hook (after-init . rainbow-mode))

;;; Programming Languages
;;;; simple to setup
(use-package nix-mode)
(use-package bazel-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package go-mode)
;; TODO: Get rust-analyzer setup, supposed to blow RLS out of the water
(use-package rustic)

;;;; javascript and typescript
(use-package eslint-fix)

(defun custom-tide-setup ()
  (tide-setup)
  (tide-hl-identifier-mode 1))
;; TODO: Use eslint before save

(use-package tide
 :hook ((typescript-mode . custom-tide-setup)))

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

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
(use-package markdown-mode)
(use-package poly-markdown :after markdown-mode)
;; org-mode additions
(use-package org-bullets
  :hook ((org-mode . org-bullets-mode)))
;; (use-package org-plus-contrib)

;;; Custom key bindings
(global-set-key (kbd "M-SPC") 'company-complete)
(global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)
(global-set-key (kbd "C-s") 'save-buffer)
;; TODO: Rebind isearch-forward
(global-set-key (kbd "C-]") 'tab-to-tab-stop)
;; rebind undo-tree-undo for undoing stuff!
;; Generally need to pick bindings for undo/redo
;; TODO: Backspace works weird in terminal...?!
;; Useful commands to rebind/learn: transpose-words, downcase-word, pop-mark

(setq x-select-enable-primary t)
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
(setq comment-auto-fill-only-comments t)
(setq-default auto-fill-function 'do-auto-fill)

;;; Custom theme
;; Custom theme to use terminal colors best
;; (load-theme 'terminal-wal t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (terminal-wal)))
 '(custom-safe-themes
   (quote
    ("5b77b74104748f954929fa3156201a95af9f0f6beb4860e2435cfd00db0219dc" "dbed1a5cfa6470f7a7338a3d9183c6d9439ea3f03fdd73879f60cd128b5ed05e" "b7388ac03767752ade970303768d65dd5d1b47a860308866a56df30ed1a16c2f" "eabaa2ba26896ab0253f87c1a3ba62fe137a44f22965ccd04f89644bead32e75" "4f87a907299c237ec58c634647b44aca5ee636fb7861da19a9defa0b0658b26e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'terminal-wal t)
