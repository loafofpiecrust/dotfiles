;;; Emacs Configuration
;;;; Bootstrap package manager
;; Fix TLS for emacs 26
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; I'm not planning to modify packages directly, so only rebuild on newly
;; published versions
(setq straight-check-for-modifications nil)

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

;; Easier binding definitions
(use-package general)

;; TODO: Make frame background translucent
;; TODO: Custom colors for refined vdiff chunks, letting us visualize
;; partial-line changes.
;; TODO: totally disable scroll?!

;;;; GUI
;; Load theme early to prevent flickering
;; (use-package gruvbox-theme
;;   :config (load-theme 'gruvbox t))
(use-package moe-theme
  :config
  (moe-theme-set-color 'orange)
  (moe-dark)
  ;; (require 'moe-theme-switcher)
  )

;; font test: `' o O 0 () [] {} *i
(set-face-attribute 'default nil
                    :family "SF Mono"
                    ;; :family "Cascadia Code"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Disable tool-bar and menu-bar
;; (tool-bar-mode -1)
(unless (string-equal system-type "darwin")
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(scroll-bar-mode -1)
(fringe-mode '(10 . 0))
(show-paren-mode t)
(save-place-mode t)

;; Setup basic settings
(setq-default mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil))
              mouse-wheel-progressive-speed nil
              ;; Window focus
              ;; focus-follows-mouse t
              ;; mouse-autoselect-window -.1
              ;; Text display
              line-spacing 0
              display-line-numbers-width 3
              show-paren-style 'expression
              ;; Indentation and wrapping
              tab-width 4
              indent-tabs-mode nil      ; use spaces for indentation
              fill-column 80
              ;; misc settings
              apropos-do-all t
              require-final-newline t
              load-prefer-newer t)

(use-package exec-path-from-shell
  :defer 0.1
  :config (exec-path-from-shell-initialize))

;; Empty scratch buffers
(setq initial-scratch-message "")

;; Easier confirmation
(fset 'yes-or-no-p 'y-or-n-p)

;;;; Autofill
;; Only automatically wrap comments.
(setq-default comment-auto-fill-only-comments t
              auto-fill-function 'do-auto-fill)
(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook (lambda ()
                            (add-hook 'after-save-hook 'delete-trailing-whitespace nil t)))

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

;;; Evil mode
;; Show key combo helpers
(use-package which-key
  :config (which-key-mode)
  :init (setq which-key-enable-extended-define-key t
              which-key-idle-delay 0.5))

;; vim emulation
(use-package evil
  :init (setq evil-want-keybinding nil  ; let evil-collection bind keys
              evil-move-beyond-eol t
              evil-move-cursor-back nil
              evil-want-C-i-jump nil)
  :config (evil-mode t))

;; bind keys for many modes with better evil compatibility
(use-package evil-collection :after evil
  :custom
  (evil-collection-outline-bind-tab-p t)
  (evil-collection-company-use-tng nil)
  :config
  (dolist (m '(go-mode))
    (delete m evil-collection--supported-modes))
  (evil-collection-init))

(add-hook 'evil-insert-state-exit-hook #'company-abort)

;; commenting lines with verb 'g'
(use-package evil-commentary :after evil
  :config (evil-commentary-mode t))

;; surround things with verb 'S'
(use-package evil-surround :after evil
  :config
  (setq-default evil-surround-pairs-alist
                ;; Allow surrounding with newlines
                (cons '(13 . ("\n" . "")) evil-surround-pairs-alist))
  (global-evil-surround-mode t))

;; add more surroundings by default
(use-package evil-embrace
  :config (evil-embrace-enable-evil-surround-integration)
  :ghook ('org-mode-hook #'embrace-org-mode-hook))

;; gotta learn somehow
(use-package evil-tutor :defer t)

;; Show marks visually
(use-package evil-visual-mark-mode :after evil
  :ghook 'after-init-hook)

;; Exchange selections easily with
(use-package evil-exchange :after evil
  :config (evil-exchange-cx-install))

(use-package evil-args :after evil
  :general
  (:keymaps 'evil-inner-text-objects-map
            "a" 'evil-inner-arg)
  (:keymaps 'evil-outer-text-objects-map
            "a" 'evil-outer-arg))

;; Default some modes to use emacs bindings
;;(dolist (e '(magit-mode))
;;(add-to-list 'evil-emacs-state-modes e))

;;;; setup prefixes
;; We're going to want two different prefixes
;; SPC: global commands for managing emacs
;; \  : mode-local keybindings
(defconst global-leader "<SPC>")
(general-create-definer global-leader-def
  :prefix global-leader)

(defconst local-leader "\\")
(general-create-definer local-leader-def
  :prefix local-leader)

;;; UI Packages
;;;; Icons & Emojis
(use-package all-the-icons)
(use-package emojify
  :defer 1
  :config
  (setq emojify-emoji-styles '(unicode github))
  (global-emojify-mode t))

;;;; Fancy looks
;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

;; Show marks in fringe for lines past EOF
(use-package vi-tilde-fringe
  :ghook '(prog-mode-hook text-mode-hook))

(use-package hl-todo
  :ghook 'prog-mode-hook)

;; Temporarily highlight large insertions of text
(use-package volatile-highlights
  :ghook 'after-init-hook)

;;;; Dashboard!
(use-package page-break-lines)
(use-package dashboard
  :after evil
  :config
  (dashboard-setup-startup-hook)
  (evil-set-initial-state 'dashboard-mode 'motion)
  ;; Load in both independent and client windows.
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-startup-banner 'logo
        dashboard-set-heading-icons t
        dashboard-center-content t
        dashboard-items '((recents . 5)
                          (projects . 5)
                          (agenda . 5))))

;;;; Mode line
(use-package diminish) ; hide minor mode lines

(use-package telephone-line
  :config
  (setq telephone-line-lhs '((evil . (telephone-line-evil-tag-segment))
                             (accent . (telephone-line-vc-segment))
                             (nil . (telephone-line-projectile-segment
                                     telephone-line-erc-modified-channels-segment
                                     telephone-line-buffer-segment)))
        telephone-line-rhs '((nil . (telephone-line-input-info-segment
                                     telephone-line-misc-info-segment))
                             (accent . (telephone-line-major-mode-segment))
                             (evil . (telephone-line-airline-position-segment)))
        telephone-line-height 16)
  (telephone-line-mode t))

;; Show search candidate counts in the mode-line
(use-package anzu
  :ghook ('after-init-hook #'global-anzu-mode))
(use-package evil-anzu :after evil anzu)

;;;; Better help
(use-package helpful
  :after counsel
  :config (setq counsel-describe-function-function #'helpful-callable
                counsel-describe-variable-function #'helpful-variable)
  :general (:keymaps 'help-map
                     "f" 'helpful-callable
                     "v" 'helpful-variable
                     "k" 'helpful-key
                     "C" 'helpful-command))

;;;; Navigation
(use-package avy
  :general ('normal "ga" 'avy-goto-char))

;;; Editing
;;;; General
(use-package editorconfig
  :ghook 'prog-mode-hook)
(use-package dtrt-indent
  :ghook 'prog-mode-hook)
(use-package whole-line-or-region)

;; Always indent, no matter what
(use-package aggressive-indent
  :ghook 'prog-mode-hook
  :gfhook ('lsp-mode-hook (lambda () (aggressive-indent-mode -1))))

(use-package highlight-indent-guides
  :ghook '(prog-mode-hook org-mode-hook)
  :config (setq highlight-indent-guides-method 'fill))

;;;; Expressions
;; Make expression editing easier everywhere
(use-package smartparens
  :config
  (dolist (delim '("{" "(" "["))
    (sp-local-pair '(go-mode rust-mode c-mode javascript-mode)
                   delim nil :post-handlers '(("||\n[i]" "RET"))))
  (require 'smartparens-config)
  :ghook
  'prog-mode-hook
  ('(emacs-lisp-mode-hook lisp-mode-hook) #'smartparens-strict-mode))

(use-package evil-cleverparens
  :ghook 'smartparens-enabled-hook)

(defun sp-raise-hybrid-sexp (&optional arg)
  (interactive "P")
  (let* ((current (save-excursion
                    (sp-forward-sexp)
                    (sp-backward-sexp)
                    (sp-get-hybrid-sexp)))
         (end (save-excursion
                (goto-char (sp-get current :beg))
                (let ((block-column (current-column)))
                  (while (>= (current-column) block-column)
                    (next-line)
                    (beginning-of-line)
                    (sp-forward-sexp)
                    (sp-backward-sexp)))
                (sp-backward-sexp)
                (sp-get-hybrid-sexp)))
         (prev (save-excursion
                 (goto-char (sp-get current :beg))
                 (sp-backward-sexp)
                 (sp-get-hybrid-sexp))))
    (if (sp-compare-sexps prev current > :end)
        (sp-message :invalid-context-prev)
      (sp-backward-sexp)
      ;; (python-indent-shift-left (sp-get current :beg) (sp-get end :end))
      )
    (delete-region (sp-get prev :beg) (sp-get current :beg))
    (when (looking-at "[\n\t ]+")
      (forward-line)
      (back-to-indentation))))

(use-package highlight-parentheses
  :ghook 'prog-mode-hook)

;;;; org & outlines
(use-package outshine
  :ghook 'prog-mode-hook)

;;;; niceties
;; Highlight color codes in the buffer
(use-package rainbow-mode
  :ghook 'after-init-hook)

;;; Project management
(use-package projectile
  :config (projectile-mode t))

;; Project tree
(use-package treemacs
  :general ("C-\\" 'treemacs))
(use-package treemacs-evil :after treemacs evil)
(use-package treemacs-projectile :after treemacs evil)
(use-package treemacs-magit :after treemacs magit)

;;; Syntax checking
(use-package flycheck
  :ghook 'prog-mode-hook
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;; mini-buffer completion
(use-package ivy
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-plus)
                                (ivy-bibtex . ivy--regex-ignore-order)
                                ;; Use fuzzy matching for most cases
                                (t . ivy--regex-fuzzy))
        projectile-completion-system 'ivy))

;; TODO: Figure out how to clump these latex packages
(use-package ivy-bibtex :defer 1 :after ivy)

(use-package counsel :after ivy)

(use-package flyspell-correct-ivy :after ivy)

;;; in-buffer completion
(use-package yasnippet-snippets :defer 0.1)
(use-package yasnippet
  :after yasnippet-snippets
  :config (yas-global-mode 1))

;; TODO: Bind keys to aya-create and aya-expand
(use-package auto-yasnippet :after yasnippet)

(use-package company
  :ghook 'prog-mode-hook
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-selection-wrap-around nil
        company-require-match nil))

;; more fuzzy completion
;; (use-package company-fuzzy
;;   :hook (company-mode . company-fuzzy-mode))

;; GUI box to prevent interference with different font sizes
(when (display-graphic-p)
  (use-package company-box
    :custom
    (company-box-icons-alist 'company-box-icons-all-the-icons)
    :ghook 'company-mode-hook))

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
(use-package magit :defer t)
;; TODO: Rebind magit file bindings behind SPC g
;; I only use magit and vdiff, no vc
(setq vc-handled-backends (delq 'Git vc-handled-backends))

;; Provides evil friendly git bindings
(use-package evil-magit
  :after evil magit)
(use-package forge :after magit)
(use-package github-review :after magit)
(use-package git-timemachine :defer t
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  (general-def '(normal motion) git-timemachine-mode-map
    "[r" 'git-timemachine-show-previous-revision
    "]r" 'git-timemachine-show-next-revision))

;; Show changed lines in the margin
(use-package diff-hl
  :ghook ('after-init-hook #'global-diff-hl-mode)
  :gfhook ('magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  :config
  (diff-hl-margin-mode t)
  (diff-hl-dired-mode t)
  (diff-hl-flydiff-mode t)
  (setq-default diff-hl-flydiff-delay 0.2)
  (general-def 'normal diff-hl-mode-map
    "[c" #'diff-hl-previous-hunk
    "]c" #'diff-hl-next-hunk))

;;;; Diff configuration
;; Replace ediff with vdiff for synced scrolling and more...
(use-package vdiff
  :config
  (setq vdiff-magit-stage-is-2way t)
  (general-def 'normal vdiff-mode-map
    "\\" '(:keymap vdiff-mode-prefix-map)))

(use-package vdiff-magit
  :general (magit-mode-map
            "e" #'vdiff-magit-dwim
            "E" #'vdiff-magit))

;; Email!
;; (use-package wanderlust)

;;; Spellcheck
;; Enable spellcheck in comments and strings (requires ispell)
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(setq flyspell-issue-message-flag nil)

;;; Enable completion, pair matching, line numbers
;; TODO: Fix this for lsp-ui sideline stuff.
;; (use-package visual-fill-column
;;   :hook (visual-line-mode . visual-fill-column-mode))

(add-hook 'after-init-hook (lambda ()
                             (global-subword-mode t)
                             (global-prettify-symbols-mode t)
                             (global-auto-revert-mode t)
                             (global-visual-line-mode t)
                             (global-hl-line-mode)
                             (column-number-mode t)))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

(defun cycle-line-numbers ()
  (interactive)
  (setq display-line-numbers
        (if (eq (symbol-value 'display-line-numbers) t)
            'relative t)))

(use-package expand-region
  :general ('visual
            "=" #'er/expand-region
            "-" #'er/contract-region))

;;; Auxiliary Modes
(use-package request :defer t)
(use-package restclient :commands restclient-mode)
(use-package dumb-jump
  :defer t
  :config (setq dumb-jump-selector 'ivy))

;;; Programming Languages
;;;; Language Server Protocol!
;; lsp in conjunction with company and flycheck gives us easy auto-complete and
;; syntax checking on-the-fly.
(use-package lsp-mode
  :config
  (setq lsp-inhibit-message t
        lsp-prefer-flymake nil
        lsp-enable-on-type-formatting t)
  (local-leader-def 'normal lsp-mode-map
    "rr" #'lsp-rename
    "rf" #'lsp-format-buffer
    "re" #'lsp-execute-code-action
    "i" '("imports")
    "io" #'lsp-organize-imports
    "fi" #'lsp-goto-implementation
    "ft" #'lsp-goto-type-definition
    "fd" #'lsp-find-definition
    "fr" #'lsp-find-references)
  ;; Format code on save
  (add-hook 'before-save-hook #'lsp-format-buffer nil t)
  :custom (lsp-rust-server 'rust-analyzer)
  :commands (lsp lsp-deferred)
  :ghook ('(go-mode-hook
            rust-mode-hook
            java-mode-hook
            kotlin-mode-hook) #'lsp-deferred))

;; Show contextual code documentation pop-ups
(use-package lsp-ui
  :custom (lsp-ui-flycheck-enable t)
  :ghook 'lsp-mode-hook)

;; Auto-complete languages with LSP support
(use-package company-lsp
  :after lsp-mode
  :config (push 'company-lsp company-backends))

;;;; One liners
(use-package nix-mode :defer 1)
(use-package bazel-mode :defer 1)
(use-package yaml-mode :defer 1)
(use-package json-mode :defer 1)
(use-package rust-mode :defer 1)
(use-package go-mode
  :defer 1
  :config (local-leader-def 'normal go-mode-map
            "ia" #'go-import-add
            "ga" #'go-goto-arguments
            "gd" #'go-goto-docstring
            "gn" #'go-goto-function-name
            "gi" #'go-goto-imports))

;;;; Java
(use-package lsp-java :defer 1)
(use-package kotlin-mode :defer 1)

;;;; javascript and typescript
(use-package eslint-fix :commands eslint-fix)

(defun custom-tide-setup ()
  (tide-setup)
  (tide-hl-identifier-mode 1))
;; TODO: Use eslint before save

(use-package tide
  :ghook ('typescript-mode-hook #'custom-tide-setup))

(use-package web-mode
  :defer 1
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
(use-package graphql-mode :defer 1)
;; (use-package company-graphql)
;; (add-to-list 'company-backends 'company-graphql)

;;;; typesetting
(use-package markdown-mode :defer 1)
(use-package poly-markdown :after markdown-mode)
;; org-mode additions
(use-package org-bullets :ghook 'org-mode-hook)
(setq org-fontify-emphasized-text t
      org-agenda-files '("~/Documents/agenda")
      org-default-notes-file "~/Documents/agenda/todo.org"
      org-log-done t
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-start-on-weekday nil
      org-reverse-note-order t
      org-fast-tag-selection-single-key t)
;; (use-package org-plus-contrib)

;;;; Latex
;; latex packages have to go near the top for some reason.
;; otherwise they just mysteriously don't load.
;; Because 'auctex is doodled, must use straight directly here.
(straight-use-package 'auctex)
;; latex additions
(add-hook 'TeX-mode-hook #'TeX-fold-mode)
(setq-default TeX-engine 'xetex) ; enables unicode support

;;; Mode-specific keybindings
;;;; global
(defun switch-to-alternate-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Cntextual leader key as backslash
;; Gneric leader key as space
(add-hook 'after-init-hook
          (lambda ()
            (global-leader-def '(motion normal) override
              "SPC" #'counsel-M-x
              "o" '("org")
              "oc" #'org-capture
              "oa" #'org-agenda
              "b" '("buffers")
              "bb" #'ivy-switch-buffer
              "bk" #'kill-buffer
              "bq" #'kill-this-buffer
              "bo" #'switch-to-alternate-buffer
              "bf" #'counsel-find-file
              "bd" #'dired
              "e" '("eval")
              "ee" (kbd "C-x C-e")
              "et" (kbd "C-M-x")
              "w" '("windows")
              "wo" #'other-window
              "wq" #'delete-window
              "wk" #'delete-other-windows
              "w <left>" #'evil-window-left
              "w <right>" #'evil-window-right
              "w <up>" #'evil-window-up
              "w <down>" #'evil-window-down
              "t" '("text")
              "tw" '("words")
              "tws" '("spell-check" . flyspell-correct-wrapper)
              "twc" #'count-words
              "g" '("git")
              "gs" #'magit-status
              "gd" #'vdiff-magit-stage
              "gb" #'magit-blame
              "n" '(:keymap narrow-map)
              "p" '(:keymap projectile-command-map)
              "a" '("apps")
              "ac" #'calc
              "ae" #'flycheck-list-errors
              ;; TODO: Add debugger here under "ad"
              "m" '("modes")
              "mr" #'restclient-mode
              "ma" #'artist-mode
              "u" #'undo-tree-visualize
              "i" '("input-method")
              "is" #'set-input-method
              "it" #'toggle-input-method
              "s" '("settings")
              "sl" #'cycle-line-numbers
              "sw" #'treemacs-switch-workspace
              "h" '(:keymap help-map))))

(use-package hungry-delete
  :ghook ('after-init-hook #'global-hungry-delete-mode))

;;;; vim
;; Letters I can remap: =, 0/^, gd, maybe _, +, maybe ~
(general-def 'normal
  "U" #'undo-tree-redo
  ;; Useful binding for managing method call chains
  "K" #'newline
  "?" #'swiper
  "z=" #'flyspell-correct-at-point
  "M-;" (lambda ()
          (interactive)
          (call-interactively 'comment-dwim)
          (evil-insert-state))
  "[" '("previous")
  "]" '("next")
  "[p" #'evil-jump-backward
  "]p" #'evil-jump-forward
  "]t" #'hl-todo-next
  "[t" #'hl-todo-previous)

(general-def help-map
  "K" #'which-key-show-top-level)

(general-def 'normal evil-cleverparens-mode-map
  "[" nil
  "]" nil
  "[[" #'evil-cp-previous-closing
  "]]" #'evil-cp-next-closing)

(general-def '(normal motion visual)
  "0" (kbd "^"))

;; Fix outline bindings in non-insert states
(general-def '(normal motion) outshine-mode-map
  "TAB" #'outshine-kbd-TAB)

;; General evil mode overrides
(general-def '(normal insert)
  "C-s" (kbd "C-x C-s")
  "C-]" #'tab-to-tab-stop)

(general-def 'insert
  "C-SPC" #'company-complete)

;;;; prog-mode
(local-leader-def 'normal prog-mode-map
  "g" '("goto")
  "r" '("refactor")
  "f" '("find")
  "fd" #'dumb-jump-go
  "f/" #'dumb-jump-go-prompt)

(general-def '(normal insert) prog-mode-map
  "M-RET" #'comment-indent-new-line)

(general-def
  :states '(normal visual)
  :keymaps '(go-mode-map rust-mode-map c-mode-map)
  "M-t" #'sp-transpose-hybrid-sexp
  "D" #'sp-kill-hybrid-sexp
  "M-r" #'sp-raise-hybrid-sexp
  "M-j" #'sp-push-hybrid-sexp
  "M->" #'sp-slurp-hybrid-sexp)

;;;; org-mode
(local-leader-def 'normal org-mode-map
  ;; Mirror some evil agenda commands here for symmetry.
  "c" '("change")
  "cs" #'org-schedule
  "cd" #'org-deadline
  "ct" #'org-set-tags-command
  "ce" #'org-set-effort
  "I" #'org-clock-in
  "O" #'org-clock-out
  "s" '("sorting")
  "ss" #'org-sort
  "da" #'org-archive-subtree-default
  "r" #'org-refile
  "a" #'org-agenda
  "i" '("insert")
  "ih" #'org-table-insert-hline
  "il" #'org-insert-link
  "t" #'org-todo)

(general-def 'normal org-mode-map
  "zt" #'org-show-todo-tree)

;; Give org-mode some evil keybindings
(use-package evil-org
  :ghook 'org-mode-hook
  :config
  (evil-org-set-key-theme)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (general-def 'motion org-agenda-mode-map
    "r" #'org-agenda-refile
    "cs" #'org-agenda-schedule
    "cd" #'org-agenda-deadline))

;;;; lsp-mode

;; TODO: Rebind isearch-forward

;;;; go-mode
;; TODO: Come up with a scheme to reconcile plain "g" and "\g"
;; (general-def 'normal go-mode-map
;;   "gfa" 'go-goto-arguments
;;   "gfn" 'go-goto-function-name
;;   "gai" 'go-goto-imports)

;; TODO: Figure out mode-local artist-mode bindings?
;; (local-leader-def 'normal artist-mode-map
;;   "a" (kbd "C-c C-a"))

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
    ("a43cda2f075da1534eb50d7dce3ca559276a49c623321d55f68ad8ee218f420e" "5b77b74104748f954929fa3156201a95af9f0f6beb4860e2435cfd00db0219dc" "dbed1a5cfa6470f7a7338a3d9183c6d9439ea3f03fdd73879f60cd128b5ed05e" "b7388ac03767752ade970303768d65dd5d1b47a860308866a56df30ed1a16c2f" "eabaa2ba26896ab0253f87c1a3ba62fe137a44f22965ccd04f89644bead32e75" "4f87a907299c237ec58c634647b44aca5ee636fb7861da19a9defa0b0658b26e" default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(outline-8 ((t (:inherit org-level-8))))
 '(vdiff-addition-face ((t (:inherit diff-added))))
 '(hl-paren-face ((t (:weight bold)))))

(set-face-attribute 'diff-added nil
                    :background (color-darken-name "dark olive green" 10)
                    :foreground nil)

(set-face-attribute 'diff-changed nil
                    :background "dark slate grey"
                    :foreground nil)

;; Use a symbol for collapsed headings
(setq org-ellipsis " ▼")
(set-display-table-slot standard-display-table
                        'selective-display
                        (string-to-vector (symbol-value 'org-ellipsis)))
;; (use-package doom-themes)
;; (load-theme 'doom-Iosvkem t)

;; Swap flymake out for flycheck, ALWAYS
(add-hook 'flymake-mode-hook (lambda ()
                               (flymake-mode -1)
                               (flycheck-mode t)))

;; Disable mouse for all buffers!
;; Only use the mouse for switching between buffers.
;; (use-package disable-mouse
;;   :after evil
;;   :config
;;   (global-disable-mouse-mode)
;;   (mapc #'disable-mouse-in-keymap
;;         (list evil-motion-state-map
;;               evil-normal-state-map
;;               evil-visual-state-map
;;               evil-insert-state-map)))

;; Normalize evil keymaps for all modes that specify mode-local bindings
(general-add-hook '(vdiff-mode-hook lsp-mode-hook git-timemachine-mode-hook)
                  #'evil-normalize-keymaps)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
(put 'narrow-to-region 'disabled nil)
