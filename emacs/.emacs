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
;; TODO: Bind swiper to a key in normal state
;; TODO: Make frame background translucent
;; TODO: Slight margin between line number and text. Slight vertical margin from window top to first line.
;; TODO: Custom colors for refined vdiff chunks, letting us visualize
;; partial-line changes.
;; TODO: totally disable scroll?!

;;;; GUI
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode (if (string-equal system-type "darwin") 1 -1))
(show-paren-mode 1)
(fringe-mode '(10 . 0))
(save-place-mode t)

;; Setup basic settings
(setq-default mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil))
              mouse-wheel-progressive-speed nil
              ;; Window focus
              focus-follows-mouse t
              mouse-autoselect-window -.1
              ;; Text display
              line-spacing 0
              display-line-numbers-width 3
              ;; Indentation and wrapping
              tab-width 4
              indent-tabs-mode nil      ; use spaces for indentation
              fill-column 80
              ;; misc settings
              apropos-do-all t
              require-final-newline t
              load-prefer-newer t)

(use-package exec-path-from-shell
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

;;; UI Packages
;;;; Icons & Emojis
(use-package all-the-icons)
(use-package emojify
  :config
  (setq emojify-emoji-styles '(unicode github))
  (global-emojify-mode t))

;;;; Fancy looks
;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

;; Show marks in fringe for lines past EOF
(setq-default indicate-empty-lines t)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

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

;;;; Additions to ivy
(use-package counsel
  :hook (after-init . counsel-mode))

(use-package flyspell-correct-ivy)

;;;; Better help
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h C" . helpful-command)))

;;;; Navigation
(use-package avy)

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
  :hook ((prog-mode org-mode) . highlight-indent-guides-mode))

;;;; Expressions
;; Make expression editing easier everywhere
(use-package smartparens
  :config
  (dolist (delim '("{" "(" "["))
    (sp-local-pair '(go-mode rust-mode c-mode javascript-mode)
                   delim nil :post-handlers '(("||\n[i]" "RET"))))
  (require 'smartparens-config)
  :hook ((prog-mode . smartparens-mode)
         ((emacs-lisp-mode lisp-mode) . smartparens-strict-mode)))

(use-package evil-cleverparens
  :hook (smartparens-enabled . evil-cleverparens-mode))

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
  :init (setq evil-want-keybinding nil  ; let evil-collection bind keys
              evil-move-beyond-eol t
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
;; We're going to want two different prefixes
;; SPC: global commands for managing emacs
;; \  : mode-local keybindings
(general-create-definer global-leader-def
  :prefix "<SPC>")

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
(use-package yasnippet
  :config (yas-global-mode 1))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-require-match -1))

;; more fuzzy completion
;; (use-package company-fuzzy
;;   :hook (company-mode . company-fuzzy-mode))

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
  (diff-hl-dired-mode t)
  (diff-hl-flydiff-mode t)
  (setq-default diff-hl-flydiff-delay 0.2))

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;;;; Diff configuration
;; Replace ediff with vdiff for synced scrolling and more...
(use-package vdiff
  :config
  (setq vdiff-magit-stage-is-2way t))

(general-def 'normal vdiff-mode-map
  "\\" vdiff-mode-prefix-map)

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
;; TODO: Fix this for lsp-ui sideline stuff.
;; (use-package visual-fill-column
;;   :hook (visual-line-mode . visual-fill-column-mode))

(add-hook 'after-init-hook (lambda ()
                             (global-subword-mode t)
                             (global-prettify-symbols-mode t)
                             (global-auto-revert-mode t)
                             (global-visual-line-mode t)
                             (column-number-mode t)))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

(defun cycle-line-numbers ()
  (interactive)
  (setq display-line-numbers
        (if (eq (symbol-value 'display-line-numbers) t)
            'relative t)))

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
(use-package restclient)
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
                       (add-hook 'before-save-hook 'lsp-format-buffer nil t))))
  :commands (lsp lsp-deferred))

;; Show contextual code documentation pop-ups
(use-package lsp-ui
  :hook ((lsp-after-open . (lambda () (lsp-ui-flycheck-enable 1)))
         (lsp-mode . lsp-ui-mode)))

;; Auto-complete languages with LSP support
(use-package company-lsp
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
              "SPC" 'counsel-M-x
              "x" (kbd "C-u C-x")
              "b" '("buffers")
              "bb" 'ivy-switch-buffer
              "bk" 'kill-buffer
              "bw" 'kill-this-buffer
              "b TAB" 'switch-to-alternate-buffer
              "f" '("files")
              "ff" 'counsel-find-file
              "fd" 'dired
              "e" '("eval")
              "ee" (kbd "C-x C-e")
              "et" (kbd "C-M-x")
              "w" '("windows")
              "w TAB" 'other-window
              "ww" 'delete-window
              "wk" 'delete-other-windows
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
              "gd" 'vdiff-magit-stage
              "n" '("narrowing" . ,(kbd "C-x n"))
              "p" projectile-command-map
              "a" '("apps")
              "aa" 'org-agenda
              "ac" 'calc
              "m" '("modes")
              "mr" 'restclient-mode
              "mp" 'artist-mode
              "u" 'undo-tree-visualize
              "i" '("input-method")
              "is" 'set-input-method
              "it" 'toggle-input-method
              "s" '("settings")
              "sl" 'cycle-line-numbers
              "h" help-map)))

(use-package hungry-delete
  :config (global-hungry-delete-mode))

;;;; vim
;; Letters I can remap: =, 0/^,
(general-def 'normal
  "U" 'undo-tree-redo
  ;; Useful binding for managing method call chains
  "K" 'newline
  "ga" 'avy-goto-char
  "[" '("previous")
  "]" '("next")
  "[p" 'evil-jump-backward
  "]p" 'evil-jump-forward
  "]t" 'hl-todo-next
  "[t" 'hl-todo-previous
  "C-h K" 'which-key-show-top-level)

(general-def 'normal evil-cleverparens-mode-map
  "[" nil
  "]" nil
  "[[" 'evil-cp-previous-closing
  "]]" 'evil-cp-next-closing)

(general-def '(normal motion visual)
  "0" (kbd "^"))

;; Fix outline bindings in non-insert states
(general-def '(normal motion) outshine-mode-map
  "TAB" 'outshine-kbd-TAB)

;; General evil mode overrides
(general-def '(normal insert)
  "C-s" (kbd "C-x C-s")
  "C-]" 'tab-to-tab-stop)

;; (use-package evil-escape
;;   :config (progn (setq-default evil-escape-key-sequence (kbd "TAB TAB")
;;                                evil-escape-delay 0.2)
;;                  (evil-escape-mode t)))

(general-def 'insert
  "RET" 'evil-normal-state
  "C-SPC" 'company-complete)

(general-def company-active-map
  "TAB" 'company-complete)

;;;; prog-mode
(local-leader-def 'normal prog-mode-map
  "g" '("goto")
  "r" '("refactor")
  "f" '("find")
  "fd" 'dumb-jump-go
  "f/" 'dumb-jump-go-prompt)

(general-def '(normal insert) prog-mode-map
  "M-RET" 'comment-indent-new-line)

;;;; org-mode
(local-leader-def 'normal org-mode-map
  "s" 'org-schedule
  "d" 'org-deadline
  "c" '("clocking")
  "ci" 'org-clock-in
  "co" 'org-clock-out
  "i" '("insert")
  "ih" 'org-table-insert-hline
  "t" '("todos")
  "tt" 'org-show-todo-tree
  "tl" 'org-todo-list
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
  "rr" 'lsp-rename
  "rf" 'lsp-format-buffer
  "re" 'lsp-execute-code-action
  "i" '("imports")
  "io" 'lsp-organize-imports
  "fi" 'lsp-goto-implementation
  "ft" 'lsp-goto-type-definition
  "fd" 'lsp-find-definition
  "fr" 'lsp-find-references)

;; TODO: Rebind isearch-forward

;;;; go-mode
(local-leader-def 'normal go-mode-map
  "ia" 'go-import-add
  "ga" 'go-goto-arguments
  "gd" 'go-goto-docstring
  "gn" 'go-goto-function-name
  "gi" 'go-goto-imports)

;; TODO: Come up with a scheme to reconcile plain "g" and "\g"
;; (general-def 'normal go-mode-map
;;   "gfa" 'go-goto-arguments
;;   "gfn" 'go-goto-function-name
;;   "gai" 'go-goto-imports)

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
 '(diff-hl-flydiff-delay 0.2)
 '(diff-hl-flydiff-mode t)
 '(evil-collection-company-use-tng nil)
 '(evil-collection-outline-bind-tab-p t)
 '(global-diff-hl-mode t)
 '(lsp-rust-server (quote rust-analyzer))
 '(vdiff-magit-stage-is-2way t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(diff-added ((t (:background ,(color-darken-name "dark olive green" 20) :foreground nil))))
 ;; '(diff-changed ((t (:background "dark slate grey" :foreground nil))))
 ;; '(diff-hl-insert ((t (:inherit diff-added :background nil))))
 ;; '(diff-hl-change ((t (:inherit diff-changed :background nil))))
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
 '(vdiff-addition-face ((t (:inherit diff-added)))))

(use-package gruvbox-theme)
(load-theme 'gruvbox t)

;; font test: `' o O 0 () [] {} *i
(set-face-attribute 'default nil
                    :family "SF Mono"
                    :height 110
                    :weight 'normal
                    :width 'normal)

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
(use-package disable-mouse
  :after evil
  :config
  (global-disable-mouse-mode)
  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-insert-state-map)))
