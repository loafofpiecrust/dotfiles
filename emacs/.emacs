;;; -*- lexical-binding: t; -*-
;;; Emacs Configuration
;;;; Bootstrap package manager
;; Fix TLS for emacs 26
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; I'm not planning to modify packages directly, so only rebuild on newly
;; published versions
(setq straight-check-for-modifications '(find-at-startup))

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
(use-package dash)
(use-package s)

;; TODO: Make frame background translucent
;; TODO: Custom colors for refined vdiff chunks, letting us visualize
;; partial-line changes.
;; TODO: totally disable scroll?!

;;;; GUI
;; Load theme early to prevent flickering
(use-package doom-themes
  :config
  (setq doom-themes-treemacs-enable-variable-pitch nil
        doom-themes-treemacs-theme "doom-colors"
        ;; doom-gruvbox-brighter-comments t
        ;; doom-challenger-deep-brighter-comments t
        ;; doom-dracula-brighter-comments t
        )
  (load-theme 'doom-snazzy t)
  (doom-themes-org-config)
  (doom-themes-treemacs-config))

;; Fonts need a bit of beefing on Mac
(defvar default-font-height (if (eq system-type 'darwin) 120 115)
  "Platform-dependent default font size")

;; font test: `' o O 0 () [] {} *i
(set-face-attribute 'default nil
                    :family "SF Mono"
                    :height default-font-height
                    :weight 'medium
                    :width 'normal)

(set-face-attribute 'variable-pitch nil
                    :family "Overpass"
                    :height default-font-height)

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'control))

;; Disable tool-bar and menu-bar
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode '(nil . 0))
(show-paren-mode t)
(save-place-mode t)
(blink-cursor-mode t)
;; (pixel-scroll-mode t)

;; Setup basic settings
;; TODO: Move scroll stuff to only not on MacOS
(setq-default
 ;; Text display
 display-line-numbers-width 4
 show-paren-style 'expression
 ;; Nicer scrolling
 ;; pixel-resolution-fine-flag nil
 mouse-wheel-progressive-speed nil
 ;; mouse-wheel-scroll-amount '(1)
 scroll-margin 15
 scroll-conservatively 10000
 shift-select-mode nil
 ;; Indentation and wrapping
 tab-width 4
 indent-tabs-mode nil      ; use spaces for indentation
 fill-column 80
 ;; misc settings
 prettify-symbols-unprettify-at-point 'right-edge
 require-final-newline t
 load-prefer-newer t
 custom-safe-themes t)

;; (use-package sublimity
;;   :config
;;   (require 'sublimity-scroll)
;;   (setq mouse-wheel-progressive-speed nil
;;         mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;;   (sublimity-mode 1))

(use-package exec-path-from-shell
  :defer 0.1
  :config (exec-path-from-shell-initialize))

;; Empty scratch buffers
(setq initial-scratch-message "")

;; Easier confirmation
(fset 'yes-or-no-p 'y-or-n-p)

;;;; Periodically clean buffers
(require 'midnight)
(setq clean-buffer-list-kill-regexps '("\\`\\*Man "
                                       "\\*helpful "
                                       "\\*Calc"
                                       "\\*xref"
                                       "\\*straight-process\\*"
                                       "\\*Flycheck"
                                       "\\*forge"
                                       "magit"
                                       "\\*eshell"
                                       "Aweshell:")
      clean-buffer-list-delay-special (* 60 60 2)
      ;; Clean out potentially old buffers every hour
      midnight-period (* 60 60))
(midnight-mode)

;;;; Autofill
;; Only automatically wrap comments in code
(setq-default comment-auto-fill-only-comments t
              auto-fill-function 'do-auto-fill)
(add-hook 'prog-mode-hook 'auto-fill-mode)
;; Cleanup whitespace after saving most files
(general-add-hook '(text-mode-hook prog-mode-hook)
                  (lambda () (add-hook 'after-save-hook #'whitespace-cleanup nil t)))

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
              which-key-idle-delay 1))

;; vim emulation
(use-package evil
  :init (setq evil-want-keybinding nil  ; let evil-collection bind keys
              evil-move-beyond-eol t
              evil-respect-visual-line-mode t
              evil-move-cursor-back nil
              evil-search-module 'evil-search
              evil-ex-search-persistent-highlight nil
              evil-want-C-i-jump nil)
  :config
  (evil-mode t)

  ;; Letters I can rebind: ', =, 0/^, gd, maybe _, +, Q, <backspace>, C-k, C-j, R
  ;; TODO: Use a better command for nearby find on key f
  (general-def 'normal
    ;; Make normal paste go AT point rather than AFTER
    "p" (lambda ()
          (interactive)
          (evil-backward-char 1 nil t)
          (call-interactively #'evil-paste-after))
    ;; "P" #'evil-paste-before
    "U" #'undo-tree-redo
    ;; Useful binding for managing method call chains
    "K" #'newline
    "z=" #'flyspell-correct-at-point
    "C-p" nil
    "R" nil)

  ;; Use some standard keybindings.
  (general-def '(normal insert)
    "C-z" #'undo-tree-undo
    "C-S-z" #'undo-tree-redo)

  (general-def 'insert
    "C-v" #'evil-paste-after)

  ;; TODO: Globalize these bindings if possible
  (general-def '(motion insert)
    "C-f" #'counsel-projectile-rg
    "C-w" #'kill-this-buffer)

  (general-def '(normal insert visual)
    ;; TODO: Decide how I want to combine these commands based on general rules
    ;; about my modifier keys: C, M, S.
    "C-/" #'comment-line
    "M-/" (lambda ()
            (interactive)
            (call-interactively #'comment-dwim)
            (evil-insert-state)))

  (general-def 'motion
    ;; "gr" stands for "go run this"
    ;; By default, use "gr" to refresh and run what's at point
    "gr" (general-simulate-key "C-c C-c")
    ;; Stands for "go quit" to quit some auxiliary mode, like editing a git
    ;; commit message.
    "gq" (general-key "C-c C-k")
    "C-RET" (general-simulate-key "C-c C-c")
    "[" '("previous")
    "]" '("next")
    "C-{" #'evil-jump-backward
    "C-}" #'evil-jump-forward
    "[]" #'evil-backward-section-begin
    "][" #'evil-forward-section-begin
    "]t" #'hl-todo-next
    "[t" #'hl-todo-previous
    "[p" #'evil-backward-paragraph
    "]p" #'evil-forward-paragraph
    "?" #'swiper
    "0" (general-key "^")))

;; bind keys for many modes with better evil compatibility
(use-package evil-collection
  :custom
  (evil-collection-outline-bind-tab-p t)
  (evil-collection-company-use-tng nil)
  :config
  (dolist (m '(go-mode))
    (delete m evil-collection--supported-modes))
  (evil-collection-init))

;; Stop completion when exiting insert mode
(general-def 'insert company-active-map
  "ESC" #'company-abort)
(add-hook 'evil-insert-state-exit-hook #'company-abort)

;; Fix expression evaluation in normal state
;; (defun evil-fix-eval-last-sexp ()
;;   (if (eq evil-state 'normal)
;;       (evil-append 1)
;;     (evil-normal-state nil)))
;; (general-add-advice #'eval-last-sexp :around #'evil-fix-eval-last-sexp)

;; commenting lines with verb 'g'
(use-package evil-commentary
  :config (evil-commentary-mode))

;; surround things with verb 'S'
(use-package evil-surround
  :config
  (global-evil-surround-mode)
  (setq-default evil-surround-pairs-alist
                ;; Allow surrounding with newlines
                (cons '(13 . ("\n" . "")) evil-surround-pairs-alist)))

;; add more surroundings by default
(use-package evil-embrace
  :config (evil-embrace-enable-evil-surround-integration)
  :ghook ('org-mode-hook #'embrace-org-mode-hook))

;; gotta learn somehow
(use-package evil-tutor :defer t)

;; Exchange selections easily with
;; (use-package evil-exchange
;;   :config (evil-exchange-cx-install))

;; Manage comma-delimited arguments with noun 'a'
(use-package evil-args
  :general
  (general-def :keymaps 'evil-inner-text-objects-map
    "a" 'evil-inner-arg)
  (general-def :keymaps 'evil-outer-text-objects-map
    "a" 'evil-outer-arg))

(use-package evil-mc
  :config
  (global-evil-mc-mode 1)
  (general-def 'visual evil-mc-key-map
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg)
  ;; Borrow these keybindings from VS/Atom/Sublime
  (general-def 'normal
    "gmq" #'evil-mc-undo-all-cursors
    "C-J" #'evil-mc-make-cursor-move-next-line
    "C-K" #'evil-mc-make-cursor-move-prev-line
    "C-S-<down>" #'evil-mc-make-cursor-move-next-line
    "C-S-<up>" #'evil-mc-make-cursor-move-prev-line))

;; Extend % to match more delimeters, smarter
(use-package evil-matchit
  :config (global-evil-matchit-mode))

;;;; setup prefixes
;; We're going to want a few different prefixes
;; SPC :: global commands for managing emacs
;; \   :: major mode key bindings
;; '   :: minor mode key bindings
;; =   :: ???
;; g   :: other vim commands/go to...
;; z   :: change display
(defconst global-leader "<SPC>")
(general-create-definer global-leader-def
  :prefix global-leader)

(defconst major-leader "\\")
(general-create-definer major-leader-def
  :prefix major-leader)

(defconst minor-leader "'")
(general-create-definer minor-leader-def
  :prefix minor-leader)

;;; UI Packages
;;;; Window Management
(use-package window-purpose
  :config
  (purpose-mode)
  (purpose-x-kill-setup)
  (purpose-x-magit-multi-on)
  (setq purpose-user-mode-purposes '((deadgrep-mode . searches)
                                     (helpful-mode . searches)
                                     (xref--xref-buffer-mode . searches)
                                     (help-mode . searches)))
  (purpose-compile-user-configuration)
  (global-leader-def 'motion override
    "wd" #'purpose-toggle-window-purpose-dedicated))

(use-package ivy-purpose
  :after ivy
  :config
  (general-def '(motion insert)
    "C-b" #'ivy-purpose-switch-buffer-with-purpose))

;; Automatically manage window sizes
(use-package zoom
  :config
  (setq zoom-size '(0.7 . 0.7))
  (zoom-mode)
  ;; Make zoom work with purpose!
  (add-hook 'purpose-select-buffer-hook #'zoom--update)
  (add-hook 'xref-after-jump-hook #'zoom--update))

;;;; Icons & Emojis
(use-package all-the-icons)
(use-package emojify
  :config
  (setq emojify-emoji-styles '(unicode github))
  (global-emojify-mode t))

;;;; Fancy looks
;; Show marks in fringe for lines past EOF
(use-package vi-tilde-fringe
  :ghook '(prog-mode-hook text-mode-hook))

(use-package hl-todo
  :ghook 'prog-mode-hook)

;; Temporarily highlight large insertions of text
(use-package volatile-highlights
  :config (volatile-highlights-mode))

;;;; Dashboard!
(use-package page-break-lines)
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (evil-set-initial-state 'dashboard-mode 'motion)
  ;; Load in both independent and client windows.
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-startup-banner 'logo
        dashboard-set-heading-icons t
        dashboard-center-content t
        dashboard-items '((recents . 10)
                          (projects . 10))))

;;;; Mode line
;; hide minor mode lines
;; (use-package diminish)

;; Show search candidate counts in the mode-line
(use-package anzu
  :config (global-anzu-mode))
(use-package evil-anzu :after anzu
  :config (setq anzu-cons-mode-line-p nil))

(use-package doom-modeline
  :config
  (setq doom-modeline-height 16)
  (doom-modeline-mode))

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
  :general ('(motion normal) "ga" 'avy-goto-char-timer))

(use-package origami
  :ghook '(prog-mode-hook markdown-mode-hook))
(use-package lsp-origami
  :ghook 'origami-mode-hook)

;;; Editing
;;;; General
(use-package editorconfig
  :ghook 'prog-mode-hook)
;; Auto-detect indent for flexibly indented languages
(use-package dtrt-indent
  :ghook '(web-mode-hook js-mode-hook c-mode-hook java-mode-hook python-mode-hook))

;; Always indent, no matter what
(use-package aggressive-indent
  :ghook 'prog-mode-hook
  :gfhook ('lsp-mode-hook (lambda () (aggressive-indent-mode -1))))

(use-package highlight-indent-guides
  :ghook '(prog-mode-hook org-mode-hook)
  :config (setq highlight-indent-guides-method 'fill))

;;;; Expressions

(defconst lisp-lang-hooks '(lisp-mode-hook
                            emacs-lisp-mode-hook
                            scheme-mode-hook
                            racket-mode-hook))

;; Make expression editing easier everywhere
(use-package smartparens
  :config
  (dolist (delim '("{" "(" "["))
    (sp-local-pair '(web-mode nix-mode go-mode c-mode javascript-mode)
                   delim nil :post-handlers '(("||\n[i]" ""))))
  (require 'smartparens-config)
  :ghook
  'prog-mode-hook
  (lisp-lang-hooks #'smartparens-strict-mode))

(use-package evil-cleverparens
  :ghook 'smartparens-enabled-hook)

(use-package highlight-parentheses
  :ghook 'prog-mode-hook)

;;;; org & outlines
(use-package outshine
  :ghook 'prog-mode-hook)

;;;; niceties
;; Highlight color codes in the buffer
(use-package rainbow-mode
  :config (rainbow-mode))

;;; Project management
(use-package projectile
  :config
  (projectile-mode t)
  (global-leader-def '(normal motion) override
    "p" '(:keymap projectile-command-map)))

(use-package counsel-projectile
  :after counsel projectile
  :ghook 'counsel-mode-hook
  :general ("C-p" #'counsel-projectile-find-file))

;; Use for searching within projects
(use-package ripgrep)
(use-package deadgrep
  :general ("C-S-F" #'deadgrep))

;; Project tree
(use-package treemacs
  :general ("C-\\" #'treemacs)
  :gfhook #'variable-pitch-mode
  :config
  (general-def treemacs-mode-map
    "p" '(:keymap treemacs-project-map))
  (general-def treemacs-project-map
    "w" #'treemacs-switch-workspace))
(use-package treemacs-evil :after treemacs evil)
(use-package treemacs-projectile :after treemacs projectile)
(use-package treemacs-magit :after treemacs magit)
(use-package lsp-treemacs :after treemacs lsp)

;;; Syntax checking
(use-package flycheck
  :ghook 'prog-mode-hook
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package flycheck-posframe
  :ghook 'flycheck-mode-hook
  :config
  (flycheck-posframe-configure-pretty-defaults)
  (setq flycheck-posframe-border-width 2))

;;; mini-buffer completion
(use-package flx)
(use-package ivy
  :ghook 'after-init-hook
  :config
  (general-def ivy-minibuffer-map
    "C-<return>" #'ivy-dispatching-done)
  (setq-default ivy-use-virtual-buffers t
                enable-recursive-minibuffers t
                ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-fuzzy)
                                        (ivy-bibtex . ivy--regex-ignore-order)
                                        (counsel-M-x . ivy--regex-ignore-order)
                                        ;; Use fuzzy matching for most cases
                                        (t . ivy--regex-fuzzy))
                projectile-completion-system 'ivy))

(use-package ivy-posframe
  :ghook 'ivy-mode-hook
  :config
  (setq ivy-posframe-display-functions-alist '((swiper . nil)
                                               (t . ivy-posframe-display-at-frame-center))
        ivy-posframe-parameters '((left-fringe . 8)
                                  (right-fringe . 8)
                                  (internal-border-width . 2))))

;; TODO: Figure out how to clump these latex packages
(use-package ivy-bibtex :commands ivy-bibtex)

(use-package counsel :ghook 'ivy-mode-hook
  :config
  (setcdr (assq 'counsel-M-x ivy-initial-inputs-alist) ""))
(use-package ivy-rich :ghook 'ivy-mode-hook
  :config (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package flyspell-correct-ivy)

(use-package prescient)
(use-package ivy-prescient
  :ghook 'ivy-mode-hook)

;; TODO: Bind commands for this thing
;; (use-package lsp-ivy
;;   :straight (:host github :repo "emacs-lsp/lsp-ivy"))

;;; in-buffer completion
(use-package yasnippet-snippets)
(use-package yasnippet
  :after yasnippet-snippets company
  :config
  (yas-global-mode 1)
  ;; Disable auto completion of snippets. Instead rely on the completion dialog.
  (general-def yas-minor-mode-map
    "TAB" nil
    "<tab>" nil
    [(tab)] nil)
  ;; Add yasnippet support to every company backend
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

;; TODO: Bind keys to aya-create and aya-expand
;; (use-package auto-yasnippet :after yasnippet)

(use-package company
  :ghook 'prog-mode-hook
  :config
  ;; Press tab/shift-tab to start/stop completion in insert mode
  (general-def 'insert company-mode-map
    "C-SPC" #'company-complete)
  ;; Press tab once in the dialog to complete the common prefix
  ;; Press tab twice in the dialog to complete with the selection
  (general-def :keymaps 'company-active-map
    "C-SPC" #'company-abort
    "<tab>" #'company-complete-selection
    "<return>" nil
    "<escape>" #'company-abort)
  ;; Show completion automatically upon typing anything
  (setq-default completion-ignore-case t
                completion-styles '(basic partial-completion substring)
                company-idle-delay 0
                company-minimum-prefix-length 1
                company-selection-wrap-around nil
                company-require-match nil))

;; more fuzzy completion
;; (use-package company-fuzzy
;;   :ghook 'company-mode-hook)

(use-package company-prescient
  :ghook 'company-mode-hook)

;; GUI box to prevent interference with different font sizes
(use-package company-box
  :ghook 'company-mode-hook
  :custom
  ;; TODO: Add key binding for showing docstring
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  (company-box-max-candidates 50)
  (company-box-doc-delay 2)
  (company-box-show-single-candidate t))

;;; Version Control
;;;; Git
(use-package magit
  :config
  (evil-set-initial-state 'git-commit-mode 'insert))
;; TODO: Rebind magit file bindings behind SPC g

;; Provides evil friendly git bindings
(use-package evil-magit :after magit)
(use-package forge :after magit)
(use-package github-review :after magit)

;; View the history of the current file.
(use-package git-timemachine
  :config
  (add-hook 'git-timemachine-mode-hook #'evil-motion-state)
  (global-leader-def '(normal motion) override
    "gt" #'git-timemachine)
  (general-def '(motion normal) git-timemachine-mode-map
    minor-leader git-timemachine-mode-map)
  (general-def 'motion git-timemachine-mode-map
    "[r" #'git-timemachine-show-previous-revision
    "]r" #'git-timemachine-show-next-revision))

;; Show changed lines in the margin
(use-package diff-hl
  :gfhook ('magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode)
  (diff-hl-dired-mode)
  (diff-hl-flydiff-mode)
  (setq-default diff-hl-flydiff-delay 0.2)
  (general-def '(normal motion) diff-hl-mode-map
    "[c" #'diff-hl-previous-hunk
    "]c" #'diff-hl-next-hunk))

;; Don't ask me when following symlinks
(setq vc-follow-symlinks t)

;;;; Diff configuration
;; Replace ediff with vdiff for synced scrolling and more...
(use-package vdiff
  :commands vdiff-mode
  :config
  (setq-default vdiff-magit-stage-is-2way t)
  (general-def 'motion vdiff-mode-map
    minor-leader #'vdiff-hydra/body))

(use-package vdiff-magit
  :general (magit-mode-map
            "e" #'vdiff-magit-dwim
            "E" #'vdiff-magit))

;; Define just a few bindings for smerge, but it has pretty great mouse support
;; for handling conflicts by right clicking.
(eval-after-load 'smerge
  (progn
    (minor-leader-def 'motion smerge-mode-map
      "k" #'smerge-keep-upper
      "j" #'smerge-keep-lower
      "a" #'smerge-keep-all)
    (general-def 'motion smerge-mode-map
      "[c" #'smerge-prev
      "]c" #'smerge-next)))

;;; Writing
;;;; Spellcheck
;; Load flyspell async to prevent blocking on file load
(use-package flyspell-lazy
  :config (flyspell-lazy-mode t))
;; Enable spellcheck in comments and strings (requires ispell)
(general-add-hook '(markdown-mode-hook org-mode-hook) #'flyspell-mode)
(general-add-hook 'prog-mode-hook #'flyspell-prog-mode)
(setq-default flyspell-issue-message-flag nil
              ispell-program-name (executable-find "aspell")
              ispell-dictionary "en_US"
              ispell-extra-args '("--camel-case"))

;;;; Thesaurus
;; Requires internet to lookup words
(use-package powerthesaurus
  :general
  (global-leader-def '(normal motion) override
    "tt" '("thesaurus" . powerthesaurus-lookup-word-dwim)))

;;; Enable completion, pair matching, line numbers
;; TODO: Fix this for lsp-ui sideline stuff.
(use-package visual-fill-column
  :ghook 'markdown-mode-hook)

(general-add-hook '(org-mode-hook go-mode-hook)
                  #'electric-pair-mode)

;; Stop auto-fill in certain modes where we'd rather break lines ourselves based
;; on sentences or what have you. We use visual-fill-column to make that easier
;; on the eyes.
(general-add-hook '(markdown-mode-hook) #'turn-off-auto-fill)

(global-subword-mode)
(global-prettify-symbols-mode)
;; (global-auto-revert-mode)
(global-visual-line-mode)
(column-number-mode)

(general-add-hook '(prog-mode-hook
                    text-mode-hook)
                  #'display-line-numbers-mode)

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
;;;; REST client
(use-package restclient :commands restclient-mode)
(use-package dumb-jump
  :defer t
  :config (setq dumb-jump-selector 'ivy))

;;;; shell extensions
;; (defun eshell-open-unused ()
;;   (interactive)
;;   ())
(use-package aweshell
  :straight (:host github :repo "manateelazycat/aweshell")
  :commands aweshell)
(use-package company-shell
  :config (add-to-list 'company-backends '(company-shell company-shell-env)))

;;;; dired
;; Provide a ranger-like interface for dired
(use-package ranger
  :ghook 'dired-mode-hook)

;;; Programming Languages
;;;; Language Server Protocol!
;; lsp in conjunction with company and flycheck gives us easy auto-complete and
;; syntax checking on-the-fly.
(use-package lsp-mode
  :config
  (setq-default lsp-inhibit-message t
                lsp-prefer-flymake nil
                lsp-enable-on-type-formatting t)
  (general-def '(motion normal) lsp-mode-map
    "gd" #'lsp-find-definition)
  (major-leader-def '(normal motion) lsp-mode-map
    "fi" #'lsp-goto-implementation
    "ft" #'lsp-goto-type-definition
    "fd" #'lsp-find-definition
    "fr" #'lsp-find-references)
  (major-leader-def 'normal lsp-mode-map
    "rr" #'lsp-rename
    "rf" #'lsp-format-buffer
    "re" #'lsp-execute-code-action
    "i" '("imports")
    "io" #'lsp-organize-imports)
  ;; Format code on save
  (add-hook 'lsp-mode-hook
            (lambda () (add-hook 'before-save-hook
                            #'lsp-format-buffer
                            nil t)))
  :custom (lsp-rust-server 'rust-analyzer)
  :commands (lsp lsp-deferred)
  :ghook ('(go-mode-hook
            rust-mode-hook
            java-mode-hook
            kotlin-mode-hook
            ruby-mode-hook) #'lsp-deferred))

;; Show contextual code documentation pop-ups
(use-package lsp-ui
  :ghook 'lsp-mode-hook
  :custom
  (lsp-ui-flycheck-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-include-signature t)
  :config
  (major-leader-def 'normal lsp-ui-mode-map
    "p" '("peek")
    "pr" #'lsp-ui-peek-find-references
    "pd" #'lsp-ui-peek-find-definitions
    "pi" #'lsp-ui-peek-find-implementation))

;; Auto-complete languages with LSP support
(use-package company-lsp
  :after lsp-mode
  :config (push 'company-lsp company-backends))

;;;; Debug Adapter Protocol!!
(use-package dap-mode
  :ghook 'lsp-mode-hook
  :config
  ;; Open the hydra when we hit a breakpoint
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  (major-leader-def 'normal dap-mode-map
    "d" '("debug")
    "db" #'dap-breakpoint-toggle)
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode t))

;;;; One liners
(use-package haskell-mode)
(use-package nix-mode)
(use-package fish-mode)
(use-package bazel-mode)
(use-package terraform-mode)
(use-package git-modes)
(use-package company-terraform
  :config (company-terraform-init))
(use-package yaml-mode)
(use-package json-mode)
(use-package rust-mode)
(use-package go-mode
  :config
  ;; Code navigation key bindings
  (major-leader-def 'normal go-mode-map
    "ia" #'go-import-add)
  (major-leader-def '(normal motion) go-mode-map
    "ga" #'go-goto-arguments
    "gd" #'go-goto-docstring
    "gn" #'go-goto-function-name
    "gi" #'go-goto-imports)

  ;; Setup debugging support
  (require 'dap-go)
  (dap-go-setup))

(use-package racket-mode)

;;;; Java
(use-package lsp-java)
(use-package kotlin-mode)
(use-package groovy-mode)      ; for gradle build files
(use-package gradle-mode
  :ghook '(kotlin-mode-hook java-mode-hook groovy-mode-hook))

;;;; javascript and typescript
(use-package eslint-fix :commands eslint-fix)

(use-package eldoc-box
  :ghook ('eldoc-mode-hook #'eldoc-box-hover-mode))

(defun custom-tide-setup ()
  (tide-setup)
  (tide-hl-identifier-mode 1))
;; TODO: Use eslint before save

;; TODO: Use js2 + web + lsp if possible
(use-package tide
  :ghook ('typescript-mode-hook #'custom-tide-setup))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode)))

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
(use-package markdown-mode
  :config
  (major-leader-def 'normal markdown-mode-map
    "i`" #'markdown-insert-gfm-code-block))
(use-package poly-markdown)
;; org-mode additions
(use-package org-ref)
(use-package org-bullets :ghook 'org-mode-hook)
(setq org-fontify-emphasized-text t
      org-highlight-latex-and-related '(native)
      org-agenda-files '("~/Documents/agenda")
      org-default-notes-file "~/Documents/agenda/todo.org"
      org-log-done t
      org-link-descriptive nil
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-start-on-weekday nil
      org-reverse-note-order t
      org-fast-tag-selection-single-key t
      org-use-property-inheritance t
      org-agenda-custom-commands '(("u" "Unscheduled TODOs"
                                    ((todo ""
                                           ((org-agenda-overriding-header "Unscheduled TODOs")
                                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled)))))
                                    nil nil)))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((shell . t)))

;;;; Latex
;; latex packages have to go near the top for some reason.
;; otherwise they just mysteriously don't load.
;; Because 'auctex is doodled, must use straight directly here.
;; (straight-use-package 'auctex)
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

;; Contextual leader key as backslash
;; Generic leader key as space
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
  "bf" #'view-buffer-other-frame
  "f" '("files")
  "ff" #'counsel-find-file
  "fd" #'ranger
  "fr" #'counsel-recentf
  "e" '("eval")
  "ee" (general-key "C-x C-e")
  "et" (general-key "C-M-x")
  "w" '("windows")
  "wo" #'other-window
  "wq" #'delete-window
  "wk" #'delete-other-windows
  "w <left>" #'evil-window-left
  "w <right>" #'evil-window-right
  "w <up>" #'evil-window-up
  "w <down>" #'evil-window-down
  "t" '("text")
  "ts" '("spell-check" . flyspell-correct-wrapper)
  "tc" #'count-words
  "g" '("git")
  "gg" #'magit-status
  "gd" #'vdiff-magit-stage
  "gb" #'magit-blame
  "n" '(:keymap narrow-map)
  "a" '("apps")
  "ac" #'calc
  "ae" #'flycheck-list-errors
  "as" #'eshell
  ;; TODO: Add debugger here under "ad"
  "m" '("modes")
  "mr" #'restclient-mode
  "ma" #'artist-mode
  "mw" #'whitespace-mode
  "u" #'undo-tree-visualize
  "i" '("input-method")
  "is" #'set-input-method
  "it" #'toggle-input-method
  "s" '("settings")
  "sl" #'cycle-line-numbers
  "sw" #'treemacs-switch-workspace
  "h" '(:keymap help-map))

(use-package hungry-delete
  :config (global-hungry-delete-mode))


;;;; vim
(general-def help-map
  "K" #'which-key-show-top-level)

(general-def '(motion normal) evil-cleverparens-mode-map
  "[" nil
  "]" nil
  "[[" #'evil-cp-previous-closing
  "]]" #'evil-cp-next-closing)

;; Unbind evil keys that we want to use ourselves.
(general-def :states 'motion
  minor-leader nil
  "C-o" nil)

;; (general-def '(normal motion)
;;   :keymaps '(outshine-mode-map outline-mode-map)
;;   "C-j" 'outline-forward-same-level
;;   "C-k" 'outline-backward-same-level)

;; Fix outline bindings in non-insert states
;; (general-def '(normal motion)
;;   :keymaps 'outshine-mode-map
;;   :predicate '(outline-on-heading-p)
;;   "M-k" #'outline-move-subtree-up
;;   "M-j" #'outline-move-subtree-down
;;   "TAB" #'outshine-kbd-TAB)

;; Bind a few ctrl keys to all states
(general-def
  "C-o" #'counsel-find-file
  "C-s" (general-key "C-x C-s"))

;;;; prog-mode
(major-leader-def 'motion prog-mode-map
  "g" '("goto")
  "r" '("refactor")
  "f" '("find")
  ;; "fd" #'dumb-jump-go
  "fd" #'xref-find-definitions
  "fr" #'xref-find-references
  "fj" #'dumb-jump-go
  "f/" #'dumb-jump-go-prompt)

(general-def '(normal insert) prog-mode-map
  "M-RET" #'comment-indent-new-line)

(general-def 'normal evil-cleverparens-mode-map
  "M->" #'sp-slurp-hybrid-sexp)

(general-def
  :states '(normal visual)
  :keymaps '(go-mode-map rust-mode-map c-mode-map web-mode-map js-mode-map typescript-mode-map)
  "M-t" #'sp-transpose-hybrid-sexp
  "D" #'sp-kill-hybrid-sexp
  "M-r" #'sp-raise-hybrid-sexp
  "M-j" #'sp-push-hybrid-sexp)

;;;; org-mode
(major-leader-def 'normal org-mode-map
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

(general-def 'motion org-mode-map
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

;;;; Auxiliary Modes
;; TODO: Figure out mode-local artist-mode bindings?
(major-leader-def 'normal artist-mode-map
  "a" (general-simulate-key "C-c C-a"))

(use-package pretty-hydra)
(use-package hydra-posframe
  :straight (:host github :repo "Ladicle/hydra-posframe")
  :config
  (setq hydra-posframe-border-width 2)
  (hydra-posframe-mode))

;;; Custom theme
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:slant italic))))
 '(hl-paren-face ((t (:weight bold))) t)
 '(outline-1 ((t (:inherit org-level-1))))
 '(outline-2 ((t (:inherit org-level-2))))
 '(outline-3 ((t (:inherit org-level-3))))
 '(outline-4 ((t (:inherit org-level-4))))
 '(outline-5 ((t (:inherit org-level-5))))
 '(outline-6 ((t (:inherit org-level-6))))
 '(outline-7 ((t (:inherit org-level-7))))
 '(outline-8 ((t (:inherit org-level-8))))
 '(vdiff-addition-face ((t (:inherit diff-added))))
 ;; Match diff fringe highlighting background for higher visibility.
 '(diff-hl-insert ((t (:background "#60aa00"))))
 '(diff-hl-change ((t (:background "#da8548"))))
 '(diff-hl-delete ((t (:background "#d02b61")))))

(set-face-attribute 'diff-added nil
                    :background (color-darken-name "dark olive green" 10)
                    :foreground nil)

(set-face-attribute 'show-paren-match-expression nil
                    :inherit nil
                    :weight 'bold)

(set-face-attribute 'diff-changed nil
                    :background "dark slate grey"
                    :foreground nil)

;; Use a symbol for collapsed headings
(setq org-ellipsis " ▼")
(set-display-table-slot standard-display-table
                        'selective-display
                        (string-to-vector (symbol-value 'org-ellipsis)))

;; TODO: Consider selectively removing mouse bindings (i.e. mouse => visual mode)

;; Normalize evil keymaps for some modes that specify mode-local bindings
(general-add-hook '(vdiff-mode-hook lsp-mode-hook git-timemachine-mode-hook)
                  #'evil-normalize-keymaps)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 800 1000))
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
