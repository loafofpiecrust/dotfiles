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

(use-package org)

;; Easier binding definitions
(use-package general)

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
  (load-theme 'doom-dracula t)
  (doom-themes-org-config)
  (doom-themes-treemacs-config))

;; font test: `' o O 0 () [] {} *i
(set-face-attribute 'default nil
                    :family "SF Mono"
                    ;; Fonts need a bit of beefing on Mac
                    :height (if (eq system-type 'darwin) 120 115)
                    :weight 'medium
                    :width 'normal)

;; Disable tool-bar and menu-bar
(unless (eq system-type 'darwin)
  (progn (menu-bar-mode -1)))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode '(10 . 0))
(show-paren-mode)
(save-place-mode)

;; Setup basic settings
;; TODO: Move scroll stuff to only not on MacOS
(setq-default
 ;; Text display
 display-line-numbers-width 3
 show-paren-style 'expression
 ;; Indentation and wrapping
 tab-width 4
 indent-tabs-mode nil      ; use spaces for indentation
 fill-column 80
 ;; misc settings
 prettify-symbols-unprettify-at-point 'right-edge
 require-final-newline t
 load-prefer-newer t)

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
;; Only automatically wrap comments.
(setq-default comment-auto-fill-only-comments t
              auto-fill-function 'do-auto-fill)
(add-hook 'prog-mode-hook 'auto-fill-mode)
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
              which-key-idle-delay 0.7))

;; vim emulation
(use-package evil
  :init (setq evil-want-keybinding nil  ; let evil-collection bind keys
              evil-move-beyond-eol t
              evil-respect-visual-line-mode t
              ;; evil-move-cursor-back nil
              evil-search-module 'evil-search
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
(use-package evil-embrace :after evil
  :config (evil-embrace-enable-evil-surround-integration)
  :ghook ('org-mode-hook #'embrace-org-mode-hook))

;; gotta learn somehow
(use-package evil-tutor :defer t)

;; Exchange selections easily with
;; (use-package evil-exchange :after evil
;;   :config (evil-exchange-cx-install))

(use-package evil-args :after evil
  :general
  (:keymaps 'evil-inner-text-objects-map
            "a" 'evil-inner-arg)
  (:keymaps 'evil-outer-text-objects-map
            "a" 'evil-outer-arg))

(use-package evil-mc :after evil
  :general ('visual evil-mc-key-map
                    "A" #'evil-mc-make-cursor-in-visual-selection-end
                    "I" #'evil-mc-make-cursor-in-visual-selection-beg))

(use-package evil-matchit :after evil
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
;;;; Icons & Emojis
(use-package all-the-icons)
(use-package emojify
  :defer 1
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
  :after evil
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
(use-package evil-anzu :after evil anzu
  :config (setq anzu-cons-mode-line-p nil))

(use-package telephone-line
  :after evil-anzu
  :config
  (telephone-line-defsegment* telephone-line-anzu-segment ()
    '(:eval (anzu--update-mode-line)))
  (setq telephone-line-lhs '((evil . (telephone-line-evil-tag-segment))
                             (accent . (telephone-line-projectile-segment))
                             (nil . (telephone-line-erc-modified-channels-segment
                                     telephone-line-buffer-segment)))
        telephone-line-rhs '((nil . (telephone-line-anzu-segment
                                     telephone-line-misc-info-segment))
                             (accent . (telephone-line-major-mode-segment))
                             (evil . (telephone-line-airline-position-segment)))
        telephone-line-height 16)
  (telephone-line-mode t))

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

;;; Editing
;;;; General
(use-package editorconfig
  :ghook 'prog-mode-hook)
(use-package dtrt-indent
  :ghook 'prog-mode-hook)

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

(use-package highlight-parentheses
  :ghook 'prog-mode-hook)

;;;; org & outlines
(use-package outshine
  :ghook 'prog-mode-hook)

;;;; niceties
;; Highlight color codes in the buffer
(use-package rainbow-mode
  :defer 1
  :config (rainbow-mode))

;;;; undo-tree
(setq undo-tree-visualizer-timestamps t)

;;; Project management
(use-package projectile
  :config (projectile-mode t))

;; Use for searching within projects
(use-package ripgrep)

;; Project tree
(use-package treemacs
  :general ("C-\\" #'treemacs))
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
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-plus)
                                (ivy-bibtex . ivy--regex-ignore-order)
                                ;; Use fuzzy matching for most cases
                                (t . ivy--regex-fuzzy))
        projectile-completion-system 'ivy))

;; TODO: Figure out how to clump these latex packages
(use-package ivy-bibtex :defer 1 :after ivy)

(use-package counsel
  :config (counsel-mode))
(use-package ivy-rich
  :config (ivy-rich-mode))

(use-package flyspell-correct-ivy)

;;;; Helm
;; Helm is really good at certain things, like inserting references with
;; org-ref. We should explore using it for more stuff, and getting proper setup.
(use-package helm)

;;; in-buffer completion
(use-package yasnippet-snippets :defer 1)
(use-package yasnippet
  :after yasnippet-snippets company
  :config (yas-global-mode 1)
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
  :ghook '(prog-mode-hook eshell-mode-hook)
  :config
  ;; Press tab/shift-tab to start/stop completion in insert mode
  (general-def 'insert company-mode-map
    [remap indent-for-tab-command] #'company-indent-or-complete-common)
  ;; Press tab once in the dialog to complete the common prefix
  ;; Press tab twice in the dialog to complete with the selection
  (general-def :keymaps 'company-active-map
    "<tab>" 'company-complete
    "<backtab>" 'company-abort)
  ;; Show completion automatically upon typing anything
  (setq-default completion-ignore-case t
                completion-styles '(substring partial-completion)
                company-idle-delay nil
                company-minimum-prefix-length 1
                company-selection-wrap-around nil
                company-require-match nil))

;; more fuzzy completion
;; (use-package company-fuzzy
;;   :ghook 'company-mode-hook)

;; GUI box to prevent interference with different font sizes
(use-package company-box
  :custom
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  :ghook 'company-mode-hook)

;;; Version Control
;;;; Git
(use-package magit :defer 1)
;; TODO: Rebind magit file bindings behind SPC g

;; Provides evil friendly git bindings
(use-package evil-magit :after evil magit)
(use-package forge :after magit)
(use-package github-review :after magit)

;; View the history of the current file.
(use-package git-timemachine
  :defer t
  :config
  ;; (evil-make-overriding-map git-timemachine-mode-map 'motion)
  (add-hook 'git-timemachine-mode-hook #'evil-motion-state)
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
  :config
  (setq-default vdiff-magit-stage-is-2way t)
  (general-def '(normal motion) vdiff-mode-map
    minor-leader #'vdiff-hydra/body))

(use-package vdiff-magit
  :general (magit-mode-map
            "e" #'vdiff-magit-dwim
            "E" #'vdiff-magit))

;;; Writing
;;;; Spellcheck
;; Enable spellcheck in comments and strings (requires ispell)
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(setq-default flyspell-issue-message-flag nil
              ispell-program-name (executable-find "aspell")
              ispell-dictionary "en_GB-ize"
              ispell-extra-args '("--camel-case"))

;;;; Thesaurus
;; Requires internet to lookup words
(use-package powerthesaurus
  :commands powerthesaurus-lookup-word-dwim
  :config
  (global-leader-def '(normal motion) override
    "tt" '("thesaurus" . powerthesaurus-lookup-word-dwim)))

;;; Enable completion, pair matching, line numbers
;; TODO: Fix this for lsp-ui sideline stuff.
(use-package visual-fill-column
  :ghook 'markdown-mode-hook)

;; Stop auto-fill in certain modes where we'd rather break lines ourselves based
;; on sentences or what have you. We use visual-fill-column to make that easier
;; on the eyes.
(general-add-hook '(markdown-mode-hook) #'turn-off-auto-fill)

(global-subword-mode)
(global-prettify-symbols-mode)
;; (global-auto-revert-mode)
(global-visual-line-mode)
(column-number-mode)

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
  :straight (:host github :repo "manateelazycat/aweshell"))
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
  (setq lsp-inhibit-message t
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
  (add-hook 'lsp-mode-hook (lambda ()
                             (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
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
(use-package go-mode :defer 1
  :config
  (major-leader-def 'normal go-mode-map
    "ia" #'go-import-add)
  (major-leader-def '(normal motion) go-mode-map
    "ga" #'go-goto-arguments
    "gd" #'go-goto-docstring
    "gn" #'go-goto-function-name
    "gi" #'go-goto-imports))

(use-package racket-mode :defer 1)

;;;; Java
(use-package lsp-java :defer 1)
(use-package kotlin-mode :defer 1)
(use-package groovy-mode :defer 1)      ; for gradle build files
(use-package gradle-mode
  :ghook '(kotlin-mode-hook java-mode-hook groovy-mode-hook))

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
  "bf" #'counsel-find-file
  "bd" #'ranger
  "br" #'counsel-recentf
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
  "gt" #'git-timemachine
  "n" '(:keymap narrow-map)
  "p" '(:keymap projectile-command-map)
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
;; Letters I can rebind: ', =, 0/^, gd, maybe _, +, Q, <backspace>
(general-def 'normal
  "U" #'undo-tree-redo
  ;; Useful binding for managing method call chains
  "K" #'newline
  "z=" #'flyspell-correct-at-point
  "M-;" (lambda ()
          (interactive)
          (call-interactively 'comment-dwim)
          (evil-insert-state)))

(general-def help-map
  "K" #'which-key-show-top-level)

(general-def 'normal evil-cleverparens-mode-map
  "[" nil
  "]" nil
  "[]" #'evil-backward-section-begin
  "][" #'evil-forward-section-begin
  "[[" #'evil-cp-previous-closing
  "]]" #'evil-cp-next-closing)

(general-def '(normal motion visual)
  ;; "gr" stands for "go run this"
  ;; By default, use "gr" to refresh and run what's at point
  "gr" (general-simulate-key "C-c C-c")
  "[" '("previous")
  "]" '("next")
  "C-{" #'evil-jump-backward
  "C-}" #'evil-jump-forward
  "]t" #'hl-todo-next
  "[t" #'hl-todo-previous
  "?" #'swiper
  "0" (general-key "^")
  major-leader (general-simulate-key "C-c")
  minor-leader (lambda () (interactive)
                 (message "No minor mode commands here")))

;; Fix outline bindings in non-insert states
(general-def '(normal motion) outshine-mode-map
  "TAB" #'outshine-kbd-TAB)

;; General evil mode overrides
(general-def '(normal motion insert)
  "C-s" (general-key "C-x C-s"))

;;;; prog-mode
(major-leader-def '(motion normal) prog-mode-map
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

(general-def
  :states '(normal visual)
  :keymaps '(go-mode-map rust-mode-map c-mode-map)
  "M-t" #'sp-transpose-hybrid-sexp
  "D" #'sp-kill-hybrid-sexp
  "M-r" #'sp-raise-hybrid-sexp
  "M-j" #'sp-push-hybrid-sexp
  "M->" #'sp-slurp-hybrid-sexp)

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

(general-def '(motion normal) org-mode-map
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

;;; Custom theme
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-paren-face ((t (:weight bold))) t)
 '(org-document-title ((t (:weight bold :height 1.6))))
 '(org-level-1 ((t (:height 1.35 :inherit nil))))
 '(org-level-2 ((t (:height 1.2 :inherit nil))))
 '(org-level-3 ((t (:height 1.1 :inherit nil))))
 '(outline-1 ((t (:inherit org-level-1))))
 '(outline-2 ((t (:inherit org-level-2))))
 '(outline-3 ((t (:inherit org-level-3))))
 '(outline-4 ((t (:inherit org-level-4))))
 '(outline-5 ((t (:inherit org-level-5))))
 '(outline-6 ((t (:inherit org-level-6))))
 '(outline-7 ((t (:inherit org-level-7))))
 '(outline-8 ((t (:inherit org-level-8))))
 '(vdiff-addition-face ((t (:inherit diff-added))))
 '(diff-hl-insert ((t (:background (face-foreground 'diff-hl-insert))))))

(set-face-attribute 'diff-added nil
                    :background (color-darken-name "dark olive green" 10)
                    :foreground nil)

;; Match diff fringe highlighting background for higher visibility.
(dolist (f '(diff-hl-insert diff-hl-change diff-hl-delete))
  (set-face-attribute f nil
                      :background (face-foreground f)))

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

;; Swap flymake out for flycheck, ALWAYS
(add-hook 'flymake-mode-hook (lambda ()
                               (flymake-mode -1)
                               (flycheck-mode t)))

;; TODO: Consider selectively removing mouse bindings (i.e. mouse => visual mode)

;; Normalize evil keymaps for all modes that specify mode-local bindings
(general-add-hook '(vdiff-mode-hook lsp-mode-hook git-timemachine-mode-hook)
                  #'evil-normalize-keymaps)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 800 1000))
;; (setq gc-cons-percentage 0.5)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
