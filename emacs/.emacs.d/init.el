;;; -*- lexical-binding: t; -*-
;;; Status and Todos
;;;; Profiler Results
;; A few things cause slowdown on scroll:
;; - prefix-wrap-indent causes major lag
;; - highlight-indent-guides
;; - spaceline has a pretty small footprint

;;; Emacs Configuration
;;;; Bootstrap package manager
;; Fix TLS for emacs 26
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
;; Allow allocation of 512Mb before forcing garbage collection, but do collection when the
;; editor is idle for a few seconds, so that it's always fully responsive.
(setq gc-cons-threshold (eval-when-compile (* 512 1024 1024))
      gc-cons-percentage 0.4)
(run-with-idle-timer 2 t (lambda () (garbage-collect)))

;; I'm not planning to modify packages directly, so only rebuild on newly
;; published versions
(setq straight-check-for-modifications 'live)

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
(use-package no-littering)
(use-package esup :commands esup)

;; Easier binding definitions
(use-package general)
(use-package dash)
(use-package s)

;; Use bash for inferior shells to maintain compatibility and speed.
;; TODO: Use dash for this since it's extra fast.
(setq shell-file-name "/bin/sh")

;; TODO: Custom colors for refined vdiff chunks, letting us visualize
;; partial-line changes.

;; Terminal Benefits: Transparent background, pywal colors, faster. That's really all I
;; want from it.
;; GUI benefits: All keybindings, embedded render previews, different font
;; sizes, completion icons, posframe, better clipboard support, more complex rendering
;; support support, underlines colored differently than text, inline images.
;; DECISION: Stick to GUI emacs. Contribute back when there are issues.
;;           Try out terminal emacs sometimes. Clipboard integration & some bindings are
;; the main sticking points.

;; What Emacs has over other editors: org-mode for writing papers, tons of plugins,
;; scripting language. **MAGIT!** Restclient mode. undo-tree. heavy lisp support.
;; preview images inside buffers (org/latex). flexible keybinding.

;; What Emacs doesn't have: Speed!!, flexible text rendering, stable child frames, just open
;; folder, modern GUI, solid async operations, right click menu, smooth scroll + cursor
;; offscreen, debugging??,

;; Vim brings to the table: +speed, +ASYNC!, +better lsp support, +solid term support
;; cons: -vimscript, -messy bindings?, -limited org-mode, -less pretty undo-tree
;; -debugging is minimal

;; Conclusion: sticking with emacs, even if ONLY for magit. :D

;;;; GUI
;; Load theme early to avoid flickering
(use-package doom-themes
  :config
  (setq doom-themes-treemacs-enable-variable-pitch t
        doom-themes-treemacs-theme "doom-colors"
        doom-molokai-brighter-comments t
        doom-gruvbox-brighter-comments t)
  (load-theme 'doom-molokai t)
  (doom-themes-org-config)
  (doom-themes-treemacs-config))

(use-package ewal
  :disabled
  :straight (:host github :repo "emacsmirror/ewal")
  :config
  (use-package ewal-spacemacs-themes)
  (load-theme 'ewal-spacemacs-classic t)
  ;; Window dividers aren't needed if frames are outlined by the theme.
  (window-divider-mode -1)
  ;; (use-package ewal-evil-cursors
  ;;   :config (ewal-evil-cursors-get-colors :apply t))
  )

(set-face-attribute 'variable-pitch nil
                    :family "Overpass"
                    :height 120)

;; Render as many glyphs as possible with available fonts.
(use-package unicode-fonts
  :custom (unicode-fonts-restrict-to-fonts '("DevaVu Sans Mono"
                                             "DejaVu Sans"
                                             "Symbola"
                                             "Noto Sans"
                                             "Noto Sans Symbols"
                                             "Noto Sans Cherokee"
                                             "Material Design Icons"))
  :config
  (unicode-fonts-setup))


(use-package all-the-icons
  :config
  ;; Sometimes I use these icons, but they're non standard unicode I think.
  ;; (set-fontset-font "fontset-default" 'unicode "Material Design Icons")
  )

;; TODO: Add FreeSans as the backup font, to get obscure glyph support.
;; (set-fontset-font "fontset-default"
;;                   nil "FreeSans" nil 'append)

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'control
        mac-control-modifier 'super
        mac-mouse-wheel-smooth-scroll nil))

;; Setup basic settings
(setq-default
 ;; Text display
 display-line-numbers-width 4
 show-paren-style 'mixed
 x-underline-at-descent-line t
 ;; Nicer scrolling
 mouse-wheel-progressive-speed t
 mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
 ;; scroll-margin 15
 scroll-conservatively 10000
 shift-select-mode nil
 ;; Indentation and wrapping
 tab-width 4
 indent-tabs-mode nil                   ; use spaces for indentation
 fill-column 90
 ;; message-truncate-lines 5
 ;; misc settings
 ring-bell-function 'ignore
 warning-minimum-level :error
 prettify-symbols-unprettify-at-point 'right-edge
 require-final-newline t
 load-prefer-newer t
 custom-safe-themes t
 ;; Allow reading up to 1MB from external process output.
 read-process-output-max (* 1024 1024))

;; Should resolve issues with clicking around in the tree
(add-hook 'special-mode-hook (lambda () (setq scroll-margin 0)))

;; Empty scratch buffers
(setq initial-scratch-message "")

;; Easier confirmation
(fset 'yes-or-no-p 'y-or-n-p)

(defun extend-faces-to-eol (faces)
  "Extend the given faces to fill the line if they contain the end of it. Required for compatibility with Emacs 27"
  (when (>= emacs-major-version 27)
    (dolist (f faces)
      (set-face-attribute f nil :extend t))))

;; Copy PATH from shell
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;;;; Periodically clean buffers
(require 'midnight)
(setq clean-buffer-list-kill-regexps '("\\`\\*Man "
                                       "\\`\\*helpful "
                                       "\\`\\*Calc"
                                       "\\`\\*xref"
                                       "\\`\\*lsp"
                                       "\\`\\*company"
                                       "\\`\\*straight-process\\*"
                                       "\\`\\*Flycheck"
                                       "\\`\\*forge"
                                       "magit"
                                       "\\`\\*eshell"
                                       "Aweshell:")
      clean-buffer-list-delay-special (* 60 60 2)
      ;; Clean out potentially old buffers every hour
      midnight-period (* 60 60))
(midnight-mode)

;;;; Autofill
;; Automatically wrap comments in code
(setq-default comment-auto-fill-only-comments t
              auto-fill-function 'do-auto-fill)
(add-hook 'prog-mode-hook 'auto-fill-mode)

;; Cleanup whitespace after saving most files
(add-hook 'before-save-hook 'whitespace-cleanup)

;;;; Backup settings
(setq backup-by-copying t                                ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.cache/emacs")) ; no clutter!
      auto-save-file-name-transforms '((".*" "~/.cache/emacs" t))
      ;; TODO: Clean out ~/.cache/emacs every so often.
      delete-old-versions t
      create-lockfiles nil              ; with emacs server there's no need for lockfiles!
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      delete-by-moving-to-trash t)

;;; Evil mode
;; Show key combo helpers
(use-package which-key
  :init (setq which-key-enable-extended-define-key t)
  :config (which-key-mode))

;; vim emulation
(use-package evil
  :init (setq evil-want-keybinding nil  ; let evil-collection bind keys
              evil-move-beyond-eol t
              evil-respect-visual-line-mode t
              evil-move-cursor-back nil
              evil-search-module 'evil-search
              evil-symbol-word-search t
              evil-want-visual-char-semi-exclusive t
              evil-want-C-i-jump nil)
  :config
  (evil-mode t)
  ;; Stop completion when exiting insert mode
  (add-hook 'evil-insert-state-exit-hook 'company-abort))

;; bind keys for many modes with better evil compatibility
(use-package evil-collection
  :init
  (setq evil-collection-outline-bind-tab-p t)
  :config
  ;; There are just a few modes with undesirable bindings.
  (dolist (m '(go-mode))
    (delete m evil-collection--supported-modes))
  (evil-collection-init))

;; TODO: Put this somewhere organized...
(general-def 'motion
  "zg" 'evil-scroll-line-to-top)

;; commenting lines with verb 'gc'
(use-package evil-commentary
  :config (evil-commentary-mode))

;; surround things with verb 'S'
(use-package evil-surround
  :config
  (global-evil-surround-mode)
  (general-def 'visual evil-surround-mode-map
    "s" 'evil-surround-region
    "S" 'evil-Surround-region)
  (setq-default evil-surround-pairs-alist
                ;; Allow surrounding with newlines
                (cons '(13 . ("\n" . "")) evil-surround-pairs-alist)))

;; add more surroundings by default
(use-package evil-embrace
  :config
  (evil-embrace-enable-evil-surround-integration)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook))

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
  (general-def 'visual
    "A" 'evil-mc-make-cursor-in-visual-selection-end
    "I" 'evil-mc-make-cursor-in-visual-selection-beg)
  ;; Borrow these keybindings from VS/Atom/Sublime
  (general-def 'normal
    "gmq" 'evil-mc-undo-all-cursors
    "gmu" 'evil-mc-undo-last-added-cursor
    "C-n" (lambda ()
            (interactive)
            (when (eq nil (call-interactively 'evil-mc-undo-cursor-at-pos))
              (call-interactively 'evil-mc-make-cursor-here)
              (call-interactively 'evil-mc-pause-cursors)))
    "gmp" 'evil-mc-pause-cursors
    "gmr" 'evil-mc-resume-cursors
    "C-S-J" 'evil-mc-make-cursor-move-next-line
    "C-S-K" 'evil-mc-make-cursor-move-prev-line
    "C-S-<down>" 'evil-mc-make-cursor-move-next-line
    "C-S-<up>" 'evil-mc-make-cursor-move-prev-line))

;; Extend % to match more delimeters, smarter
(use-package evil-matchit
  :config (global-evil-matchit-mode))

(use-package posframe)

;; Show registers in popup when necessary!
(use-package evil-owl
  :config
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args '(:width 50 :height 200
                                              :left-fringe 8 :right-fringe 8))
  (evil-owl-mode))

;;;; setup prefixes
;; We're going to want a few different prefixes
;; , :: global commands for managing emacs
;; \ :: major mode key bindings
;; ' :: minor mode key bindings
;; = :: ???
;; g :: other vim commands/go to...
;; z :: change display

;; Modifiers:
;; C- :: Very common operations for navigation
;; S- :: Larger scope of operation
;; M- :: Common editing operations, especially dealing with expressions
(defconst global-leader ",")
(general-create-definer global-leader-def
  :prefix global-leader
  :states 'motion)

(defconst major-leader "'")
(general-create-definer major-leader-def
  :prefix major-leader
  :states 'normal)

;; Try to define mode-specific motions to "g" where possible.
(general-create-definer evil-g-def
  :prefix "g"
  :states 'motion)

;; Unbind any previous usage of leader keys.
(general-auto-unbind-keys)

;; Name prefix keys within my leader set for clarity.
(global-leader-def
  "a" '("apps")
  "e" '("eval")
  "m" '("modes")
  "c" '("change")
  "o" '("open")
  "d" '("delete")
  "g" '("git")
  "s" '("settings")
  "t" '("text"))

;;;; Escape anything
;; (general-def '(normal motion) [escape] 'keyboard-escape-quit)
(general-def minibuffer-local-map
  "ESC" 'minibuffer-keyboard-quit
  "C-j" 'next-line-or-history-element
  "C-k" 'previous-line-or-history-element)

(defun escape-from-something ()
  "Escapes whatever is currently plaguing you, whether multiple cursors or an auxiliary state."
  (interactive)
  (cond
   ((evil-mc-has-cursors-p) (evil-mc-undo-all-cursors))
   (t (evil-force-normal-state))))

(general-def 'normal
  "<escape>" 'escape-from-something)

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
  (global-leader-def
    "sd" 'purpose-toggle-window-purpose-dedicated))

(use-package ivy-purpose
  :after ivy
  :general (global-leader-def "b" 'ivy-purpose-switch-buffer-with-purpose))

;; Automatically manage window sizes
(use-package zoom
  :general ('motion "zx" 'zoom)
  :config
  (setq zoom-size '(0.7 . 0.7))
  ;; Make zoom work with purpose!
  ;; (add-hook 'purpose-select-buffer-hook 'zoom--update)
  ;; (add-hook 'xref-after-jump-hook 'zoom--update)
  )

;;;; Icons & Emojis
(use-package emojify
  :defer 1
  :config
  (setq emojify-emoji-styles '(unicode github))
  (global-emojify-mode t))

;;;; Fancy looks
;; Show marks in fringe for lines past EOF
(use-package vi-tilde-fringe
  :ghook '(prog-mode-hook text-mode-hook org-mode-hook))

(use-package hl-todo
  :ghook '(prog-mode-hook org-mode-hook text-mode-hook)
  :config
  (general-def 'motion hl-todo-mode-map
    "]t" 'hl-todo-next
    "[t" 'hl-todo-previous))

;;;; Dashboard!
(use-package page-break-lines)
(use-package dashboard
  :config
  (evil-set-initial-state 'dashboard-mode 'motion)
  ;; Load in both independent and client windows.
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-startup-banner 'logo
        dashboard-set-heading-icons t
        dashboard-center-content t
        dashboard-items '((recents . 10)
                          (projects . 10)))
  (dashboard-setup-startup-hook))

;;;; Mode line
;; hide minor mode lines
(use-package diminish)

;; Show search candidate counts in the mode-line
(use-package anzu
  :config
  (setq anzu-cons-mode-line-p nil)
  (global-anzu-mode))
(use-package evil-anzu :after anzu)

(use-package spaceline
  :disabled
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        powerline-default-separator 'wave)
  :config
  (spaceline-spacemacs-theme))

(use-package doom-modeline
  :custom
  (doom-modeline-icon t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-gnus nil)
  (doom-modeline-irc nil)
  (doom-modeline-project-detection 'project)
  :config (doom-modeline-mode))

;;;; Better help
(use-package helpful
  :after counsel
  :config
  (setq counsel-describe-function-function 'helpful-callable
        counsel-describe-variable-function 'helpful-variable)
  (general-def :keymaps 'help-map
    "f" 'counsel-describe-function
    "v" 'counsel-describe-variable
    "k" 'helpful-key
    "C" 'helpful-command))

;;;; Navigation
(use-package avy
  :general ('motion "go" 'avy-goto-char-timer))

(use-package origami
  :ghook '(prog-mode-hook markdown-mode-hook))

;;; Editing
;;;; General
(use-package editorconfig
  :ghook 'prog-mode-hook)
;; Auto-detect indent for flexibly indented languages
(use-package dtrt-indent
  :disabled
  ;; TODO: Disable this for polymode so Markdown indentation doesn't inherit to code blocks.
  :ghook '(web-mode-hook c-mode-hook java-mode-hook python-mode-hook))

;; Always indent, no matter what
;; FIXME: This package makes editing lag heavily. We should use a more hollistic on-demand
;; formatting solution. This real-time indenting is nice but honestly quite heavy.
(use-package aggressive-indent
  :disabled
  :config
  (global-aggressive-indent-mode)
  ;; Disable aggressive indent with LSP, since we have auto formatting and this slows
  ;; everything down a bunch.
  (general-add-hook '(lsp-mode-hook)
                    (lambda () (aggressive-indent-mode -1))))

;; This replaces aggressive indent by only indenting new lines.
(electric-indent-mode)

(use-package highlight-indent-guides
  :disabled
  :ghook '(prog-mode-hook org-mode-hook markdown-mode-hook)
  :config (setq highlight-indent-guides-method 'character))

;;;; Expressions
(defconst lisp-lang-hooks '(lisp-mode-hook
                            emacs-lisp-mode-hook
                            scheme-mode-hook
                            racket-mode-hook))

;; Make expression editing easier everywhere.
(use-package smartparens
  :config
  ;; Automatically open blocks in most languages (that don't provide this natively).
  (dolist (delim '("{" "(" "["))
    (sp-local-pair '(nix-mode go-mode c-mode javascript-mode swift-mode graphql-mode)
                   delim nil :post-handlers '(("||\n[i]" "RET"))))
  (require 'smartparens-config)
  :ghook
  '(prog-mode-hook conf-unix-mode-hook conf-toml-mode-hook)
  (lisp-lang-hooks 'smartparens-strict-mode)
  :gfhook '(show-smartparens-mode))

(use-package evil-cleverparens
  ;; :ghook 'smartparens-enabled-hook
  :init
  (setq evil-cleverparens-swap-move-by-word-and-symbol t
        evil-cleverparens-use-regular-insert t)
  :config
  (general-def 'normal evil-cleverparens-mode-map
    "[" nil
    "]" nil
    "[[" 'evil-cp-previous-closing
    "]]" 'evil-cp-next-closing
    "M-O" nil                           ; Support terminal arrow key movement.
    "M->" 'sp-slurp-hybrid-sexp))

;;;; niceties
;; Highlight color codes in the buffer.
(use-package rainbow-mode
  :config (rainbow-mode))

;; Highlight multiple layers of surrounding punctuation.
(use-package highlight-parentheses
  :ghook 'prog-mode-hook)

;;; Project management
(use-package projectile
  :config
  (projectile-mode t)
  (global-leader-def
    "p" '(:keymap projectile-command-map :wk "project")))

(use-package counsel-projectile
  :after counsel projectile
  :ghook 'counsel-mode-hook
  :config (general-def 'motion
            "C-f" 'counsel-projectile-rg))

;; Use for searching within projects
(use-package ripgrep)
(use-package deadgrep :commands deadgrep
  :config
  ;; I almost always want to open results in another window.
  (general-def 'normal deadgrep-mode-map
    "RET" 'deadgrep-visit-result-other-window
    "<S-return>" 'deadgrep-visit-result))

;; Project tree
(use-package treemacs
  :general ("C-\\" 'treemacs)
  :config
  (setq treemacs-is-never-other-window t
        treemacs-eldoc-display nil)
  (general-def treemacs-mode-map
    "p" '(:keymap treemacs-project-map))
  (general-def treemacs-project-map
    "w" 'treemacs-switch-workspace)
  (general-def projectile-command-map
    "w" 'treemacs-switch-workspace)
  ;; Mnemonic: "[o]pen [t]ree"
  (global-leader-def "ot" 'treemacs-select-window)

  (defun treemacs-create-temp-workspace ()
    "Create or rebuild a temporary workspace containing the current project."
    (interactive)
    ;; Delete the temporary workspace, if it exists.
    (let* ((names->workspaces (--map (cons (treemacs-workspace->name it) it) treemacs--workspaces))
           (ws (assoc "Temporary" names->workspaces)))
      (when ws (setq treemacs--workspaces (delete (cdr ws) treemacs--workspaces))))
    ;; Recreate the temporary workspace.
    (treemacs-do-create-workspace "Temporary")
    ;; Switch to it.
    (setf (treemacs-current-workspace)
          (--first (string= (treemacs-workspace->name it) "Temporary") treemacs--workspaces))
    ;; Add the current project.
    (treemacs-add-project-to-workspace (projectile-project-root))
    ;; Refresh related buffers and files.
    (treemacs--persist)
    (treemacs--invalidate-buffer-project-cache)
    (treemacs--rerender-after-workspace-change))

  (defun treemacs-project-in-workspace ()
    "Returns the current project if it's within the current workspace, otherwise nil."
    (let* ((proj (projectile-project-root)))
      (--first (string= (file-name-as-directory (treemacs-project->path it))
                        proj)
               (treemacs-workspace->projects (treemacs-current-workspace)))))

  (defun treemacs-switch-to-closest-workspace ()
    "Switch to the first workspace that contains this project, if it isn't in the current one."
    (interactive)
    (let* ((current-ws (treemacs-current-workspace))
           (proj (projectile-project-root)))
      (unless (treemacs-project-in-workspace)
        (setf (treemacs-current-workspace)
              (treemacs--find-workspace proj))
        (treemacs--rerender-after-workspace-change))))

  ;; Auxiliary support packages
  (use-package treemacs-evil :after evil)
  (use-package treemacs-projectile :after projectile)
  (use-package treemacs-magit :after magit)
  (use-package lsp-treemacs :after lsp))

;;; Syntax checking
(use-package flycheck
  :config
  ;; (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (general-def :keymaps 'help-map
    "e" 'flycheck-explain-error-at-point))

(use-package flycheck-posframe
  :disabled
  :ghook 'flycheck-mode-hook
  :config (flycheck-posframe-configure-pretty-defaults))

;;; mini-buffer completion
;; Completion scheme for everywhere:
;; <tab>     :: Next candidate
;; <backtab> :: Previous candidate
;; <return>  :: Complete with selection, if explicit action made or if in minibuffer.
(use-package flx)
(use-package ivy
  :ghook 'after-init-hook
  :config
  (general-def ivy-minibuffer-map
    ;; "TAB" 'ivy-next-line
    ;; "<backtab>" 'ivy-previous-line
    "C-j" 'ivy-next-line
    "C-k" 'ivy-previous-line
    "RET" 'ivy-alt-done
    "C-<return>" 'ivy-dispatching-done)
  (setq-default ivy-use-virtual-buffers t
                enable-recursive-minibuffers t
                ivy-count-format "(%d/%d) "
                ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-fuzzy)
                                        (ivy-bibtex . ivy--regex-ignore-order)
                                        (counsel-M-x . ivy--regex-ignore-order)
                                        ;; Use fuzzy matching for most cases
                                        (t . ivy--regex-fuzzy))
                projectile-completion-system 'ivy))

(use-package ivy-posframe
  :disabled
  :ghook 'ivy-mode-hook
  :config
  (setq ivy-posframe-display-functions-alist '((swiper . nil)
                                               (t . ivy-posframe-display-at-frame-center))
        ivy-posframe-parameters '((left-fringe . 8)
                                  (right-fringe . 8)
                                  (internal-border-width . 2))))

(use-package ivy-bibtex :commands ivy-bibtex)

(use-package counsel
  :ghook 'ivy-mode-hook
  :config (setcdr (assq 'counsel-M-x ivy-initial-inputs-alist) ""))

;; Show more details about ivy entries.
(use-package all-the-icons-ivy-rich
  :ghook 'ivy-mode-hook)
(use-package ivy-rich
  :ghook 'ivy-mode-hook
  :config (setcdr (assq t ivy-format-functions-alist) 'ivy-format-function-line))

(use-package flyspell-correct-ivy :commands flyspell-mode)

;; Use completion history to sort candidates.
(use-package prescient
  :config (prescient-persist-mode))
(use-package ivy-prescient
  :ghook 'ivy-mode-hook)

;;; in-buffer completion
(use-package yasnippet-snippets :after company)
(use-package yasnippet
  :after yasnippet-snippets
  :config
  (yas-global-mode 1)
  ;; Disable auto completion of snippets. Instead rely on the completion dialog.
  (general-def yas-minor-mode-map
    "TAB" nil
    "<tab>" nil
    [(tab)] nil))

(use-package company
  :ghook '(prog-mode-hook org-mode-hook)
  :config
  ;; Use Tab to complete the selected candidate.
  (general-def :keymaps 'company-active-map
    "<tab>" 'company-complete-selection)
  ;; Return finishes completion only if you've explicitly picked a candidate.
  (general-def :keymaps 'company-active-map
    :predicate '(company-explicit-action-p)
    "RET" 'company-complete-selection
    "<return>" 'company-complete-selection)
  (general-def :keymaps 'company-mode-map
    "C-SPC" 'company-complete)
  ;; Show completion automatically upon typing anything
  (setq-default completion-ignore-case t
                completion-styles '(basic partial-completion substring)
                ;; completion-styles '(flex)
                company-idle-delay 0
                company-minimum-prefix-length 1
                company-selection-wrap-around nil
                company-tooltip-align-annotations t
                company-dabbrev-downcase nil
                company-auto-complete 'company-explicit-action-p
                company-require-match nil))

;; GUI box to prevent interference with different font sizes
(use-package company-box
  :ghook 'company-mode-hook
  :custom
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  (company-box-max-candidates 50)
  (company-box-doc-delay 2)
  (company-box-show-single-candidate t))

(use-package compdef :after company)

;; Define completion backends for particular modes.
;; This reduces overhead for completion and gives us the best results.
(compdef
 :modes 'org-mode
 :company '((company-capf :with company-yasnippet))
 :capf 'pcomplete-completions-at-point)

(compdef
 :modes 'prog-mode
 :company '((company-capf company-keywords company-dabbrev-code :with company-yasnippet)))

;; FIXME: Fuzzy doesn't expand snippets.
(use-package company-fuzzy
  :disabled
  :ghook 'company-mode-hook)

;;; Version Control
;;;; Git
(use-package magit
  :gfhook 'hl-line-mode
  :general (global-leader-def
             "gg" 'magit-status
             "gb" 'magit-blame)
  :config
  (setq auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p
        magit-refresh-status-buffer nil)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (extend-faces-to-eol '(magit-diff-context-highlight
                         magit-diff-removed-highlight
                         magit-diff-added-highlight
                         magit-diff-hunk-heading-highlight
                         magit-diff-hunk-heading)))

;; Provides evil friendly git bindings
(use-package evil-magit :after magit
  :config
  (evil-set-initial-state 'git-commit-mode 'insert))
(use-package forge :after magit)
(use-package github-review :after magit)

;; View the history of the current file.
(use-package git-timemachine
  :general (global-leader-def "gt" 'git-timemachine)
  :gfhook 'evil-motion-state
  :config
  (general-def 'motion git-timemachine-mode-map
    "[r" 'git-timemachine-show-previous-revision
    "]r" 'git-timemachine-show-next-revision))

;; Show changed lines in the margin
(use-package diff-hl
  :gfhook ('magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode)
  (diff-hl-dired-mode)
  (diff-hl-flydiff-mode)
  (setq-default diff-hl-flydiff-delay 0.2)
  ;; TODO: Rebind this under another leader (major?)
  ;; (evil-g-def 'normal diff-hl-mode-map
  ;;   "rh" 'diff-hl-revert-hunk)
  (general-def 'motion diff-hl-mode-map
    "[c" 'diff-hl-previous-hunk
    "]c" 'diff-hl-next-hunk))

;; Don't ask me when following symlinks
(setq vc-follow-symlinks t)

;;;; Diff configuration
;; Replace ediff with vdiff for synced scrolling and more...
(use-package vdiff
  :commands vdiff-mode
  :config
  (setq-default vdiff-magit-stage-is-2way t)
  (major-leader-def 'motion vdiff-mode-map
    "m" 'vdiff-hydra/body))

(use-package vdiff-magit
  :general (magit-mode-map
            "e" 'vdiff-magit-dwim
            "E" 'vdiff-magit))

;; Define just a few bindings for smerge, but it has pretty great mouse support
;; for handling conflicts by right clicking.
;; (minor-leader-def 'motion smerge-mode-map
;;   "k" 'smerge-keep-upper
;;   "j" 'smerge-keep-lower
;;   "a" 'smerge-keep-all)
(general-def 'motion smerge-mode-map
  "[c" 'smerge-prev
  "]c" 'smerge-next)

;;; Writing
;;;; Spellcheck
;; Load flyspell async to prevent blocking on file load
(use-package flyspell-lazy
  :disabled
  :ghook 'flyspell-mode-hook)
;; Enable spellcheck in comments and strings (requires ispell)
;; (general-add-hook '(markdown-mode-hook org-mode-hook) 'flyspell-mode)
;; (general-add-hook 'prog-mode-hook 'flyspell-prog-mode)
(setq-default flyspell-issue-message-flag nil
              ispell-program-name (executable-find "aspell")
              ispell-dictionary "en_US"
              ispell-extra-args '("--camel-case"))

(general-def 'motion flyspell-mode-map
  "]s" 'flyspell-goto-next-error)

;; This should generally replace flyspell for us. Only checks words currently on-screen.
(use-package spell-fu
  :ghook '(prog-mode-hook markdown-mode-hook org-mode-hook)
  :general (general-def 'motion
             "]s" 'spell-fu-goto-next-error
             "[s" 'spell-fu-goto-previous-error))

;;;; Thesaurus
;; Requires internet to lookup words
(use-package powerthesaurus
  :general
  (global-leader-def
    "tt" '(powerthesaurus-lookup-word-dwim :wk "thesaurus")))

;;; Enable completion, pair matching, line numbers
(use-package visual-fill-column
  :ghook '(markdown-mode-hook)
  :config
  (setq-default visual-fill-column-width 110))

;; Smartly indent wrapped lines
(use-package adaptive-wrap
  :disabled
  :ghook ('visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(general-add-hook '(org-mode-hook conf-toml-mode-hook markdown-mode-hook)
                  #'electric-pair-local-mode)

;;;; Python
(add-to-list 'auto-mode-alist '("Pipfile\\'" . conf-toml-mode))
(use-package pipenv :ghook 'python-mode-hook)

;; Stop auto-fill in certain modes where we'd rather break lines ourselves based
;; on sentences or what have you. We use visual-fill-column to make that easier
;; on the eyes.
(general-add-hook '(markdown-mode-hook) 'turn-off-auto-fill)

;; Separate words in camelCase symbol names
(global-subword-mode)
;; (global-prettify-symbols-mode)
;; (global-auto-revert-mode)
(global-visual-line-mode)
(column-number-mode)

(general-add-hook '(prog-mode-hook text-mode-hook org-mode-hook conf-unix-mode-hook conf-toml-mode-hook)
                  'display-line-numbers-mode)

;; Let special modes handle lines however they want to.
(general-add-hook '(special-mode-hook dired-mode-hook)
                  (lambda () (visual-line-mode -1)))

;; (setq-default display-line-numbers-type 'visual)
(defun cycle-line-numbers ()
  (interactive)
  (setq display-line-numbers
        (if (eq (symbol-value 'display-line-numbers) t)
            'visual t)))

(use-package expand-region
  :general ('visual "=" 'er/expand-region
                    "-" 'er/contract-region))

;;; Auxiliary Modes
;;;; REST client
(use-package restclient
  :mode "\\.http\\'")
;; (use-package verb
;;   :mode "\\.verb\\'")

;;;; shell extensions
;; (use-package aweshell
;;   :disabled
;;   :straight (:host github :repo "manateelazycat/aweshell")
;;   :commands aweshell)

;;;; dired
;; Provide a ranger-like interface for dired
(use-package ranger
  :general (global-leader-def "od" 'ranger))

;;; Programming Languages
(use-package dumb-jump
  :defer t
  :config (setq dumb-jump-selector 'ivy))

;;;; Language Server Protocol!
;; lsp in conjunction with company and flycheck gives us easy auto-complete and
;; syntax checking on-the-fly.
(use-package lsp-mode
  :custom (lsp-eldoc-render-all nil)
  :config
  (setq-default lsp-inhibit-message t
                lsp-prefer-flymake nil
                lsp-enable-on-type-formatting nil
                lsp-signature-auto-activate t
                lsp-signature-doc-lines 1
                lsp-rust-server 'rust-analyzer)
  (evil-g-def 'motion lsp-mode-map
    "d" 'lsp-find-definition
    "fi" 'lsp-goto-implementation
    "ft" 'lsp-goto-type-definition
    "fr" 'lsp-find-references)
  (evil-g-def 'normal lsp-mode-map
    "r" '("refactor")
    "rr" 'lsp-rename
    "rf" 'lsp-format-buffer
    "re" 'lsp-execute-code-action
    "ri" 'lsp-organize-imports)
  ;; Format code on save
  (add-hook 'lsp-mode-hook
            (lambda () (add-hook 'before-save-hook
                            'lsp-format-buffer
                            nil t)))

  :init
  (general-add-hook '(go-mode-hook
                      rust-mode-hook
                      java-mode-hook
                      kotlin-mode-hook
                      ruby-mode-hook
                      python-mode-hook
                      typescript-mode-hook
                      web-mode-hook
                      julia-mode-hook
                      js-mode-hook)
                    (lambda () (unless (bound-and-true-p polymode-mode)
                            (lsp-deferred))))
  :commands (lsp lsp-deferred)
  :compdef lsp-mode
  :company ((company-capf :with company-yasnippet)))

;; Show contextual code documentation pop-ups
(use-package lsp-ui
  :ghook 'lsp-mode-hook
  :custom
  ;; (lsp-ui-flycheck-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-enable nil)
  :config
  (defun lsp-ui-doc-toggle ()
    (interactive)
    (if (lsp-ui-doc--visible-p)
        (lsp-ui-doc-hide)
      (lsp-ui-doc-show)))
  ;; TODO: Consider zf- here, since these are display changes. This mirrors gf-.
  (evil-g-def 'motion lsp-mode-map
    "p" '("peek")
    "pt" 'lsp-ui-doc-toggle
    "pr" 'lsp-ui-peek-find-references
    "pd" 'lsp-ui-peek-find-definitions
    "pi" 'lsp-ui-peek-find-implementation))

;;;; Debug Adapter Protocol!!
(use-package dap-mode
  :ghook 'lsp-mode-hook
  :config
  ;; Open the hydra when we hit a breakpoint
  (add-hook 'dap-stopped-hook 'dap-hydra)
  ;; TODO: Is this needed when we can click on the gutter? Figure out the best binding for this.
  ;; TODO: Consider binding under "g" to get rid of this major-leader
  (major-leader-def 'normal dap-mode-map
    "d" '("debug")
    "db" 'dap-breakpoint-toggle)
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode t))

;;;; One liners
;; Default dot-files to conf
(add-to-list 'auto-mode-alist '("\\.env\\'" . conf-unix-mode))
(use-package haskell-mode
  :mode (("\\.hsc\\'" . haskell-mode)
         ("\\.l[gh]s\\'" . literate-haskell-mode)
         ("\\.hsig\\'" . haskell-mode)
         ("\\.[gh]s\\'" . haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)
         ("\\.ghci\\'" . ghci-script-mode)
         ("\\.hcr\\'" . ghc-core-mode)))
(use-package nix-mode :mode "\\.nix\\'")
(use-package company-nixos-options
  :after nix-mode
  :compdef nix-mode
  :company ((company-nixos-options company-dabbrev :with company-yasnippet)))
(use-package fish-mode :mode "\\.fish\\'")
(use-package bazel-mode)
(use-package dockerfile-mode :mode "Dockerfile\\'")
(use-package terraform-mode :mode "\\.tf\\(vars\\)?\\'")
(use-package company-terraform
  :after terraform-mode
  :compdef terraform-mode
  :company (company-dabbrev company-terraform company-yasnippet))
(use-package git-modes)
(use-package yaml-mode)
(use-package json-mode
  :mode "\\(?:\\(?:\\.json\\|\\.jsonld\\|\\.babelrc\\|\\.bowerrc\\|composer\\.lock\\)\\'\\)")
(use-package swift-mode :mode "\\.swift\\'")
(use-package rust-mode :mode "\\.rs\\'")
(use-package go-mode
  :mode "\\.go\\'"
  :config
  ;; Code navigation key bindings
  ;; TODO: Integrate better with major-leader?
  (evil-g-def 'normal go-mode-map
    "ai" 'go-import-add)
  (evil-g-def 'motion go-mode-map
    "fa" 'go-goto-arguments
    "fd" 'go-goto-docstring
    "fn" 'go-goto-function-name
    "fi" 'go-goto-imports)

  ;; Setup debugging support when go-mode starts.
  (general-add-hook 'go-mode-hook
                    (lambda ()
                      (require 'dap-go)
                      (dap-go-setup))
                    nil nil t))

(use-package racket-mode :mode "\\.rkt[dl]?\\'")

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

(use-package julia-mode :mode "\\.jl\\'")
(use-package lsp-julia)

;;;; Java
(use-package lsp-java :commands java-mode)
(use-package kotlin-mode :mode "\\.kts?\\'")
(use-package groovy-mode)      ; for gradle build files
(use-package gradle-mode
  :ghook '(kotlin-mode-hook java-mode-hook groovy-mode-hook))

;;;; javascript and typescript
(use-package eslint-fix :commands eslint-fix)

;; (use-package eldoc-box
;;   :ghook ('eldoc-mode-hook 'eldoc-box-hover-mode))

(defun custom-tide-setup ()
  (tide-setup)
  (tide-hl-identifier-mode 1))
;; TODO: Use eslint before save

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)))

(use-package typescript-mode :mode "\\.ts\\'")
;; TODO Resolve tsx dilemma of TS syntax with embedded JSX. web-mode is inconsistent, and
;; js-mode doesn't support TS highlighting.
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . javascript-mode))

;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (let ((ext (file-name-extension buffer-file-name)))
;;               (when (or (string-equal ext "tsx")
;;                         (string-equal ext "jsx"))
;;                 (custom-tide-setup)))))

;; Use eslint for js/ts
;; FIXME!
;; (flycheck-disable-checker 'typescript-tslint)
;;(flycheck-disable-checker 'javascript-jshint)
;;(flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-mode 'javascript-eslint 'typescript-mode)


;;;; GraphQL
(use-package graphql-mode
  :mode (("\\.gql\\'" . graphql-mode)
         ("\\.graphql\\'" . graphql-mode)))

;;;; typesetting
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.mdx\\'" . markdown-mode))
  :config
  (setq markdown-indent-on-enter 'indent-and-new-item)
  (evil-g-def 'normal markdown-mode-map
    "as" 'markdown-insert-gfm-code-block)
  (general-def markdown-mode-map
    "C-j" 'markdown-next-visible-heading
    "C-k" 'markdown-previous-visible-heading
    "C-*" 'markdown-insert-list-item
    "S-RET" (lambda () (interactive)
              (newline)
              (markdown-indent-line))))
(use-package polymode
  :config
  (general-def
    :states 'motion
    :keymaps 'polymode-minor-mode-map
    "znc" 'polymode-toggle-chunk-narrowing))
(use-package poly-markdown
  :commands (gfm-mode markdown-mode))
(use-package poly-org
  :commands (org-mode))
;; org-mode additions
(use-package org
  :ensure nil
  :general
  (general-def 'motion org-mode-map
    "zt" 'org-show-todo-tree
    ;; Mnemonic is "show preview"
    "zp" 'org-toggle-latex-fragment)
  (major-leader-def 'normal org-mode-map
    "e" 'org-export-dispatch
    ;; Mirror some evil agenda commands here for symmetry.
    "c" '("change")
    "cs" 'org-schedule
    "cd" 'org-deadline
    "ct" 'org-set-tags-command
    "ce" 'org-set-effort
    "I" 'org-clock-in
    "O" 'org-clock-out
    "s" '("sorting")
    "ss" 'org-sort
    "da" 'org-archive-subtree-default
    "r" 'org-refile
    "a" 'org-agenda
    "i" '("insert")
    "ih" 'org-table-insert-hline
    "il" 'org-insert-link
    "t" 'org-todo)
  :config
  (setq org-fontify-emphasized-text t
        org-highlight-latex-and-related '(native script entities)
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
        org-export-with-smart-quotes t
        org-agenda-custom-commands '(("u" "Unscheduled TODOs"
                                      ((todo ""
                                             ((org-agenda-overriding-header "Unscheduled TODOs")
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled)))))
                                      nil nil))
        ;; Make sure bibtex runs on PDF export.
        org-latex-compiler "lualatex"
        org-latex-pdf-process (list "latexmk -bibtex -f -pdf -%latex %f")
        org-latex-prefer-user-labels t)
  ;; Delete intermediate files in org -> LaTeX -> PDF export.
  (eval-after-load 'ox-latex '(progn
                                (add-to-list 'org-latex-logfiles-extensions "tex")
                                (add-to-list 'org-latex-logfiles-extensions "bbl"))))

(use-package org-ref :after org
  :custom (org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (major-leader-def 'normal org-mode-map
    "ir" 'org-ref-insert-ref-link
    "ic" 'org-ref-insert-link))

;; Replaces star headings with pretty bullet characters.
(use-package org-superstar :ghook 'org-mode-hook)
(use-package org-sticky-header :ghook 'org-mode-hook)

;; Might fix org-bullets?
(setq inhibit-compacting-font-caches t)



;; (eval-after-load 'org
;;   (org-babel-do-load-languages 'org-babel-load-languages
;;                                '((shell . t))))

;;;; Latex
;; latex packages have to go near the top for some reason.
;; otherwise they just mysteriously don't load.
;; Because 'auctex is doodled, must use straight directly here.
;; (straight-use-package 'auctex)
;; latex additions
(add-hook 'TeX-mode-hook 'TeX-fold-mode)
(setq-default TeX-engine 'xetex) ; enables unicode support

;;;; Formatting
(use-package format-all
  :general (evil-g-def 'normal :keymaps '(prog-mode-map)
             "rf" 'format-all-buffer))

;;; Mode-specific keybindings
;;;; global
(defun switch-to-alternate-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(general-def '(normal motion)
  "SPC" (general-key "M-x"))

(use-package kill-or-bury-alive
  :general (global-leader-def "dd" 'kill-or-bury-alive))

;; Contextual leader key
;; Generic leader key as space
(global-leader-def
  "," 'other-window
  "m" 'counsel-major
  "r" '(:wk "recover")
  "rr" 'recover-this-file
  "rb" 'revert-buffer
  "w" '(:wk "write")
  "ww" (general-simulate-key "C-x C-s")
  "wf" 'write-file                      ; akin to "Save as..."
  "w," 'save-some-buffers
  "dw" 'delete-window
  "d," 'delete-other-windows
  "db" 'kill-buffer
  "dp" 'posframe-delete-all           ; Only for emergencies
  "df" 'delete-file
  "of" 'counsel-find-file
  "or" 'counsel-recentf
  "oo" 'switch-to-alternate-buffer
  "ow" 'view-buffer-other-window
  "ob" 'ivy-switch-buffer
  "oh" 'split-window-right
  "ov" 'split-window-below
  "oc" 'calc
  "os" 'eshell
  "oe" 'flycheck-list-errors
  "b" 'ivy-purpose-switch-buffer-with-purpose
  "/" 'deadgrep
  "u" 'undo-tree-visualize
  ;; Fix expression evaluation position in normal state
  ;; Our cursor is technically before the character we're on, in normal state.
  "ee" (lambda ()
         (interactive)
         (save-excursion
           (evil-forward-char 1)
           (call-interactively 'eval-last-sexp)))
  "ed" (general-key "C-M-x")
  "gs" 'vdiff-magit-stage
  "gp" (lambda ()
         (interactive)
         (magit-remote-prune "origin")
         (magit-shell-command-topdir "git prune-removed"))
  "ca" 'artist-mode
  "ci" 'set-input-method
  "cw" 'read-only-mode
  "sl" 'cycle-line-numbers
  "si" 'toggle-input-method
  "sw" 'whitespace-mode
  "sh" 'hl-line-mode
  "sr" 'auto-revert-mode
  "h" '(:keymap help-map :wk "help"))

(general-def 'motion
  "zw" 'count-words)
;; Single-key save binding because saving is so common!
(general-def 'normal
  "<print>" (general-simulate-key "C-x C-s"))

(general-def
  :states 'motion
  :prefix "zn"
  nil '("narrow")
  "d" 'narrow-to-defun
  "n" 'narrow-to-region
  "p" 'narrow-to-page
  "w" 'widen)

(use-package hungry-delete
  :config (global-hungry-delete-mode))

;;;; vim
(general-def help-map
  "K" 'which-key-show-top-level)

;; Unbind evil keys that we want to use ourselves.
;; Letters I can rebind: ', =, 0/^, gd, maybe _, +, Q, <backspace>, C-k, C-j, R, s, S
(general-unbind '(motion normal)
  ;; Disable dragging mouse selection.
  ;; This fixes buttons, changing focus, basically anything using mouse.
  "<down-mouse-1>"
  "C-o" "C-p" "C-f" "C-w" "C-," "C-."
  "R" "Q" "=")
(general-unbind 'insert
  "C-j" "C-k")

;; Swap word motions so that the unshifted forms move symbols rather than
;; sub-words, giving the shifted forms the more granular action.
;; Result is w = symbol, W = (sub)word, o = WORD (includes symbols)
(general-swap-key nil '(evil-inner-text-objects-map evil-outer-text-objects-map)
  "w" "W"
  "w" "o")

(general-swap-key nil 'evil-motion-state-map
  "w" "W"
  "b" "B"
  "e" "E"
  "ge" "gE")

(defun evil-paste-at-point ()
  (interactive)
  (evil-backward-char 1 nil t)
  (call-interactively 'evil-paste-after))

;; TODO: Use a better command for nearby find on key f
(general-def 'normal
  ;; Make normal paste go AT point rather than AFTER
  "p" 'evil-paste-at-point
  "U" 'undo-tree-redo
  ;; Useful binding for managing method call chains
  "K" 'newline
  "z=" 'flyspell-correct-at-point)

;; Use some standard keybindings.
(general-unbind 'insert
  "C-v" "C-z" "C-y" "C-w")
(general-def
  ;; Global common bindings for emacs & insert states
  "C-v" 'yank
  "C-z" 'undo-tree-undo
  ;; "C-y" 'undo-tree-redo
  "C-o" 'counsel-find-file
  "C-s" (general-key "C-x C-s")
  "C-=" 'text-scale-increase
  "C--" 'text-scale-decrease
  ;; I don't like drag-selection by mouse.
  "<down-mouse-1>" nil
  "<drag-mouse-1>" nil)

(general-def minibuffer-local-map
  "C-v" 'yank)

(general-def '(normal insert)
  ;; TODO: Decide how I want to combine these commands based on general rules
  ;; about my modifier keys: C, M, S.
  "M-;" (lambda ()
          (interactive)
          (call-interactively 'comment-dwim)
          (evil-insert-state)))

(general-def 'visual
  "M-;" 'comment-dwim)

(general-def 'motion
  ;; "gr" stands for "go run this"
  ;; By default, use "gr" to refresh and run what's at point
  "gr" (general-simulate-key "C-c C-c")
  "gs" 'imenu
  "C-RET" (general-simulate-key "C-c C-c")
  ;; Make it really easy to execute commands!
  "SPC" (general-key "M-x")
  ;; Make -/+ for navigating line starts more ergonomic.
  "=" 'evil-next-line-first-non-blank
  "[" '("previous")
  "]" '("next")
  ;; C-[ Requires special handling to avoid equaling ESC
  "C-{" 'evil-jump-backward
  "C-}" 'evil-jump-forward
  "[]" 'evil-backward-section-begin
  "][" 'evil-forward-section-begin
  "[p" 'evil-backward-paragraph
  "]p" 'evil-forward-paragraph
  "[e" 'previous-error
  "]e" 'next-error
  "?" 'swiper
  "0" (general-key "^"))

(general-def 'normal
  ;; Stands for "go quit" to quit some auxiliary mode, like editing a git
  ;; commit message.
  "gq" (general-key "C-c C-k"))

;; (general-def '(normal motion)
;;   :keymaps '(outshine-mode-map outline-mode-map)
;;   "C-j" 'outline-forward-same-level
;;   "C-k" 'outline-backward-same-level)


;;;; prog-mode
(general-unbind 'normal "gf")
(evil-g-def 'motion prog-mode-map
  ;; "r" '("refactor")
  "f" '("find")
  ;; "fd" 'xref-find-definitions
  "fr" 'xref-find-references
  "fj" 'dumb-jump-go
  "/" 'dumb-jump-go-prompt)

(general-def '(normal insert) prog-mode-map
  "M-RET" 'comment-indent-new-line)

(general-def
  :states '(normal visual)
  :keymaps '(go-mode-map rust-mode-map c-mode-map web-mode-map js-mode-map typescript-mode-map)
  "M-t" 'sp-transpose-hybrid-sexp
  "D" 'sp-kill-hybrid-sexp
  "M-r" 'sp-raise-hybrid-sexp
  "M-j" 'sp-push-hybrid-sexp)

;;;; org-mode
;; Give org-mode some evil keybindings
(use-package evil-org
  :ghook 'org-mode-hook
  :config
  (evil-org-set-key-theme)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (general-def 'motion org-agenda-mode-map
    "r" 'org-agenda-refile
    "cs" 'org-agenda-schedule
    "cd" 'org-agenda-deadline))

;;;; Auxiliary Modes
;; TODO: Figure out mode-local artist-mode bindings?
(major-leader-def 'normal artist-mode-map
  "a" (general-simulate-key "C-c C-a"))

;; (use-package pretty-hydra :defer t)

;;;; org & outlines
(use-package outshine
  :ghook 'prog-mode-hook
  :config
  ;; Fix outline bindings in non-insert states
  (general-def
    :keymaps 'outshine-mode-map
    ;; :predicate '(outline-on-heading-p)
    ;; "M-k" 'outline-move-subtree-up
    ;; "M-j" 'outline-move-subtree-down
    "TAB" 'outshine-kbd-TAB))

;; TODO: Consider rebinding [[ and ]], as I haven't used them as-is.
(general-def :keymaps '(outline-mode-map outline-minor-mode-map)
  "C-j" 'outline-next-heading
  "C-k" 'outline-previous-heading)

;;; Custom theme
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-selection ((t (:extend t))))
 '(diff-hl-change ((t (:background "#da8548"))))
 '(diff-hl-delete ((t (:background "#d02b61"))))
 '(diff-hl-insert ((t (:background "#60aa00"))))
 ;; Change wavy underline to straight style for readability.
 '(flycheck-error ((t (:underline "#e74c3c"))))
 '(flycheck-info ((t (:underline "#b6e63e"))))
 '(flycheck-warning ((t (:underline "#e2c770"))))
 '(flyspell-duplicate ((t (:underline "#e2c770"))))
 '(flyspell-incorrect ((t (:underline "#e74c3c"))))
 '(hl-line ((t (:extend t))))
 '(hl-paren-face ((t (:weight bold :background nil))) t)
 '(ivy-current-match ((t (:extend t))))
 ;; '(ivy-posframe ((t (:inherit default))))
 '(markdown-code-face ((t (:extend t))))
 '(outline-1 ((t (:inherit org-level-1))))
 '(outline-2 ((t (:inherit org-level-2))))
 '(outline-3 ((t (:inherit org-level-3))))
 '(outline-4 ((t (:inherit org-level-4))))
 '(outline-5 ((t (:inherit org-level-5))))
 '(outline-6 ((t (:inherit org-level-6))))
 '(outline-7 ((t (:inherit org-level-7))))
 '(outline-8 ((t (:inherit org-level-8))))
 '(vdiff-addition-face ((t (:inherit diff-added)))))

;; TODO: Bind f3 to kmacro-end-and-call-macro

;; (setq-default window-divider-default-right-width 2
;;               window-divider-default-bottom-width 2
;;               window-divider-default-places t)

(set-face-attribute 'diff-added nil
                    :background (color-darken-name "dark olive green" 10)
                    :foreground nil)

(set-face-attribute 'show-paren-match-expression nil
                    :inherit nil
                    :background nil
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
                  'evil-normalize-keymaps)

;;; My custom packages!
(add-to-list 'load-path "~/.emacs.d/custom")

;; IM for typing the Cherokee syllabary.
(require 'cherokee-input)

;;; Final Adjustments

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(put 'narrow-to-region 'disabled nil)

;; Suppress common error messages that get annoying.
(defun my-command-error-function (data context caller)
  "Ignore common benign error signals like end-of-buffer;
   pass the rest to the default handler."
  (when (not (memq (car data) debug-ignored-errors))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)

(use-package tree-sitter
  :straight (:host github :repo "ubolonton/emacs-tree-sitter"))

(defun tree-sitter-node-at-point ()
  "Return the node containing point in the current syntax tree"
  (interactive)
  ;; Traverse the tree starting at root until we find the right node.
  (let ((pt (point))
        (root (ts-root-node tree-sitter-tree)))
    (ts-get-descendant-for-position-range root pt (+ pt 1))))

(defun tree-sitter-node-in-selection ()
  (ts-get-descendant-for-position-range (ts-root-node tree-sitter-tree)
                                        (region-beginning)
                                        (region-end)))

(defun tree-sitter-selected-node ()
  (if mark-active
      (tree-sitter-node-in-selection)
    (tree-sitter-node-at-point)))

(defun tree-sitter-select-node ()
  "Select the node containing point in the syntax tree"
  (interactive)
  (let ((node (tree-sitter-node-at-point)))
    (evil-visual-select (ts-node-start-position node) (- (ts-node-end-position node) 1))))

(defun tree-sitter-expand-selection ()
  "Expand the selection past the node contained by selection"
  (interactive)
  (if mark-active
      (let* ((node (tree-sitter-node-in-selection))
             (parent (ts-get-parent node)))
        (evil-visual-select (ts-node-start-position parent)
                            (- (ts-node-end-position parent) 1)))
    (tree-sitter-select-node)))

(defun tree-sitter-slurp ()
  (interactive)
  (let* ((node (ts-get-ancestor-by (tree-sitter-node-at-point)
                                   (lambda (n) (ts-get-next-sibling n))))
         (sibling (ts-get-next-sibling node))
         (text (delete-and-extract-region (ts-node-start-position sibling)
                                          (ts-node-end-position sibling))))
    (save-excursion
      (goto-char (- (ts-node-end-position node) 1))
      (insert text))))

(defun tree-sitter-barf ()
  "Barf the last expression from the selected syntax element."
  (interactive)
  (let* ((node (tree-sitter-selected-node))
         (last-child (ts-get-nth-child node (- (ts-count-children node) 1)))
         (text (delete-and-extract-region (ts-node-start-position last-child)
                                          (ts-node-end-position last-child))))
    (save-excursion
      (goto-char (+ (ts-node-end-position node) 1))
      (insert text))))

;; (defun ts-get-top-parent (node)
;;   "Return the top-level parent of the given node"
;;   (let ((parent (ts-get-parent node)))
;;     (if (equal "source_file" (ts-node-type parent))
;;         node
;;       (ts-get-top-parent parent))))

(defun ts-get-ancestor-by (node f)
  "Return the ancestor of a particular type, if any."
  (if (funcall f node)
      node
    (ts-get-ancestor-by (ts-get-parent node) f)))

;; (defun ts-node-evil-range (node &optional type)
;;   (evil-range (ts-node-start-position node)
;;               (ts-node-end-position node)
;;               type
;;               :expanded t))

;; (evil-define-text-object evil-ts-a-def (count &optional beg end type)
;;   "tree-sitter top-level definition object"
;;   (let* ((top (ts-get-top-parent (tree-sitter-selected-node))))
;;     (ts-node-evil-range top 'inclusive)))

;; (evil-define-text-object evil-ts-inner-def (count &optional beg end type)
;;   (let* ((top (ts-get-top-parent (tree-sitter-selected-node)))
;;          (block (ts-get-nth-child top (- (ts-count-children top) 1))))
;;     (evil-range (+ (ts-node-start-position block) 1)
;;                 (- (ts-node-end-position block) 1)
;;                 'inclusive
;;                 :expanded t)))

;; (evil-define-text-object evil-ts-a-function (count &optional beg end type)
;;   (ts-node-evil-range (ts-get-ancestor-by (tree-sitter-selected-node)
;;                                           (lambda (n) (member (ts-node-type n) '("function_item" "closure_expression"))))
;;                       'inclusive))

;; (evil-define-text-object evil-ts-inner-function (count &optional beg end type)
;;   (let* ((fun (ts-get-ancestor-by (tree-sitter-selected-node)
;;                                   (lambda (n) (member (ts-node-type n) '("function_item" "closure_expression")))))
;;          (block (ts-get-nth-child fun (- (ts-count-children fun) 1))))
;;     (evil-range (+ (ts-node-start-position block) 1)
;;                 (- (ts-node-end-position block) 1)
;;                 'inclusive
;;                 :expanded t)))

;; (define-key evil-outer-text-objects-map "d" #'evil-ts-a-def)
;; (define-key evil-inner-text-objects-map "d" #'evil-ts-inner-def)
;; (define-key evil-outer-text-objects-map "f" #'evil-ts-a-function)
;; (define-key evil-inner-text-objects-map "f" #'evil-ts-inner-function)

(use-package string-inflection :defer 1)


(use-package mu4e
  :config
  (setq mu4e-maildir (expand-file-name "~/.mail")))
