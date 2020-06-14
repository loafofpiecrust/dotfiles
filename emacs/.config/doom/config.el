;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Garbage collect after a few seconds of being idle.
;; This prevents GC hangs while using Emacs.
(setq gc-cons-percentage 0.3)
(run-with-idle-timer 2 t (lambda () (garbage-collect)))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Taylor Snead"
      user-mail-address "taylorsnead@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq! doom-font (font-spec :family "Ubuntu Mono" :size 16 :weight 'medium)
       doom-variable-pitch-font (font-spec :family "sans" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-peacock
      doom-peacock-brighter-comments t)

(use-package! ewal)
(use-package! ewal-doom-themes :after ewal)

;; Automatically switch between light and dark themes at sunrise/sunset.
(use-package! theme-changer
  :defer 1
  :config
  ;; TODO Set location.
  (change-theme 'doom-one-light 'doom-peacock))


;; Use text checkboxes instead of widgets.
(setq! widget-image-enable nil)

(use-package! compdef)

(use-package! org
  :init
  ;; If you use `org' and don't want your org files in the default location below,
  ;; change `org-directory'. It must be set before org loads!
  (setq org-directory "~/org/")
  :config
  (setq! initial-major-mode 'org-mode
         org-latex-compiler "xelatex"
         org-latex-pdf-process (list "tectonic %f")
         org-latex-prefer-user-labels t
         org-log-done t
         org-highlight-latex-and-related '(native script entities)
         org-link-descriptive nil
         org-list-demote-modify-bullet '(("+" . "-") ("-" . "+"))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq shell-file-name "/bin/sh"
      mouse-wheel-progressive-speed t
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))

;; remove all scrollbars!
(horizontal-scroll-bar-mode -1)

(global-visual-line-mode)

;; Use alt + {j,k} for dragging stuff, not just arrow keys.
(after! drag-stuff
  (map! "M-j" #'drag-stuff-down
        "M-k" #'drag-stuff-up))

(after! undo-tree
  (map! :map undo-tree-map
        (:leader "ou" 'undo-tree-visualize)
        :n "U" 'undo-tree-redo
        :i "C-z" 'undo-tree-undo
        :i "C-S-Z" 'undo-tree-redo))

(after! counsel
  (map! :leader
        "tm" 'counsel-major
        "sg" 'counsel-git-grep))

(after! magit
  ;; Allow "gr" to finish a commit message.
  (map! :map with-editor-mode-map
        :n "gr" 'with-editor-finish))

;; We want the same save binding everywhere!
(map! "C-s" (general-key "C-x C-s")
      :gi "C-v" 'evil-paste-after)

(after! evil
  ;; Prevent accidental commands when exploring little-used modes.
  (map! :m doom-localleader-key nil)
  ;; Untangle TAB from C-i, so we can indent!
  (define-key input-decode-map [(control ?i)] [control-i])
  (define-key input-decode-map [(control ?I)] [(shift control-i)])
  (map! :map evil-motion-state-map "C-i" nil)
  (define-key evil-motion-state-map [control-i] 'evil-jump-forward)
  ;; Indent current line after more evil commands.
  (map! :n "J" (cmd! (call-interactively #'evil-join)
                     (indent-according-to-mode))))


(use-package! tree-sitter-langs)
(use-package! tree-sitter
  :hook ((python-mode) . tree-sitter-mode))

(use-package! doom-modeline
  :config
  (setq! doom-modeline-height 32
         doom-modeline-irc nil
         doom-modeline-gnus nil))

(use-package! prog-mode
  :company ((company-capf company-dabbrev-code :with company-yasnippet))
  :config
  ;; Automatically wrap comments in code
  (setq-default comment-auto-fill-only-comments t
                auto-fill-function 'do-auto-fill)
  (add-hook 'prog-mode-hook 'auto-fill-mode))

(use-package! lsp-mode
  :company ((company-capf :with company-yasnippet))
  :config
  (setq! lsp-eldoc-render-all nil
         lsp-signature-render-documentation nil
         lsp-symbol-highlighting-skip-current t
         message-truncate-lines 5))

;; Disable background color for highlighted parens
(custom-set-faces! '(show-paren-match :background nil))

(use-package! evil
  :config
  (add-hook 'evil-insert-state-exit-hook 'company-abort))

(use-package! rustic
  :hook (rustic-mode . rainbow-delimiters-mode)
  :config
  (setq! rustic-lsp-server 'rust-analyzer))

;; TODO Figure out pipe matching for rust considering single | in match patterns.
;; (after! (smartparens rustic)
;;   (sp-local-pair 'rustic-mode "|" "|"))

(use-package! rainbow-mode
  :defer 1
  :config (rainbow-mode))

(use-package! deadgrep
  :commands deadgrep)

(use-package! org
  :company ((company-capf :with company-yasnippet))
  :capf pcomplete-completions-at-point)

(use-package! org-ref
  :after org
  :config
  (setq! org-ref-completion-library 'org-ref-ivy-cite)
  (map! :map org-mode-map
        :localleader
        :n "ri" 'org-ref-insert-ref-link))

;; TODO Decide between mode-line up top and sticky header!
(use-package! org-sticky-header
  :hook (org-mode . org-sticky-header-mode))

(map! :mv "zw" 'count-words)
(map! :leader "oc" 'calc)

(use-package! literate-calc-mode
  :commands (literate-calc-mode literate-calc-minor-mode)
  :mode (("\\.calc\\'" . literate-calc-mode))
  :init (map! :leader "tc" 'literate-calc-minor-mode))

(use-package! graphql-mode
  :mode (("\\.gql\\'" . graphql-mode)
         ("\\.graphql\\'" . graphql-mode)))
(use-package! bazel-mode)

;; TODO Midnight mode?
;; TODO Learn multi-cursor bindings
;;;; Icons & Emojis
(use-package! emojify
  :defer 1
  :config
  (setq! emojify-emoji-styles '(unicode github))
  (global-emojify-mode))

;; TODO Use origami instead of vimish-fold, maybe.
;; TODO bind smartparens stuff?
(use-package! deadgrep
  :commands deadgrep
  :init (map! :leader "sd" 'deadgrep)
  :config
  (map! :map deadgrep-mode-map
        :n "RET" 'deadgrep-visit-result-other-window
        :n "<S-return>" 'deadgrep-visit-result))
;; TODO Bind treemacs current project exclusive
;; TODO bind ivy-alt-done to something other than C-o
;; TODO Configure ivy-posframe with high min width and central position.
;; (use-package! ivy-posframe
;;   :config
;;   (setq! ivy-posframe-style 'frame-center
;;          ivy-posframe-min-width 100))
;; TODO compdef to get yasnippet+company
(use-package! vdiff
  :commands vdiff-mode
  :config
  (setq! vdiff-magit-stage-is-2way t)
  (map! :map vdiff-mode-map
        :localleader "m" 'vdiff-hydra/body))
(use-package! vdiff-magit
  :after (magit vdiff)
  :general (magit-mode-map
            "e" 'vdiff-magit-dwim
            "E" 'vdiff-magit))

;; TODO Setup keys for navigating merge conflicts.
;; (use-package smerge-mode
;;   :config
;;   (general-def 'motion smerge-mode-map
;;     "[c" 'smerge-prev
;;     "]c" 'smerge-next))

;; Spell check options
(use-package! ispell
  :config
  (setq! ispell-dictionary "en_US"
         ;; Add camelCase spellcheck
         ispell-extra-args '("--camel-case" "--sug-mode=ultra" "--run-together" "--dont-tex-check-comments")))
;; TODO bind expand-region in visual mode
;; (use-package! expand-region
;;   :general
;;   (define-key! 'visual
;;     "=" 'er/expand-region
;;     "-" 'er/contract-region))
;; TODO Test out and configure lsp-ui
;; TODO setup polymode
(use-package! polymode
  ;; :config
  ;; (map! :map polymode-minor-mode-map
  ;;       :m "znc" 'polymode-toggle-chunk-narrowing)
  )
(use-package! poly-markdown
  :commands (gfm-mode markdown-mode))
(use-package! poly-org
  :commands (org-mode))
;; TODO Limit docs shown for current function to the type signature (one line), only showing the rest upon using K.
;; TODO Rebind C-c C-c in with-editor-mode (magit commit messages) to "gr" or similar

(after! company
  (setq! company-auto-complete 'company-explicit-action-p
         company-idle-delay 0.1)
  ;; TODO Fix this so we can indent instead of completing all the time!
  (map! :map company-active-map
        "<tab>" 'company-complete-selection
        "TAB" 'company-complete-selection
        "RET" nil
        [return] nil))

;; Align table cells even with variable pitch!
(use-package! valign
  :hook (org-mode . valign-mode))

(use-package! explain-pause-mode
  :disabled
  :config (explain-pause-mode t))

(use-package! evil-owl
  :after evil
  :config (evil-owl-mode))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

(custom-set-faces!
  '(line-number :foreground nil :inherit org-tag))
