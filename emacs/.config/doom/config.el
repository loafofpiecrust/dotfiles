;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; References:
;; https://github.com/rougier/elegant-emacs

(add-load-path! "custom")

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Garbage collect after a few seconds of being idle.
;; This prevents GC hangs while using Emacs.
(run-with-idle-timer 5 t 'garbage-collect)

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
(setq! doom-font (font-spec :family "SF Mono" :size 15)
       doom-variable-pitch-font (font-spec :family "sans" :size 15))

;; Test for unicode icons (should be marked "seen" and "important")
;; neu          11:43:48        Information Technology... Received: INC0628880 – Fwd: Office 365 Transition Ridiculous

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox
      doom-gruvbox-brighter-comments t
      doom-peacock-brighter-comments t)

(setq! shell-file-name "/bin/bash")

(use-package! ewal
  :defer 1
  :init
  ;; Use all 16 colors from our palette, not just the primary 8.
  (setq! ewal-ansi-color-name-symbols '(black red green yellow blue purple cyan white brightblack brightred brightgreen brightyellow brightblue brightpurple brightcyan brightwhite)))
(use-package! ewal-doom-themes :after ewal)

;; Automatically switch between light and dark themes at sunrise/sunset.
;;(use-package! theme-changer
;;:disabled
;;:defer 1
;;:config
;; TODO Set location.
;;(change-theme 'doom-one-light 'doom-peacock))


;; Use text checkboxes instead of widgets.
(setq! widget-image-enable nil)

;; (use-package! compdef)

(use-package! org
  :init
  ;; If you use `org' and don't want your org files in the default location below,
  ;; change `org-directory'. It must be set before org loads!
  (setq org-directory "~/org/")
  :config
  (add-hook! 'org-mode-hook (lambda () (setq display-line-numbers nil)))
  (setq! initial-major-mode 'org-mode
         org-link-descriptive t
         org-latex-compiler "xelatex"
         org-latex-pdf-process (list "tectonic %f")
         org-latex-prefer-user-labels t
         org-log-done t
         org-use-property-inheritance t
         org-list-allow-alphabetical t
         org-catch-invisible-edits 'smart
         org-ellipsis " ▾ "
         org-highlight-latex-and-related '(native script entities)
         org-link-descriptive nil
         org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))))

(after! org-superstar
  ;; Bullet symbols: ‣•◦⦾⦿✷🟆➤⮞⁕⊙ ⁖🜔🜕🜖🜗🝆🝎❯⁕✸✿✤✜◆▶∴
  (setq! org-superstar-headline-bullets-list '("⁖" "✸" "✿" "✤" "❁" "✜" "◆")
         ;;org-superstar-headline-bullets-list '("⠿" "⠽" "⠮" "⠭" "⠕" "⠨" "⠐")
         org-superstar-prettify-item-bullets t
         org-superstar-item-bullet-alist '((?* . ?‣)
                                           (?- . ?•)
                                           (?+ . ?◦))))

(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

(after! calc
  (setq calc-symbolic-mode t))

(setq auth-sources '("~/.authinfo.gpg"))

(setq-default delete-by-moving-to-trash t
              x-stretch-cursor t
              auto-save-default t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; (setq mouse-wheel-progressive-speed t
;;       mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))

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

;; We want the same save binding everywhere!
(map! "C-s" (general-key "C-x C-s")
      :gi "C-v" 'evil-paste-after)

;; Extra bindings for compilation and editing commit messages.
(map! :map (compilation-mode-map with-editor-mode-map)
      ;; Stands for "go run", finishes the current operation.
      :n "gr" (general-simulate-key "C-c C-c")
      ;; Stands for "go quit"
      :nm "gq" (general-simulate-key "C-c C-k"))

(defun evil-normalize-ctrl-i (&optional frame)
  ;;"Untable TAB from C-i, so we can indent."
  (define-key input-decode-map [(control ?i)] [control-i])
  (define-key input-decode-map [(control ?I)] [(shift control-i)])
  (map! :map evil-motion-state-map "C-i" nil)
  (define-key evil-motion-state-map [control-i] 'evil-jump-forward))

;; When Emacs is in server mode, we have to normalize C-i in a graphical window.
(add-hook 'after-make-frame-functions #'evil-normalize-ctrl-i)

(after! (evil evil-collection)
  ;; Prevent accidental commands when exploring little-used modes.
  (map! :m doom-localleader-key nil)
  (evil-normalize-ctrl-i)
  ;; Indent current line after more evil commands.
  (map! :map prog-mode-map
        :n "J" (cmd! (call-interactively #'evil-join)
                     (indent-according-to-mode))))


(use-package! tree-sitter-langs :after tree-sitter)
(use-package! tree-sitter
  :hook ((python-mode rustic-mode) . tree-sitter-mode)
  :config
  ;; Add support for JSX.
  (appendq! tree-sitter-major-mode-language-alist
            '((typescript-tsx-mode . typescript)
              (rjsx-mode . javascript))))

(use-package! doom-modeline
  :config
  (setq! ;;doom-modeline-height 30
   doom-modeline-irc nil
   doom-modeline-gnus nil))

(use-package! prog-mode
  :config
  (set-company-backend! 'prog-mode '(company-capf company-dabbrev-code))
  ;; Consider each segment of a camelCase one word,
  ;; and wrap lines at the window edge.
  (general-add-hook 'prog-mode-hook '(auto-fill-mode subword-mode))
  ;; Automatically wrap comments in code
  (setq-default comment-auto-fill-only-comments t))

(use-package! lsp-mode
  :config
  ;; (set-company-backend! 'lsp-mode 'company-capf)
  (setq! lsp-eldoc-render-all nil
         lsp-signature-render-documentation nil
         lsp-symbol-highlighting-skip-current t
         ;; Don't show flycheck stuff in the sideline.
         lsp-ui-sideline-show-diagnostics nil
         lsp-ui-sideline-update-mode 'line))

(use-package! git-timemachine
  :config
  (map! :map git-timemachine-mode-map
        "[r" 'git-timemachine-show-previous-revision
        "]r" 'git-timemachine-show-next-revision))

;; Disable background color for highlighted parens
(custom-set-faces! '(show-paren-match :background nil))

(use-package! evil
  :config
  (add-hook 'evil-insert-state-exit-hook 'company-abort))

(use-package! rustic
  ;; :hook (rustic-mode . rainbow-delimiters-mode)
  :config
  (setq! rustic-lsp-server 'rust-analyzer))

;; TODO Figure out pipe matching for rust considering single | in match patterns.
;; (after! (smartparens rustic)
;;   (sp-local-pair 'rustic-mode "|" "|"))

;; (use-package! rainbow-mode
;;   :defer 1
;;   :config (rainbow-mode))

;; (use-package! deadgrep
;;   :commands deadgrep)

;; (use-package! org
;;   :company ((company-capf))
;;   :capf pcomplete-completions-at-point)

(use-package! org-ref
  :after org
  :config
  (setq! org-ref-completion-library 'org-ref-ivy-cite)
  (map! :map org-mode-map
        :localleader
        :n "ri" 'org-ref-insert-ref-link))

;; TODO Decide between mode-line up top and sticky header!
(use-package! org-sticky-header
  :disabled
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
;; (use-package! bazel-mode)

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
  :disabled
  :commands deadgrep
  :init (map! :leader "sd" 'deadgrep)
  :config
  (map! :map deadgrep-mode-map
        :n "RET" 'deadgrep-visit-result-other-window
        :n "<S-return>" 'deadgrep-visit-result))

;; Focus project tree with "op" instead of toggling.
(use-package! treemacs
  :config
  (setq! treemacs-is-never-other-window t)
  (defun +treemacs/focus ()
    "Initialize or focus treemacs.

Ensures that only the current project is present and all other projects have
been removed.

Use `treemacs-select-window' command for old functionality."
    (interactive)
    (require 'treemacs)
    (if (doom-project-p)
        (treemacs-add-and-display-current-project)
      (treemacs-select-window)))
  (map! :leader "op" '+treemacs/focus))

;; TODO bind ivy-alt-done to something other than C-o
;; TODO Configure ivy-posframe with high min width and central position.
;; (use-package! ivy-posframe
;;   :config
;;   (setq! ivy-posframe-style 'frame-center
;;          ivy-posframe-min-width 100))
;;

;; Make yasnippet easier to access in insert mode.
;; insert: C-p, normal: SPC i s
;; TODO Get rid of all yasnippet-company business.
(use-package! yasnippet
  :config
  (setq! yas-triggers-in-field t)
  (map! :map yas-minor-mode-map
        :i "C-p" 'yas-insert-snippet))

(use-package! projectile
  :config
  (setq! projectile-sort-order 'recently-active))

(use-package! vdiff
  :commands vdiff-mode
  :config
  (setq! vdiff-magit-stage-is-2way t)
  (map! :map vdiff-mode-map
        :localleader "m" 'vdiff-hydra/body))
(use-package! vdiff-magit
  :after-call magit-status
  :general (magit-mode-map
            "e" 'vdiff-magit-dwim
            "E" 'vdiff-magit))

;; Provide syntax highlighting to magit diffs.
(use-package! magit-delta
  :disabled
  :hook (magit-mode . magit-delta-mode)
  :config
  ;; FIXME Propogate the emacs theme to delta.
  (setq magit-delta-default-dark-theme "ansi-dark"))

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
  :defer-incrementally (polymode-core polymode-classes polymode-methods polymode-base polymode-export polymode-weave)
  ;; :config
  ;; (map! :map polymode-minor-mode-map
  ;;       :m "znc" 'polymode-toggle-chunk-narrowing)
  )
(use-package! poly-markdown
  :mode (("\\.md\\'" . poly-markdown-mode)))
(use-package! poly-org
  :mode (("\\.org\\'" . poly-org-mode)))
;; TODO Limit docs shown for current function to the type signature (one line), only showing the rest upon using K.
;; TODO Rebind C-c C-c in with-editor-mode (magit commit messages) to "gr" or similar

(after! (company company-box)
  (setq! company-auto-complete 'company-explicit-action-p
         ;; company-idle-delay 0.1
         company-box-doc-delay 2)
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
  :config (evil-owl-mode))

(use-package! flycheck-inline
  :hook (flycheck-mode . flycheck-inline-mode))

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

(use-package! cherokee-input)

;; Make headlines big!
(custom-set-faces!
  '(org-document-title :weight extra-bold :height 1.5)
  '(outline-1 :weight extra-bold :height 1.3)
  '(outline-2 :weight bold :height 1.2)
  '(outline-3 :weight bold :height 1.1)
  '(outline-4 :weight semi-bold :height 1.08)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-7 :weight semi-bold)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold)
  ;; Style markdown headers the same way.
  '(markdown-header-face-1 :inherit outline-1)
  '(markdown-header-face-2 :inherit outline-2)
  '(markdown-header-face-3 :inherit outline-3)
  '(markdown-header-face-4 :inherit outline-4)
  '(markdown-header-face-5 :inherit outline-5)
  ;; Not all themes provide this inheritance.
  '(org-level-1 :inherit outline-1)
  '(org-level-2 :inherit outline-2)
  '(org-level-3 :inherit outline-3)
  '(org-level-4 :inherit outline-4)
  '(org-level-5 :inherit outline-5)
  '(org-level-6 :inherit outline-6))

;; Make line numbers more visible on many themes.
(custom-set-faces!
  '(line-number :foreground nil :inherit org-tag))

;; Turn all wavy underlines into straight ones for readability.
(custom-set-faces!
  '(flyspell-duplicate :underline nil :inherit flycheck-warning)
  '(flyspell-incorrect :underline nil :inherit flycheck-error)
  '(flycheck-info :underline (:style line :color "#22ad6a"))
  '(flycheck-warning :underline (:style line :color "#f2b64b"))
  '(flycheck-error :underline (:style line :color "#ab1f38")))

(use-package! unicode-fonts
  :config
  ;; Replace all instances of Symbola with a monospacified Symbola.
  (mapc (lambda (bl)
          (setf (cadr bl)
                (mapcar (lambda (font)
                          (if (string= font "Symbola")
                              "Symbola monospacified for Source Code Pro"
                            font))
                        (cadr bl))))
        unicode-fonts-block-font-mapping)
  (setq my/private-use-fonts '("Font Awesome 5 Free"
                               "github-octicons"
                               "file-icons"
                               "all-the-icons"
                               "Material Design Icons"))
  (push `("Private Use Area" ,my/private-use-fonts)
        unicode-fonts-block-font-mapping)
  (push '("IPA Extensions" ("Source Code Pro"))
        unicode-fonts-block-font-mapping)
  (push '("Modifier Letter Small H" "Modifier Letter Small H"
          ("Source Code Pro"))
        unicode-fonts-overrides-mapping)
  ;; (push `("Simplified Chinese Script" ("Migu 1M")))
  ;;λ
  (set-fontset-font t 'han (font-spec :family "SF Mono Square" :size 18))
  (set-fontset-font t 'greek (font-spec :family "SF Mono" :size 15))
  (set-fontset-font t 'mathematical (font-spec :family "Fira Math" :size 15))
  (setq unicode-fonts-fallback-font-list '("Symbola monospacified for Source Code Pro" "Hasklig"))
  (setq unicode-fonts-restrict-to-fonts (append '("DejaVu Sans Mono"
                                                  "Noto Sans"
                                                  "Noto Sans Symbols"
                                                  "Noto Sans Symbols2"
                                                  "Noto Sans Cherokee"
                                                  "Everson Mono"
                                                  "Source Code Pro"
                                                  "Symbola monospacified for Source Code Pro"
                                                  "Quivira"
                                                  "Noto Sans CJK JP")
                                                my/private-use-fonts)))

;; (use-package! unicode-fonts
;;      :disabled
;;   :init (setq! unicode-fonts-restrict-to-fonts '("DevaVu Sans Mono"
;;                                                  "DejaVu Sans"
;;                                                  "Symbola"
;;                                                  "Noto Sans"
;;                                                  "Noto Sans Symbols"
;;                                                  "Noto Sans Cherokee"
;;                                                  "Material Design Icons")))

;; Use this instead of "SPC w w" to exclude treemacs.
;; (map! :localleader "\\" 'other-window)

;; (map! :map (evil-ex-map)
;;       :n "C-v" 'evil-paste-after)

(use-package! ivy
  :config
  ;; Use a hydra for ivy alternate actions.
  (setq! ivy-read-action-function 'ivy-hydra-read-action)
  (map! :map ivy-minibuffer-map
        "C-RET" 'ivy-immediate-done))

(use-package! relative-buffers
  :disabled
  :defer 1
  :config (global-relative-buffers-mode))

;; Associate TAB with all workspace bindings, instead of brackets + w.
(map! :n "[ TAB" '+workspace/switch-left
      :n "] TAB" '+workspace/switch-right)

;; I never use this and it causes weird issues with Wayland + Slack.
(map! "<Scroll_Lock>" 'ignore)

(after! mu4e
  (setq! +mu4e-backend 'offlineimap
         mu4e-get-mail-command "sync-email.sh"
         ;; mu4e-maildir "~/.mail"
         mu4e-attachment-dir "~/Downloads"
         ;; mu4e-sent-messages-behavior 'sent
         mu4e-headers-leave-behavior 'apply
         ;; FIXME Sadly causes more issues than it fixes.
         ;; mu4e-headers-advance-after-mark nil
         mu4e-view-prefer-html t
         mu4e-update-interval 300
         mu4e-compose-context-policy 'ask
         mu4e-context-policy 'pick-first
         org-mu4e-convert-to-html nil
         ;; Add full citation when replying to emails.
         message-citation-line-function 'message-insert-formatted-citation-line
         ;; These work well if my font has CJK, otherwise Unicode icons may be better.
         mu4e-headers-draft-mark '("D" . "✎")
         mu4e-headers-flagged-mark '("F" . "★")
         mu4e-headers-new-mark '("N" . "!")
         mu4e-headers-seen-mark '("S" . "◎")
         mu4e-headers-unread-mark '("u" . "◉")
         mu4e-headers-replied-mark '("R" . "⤷")
         mu4e-headers-attach-mark '("a" . "🖿")
         ;; mu4e-html2text-command "w3m -dump -T text/html"
         ;; Convert received messages from html to org.
         mu4e-html2text-command "pandoc -f html -t gfm-raw_html-smart-escaped_line_breaks --wrap=preserve --lua-filter ~/Downloads/remove-ids.lua"
         mu4e-view-show-images t)
  ;; I really do want evil bindings for viewing emails.
  (remove-hook 'mu4e-view-mode-hook #'evil-emacs-state)
  ;; Make sure we can view inline images
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  ;; Disable line highlight when viewing emails.
  (add-hook 'mu4e-view-mode-hook (lambda () (hl-line-mode -1)))
  ;; Execute marks without confirmation.
  (map! :map (mu4e-headers-mode-map mu4e-view-mode-map)
        :n "x" (cmd! (mu4e-mark-execute-all t)))
  ;; Allow me to reload search results.
  (map! :map mu4e-headers-mode-map
        :n "gr" #'mu4e-headers-rerun-search)
  ;; Add my email accounts.
  (set-email-account! "neu"
                      '((mu4e-sent-folder . "/neu/Sent")
                        (mu4e-drafts-folder . "/neu/Drafts")
                        (mu4e-trash-folder . "/neu/Trash")
                        (mu4e-refile-folder . "/neu/Archive")
                        (mu4e-spam-folder . "/neu/Junk")
                        (user-mail-address . "snead.t@northeastern.edu")
                        (smtpmail-smtp-user . "snead.t@northeastern.edu")
                        ;; Send through the local Davmail SMTP server.
                        (smtpmail-smtp-service . 1025)
                        (smtpmail-smtp-server . "localhost")
                        (smtpmail-stream-type . plain)
                        (message-citation-line-format . "On %a, %b %d, %Y at %R %f wrote:\n")))
  (set-email-account! "gmail"
                      '((mu4e-sent-folder . "/gmail/[Gmail].Sent Mail")
                        (mu4e-drafts-folder . "/gmail/[Gmail].Drafts")
                        (mu4e-trash-folder . "/gmail/[Gmail].Trash")
                        (mu4e-refile-folder . "/gmail/Graveyard")
                        (mu4e-spam-folder . "/gmail/[Gmail].Spam")
                        (user-mail-address . "taylorsnead@gmail.com")
                        (smtpmail-smtp-user . "taylorsnead@gmail.com")
                        (smtpmail-smtp-server . "smtp.gmail.com")
                        (smtpmail-smtp-service . 587)
                        (smtpmail-stream-type . starttls)
                        (message-citation-line-format . "On %a, %b %d, %Y at %R %f wrote:\n")))

  (defun mu4e-all-contexts-var (sym)
    "A list of all the values of the given symbol in each mu4e context."
    (mapcar (lambda (ctx) (cdr (assoc sym (mu4e-context-vars ctx))))
            mu4e-contexts))

  ;; Build bookmark queries.
  (let* ((all-trash (mu4e-all-contexts-var 'mu4e-trash-folder))
         (all-spam (mu4e-all-contexts-var 'mu4e-spam-folder)))
    (setq my/show-all-trash (mapconcat (lambda (d) (format "maildir:%s" d))
                                       all-trash " or ")
          my/hide-all-trash (mapconcat (lambda (d) (format "not maildir:%s" d))
                                       (append all-trash all-spam) " and ")
          my/show-all-inboxes (mapconcat (lambda (d) (format "maildir:/%s/INBOX" d))
                                         '("gmail" "neu") " or ")))
  (setq! mu4e-bookmarks
         '((:name "Inbox" :query my/show-all-inboxes :key ?i)
           (:name "Unread Messages" :query (format "flag:unread and (%s)" my/hide-all-trash) :key ?u)
           (:name "Today" :query (format "date:today..now and (%s)" my/hide-all-trash) :key ?t)
           (:name "This Week" :query (format "date:7d..now and (%s)" my/hide-all-trash) :hide-unread t :key ?w)
           (:name "Trash" :query my/show-all-trash :key ?T)))

  (defun mu4e-compose-from-mailto (mailto-string)
    (require 'mu4e)
    (unless mu4e~server-props (mu4e t) (sleep-for 0.1))
    (let* ((mailto (rfc2368-parse-mailto-url mailto-string))
           (to (cdr (assoc "To" mailto)))
           (subject (or (cdr (assoc "Subject" mailto)) ""))
           (body (cdr (assoc "Body" mailto)))
           (org-msg-greeting-fmt (if (assoc "Body" mailto)
                                     (replace-regexp-in-string "%" "%%"
                                                               (cdr (assoc "Body" mailto)))
                                   org-msg-greeting-fmt))
           (headers (-filter (lambda (spec) (not (-contains-p '("To" "Subject" "Body") (car spec)))) mailto)))
      (mu4e~compose-mail to subject headers)))
  )

;; Write emails in org-mode, sent as legit HTML!
(use-package! org-msg
  :disabled
  :after mu4e
  :config
  (setq! org-msg-startup "noindent inlineimages"
         org-msg-options "html-postamble:nil toc:nil author:nil email:nil num:nil \\n:t"
         org-msg-text-plain-alternative t
         org-msg-enforce-css '())
  (org-msg-mode))

(use-package md-msg
  ;; :disabled
  :after mu4e
  :config
  (md-msg-mode))

;; Notify me when I receive emails.
(use-package! mu4e-alert
  :hook (after-init . mu4e-alert-enable-notifications)
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (setq mu4e-alert-email-notification-types '(count)
        mu4e-alert-interesting-mail-query (format "flag:unread and (%s)" my/show-all-inboxes)))

(use-package! mu4e-send-delay
  :disabled
  :after mu4e
  :config
  (add-hook 'mu4e-main-mode-hook #'mu4e-send-delay-initialize-send-queue-timer)
  (mu4e-send-delay-setup))

;; (after! mu4e
;;   ;; Prompt for the email to send from when composing.
;;   (defun my-mu4e-set-account ()
;;     "Set the account for composing a message."
;;     (unless (and mu4e-compose-parent-message
;;                  (let ((to (cdr (car (mu4e-message-field mu4e-compose-parent-message :to))))
;;                        (from (cdr (car (mu4e-message-field mu4e-compose-parent-message :from)))))
;;                  (if (member to (plist-get mu4e~server-props :personal-addresses))
;;                      (setq user-mail-address to)
;;                    (if (member from (plist-get mu4e~server-props :personal-addresses))
;;                        (setq user-mail-address from)
;;                        nil))))
;;       (ivy-read "Account: " (plist-get mu4e~server-props :personal-addresses) :action (lambda (candidate) (setq user-mail-address candidate)))))

;;   (add-hook 'mu4e-compose-pre-hook 'my/mu4e-set-account))

;; FIXME This doesn't work with mu > 1.2
;; (use-package! mu4e-conversation
;;   :after mu4e
;;   :config
;;   (global-mu4e-conversation-mode))
;;
;;


(map! "M-[" #'+workspace/switch-left
      "M-]" #'+workspace/switch-right)

(after! web-mode
  (add-to-list 'web-mode-engines-alist '("django" . "\\.tera\\.(xml|html)\\'")))

(use-package! olivetti
  :commands olivetti-mode
  :init (map! :leader "to" #'olivetti-mode)
  :config
  (setq! olivetti-body-width 80))

(setq! +pretty-code-symbols
       '(:name "»"
         :src_block "»"
         :src_block_end "«"
         :quote "“"
         :quote_end "”"
         :lambda "λ"
         :def "ƒ"
         :composition "∘"
         :map "↦"
         :null "∅"
         :not "¬"
         :in "∈"
         :not-in "∉"
         :and "∧"
         :or "∨"
         ;; :for "∀"
         :some "∃"
         :return "⤷"
         :yield "∑"
         :union "⋃"
         :intersect "∩"
         :diff "∖"
         :tuple "⨂"
         ;; :pipe ""
         :dot "•"
         ;; Org-specific symbols
         :title "𝙏"
         :subtitle "𝙩"
         :begin_quote   "❮"
         :end_quote     "❯"
         :begin_export  "⯮"
         :end_export    "⯬"
         ;; :properties    "⛭"
         :end           "∎"))

(set-pretty-symbols! 'org-mode
  :merge t
  :title "#+TITLE:"
  :begin_quote "#+BEGIN_QUOTE"
  :end_quote "#+END_QUOTE"
  :begin_export "#+BEGIN_EXPORT"
  :end_export "#+END_EXPORT"
  ;; :properties ":PROPERTIES:"
  :end ":END:")

(set-pretty-symbols! 'markdown-mode
  :src_block "```")

(map! :leader "tp" #'prettify-symbols-mode)

(after! (mu4e org-msg)
  (defvar org-msg-mu4e-view-mode-map (copy-keymap mu4e-view-mode-map))
  (define-derived-mode org-msg-mu4e-view-mode
    org-msg-edit-mode
    "mu4e:view-org"
    "View mu4e messages with org mode magic."
    (setq-local display-line-numbers nil)))

(map! :map compilation-mode-map
      :n "gr" #'recompile)
