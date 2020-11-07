;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; References:
;; https://github.com/rougier/elegant-emacs

;; Keep emacs from being sluggish while typing.
(setq-default gc-cons-percentage 0.3)

;; Let me load my custom packages.
(add-load-path! "custom")

;; remove all scrollbars!
(horizontal-scroll-bar-mode -1)
(column-number-mode)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

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
(setq doom-font (font-spec :family "monospace" :size 15 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "sans" :size 17)
      ;; These fonts were fucking up display of math symbols! Remove them!
      doom-unicode-extra-fonts nil)

(setq-default line-spacing 1)

;; Emacs 28 adds this new face with a different font for comments.
;; I want to retain the same font as normal code for now.
(custom-set-faces! '(fixed-pitch-serif :family nil))

;; Test for unicode icons (should be marked "seen" and "important")
;; neu          11:43:48        Information Technology... Received: INC0628880 – Fwd: Office 365 Transition Ridiculous

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-acario-light
      doom-gruvbox-brighter-comments t
      doom-peacock-brighter-comments t
      doom-monokai-classic-brighter-comments t
      doom-acario-light-brighter-comments t
      doom-one-light-brighter-comments t)

;; Pull exwm config from separate file.
(use-package! my-exwm-config
  :if (getenv "EMACS_EXWM"))

(after! unicode-fonts
  ;; Replace all instances of Symbola with a monospacified Symbola.
  (mapc (lambda (bl)
          (setf (cadr bl)
                (mapcar (lambda (font)
                          (if (string= font "Symbola")
                              "Symbola monospacified for Source Code Pro"
                            font))
                        (cadr bl))))
        unicode-fonts-block-font-mapping)
  ;; Designate private use to font awesome, mostly.
  (setq my/private-use-fonts '("Font Awesome 5 Free"
                               "github-octicons"
                               "file-icons"
                               "all-the-icons"
                               "Material Design Icons"))
  (push `("Private Use Area" ,my/private-use-fonts)
        unicode-fonts-block-font-mapping)
  ;; Source Code Pro shares metrics with SF Mono and has full IPA.
  (push '("IPA Extensions" ("Source Code Pro"))
        unicode-fonts-block-font-mapping)
  (push '("Modifier Letter Small H" "Modifier Letter Small H"
          ("DejaVu Sans Mono"))
        unicode-fonts-overrides-mapping)
  (setq unicode-fonts-fallback-font-list '("Symbola monospacified for Source Code Pro" "Source Code Pro"))
  (setq unicode-fonts-restrict-to-fonts (append '("DejaVu Sans Mono"
                                                  "Noto Sans"
                                                  "Noto Sans Symbols"
                                                  "Noto Sans Symbols2"
                                                  "Noto Sans Cherokee"
                                                  "Source Code Pro"
                                                  "Symbola monospacified for Source Code Pro"
                                                  "Noto Sans CJK JP"
                                                  "Noto Sans CJK SC"
                                                  "Noto Sans CJK TC")
                                                my/private-use-fonts)))

;;;; Themes and color management
(use-package! ewal
  :after doom-themes
  :config (ewal-load-colors)
  :init
  ;; Use all 16 colors from our palette, not just the primary 8.
  (setq ewal-ansi-color-name-symbols '(black red green yellow blue magenta cyan white brightblack brightred brightgreen brightyellow brightblue brightmagenta brightcyan brightwhite)))
(use-package! ewal-doom-themes
  :after ewal
  :config
  (setq ewal-doom-vibrant-brighter-comments t))
(use-package! theme-changer
  :after doom-themes
  :defer 0.5
  :init
  (setq calendar-location-name "Boston, MA"
        calendar-latitude 42.360
        calendar-longitude -71.059)
  :config
  (change-theme doom-theme 'ewal-doom-dark))

;;;; org-mode adjustments
(use-package! org
  :no-require t
  ;; If you use `org' and don't want your org files in the default location below,
  ;; change `org-directory'. It must be set before org loads!
  :init (setq org-directory "~/org/")
  :config
  ;; Change some org display properties.
  (setq-default org-link-descriptive t
                org-indent-indentation-per-level 2
                org-use-property-inheritance t
                org-list-allow-alphabetical t
                org-catch-invisible-edits 'smart
                org-ellipsis " ▾ "
                org-link-descriptive nil
                org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
  ;; Adjust LaTeX display and export with tectonic.
  (setq-default org-latex-compiler "xelatex"
                org-latex-pdf-process '("tectonic %f")
                org-latex-prefer-user-labels t
                org-log-done t
                org-highlight-latex-and-related '(native script entities)))

(after! org-superstar
  ;; Bullet symbols: ‣•◦⦾⦿✷🟆➤⮞⁕⊙ ⁖🜔🜕🜖🜗🝆🝎❯⁕✸✿✤✜◆∴∷
  (setq org-superstar-headline-bullets-list '("∷" "✽" "✿" "✤" "✸" "❁" "✜")
        org-superstar-prettify-item-bullets t
        org-superstar-item-bullet-alist '((?* . ?◆)
                                          (?- . ?●)
                                          (?+ . ?⭘))))

(after! calc
  (setq calc-symbolic-mode t))

(setq auth-sources '("~/.authinfo.gpg"))

(setq delete-by-moving-to-trash t
      x-stretch-cursor t
      ;; auto-save-default t
      )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; (defun disable-line-numbers ()
;;   (interactive)
;;   (defvar doom--line-number-style display-line-numbers-type)
;;   (setq display-line-numbers-type nil
;;         doom--line-number-style nil))
;;(add-hook! '(org-mode-hook markdown-mode-hook) 'doom-disable-line-numbers-h)

;; (global-visual-line-mode)

;; Use alt + {j,k} for dragging stuff, not just arrow keys.
(after! drag-stuff
  (map! "M-j" #'drag-stuff-down
        "M-k" #'drag-stuff-up))

(after! (undo-tree evil evil-collection)
  (map! :map undo-tree-map
        (:leader "ou" 'undo-tree-visualize)
        ;; :n "U" 'undo-tree-redo
        ;; :i "C-z" 'undo-tree-undo
        ;; :i "C-S-Z" 'undo-tree-redo
        ))

(after! (evil evil-collection)
  (map! :n "U" 'evil-redo))

(after! (counsel evil evil-collection)
  (map! :leader
        "tm" 'counsel-major
        "sg" 'counsel-git-grep))

(after! (evil evil-collection)
  ;; When Emacs is in server mode, we have to normalize C-i in a graphical window.
  (defun evil-normalize-ctrl-i (&optional frame)
    ;;"Untable TAB from C-i, so we can indent."
    (define-key input-decode-map [(control ?i)] [control-i])
    (define-key input-decode-map [(control ?I)] [(shift control-i)])
    (map! :map evil-motion-state-map "C-i" nil)
    (define-key evil-motion-state-map [control-i] 'evil-jump-forward))

  (add-hook 'doom-first-buffer-hook #'evil-normalize-ctrl-i)
  ;; Prevent accidental commands when exploring little-used modes.
  (map! :m doom-localleader-key nil)
  (evil-normalize-ctrl-i)
  ;; Indent current line after more evil commands.
  ;; (advice-add 'evil-join :after #'indent-according-to-mode)
  (map! "C-j" 'newline-and-indent)

  ;; Extra bindings for compilation and editing commit messages.
  (map! :map (compilation-mode-map with-editor-mode-map message-mode-map)
        ;; Stands for "go run", finishes the current operation.
        :nv "gr" (general-simulate-key "C-c C-c")
        ;; Stands for "go quit"
        :nm "gq" (general-simulate-key "C-c C-k"))

  ;; We want the same save binding everywhere!
  (map! "C-s" (general-key "C-x C-s")
        :gi "C-v" 'evil-paste-after))

(after! (org evil evil-collection)
  (map! :map (org-mode-map)
        :nv "gr" (general-simulate-key "C-c C-c")))

(use-package! tree-sitter
  :hook (doom-first-buffer . global-tree-sitter-mode)
  :config
  (require 'tree-sitter-langs)
  ;; TODO Fix JSX support.
  (appendq! tree-sitter-major-mode-language-alist
            '((typescript-tsx-mode . typescript)))
  (add-hook! 'tree-sitter-mode-hook #'tree-sitter-hl-mode))

(after! (spell-fu tree-sitter)
  ;; Make spell-fu compatible with tree-sitter.
  (setq-default spell-fu-faces-include
                '(tree-sitter-hl-face:comment
                  tree-sitter-hl-face:doc
                  tree-sitter-hl-face:string
                  font-lock-comment-face
                  font-lock-doc-face
                  font-lock-string-face)))

(after! prog-mode
  ;; Consider each segment of a camelCase one word,
  (add-hook! 'prog-mode-hook '(auto-fill-mode subword-mode))
  ;; Automatically wrap comments in code
  (setq-default comment-auto-fill-only-comments t))

(setq-hook! '(text-mode-hook prog-mode-hook vterm-mode-hook eshell-mode-hook)
  truncate-lines nil)

(after! lsp-mode
  (setq lsp-eldoc-render-all nil
        lsp-signature-render-documentation nil
        lsp-symbol-highlighting-skip-current t
        ;; Don't show flycheck stuff in the sideline.
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-update-mode 'line))

(after! (git-timemachine evil-collection)
  (map! :map git-timemachine-mode-map
        "[r" 'git-timemachine-show-previous-revision
        "]r" 'git-timemachine-show-next-revision))

;; Disable background color for highlighted parens
(custom-set-faces! '(show-paren-match :background nil))

(after! (evil evil-collection)
  (add-hook 'evil-insert-state-exit-hook 'company-abort)
  ;; Associate TAB with all workspace bindings, instead of brackets + w.
  (map! :n "[ TAB" '+workspace/switch-left
        :n "] TAB" '+workspace/switch-right)

  ;; I never use this and it causes weird issues with Wayland + Slack.
  (map! "<Scroll_Lock>" 'ignore)

  (map! "M-[" #'+workspace/switch-left
        "M-]" #'+workspace/switch-right)
  (map! :leader "tp" #'prettify-symbols-mode)

  (map! :map compilation-mode-map
        :n "gr" #'recompile)
  (map! :leader "os" 'eshell)
  (map! :nv "zw" 'count-words
        :n "zG" '+spell/remove-word)
  (map! :leader "oc" 'calc))

;; TODO Figure out pipe matching for rust considering single | in match patterns.
;; (after! (smartparens rustic)
;;   (sp-local-pair 'rustic-mode "|" "|"))

(use-package! org-ref
  :after org
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (map! :map org-mode-map
        :localleader
        :n "ri" 'org-ref-insert-ref-link))

(use-package! literate-calc-mode
  :commands (literate-calc-mode literate-calc-minor-mode)
  :mode (("\\.calc\\'" . literate-calc-mode))
  :init (map! :leader "tc" 'literate-calc-minor-mode))

(use-package! graphql-mode
  :mode (("\\.gql\\'" . graphql-mode)
         ("\\.graphql\\'" . graphql-mode)))

;; TODO Learn multi-cursor bindings

;; Focus project tree with "op" instead of toggling.
(after! treemacs
  (setq treemacs-is-never-other-window t)
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

;; Make yasnippet easier to access in insert mode.
;; insert: C-p, normal: SPC i s
;; TODO Get rid of all yasnippet-company business.
;; (after! yasnippet
;;   (setq yas-triggers-in-field t)
;;   (map! :map yas-minor-mode-map
;;         :i "C-p" 'yas-insert-snippet))

(after! projectile
  (setq projectile-sort-order 'recently-active))

;; Provide syntax highlighting to magit diffs.
;; (use-package! magit-delta
;;   :hook (magit-mode . magit-delta-mode)
;;   :config
;;   ;; FIXME Propagate the emacs theme to delta.
;;   (setq magit-delta-default-dark-theme "ansi-dark"))

;; Spell check options
;; (after! ispell
;;   (setq ispell-dictionary "en_US"
;;         ;; Add camelCase spellcheck
;;         ispell-extra-args '("--camel-case" "--sug-mode=ultra" "--run-together")))

(use-package! polymode
  :defer-incrementally (polymode-core polymode-classes polymode-methods polymode-base polymode-export polymode-weave))
(use-package! poly-markdown
  :mode (("\\.md\\'" . poly-markdown-mode)))
(use-package! poly-org
  :mode (("\\.org\\'" . poly-org-mode)))
;; TODO Limit docs shown for current function to the type signature (one line), only showing the rest upon using K.
;; TODO Rebind C-c C-c in with-editor-mode (magit commit messages) to "gr" or similar

(after! (company company-box)
  (setq company-auto-commit 'company-explicit-action-p
        ;; company-idle-delay 0.35
        company-box-doc-delay 2)
  ;; TODO Fix this so we can indent instead of completing all the time!
  (map! :map company-active-map
        "<tab>" 'company-complete-selection
        "TAB" 'company-complete-selection
        "RET" nil
        [return] nil))

(use-package! evil-owl
  :after evil
  :config
  (setq! evil-owl-display-method 'posframe
         evil-owl-idle-delay 0.5)
  (evil-owl-mode))

(use-package! flycheck-inline
  :after flycheck
  :hook (flycheck-mode . flycheck-inline-mode))

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
  '((outline-7 outline-8 outline-9) :weight semi-bold)
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
  '(org-level-6 :inherit outline-6)
  ;; Make line numbers more visible on many themes.
  '(line-number :foreground nil :inherit org-tag))

;; (custom-set-faces!
;;   `(vertical-border :foreground ,(ewal-get-color 'green)))

(custom-set-faces!
  '(minibuffer-prompt :family nil)
  '(pyim-page :height 1.1))

;; Turn all wavy underlines into straight ones for readability.
(custom-set-faces!
  '(spell-fu-incorrect-face :underline (:style line :color "red")))

(after! ivy
  ;; Use a hydra for ivy alternate actions.
  (setq ivy-read-action-function 'ivy-read-action-by-key
        ivy-truncate-lines nil)
  (map! :map ivy-minibuffer-map
        "C-RET" 'ivy-immediate-done))

;; Open urls with xdg-open so that app links open directly.
;; This let's me open zoommtg:// urls right into zoom.
(setq browse-url-generic-program "xdg-open"
      browse-url-browser-function #'browse-url-generic)

(after! message
  (setq message-cite-style message-cite-style-thunderbird
        message-cite-function 'message-cite-original))

;; Gmail Compatibility, modified from core DOOM emacs.
(after! mu4e
  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  (defvar +mu4e-context-gmail nil
    "Whether the current mu4e context is associated with a GMail-like server.")

  ;; In my workflow, emails won't be moved at all. Only their flags/labels are
  ;; changed. Se we redefine the trash and refile marks not to do any moving.
  ;; However, the real magic happens in `+mu4e|gmail-fix-flags'.
  ;;
  ;; Gmail will handle the rest.
  (defun +mu4e--mark-seen (docid _msg target)
    (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-u-N"))

  ;; (delq! 'delete mu4e-marks #'assq)
  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (_target msg) (mu4e-get-trash-folder msg))
              :action
              (lambda (docid msg target)
                (with~mu4e-context-vars (mu4e-context-determine msg nil)
                    (if +mu4e-context-gmail
                        (+mu4e--mark-seen docid msg target)
                      (mu4e~proc-move docid (mu4e~mark-check-target target) "-N-u")))))

        ;; Refile will be my "archive" function.
        (alist-get 'refile mu4e-marks)
        (list :char '("r" . "▼")
              :prompt "rrefile"
              :dyn-target (lambda (_target msg) (mu4e-get-refile-folder msg))
              :action
              (lambda (docid msg target)
                (with~mu4e-context-vars (mu4e-context-determine msg nil)
                    (if +mu4e-context-gmail
                        (+mu4e--mark-seen docid msg target)
                      (mu4e~proc-move docid (mu4e~mark-check-target target) "-N-u"))))))

  (defun +mu4e-gmail-fix-flags-h (mark msg)
    "This hook correctly modifies gmail flags on emails when they are marked.
Without it, refiling (archiving), trashing, and flagging (starring) email
won't properly result in the corresponding gmail action, since the marks
are ineffectual otherwise."
    (with~mu4e-context-vars (mu4e-context-determine msg nil)
        (when +mu4e-context-gmail
          (pcase mark
            (`trash  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
            (`refile (mu4e-action-retag-message msg "-\\Inbox"))
            (`flag   (mu4e-action-retag-message msg "+\\Starred"))
            (`unflag (mu4e-action-retag-message msg "-\\Starred"))))))

  (add-hook! 'mu4e-mark-execute-pre-hook #'+mu4e-gmail-fix-flags-h))

(setq mu4e-update-interval 300)

(after! mu4e
  ;; Gmail handles labels/folders differently than others do?!
  (map! :map (mu4e-headers-mode-map mu4e-view-mode-map)
        :ng "C--" nil)
  (setq +mu4e-backend 'mbsync
        +mu4e-workspace-name "*email*"
        ;; mu4e-split-view 'horizontal
        mu4e-compose-cite-function 'message-cite-original
        ;; mu4e-headers-visible-columns 100
        mu4e-attachment-dir "~/Downloads"
        mu4e-headers-include-related nil
        mu4e-headers-skip-duplicates nil
        mu4e-headers-leave-behavior 'apply
        mu4e-view-prefer-html t
        mu4e-update-interval 300
        mu4e-compose-context-policy 'ask
        mu4e-context-policy 'pick-first
        ;; I don't use mu4e built-in conversion to html.
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
        ;; Convert received messages from html to org.
        mu4e-html2text-command "pandoc -f html -t markdown-raw_html-smart-link_attributes+emoji-header_attributes-blank_before_blockquote-simple_tables-multiline_tables-inline_code_attributes-escaped_line_breaks+hard_line_breaks --atx-headers --wrap=none --columns=80 --lua-filter ~/Downloads/remove-ids.lua"
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
                      `((mu4e-sent-folder . "/neu/Sent")
                        (mu4e-drafts-folder . "/neu/Drafts")
                        (mu4e-trash-folder . "/neu/Trash")
                        (mu4e-refile-folder . "/neu/Archive")
                        (mu4e-spam-folder . "/neu/Junk")
                        ;; Outlook expects me to move items normally.
                        (user-mail-address . "snead.t@northeastern.edu")
                        (smtpmail-smtp-user . "snead.t@northeastern.edu")
                        ;; Send through the local Davmail SMTP server.
                        (smtpmail-smtp-service . 1025)
                        (smtpmail-smtp-server . "localhost")
                        (smtpmail-stream-type . plain)
                        (+mu4e-context-gmail . ,nil)
                        ;; Mimic outlook's citation style.
                        (message-yank-prefix . "")
                        (message-yank-cited-prefix . "")
                        (message-yank-empty-prefix . "")
                        (message-citation-line-format . "\n\n-----------------------\nOn %a, %b %d %Y, %N wrote:\n")))

  (set-email-account! "gmail"
                      `((mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail")
                        (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
                        (mu4e-trash-folder . "/gmail/[Gmail]/Trash")
                        (mu4e-refile-folder . "/gmail/Graveyard")
                        (mu4e-spam-folder . "/gmail/[Gmail]/Spam")
                        ;; Gmail expects me to change labels rather than move stuff?
                        (user-mail-address . "taylorsnead@gmail.com")
                        (+mu4e-context-gmail . ,t)
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
         (all-spam (mu4e-all-contexts-var 'mu4e-spam-folder))
         (all-archive (mu4e-all-contexts-var 'mu4e-refile-folder)))
    (setq my/show-all-trash (mapconcat (lambda (d) (format "maildir:%s" d))
                                       all-trash " or ")
          my/hide-all-trash (concat (mapconcat (lambda (d) (format "not maildir:%s" d))
                                               (append all-trash all-spam) " and ")
                                    " and not flag:trashed")
          my/show-all-inboxes (concat (mapconcat (lambda (d) (format "maildir:/%s/INBOX" d))
                                                 '("gmail" "neu") " or ")
                                      " and not flag:trashed")
          my/show-all-archive (concat (mapconcat (lambda (d) (format "maildir:%s" d))
                                                 all-archive " or ")
                                      " and not flag:trashed")))
  ;; Add bookmarks for all important mail categories.
  (setq mu4e-bookmarks
        '((:name "Inbox" :query my/show-all-inboxes :key ?i)
          (:name "Unread Messages" :query (format "flag:unread and (%s)" my/hide-all-trash) :key ?u)
          (:name "Today" :query (format "date:today..now and (%s)" my/hide-all-trash) :key ?t)
          (:name "This Week" :query (format "date:7d..now and (%s)" my/hide-all-trash) :hide-unread t :key ?w)
          (:name "Archive" :query my/show-all-archive :key ?a)
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

;; (after! (mu4e persp-mode)
;;   (persp-def-auto-persp "*email*"
;;                         :buffer-name "^\\*mu4e"
;;                         :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
;;                                    (persp-add-buffer-on-find-file nil)
;;                                    persp-add-buffer-on-after-change-major-mode)
;;                         :hooks '(after-switch-to-buffer-functions)
;;                         :switch 'frame)
;;   (persp-def-auto-persp "browse"
;;                         :buffer-name "^\\[Firefox\\]"
;;                         :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
;;                                    (persp-add-buffer-on-find-file nil)
;;                                    persp-add-buffer-on-after-change-major-mode)
;;                         :hooks '(after-switch-to-buffer-functions)
;;                         :switch 'frame))

;; Write emails in markdown, sent as legit HTML!
(use-package! md-msg
  ;; Enable markdown for all mu4e reading and writing purposes.
  :hook (mu4e-headers-mode . md-msg-mode)
  :config
  (setq mml-content-disposition-alist '((text (rtf . "attachment")
                                              (t . nil))
                                        (t . "attachment")))
  ;; TODO Move this binding to md-msg itself.
  (map! :map md-msg-view-mode-map
        :n "q" 'md-msg-view-quit-buffer)
  (map! :map md-msg-edit-mode-map
        :n "gr" 'message-send-and-exit)
  ;; Make email nicer to read and write.
  (add-hook! '(md-msg-view-mode-hook md-msg-edit-mode-hook) #'olivetti-mode))

;; Notify me when I receive emails.
(use-package! mu4e-alert
  :defer 3
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (setq mu4e-alert-email-notification-types '(count)
        mu4e-alert-interesting-mail-query (format "flag:unread and (%s) and not flag:trashed" my/show-all-inboxes))
  (mu4e-alert-enable-notifications)
  ;; FIXME Start mu4e in the background to retrieve new mail at boot.
  (mu4e t))

(use-package! mu4e-send-delay
  :disabled
  :after mu4e
  :config
  (add-hook 'mu4e-main-mode-hook #'mu4e-send-delay-initialize-send-queue-timer)
  (mu4e-send-delay-setup))

(after! web-mode
  (add-to-list 'web-mode-engines-alist '("django" . "\\.tera\\.(xml|html)\\'")))

(defun disable-line-numbers ()
  (display-line-numbers-mode -1))

(use-package! olivetti
  :commands olivetti-mode
  :init
  (map! :leader "to" #'olivetti-mode)
  (after! org
    (add-hook 'org-mode-hook #'olivetti-mode))
  (after! markdown-mode
    (add-hook 'markdown-mode-hook #'olivetti-mode))
  (after! magit
    (add-hook 'magit-status-mode-hook #'olivetti-mode))

  :config
  (add-hook 'olivetti-mode-hook #'disable-line-numbers)
  (setq-default olivetti-body-width 80))


(setq! +ligatures-extra-symbols
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
         ;; :not "¬"
         ;; :in "∈"
         ;; :not-in "∉"
         ;; :and "∧"
         ;; :or "∨"
         ;; :for "∀"
         ;; :some "∃"
         :return "↑"
         :yield "∃"
         :union "⋃"
         :intersect "∩"
         ;; :diff "∖"
         ;; :tuple "⨂"
         ;; :pipe ""
         :dot "•"
         ;; Org-specific symbols
         :title "∷"
         :subtitle "𝙩"
         :begin_quote   "❮"
         :end_quote     "❯"
         :begin_export  "⯮"
         :end_export    "⯬"
         :section    "§"
         :end           "∎"
         :exclamation "!"
         :dash "-"
         :endash "--"
         :asterisk "*"
         :lt "<"
         :nothing ""
         :at_symbol "@"
         :pound "#"
         :pipe "|"
         :turnstile "|—"
         :arrow "->"))

(after! org
  (set-ligatures! 'org-mode
    :title "#+TITLE:"
    :begin_quote "#+BEGIN_QUOTE"
    :end_quote "#+END_QUOTE"
    :begin_export "#+BEGIN_EXPORT"
    :end_export "#+END_EXPORT"
    :section ":PROPERTIES:"
    :end ":END:"))

(after! markdown-mode
  (set-ligatures! 'markdown-mode
    :src_block "```"))

(after! web-mode
  (setq! web-mode-prettify-symbols-alist nil))


;; Prettify escaped symbols in viewed emails as much as possible.
;; This doesn't affect when writing/responding to emails.
;; TODO maybe there's a better machanism for replacing these that works more consistently?
(after! md-msg
  (set-ligatures! 'md-msg-view-mode
    :exclamation "\\!"
    :dash "\\-"
    :endash "\\--"
    :asterisk "\\*"
    :lt "\\<"
    ;; :nothing "\n\\\n"
    ;; :nothing "\n\n\n"
    ;; :nothing "\\"
    :at_symbol "\\@"
    :pound "\\#"
    :arrow "-\\>"
    :pipe "\\|"
    :turnstile "\\|-"))



(after! mixed-pitch
  (appendq! mixed-pitch-fixed-pitch-faces '(outline-1 outline-2 outline-3 outline-4 outline-5
                                                      outline-6 outline-7 outline-8 outline-9)))

;;;; Periodically clean buffers
(use-package midnight
  :defer 5
  :config
  (setq clean-buffer-list-kill-regexps '("\\`\\*Man "
                                         "\\`\\*helpful "
                                         "\\`\\*Calc"
                                         "\\`\\*xref"
                                         "\\`\\*lsp"
                                         "\\`\\*company"
                                         "\\`\\*straight-process\\*"
                                         "\\`\\*Flycheck"
                                         "\\`\\*forge"
                                         "\\`*ivy-occur"
                                         "magit"
                                         "\\`vterm"
                                         "\\`\\*eshell"
                                         "Aweshell:")
        clean-buffer-list-delay-general 1
        clean-buffer-list-delay-special (* 60 60 2)
        ;; Clean out potentially old buffers every hour
        midnight-period (* 60 60))
  (midnight-mode))

(setq window-divider-default-right-width 6
      window-divider-default-bottom-width 6)

(after! ivy-posframe
  (setcdr (assoc t ivy-posframe-display-functions-alist)
          'ivy-posframe-display-at-frame-top-center)

  (setq ivy-posframe-width 130
        ivy-posframe-height 20))

;; Using C-/ for comments aligns with other editors.
(after! evil
  (map! :nv "C-/" 'comment-dwim))

(use-package! ox-moderncv
  :after org
  :config
  (defun org-cv-export-to-pdf ()
    (interactive)
    (let* ((org-latex-default-packages-alist nil)
           (org-latex-packages-alist nil)
           (outfile (org-export-output-file-name ".tex" nil)))
      (org-export-to-file 'moderncv outfile
        nil nil nil nil nil
        (lambda (f) (org-latex-compile f))))))

;; TODO Disable lsp-ui to fix loss of window config in exwm!

;; The default popup is SLOW, use posframe or minibuffer.
;; TODO We need chinese font with same height as my font.
(after! pyim
  (setq! pyim-page-tooltip 'posframe))

(use-package! bitwarden
  :defer 1
  :config
  (defun counsel-bitwarden-getpass (&optional arg)
    "Pick an account and copy the password for it to the kill-ring."
    (interactive "P")
    (ivy-read "Copy password for account: "
              (mapcar (lambda (item) (gethash "name" item))
                      (bitwarden-search))
              :action (lambda (acc)
                        (kill-new (bitwarden-getpass acc))
                        (message "Copied password for %s" acc))
              :caller 'counsel-bitwarden-getpass)))

(use-package! ivy-avy
  :after ivy)

(after! hl-todo
  (add-hook! 'org-mode-hook #'hl-todo-mode))

(use-package! string-inflection
  :defer 1)

(use-package! zoom
  ;; :hook (doom-first-input . zoom-mode)
  :config
  (setq! zoom-size '(0.65 . 0.65)
         zoom-ignored-major-modes '(ranger-mode helpful-mode)
         zoom-ignored-buffer-name-regexps '("^*mu4e" "^*Org" "^*helpful")))

;; Shows habits on a consistency graph.
(use-package! org-habit :after org)

;; Notify me when a deadline is fast approaching.
(use-package! org-notify
  :defer 5
  :config
  (org-notify-add 'default
                  ;; If we're more than an hour past the deadline, don't notify at all.
                  '(:time "-1h"
                    :actions ())
                  ;; A couple hours before a deadline, start sending system notifications every 20 minutes.
                  '(:time "2h"
                    :period "20m"
                    :duration 10
                    :actions -notify))
  (org-notify-start))

;; FIXME evil bindings don't work and workspaces mess this up.
(use-package! pdf-continuous-scroll-mode
  ;; :hook (pdf-view-mode . pdf-continuous-scroll-mode)
  :disabled
  :config
  (map! :map 'pdf-continuous-scroll-mode-map
        :n "j" #'pdf-continuous-scroll-forward
        :n "k" #'pdf-continuous-scroll-backward
        :n "C-j" #'pdf-continuous-next-page
        :n "C-k" #'pdf-continuous-previous-page
        :n "G" #'pdf-cscroll-last-page
        :n "g g" #'pdf-cscroll-first-page
        :n "<mouse-4>" #'pdf-continuous-scroll-forward
        :n "<mouse-5>" #'pdf-continuous-scroll-backward))

(use-package! dired-show-readme
  :disabled
  :hook (dired-mode . dired-show-readme-mode))

(after! evil
  (defun playerctl-play-pause ()
    (interactive)
    (exec "playerctl play-pause"))
  (defun playerctl-next ()
    (interactive)
    (exec "playerctl next"))
  (defun playerctl-previous ()
    (interactive)
    (exec "playerctl previous"))
  (defun open-browser ()
    (interactive)
    (exec "firefox"))
  (map! :leader
        "j" #'ace-window
        "o o" #'counsel-linux-app
        "o b" #'open-browser
        "w U" #'winner-redo
        "w D" #'delete-other-windows
        "<f19>" #'+ivy/projectile-find-file
        "TAB" #'+workspace/switch-to-other
        "\\" #'set-input-method
        ";" #'toggle-input-method
        "w s" (cmd! (evil-window-vsplit) (other-window 1))
        "w v" (cmd! (evil-window-split) (other-window 1))
        "m c" #'playerctl-play-pause
        "m n" #'playerctl-next
        "m p" #'playerctl-previous
        "m s" #'+wm/screenshot
        "m S" #'desktop-environment-screenshot-part
        "DEL" #'+workspace/delete))

;; Show window hints big and above X windows.
(after! ace-window
  (setq! aw-display-style 'posframe
         aw-posframe-parameters '((parent-frame . nil))))

;; Allow easy NPM commands in most programming buffers.
(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) #'npm-mode)

(use-package! ivy-fuz
  :disabled
  :after ivy
  :defer-incrementally fuz
  :custom
  (ivy-sort-matches-functions-alist '((t . ivy-fuz-sort-fn)))
  (ivy-re-builders-alist '((t . ivy-fuz-regex-fuzzy)))
  :config
  (add-to-list 'ivy-highlight-functions-alist '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn)))

(use-package! spotify
  :commands (spotify-remote-mode global-spotify-remote-mode)
  :config
  (setq spotify-oauth2-client-id "f3e530a58362402fab4ca04976916f80"
        spotify-oauth2-client-secret "71a43675cd8247edafa85351bede7cf6")
  (map! :map spotify-mode-map
        doom-localleader-key 'spotify-command-map))


;; LSP formatting is messed up for Javascript, so disable it.
(setq! +format-with-lsp nil)

;; (after! js2-mode
;;   (setq-hook! js2-mode-hook +format-with-lsp nil))

;; (after! web-mode
;;   (setq-hook! typescript-tsx-mode-hook +format-with-lsp nil))
;; (setq-hook! typescript-mode-hook +format-with-lsp nil)

;; (setq! fancy-splash-image "~/.config/wpg/.current"
;;        +doom-dashboard-banner-padding '(0 . 0)
;;        +doom-dashboard--width 0.9)
