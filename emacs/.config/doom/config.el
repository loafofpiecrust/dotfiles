;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; References:
;; https://github.com/rougier/elegant-emacs

;; Let me load my custom packages.
(add-load-path! "custom")
(add-load-path! "/run/current-system/sw/share/emacs/site-lisp/mu4e")

;; Load custom themes from the "themes" folder here.
(setq custom-theme-directory (expand-file-name "~/.config/doom/themes"))

;; Make shell commands run faster using bash...
(setq shell-file-name "/run/current-system/sw/bin/bash")
;; ...But let me use fish for interactive sessions.
(after! vterm
  (setq vterm-shell "/run/current-system/sw/bin/fish"))

(use-package! memoize)

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
;; Symbol test: _ -> => , . `' "" O0l1*#
(setq doom-font (font-spec :family "SF Mono" :size 15 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 18 :weight 'semi-bold)
      ;; doom-unicode-font (font-spec :family "Symbola monospacified for Source Code Pro" :size 15)
      ;; These fonts were fucking up display of math symbols! Remove them!
      ;; doom-unicode-extra-fonts nil
      )

(setq doom-scratch-initial-major-mode 'org-mode)

;; Give each line some room to breathe.
(setq-default line-spacing 2)

(defun +snead/increase-mem ()
  "Allow Emacs to use up to 1 GB of memory.
It seems excessive, but apparently necessary for fluid LSP usage!"
  (setq +lsp--default-gcmh-high-cons-threshold 1073741824
        gcmh-high-cons-threshold 1073741824))

;; Increase garbage collection threshold while active.
;; This keeps emacs from being sluggish while typing.
(after! lsp-mode (+snead/increase-mem))
(after! gcmh
  (setq-default gcmh-idle-delay 5)
  (+snead/increase-mem))

(defvar +snead/frame-border-width 3)
(defvar +snead/frame-fringe 8)

(after! hide-mode-line
  (setq-default hide-mode-line-format nil)
  ;; (setq-hook! 'hide-mode-line-mode-hook header-line-format nil)
  )

;; Remove the fringe from all windows, unless you're visiting a file.
;; In that case, the fringe is very useful for git status and error information.
(defun +snead/add-fringe (&optional no-fringe)
  (setq-local left-fringe-width (if no-fringe 0 4)
              right-fringe-width (if no-fringe 0 4)))

(defun +snead/remove-fringe ()
  (interactive)
  (set-fringe-mode 0))

(defvar +snead/fringe-deny-modes '(pdf-view-mode))
(defun +snead/set-fringe ()
  "Add a fringe to windows carrying file-visiting buffers."
  (when (and buffer-file-name
             (not (memq major-mode +snead/fringe-deny-modes)))
    (+snead/add-fringe)))

(after! fringe
  (set-fringe-mode 0)
  (add-hook! '(exwm-mode-hook pdf-view-mode-hook) #'+snead/remove-fringe)
  ;; (add-hook 'after-change-major-mode-hook #'+snead/set-fringe)
  (add-hook! '(vterm-mode-hook) #'+snead/add-fringe))

;; Disable line highlighting by default, relying on mode-specific faces and
;; highlighting the current line number.
(setq global-hl-line-modes '())

(setq tab-always-indent t)

(use-package! emacs
  :config
  (setq-default gc-cons-percentage 0.5)

  (setq user-full-name "Taylor Snead"
        user-mail-address "taylor@snead.xyz")

  (setq confirm-kill-processes nil)

  (setq-default truncate-lines nil)

  (setq-default scroll-margin 2)

  ;; Inhibit auto-save messages because they're mostly distracting.
  (setq-default auto-save-no-message t)

  (setq delete-by-moving-to-trash t
        x-stretch-cursor t)

  (setq-hook! '(vterm-mode-hook eshell-mode-hook)
    truncate-lines nil)

  (appendq! initial-frame-alist '((left-fringe . 0)
                                  (right-fringe . 0)))

  (setq custom-safe-themes t))

(use-package! solar
  :config
  (setq calendar-location-name "Boston, MA"
        calendar-latitude 42.360
        calendar-longitude -71.059))

(after! scroll-bar
  ;; remove all scrollbars!
  (horizontal-scroll-bar-mode -1))

;; Add dividers between each window.
(after! frame
  (setq window-divider-default-right-width 6
        window-divider-default-bottom-width 6))

;; Store various logins and things in a gpg file when necessary.
(after! auth-source
  (setq auth-sources '("~/.authinfo.gpg")))

;; Always show line numbers.
(after! display-line-numbers
  (setq display-line-numbers-type t
        display-line-numbers-grow-only t))

(after! prog-mode
  ;; Consider each segment of a camelCase one word,
  (add-hook! 'prog-mode-hook '(auto-fill-mode subword-mode))
  ;; Automatically wrap comments in code
  (setq-default comment-auto-fill-only-comments t))

;; Make calculator easy to access.
(map! :leader "oc" #'calc)
(after! calc
  (setq calc-symbolic-mode t))

(after! browse-url
  ;; Open urls with xdg-open so that app links open directly.
  ;; This let's me open zoommtg:// urls right into zoom.
  (setq browse-url-generic-program "xdg-open"
        browse-url-browser-function #'browse-url-generic))


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
  `(line-number :foreground ,nil :inherit org-tag)
  ;; Emacs 28 adds this new face with a different font for comments.
  ;; I want to retain the same font as normal code for now.
  `(fixed-pitch-serif :family ,nil)
  ;; Disable background color for highlighted parens
  ;; '(show-paren-match :background nil)
  `(minibuffer-prompt :family ,nil)
  ;; '(pyim-page :height 1.1)
  )

;; Test for unicode icons (should be marked "seen" and "important")
;; neu          11:43:48        Information Technology... Received: INC0628880 – Fwd: Office 365 Transition Ridiculous

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-operandi
      doom-gruvbox-brighter-comments t
      doom-peacock-brighter-comments t
      doom-monokai-classic-brighter-comments t
      doom-acario-light-brighter-comments t
      doom-one-light-brighter-comments t)

;; Pull exwm config from separate file.
(use-package! my-exwm-config
  :if (equal "t" (getenv "EMACS_EXWM")))

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
  (setq ewal-ansi-color-name-symbols '(black red green yellow blue magenta cyan white
                                             brightblack brightred brightgreen brightyellow
                                             brightblue brightmagenta brightcyan brightwhite)))

(use-package! ewal-doom-themes
  :after ewal
  :config
  (setq ewal-doom-vibrant-brighter-comments t))

(use-package! theme-changer
  :disabled
  :after doom-themes
  :config
  (change-theme doom-theme 'ewal-doom-dark))

;;;; org-mode adjustments
(setq org-directory "~/org/")
(after! org
  ;; If you use `org' and don't want your org files in the default location below,
  ;; change `org-directory'. It must be set before org loads!
  ;; Agenda settings
  (setq-default org-deadline-warning-days 10)
  ;; Change some org display properties.
  (setq-default org-link-descriptive t
                org-indent-indentation-per-level 1
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
                                          (?+ . ?○))))

;;;; Password Management!
(use-package! bitwarden
  :commands (bitwarden-getpass
             bitwarden-edit
             bitwarden-generate-password
             bitwarden-getpass-for-user)
  :init
  ;; Bind my most used workflows under <leader> a (authentication)
  (map! :leader
        :prefix ("a" . "auth")
        "p" #'bitwarden-getpass
        "e" #'bitwarden-edit-item
        "g" #'bitwarden-generate-password
        "n" #'bitwarden-create-item)
  :config
  ;; I use my main email address for bitwarden, so don't prompt me for it.
  (setq bitwarden-user user-mail-address
        ;; TODO Use auth-source to save my bitwarden password?? Seems bad to
        ;; save my master password literally anywhere. Plus, I would just need
        ;; my store password to unlock the BW pass.
        bitwarden-automatic-unlock (lambda () (read-passwd "[Bitwarden] Master password: ")))

  ;; (memoize #'bitwarden-search "1 hour")

  (defun bitwarden-getpass-for-user (domain username)
    "Return the password for an account with the given USERNAME under the given DOMAIN.
If the vault is locked, prompt the user for their master email and password."
    (interactive)
    ;; Ensure the vault is unlocked, prompting for login if not.
    (unless (bitwarden-unlocked-p) (bitwarden-unlock))
    ;; Look for the matching account under the given domain.
    (let ((acc (cl-find-if (lambda (acc)
                             (string= username (gethash "username" (gethash "login" acc))))
                           (bitwarden-search domain))))
      (and acc (gethash "password" (gethash "login" acc)))))

  (defun bitwarden--read (prompt &optional search-str)
    (let* ((items (mapcar (lambda (item) (cons (format "%s (%s)"
                                                       (gethash "name" item)
                                                       (gethash "username" (gethash "login" item)))
                                               item))
                          (bitwarden-search search-str)))
           (choice (completing-read (concat "[Bitwarden] " prompt)
                                    items)))
      (cdr (assoc choice items))))

  ;; TODO Fix login, then unlock after sync.
  (defun bitwarden-getpass (&optional account print-message)
    "Pick an account and copy the password for it to the kill-ring."
    (interactive)
    ;; Ensure the vault is unlocked, prompting for login if not.
    (unless (bitwarden-unlocked-p) (bitwarden-unlock))
    (if (interactive-p)
        ;; New interactive prompt for account or username.
        (let ((acc (bitwarden--read "Copy password: ")))
          (kill-new (gethash "password" (gethash "login" acc)))
          (message "Copied %s password for %s"
                   (gethash "name" acc)
                   (gethash "username" (gethash "login" acc))))
      ;; Copied over from original bitwarden-getpass
      (bitwarden--handle-message
       (bitwarden--auto-cmd (list "get" "password" account))
       print-message)))

  (defun bitwarden-search (&optional search-str search-type)
    "Search for vault for items containing SEARCH-STR.

Returns a vector of hashtables of the results."
    (let* ((args (and search-str (list (format "--%s" (or search-type "search"))
                                       search-str)))
           (ret (bitwarden--auto-cmd (append (list "list" "items") args)))
           (result (bitwarden--handle-message ret)))
      (when result
        (let* ((json-object-type 'hash-table)
               (json-key-type 'string)
               (json (json-parse-string result)))
          json))))

  (defun bitwarden--encode (obj)
    (let ((json-object-type 'hash-table)
          (json-key-type 'string))
      (shell-command-to-string (format "echo '%s' | bw encode" (json-serialize obj)))))

  (defun bitwarden-edit-item (&optional existing-account)
    (interactive)
    (unless (bitwarden-unlocked-p) (bitwarden-unlock))
    (let* ((acc (or nil (bitwarden--read "Edit account: ")))
           (login (gethash "login" acc))
           (username (read-string "Username: " (gethash "username" login)))
           (password (read-string "Password: " (gethash "password" login)))
           (acc-id (gethash "id" acc)))
      ;; Replace the existing username and password.
      (puthash "username" username login)
      (puthash "password" password login)
      (puthash "login" login acc)
      ;; Push the updated entry to the vault.
      (call-process bitwarden-bw-executable nil 0 nil
                    "edit" "item"
                    acc-id
                    (bitwarden--encode acc))
      (message "Updated %s account for %s" (gethash "name" acc) username)))

  (defun bitwarden-create-item ()
    (interactive)
    (unless (bitwarden-unlocked-p) (bitwarden-unlock))
    (let* ((domain (read-string "Domain name: "))
           (name (read-string "Entry name: " domain))
           (username (read-string "Username: "))
           (password (read-string "Password: "))
           (acc (make-hash-table :test #'equal))
           (login (make-hash-table :test #'equal))
           (uri (make-hash-table :test #'equal)))
      ;; Replace the existing username and password.
      (puthash "username" username login)
      (puthash "password" password login)
      (puthash "match" :null uri)
      (puthash "uri" domain uri)
      (puthash "uris" [uri] login)
      (puthash "login" login acc)
      (puthash "name" name acc)
      ;; Push the updated entry to the vault.
      (call-process bitwarden-bw-executable nil 0 nil
                    "create" "item"
                    (bitwarden--encode acc))
      (message "Created %s account for %s" domain username)))

  (defun bitwarden-generate-password ()
    (interactive)
    (let ((len (read-number "[Bitwarden] Password length: " 24)))
      (kill-new (shell-command-to-string
                 (format "bw generate -ulns --length %d" len)))
      (message "New password copied to the clipboard")))

  (defun bitwarden-logged-in-p ()
    "Check if `bitwarden-user' is logged in.
Returns nil if not logged in."
    (not (string-blank-p (shell-command-to-string (format "cat '%s' | jq .__PROTECTED__key --raw-output"
                                                          bitwarden-data-file)))))



  ;; TODO Function to generate a password, then push it into the kill-ring so I
  ;; can paste it into a web prompt, then when editing the account.

  (require 'auth-source-bitwarden)
  (auth-source-bitwarden-enable))

;; Use alt + {j,k} for dragging stuff, not just arrow keys.
(after! drag-stuff
  (map! "M-j" #'drag-stuff-down
        "M-k" #'drag-stuff-up))

(after! undo-tree
  (map! :map undo-tree-map
        (:leader "ou" 'undo-tree-visualize)))

;; I like using <u> for undo and <U> for redo. It's symmetrical.
(map! :n "U" 'evil-redo)

(after! (evil evil-collection)
  ;; When Emacs is in server mode, we have to normalize C-i in a graphical window.
  (defun evil-normalize-ctrl-i (&optional frame)
    ;;"Untable TAB from C-i, so we can indent."
    (define-key input-decode-map [(control ?i)] [control-i])
    (define-key input-decode-map [(control ?I)] [(shift control-i)])
    (map! :map evil-motion-state-map "C-i" nil)
    (define-key evil-motion-state-map [control-i] 'evil-jump-forward))

  ;; Disable echo area messages when changing evil states.
  (setq evil-insert-state-message nil
        evil-replace-state-message nil
        evil-emacs-state-message nil
        evil-visual-line-message nil
        evil-visual-char-message nil
        evil-visual-block-message nil)

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

  (defun +evil-paste-at-point ()
    (interactive)
    (evil-paste-from-register ?0))

  ;; We want the same save binding everywhere!
  (map! :gi "C-s" (general-key "C-x C-s")
        :gi "C-v" '+evil-paste-at-point))

(after! org
  (map! :map org-mode-map
        :nv "gr" (general-simulate-key "C-c C-c")))

(use-package! tree-sitter
  :hook ((rustic-mode python-mode json-mode js-mode js2-mode typescript-mode go-mode sh-mode tuareg-mode) . tree-sitter-mode)
  :config
  (require 'tree-sitter-langs)
  ;; TODO Fix JSX support.
  (push '(typescript-tsx-mode . typescript) tree-sitter-major-mode-language-alist)
  (add-hook! 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Make spell-fu compatible with tree-sitter.
(after! (spell-fu tree-sitter)
  (setq-default spell-fu-faces-include
                '(tree-sitter-hl-face:comment
                  tree-sitter-hl-face:doc
                  tree-sitter-hl-face:string
                  font-lock-comment-face
                  font-lock-doc-face
                  font-lock-string-face)))

;; Only load the lsp servers that I might actually use.
;; Loading these packages is about 40% of the delay when first starting a
;; language server.
;; The rest is actually loading the server and can't be reduced much.
(setq lsp-client-packages '(ccls lsp-bash lsp-clangd lsp-cmake lsp-css lsp-dart lsp-dockerfile
                                 lsp-eslint lsp-go lsp-haskell lsp-java lsp-javascript lsp-json lsp-kotlin
                                 lsp-lua lsp-nix lsp-ocaml lsp-pyls lsp-python-ms lsp-rust lsp-tex lsp-terraform lsp-xml
                                 lsp-yaml lsp-r))
(after! lsp-mode
  (setq lsp-eldoc-render-all nil
        lsp-signature-render-documentation nil
        lsp-symbol-highlighting-skip-current t
        ;; Always prompt for actions so I know what I'm doing.
        lsp-auto-execute-action nil
        ;; Don't show flycheck stuff in the sideline.
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-update-mode 'line))

(map! :after git-timemachine
      :map git-timemachine-mode-map
      "[r" 'git-timemachine-show-previous-revision
      "]r" 'git-timemachine-show-next-revision)

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
        :n "zG" '+spell/remove-word))

(use-package org-ref
  ;; :after-call org-mode
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
(after! ispell
  (setq ispell-dictionary "en"
        ispell-personal-dictionary "~/.aspell.en.pws"
        ;; ispell-extra-args '("--camel-case" "--sug-mode=ultra" "--run-together")
        ))

;; (use-package! polymode
;;   :disabled
;;   :defer t
;;   :defer-incrementally (polymode-core polymode-classes polymode-methods polymode-base polymode-export polymode-weave))
;; (use-package! poly-markdown
;;   :disabled
;;   :mode (("\\.md\\'" . poly-markdown-mode)))
;; (use-package! poly-org
;;   :disabled
;;   :mode (("\\.org\\'" . poly-org-mode)))
;; TODO Limit docs shown for current function to the type signature (one line), only showing the rest upon using K.
;; TODO Rebind C-c C-c in with-editor-mode (magit commit messages) to "gr" or similar

(after! (company company-box)
  (setq ;; company-auto-commit 'company-explicit-action-p
   ;; Icons make completion quite sluggish!
   company-box-enable-icon nil
   company-box-doc-frame-parameters `((internal-border-width . ,+snead/frame-border-width)
                                      (left-fringe . ,+snead/frame-fringe)
                                      (right-fringe . ,+snead/frame-fringe))
   company-idle-delay 0.25
   ;;company-box-doc-delay 2)
   )
  ;; (when (featurep 'exwm)
  ;;   (appendq! company-box-doc-frame-parameters '((parent-frame . nil))))
  ;; TODO Fix this so we can indent instead of completing all the time!
  (map! :map company-active-map
        "<tab>" 'company-complete-selection
        "TAB" 'company-complete-selection
        "RET" nil
        [return] nil))

(use-package! evil-owl
  :hook (doom-first-file . evil-owl-mode)
  :config
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args `(:internal-border-width ,+snead/frame-border-width
                                       :left-fringe ,+snead/frame-fringe
                                       :right-fringe ,+snead/frame-fringe)
        evil-owl-idle-delay 0.5))

(use-package! flycheck-inline
  :hook (flycheck-mode . flycheck-inline-mode))

(use-package! cherokee-input)

;; Make headlines big!

;; (custom-set-faces!
;;   `(vertical-border :foreground ,(ewal-get-color 'green)))

(after! ivy
  ;; Use a hydra for ivy alternate actions.
  (setq ;;ivy-read-action-function 'ivy-read-action-ivy
   ivy-truncate-lines nil)
  (map! :map ivy-minibuffer-map
        "C-RET" 'ivy-immediate-done))

(after! all-the-icons-ivy
  (setq all-the-icons-ivy-icon-args (list :height 1 :v-adjust -0.1)))

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

;; (use-package! org-mu4e
;;   :after mu4e)

(setq mu4e-update-interval 300)
(after! mu4e
  (map! :map (mu4e-headers-mode-map mu4e-view-mode-map)
        :ng "C--" nil)
  (setq +mu4e-backend 'mbsync
        +mu4e-workspace-name "*email*"
        mu4e-completing-read-function 'completing-read
        mu4e-split-view 'vertical
        mu4e-headers-visible-columns 100
        mu4e-compose-cite-function 'message-cite-original
        ;; mu4e-headers-visible-columns 100
        mu4e-attachment-dir "~/Downloads"
        ;; I can choose to view the whole thread if I want to see related messages.
        mu4e-headers-include-related nil
        ;; Sometimes I have issues with duplicates, so I need to see them.
        mu4e-headers-skip-duplicates nil
        mu4e-headers-leave-behavior 'apply
        mu4e-view-prefer-html t
        ;; mu4e-compose-format-flowed nil
        mu4e-update-interval 300
        mu4e-compose-context-policy 'ask
        mu4e-context-policy 'pick-first
        mu4e-index-update-error-warning nil
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
        mu4e-headers-time-format "%R"
        ;; Convert received messages from html to org.
        mu4e-html2text-command "pandoc -f html -t markdown-raw_html-smart-link_attributes+emoji-header_attributes-blank_before_blockquote-simple_tables-inline_code_attributes-escaped_line_breaks+hard_line_breaks --atx-headers --wrap=auto --columns=80 --lua-filter ~/.config/doom/remove-ids.lua"
        mu4e-view-show-images t)
  ;; I really do want evil bindings for viewing emails.
  (remove-hook 'mu4e-view-mode-hook #'evil-emacs-state)
  ;; Disable line highlight when viewing emails.
  (add-hook 'mu4e-view-mode-hook #'doom-disable-hl-line-h)
  ;; Execute marks without confirmation.
  (map! :map (mu4e-headers-mode-map mu4e-view-mode-map)
        :n "x" (cmd! (mu4e-mark-execute-all t)))
  ;; Allow me to reload search results.
  (map! :map mu4e-headers-mode-map
        :n "gr" #'mu4e-headers-rerun-search)

  (setq mu4e-view-actions '(("capture message" . mu4e-action-capture-message)
                            ("browser view" . mu4e-action-view-in-browser)
                            ("pdf view" . mu4e-action-view-as-pdf)
                            ("thread view" . mu4e-action-show-thread)))

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
                        (mu4e-sent-messages-behavior . delete)
                        ;; Mimic outlook's citation style.
                        (message-yank-prefix . "")
                        (message-yank-cited-prefix . "")
                        (message-yank-empty-prefix . "")
                        (message-citation-line-format . "-----------------------\nOn %a, %b %d %Y, %N wrote:\n")))

  (set-email-account! "personal"
                      `((mu4e-sent-folder . "/personal/Sent")
                        (mu4e-drafts-folder . "/personal/Drafts")
                        (mu4e-trash-folder . "/personal/Trash")
                        (mu4e-refile-folder . "/personal/Archive")
                        (mu4e-spam-folder . "/personal/Junk")
                        (user-mail-address . "taylor@snead.xyz")
                        (smtpmail-smtp-user . "taylor@snead.xyz")
                        (smtpmail-smtp-server . "smtp.mailbox.org")
                        (smtpmail-smtp-service . 587)
                        (smtpmail-stream-type . starttls)
                        (+mu4e-context-gmail . ,nil)
                        (message-yank-prefix . "> ")
                        (message-yank-cited-prefix . "> ")
                        (message-yank-empty-prefix . "> ")
                        (mu4e-sent-messages-behavior . sent)
                        (message-citation-line-format . "On %a, %b %d, %Y at %R %f wrote:\n")))

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
                        (mu4e-sent-messages-behavior . delete)
                        (message-yank-prefix . "> ")
                        (message-yank-cited-prefix . "> ")
                        (message-yank-empty-prefix . "> ")
                        (message-citation-line-format . "On %a, %b %d, %Y at %R %f wrote:\n")))

  (defun mu4e-all-contexts-var (sym)
    "A list of all the values of the given symbol in each mu4e context."
    (mapcar (lambda (ctx) (cdr (assoc sym (mu4e-context-vars ctx))))
            mu4e-contexts))

  ;; Build bookmark queries.
  (let* ((all-trash (mu4e-all-contexts-var 'mu4e-trash-folder))
         (all-spam (mu4e-all-contexts-var 'mu4e-spam-folder))
         (all-archive (mu4e-all-contexts-var 'mu4e-refile-folder))
         (all-sent (mu4e-all-contexts-var 'mu4e-sent-folder)))
    (setq my/show-all-trash (mapconcat (lambda (d) (format "maildir:%s" d))
                                       all-trash " or ")
          my/hide-all-trash (concat (mapconcat (lambda (d) (format "not maildir:%s" d))
                                               (append all-trash all-spam) " and ")
                                    " and not flag:trashed")
          my/show-all-inboxes (format "(%s) and not flag:trashed" (mapconcat (lambda (d) (format "maildir:/%s/INBOX" d))
                                                                             '("gmail" "neu" "personal") " or "))
          my/show-all-archive (concat (mapconcat (lambda (d) (format "maildir:%s" d))
                                                 all-archive " or ")
                                      " and not flag:trashed")
          my/show-all-sent (mapconcat (lambda (d) (format "maildir:%s" d))
                                      all-sent " or ")))
  ;; Add bookmarks for all important mail categories.
  (setq mu4e-bookmarks
        '((:name "Inbox" :query my/show-all-inboxes :key ?i)
          (:name "Unread Messages" :query (format "flag:unread and (%s)" my/hide-all-trash) :key ?u)
          (:name "Today" :query (format "date:today..now and (%s)" my/hide-all-trash) :key ?t)
          (:name "This Week" :query (format "date:7d..now and (%s)" my/hide-all-trash) :hide-unread t :key ?w)
          (:name "Archive" :query my/show-all-archive :key ?a)
          (:name "Sent" :query my/show-all-sent :key ?s)
          (:name "Trash" :query my/show-all-trash :key ?T)))

  ;; Make opening the inbox even faster with one key press.
  (defun +mu4e-open-inbox ()
    (interactive)
    (mu4e-headers-search my/show-all-inboxes))

  (map! :map (mu4e-main-mode-map mu4e-headers-mode-map)
        :n "i" #'+mu4e-open-inbox)

  (map! :map mu4e-main-mode-map
        :n "gr" #'mu4e-update-mail-and-index)

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
  (add-hook! '(md-msg-view-mode-hook md-msg-edit-mode-hook mu4e-view-mode-hook) #'olivetti-mode))

(after! alert
  (setq alert-default-style 'libnotify))

;; Notify me when compilations finish!
(defun +alert/compilation (buffer status)
  (alert (s-capitalize status)
         :title (buffer-name buffer)))
(add-hook 'compilation-finish-functions #'+alert/compilation)

;; Notify me when I receive emails.
(use-package! mu4e-alert
  :if (equal "t" (getenv "EMACS_EXWM"))
  :defer 5
  :config
  ;; (mu4e-alert-set-default-style 'libnotify)
  (setq mu4e-alert-email-notification-types '(count)
        mu4e-alert-email-count-title "Email"
        mu4e-alert-interesting-mail-query (format "date:7d..now and flag:unread and (%s)" my/show-all-inboxes))
  (mu4e-alert-enable-notifications)
  ;; FIXME Start mu4e in the background to retrieve new mail at boot.
  (mu4e t))

;; (use-package! mu4e-send-delay
;;   :disabled
;;   :after mu4e
;;   :config
;;   (add-hook 'mu4e-main-mode-hook #'mu4e-send-delay-initialize-send-queue-timer)
;;   (mu4e-send-delay-setup))

(after! web-mode
  (add-to-list 'web-mode-engines-alist '("django" . "\\.tera\\.(xml|html)\\'")))

(defun disable-line-numbers ()
  (interactive)
  (display-line-numbers-mode -1))

(add-hook 'pdf-outline-buffer-mode-hook #'disable-line-numbers)

(use-package! olivetti
  :hook ((markdown-mode magit-status-mode forge-topic-mode) . olivetti-mode)
  :bind (:map doom-leader-map
         ("to" . olivetti-mode))
  :config
  (add-hook 'olivetti-mode-hook #'disable-line-numbers)
  (setq-default olivetti-body-width 95))

(setq +ligatures-extra-symbols
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
        :dot "•"
        ;; Org-specific symbols
        :title "#"
        :subtitle "##"
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
        :arrow "->"
        :vertical "│"
        :merge-right "├╮"
        :split-right "├╯"))

;; (after! org
;;   (set-ligatures! 'org-mode
;;     :title "#+TITLE:"
;;     :title "#+title:"
;;     :quote "#+BEGIN_QUOTE"
;;     :quote_end "#+END_QUOTE"
;;     :quote "#+begin_quote"
;;     :quote_end "#+end_quote"
;;     :begin_export "#+BEGIN_EXPORT"
;;     :end_export "#+END_EXPORT"
;;     :begin_export "#+begin_export"
;;     :end_export "#+end_export"
;;     :begin_quote "#+BEGIN_VERSE"
;;     :end_quote "#+END_VERSE"
;;     :begin_quote "#+begin_verse"
;;     :end_quote "#+end_verse"
;;     :section ":PROPERTIES:"
;;     :end ":END:"))

(after! markdown-mode
  (set-ligatures! 'markdown-mode
    :src_block "```"))

(after! web-mode
  (setq web-mode-prettify-symbols-alist nil))


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

(load! "custom/mixed-pitch")
;; (use-package! mixed-pitch
;;   :commands mixed-pitch-mode
;;   :init
(map! :leader "tm" #'mixed-pitch-mode)
;; :config
(setq mixed-pitch-set-height t)
(dolist (e '(outline-1 outline-2 outline-3 outline-4 outline-5
                       outline-6 outline-7 outline-8 outline-9
                       org-date org-special-keyword org-drawer))
  (add-to-list 'mixed-pitch-fixed-pitch-faces e))

(custom-set-faces!
  '(mixed-pitch-variable-pitch :family "Times New Roman" :height 1.25))
;;)

;;;; Periodically clean buffers
(use-package! midnight
  :hook (doom-first-buffer . midnight-mode)
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
        midnight-period (* 60 60)))

;; Using C-/ for comments aligns with other editors.
;; IMPORTANT: This MUST be in the global map or else undo-tree doesn't work!
(map! :map global-map
      "C-/" 'comment-dwim)
(map! :i "C-z" 'undo)

(use-package! ox-moderncv
  :commands (org-cv-export-to-pdf)
  :config
  (defun org-cv-export-to-pdf ()
    (interactive)
    (let* ((org-latex-default-packages-alist nil)
           (org-latex-packages-alist nil)
           (org-latex-with-hyperref nil)
           (outfile (org-export-output-file-name ".tex" nil)))
      (org-export-to-file 'moderncv outfile
        nil nil nil nil nil
        (lambda (f) (org-latex-compile f))))))

;; TODO Disable lsp-ui to fix loss of window config in exwm!

;; The default popup is SLOW, use posframe or minibuffer.
;; TODO We need chinese font with same height as my font.
(after! pyim
  (setq pyim-page-tooltip 'posframe))

(use-package! string-inflection)

(use-package! zoom
  :disabled
  ;; :hook (doom-first-input . zoom-mode)
  :config
  (setq zoom-size '(0.65 . 0.65)
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

;; TODO Submit a PR to doom-emacs fixing this in +workspace/switch-to
(defun +workspace/switch-to-other (index)
  "Switch to a workspace at a given INDEX. A negative number will start from the
end of the workspace list."
  (interactive
   (list (or current-prefix-arg
             (if (featurep! :completion ivy)
                 (ivy-read "Switch to workspace: "
                           (+workspace-list-names)
                           :caller #'+workspace/switch-to
                           :preselect +workspace--last)
               (completing-read "Switch to workspace: "
                                (+workspace-list-names)
                                nil nil nil nil
                                +workspace--last)))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (condition-case-unless-debug ex
      (let ((names (+workspace-list-names))
            (old-name (+workspace-current-name)))
        (cond ((numberp index)
               (let ((dest (nth index names)))
                 (unless dest
                   (error "No workspace at #%s" (1+ index)))
                 (+workspace-switch dest)))
              ((stringp index)
               (+workspace-switch index t))
              (t
               (error "Not a valid index: %s" index)))
        (unless (called-interactively-p 'interactive)
          (if (equal (+workspace-current-name) old-name)
              (+workspace-message (format "Already in %s" old-name) 'warn)
            (+workspace/display))))
    ('error (+workspace-error (cadr ex) t))))

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
        "o b" #'open-browser
        "o g" #'=calendar
        "w U" #'winner-redo
        "w D" #'delete-other-windows
        ;; "<f19>" (general-key (format "%s %s" doom-leader-key doom-leader-key))
        "TAB" #'+workspace/switch-to-other
        "\\" #'set-input-method
        ";" #'toggle-input-method
        "w s" (cmd! (evil-window-vsplit) (other-window 1))
        "w v" (cmd! (evil-window-split) (other-window 1))
        :desc "media" "m" nil
        "m c" #'playerctl-play-pause
        "m n" #'playerctl-next
        "m p" #'playerctl-previous
        "m s" #'+wm/screenshot
        "m S" #'desktop-environment-screenshot-part
        "DEL" #'+workspace/delete))

;; Show window hints big and above X windows.
;; Load this early to give windows their header-line hints ASAP.
(use-package! ace-window
  :init
  (map! :leader "j" #'ace-window)
  :config
  (setq aw-display-style 'overlay
        aw-posframe-parameters '())
  ;; (when (featurep 'exwm)
  ;;   (setq aw-posframe-parameters '((parent-frame . nil))))
  ;; Show the window key in the header line.
  (ace-window-display-mode))

;; Allow easy NPM commands in most programming buffers.
(use-package! npm-mode
  :hook ((prog-mode text-mode conf-mode) . npm-mode))

(use-package! spotify
  :disabled
  :commands (spotify-remote-mode global-spotify-remote-mode)
  :config
  (setq spotify-oauth2-client-id "f3e530a58362402fab4ca04976916f80"
        spotify-oauth2-client-secret "71a43675cd8247edafa85351bede7cf6")
  (map! :map spotify-mode-map
        doom-localleader-key 'spotify-command-map))


;; LSP formatting is messed up for Javascript, so disable it.
(setq +format-with-lsp nil)
(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode
            sql-mode
            tex-mode
            latex-mode
            ;; There are several different formats I use web-mode for that
            ;; can't be reliably formatted on save.
            web-mode
            mhtml-mode
            mu4e-compose-mode
            md-msg-edit-mode
            message-mode))

;; (setq! fancy-splash-image "~/.config/wpg/.current"
;;        +doom-dashboard-banner-padding '(0 . 0)
;;        +doom-dashboard--width 0.9)

(after! calfw
  (remove-hook! 'cfw:calendar-mode-hook 'hide-mode-line-mode))

(after! vterm
  (setq vterm-buffer-name-string "vterm %s"))

(after! highlight-indent-guides
  (setq-default highlight-indent-guides-method 'character))

;; (after! paren
;;   (defun +snead/switch-show-paren (&optional arg)
;;     (interactive)
;;     (show-paren-mode -1)
;;     (show-smartparens-mode arg))
;;   (add-hook! 'show-paren-mode-hook #'+snead/switch-show-paren))

(use-package! emms
  :commands emms-smart-browse
  :init
  (map! :leader "ol" #'emms-smart-browse)
  (advice-add 'emms-smart-browse :before (defun +emms/switch-workspace () (+workspace-switch "*music*" t)))
  :custom
  (emms-source-file-default-directory "~/Music/")
  ;; (emms-player-list '(emms-player-mpg321
  ;;                     emms-player-ogg123
  ;;                     emms-player-mplayer))
  :config
  (emms-all)
  (emms-default-players)
  (require 'emms-browser)
  (require 'emms-player-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (setq emms-info-asynchronously t)
  (require 'emms-info-libtag)
  (setq emms-info-functions '(emms-info-mpd emms-info-libtag emms-info-cueinfo))
  (emms-add-directory-tree emms-source-file-default-directory)
  ;; Always open EMMS in its own workspace.
  )

;; (insert-image (create-image
;; "~/.config/doom/vscode-icons/icons/file_type_rust.svg" 'svg nil :scale 1))

;; (defun all-the-icons-ivy-icon-for-file (s)
;;   "Return icon for filename S.
;; Return the octicon for directory if S is a directory.
;; Otherwise fallback to calling `all-the-icons-icon-for-file'."
;;   (cond
;;    ((string-match-p "\\/$" s)
;;     (apply 'all-the-icons-octicon
;;      (append
;;       (list "file-directory")
;;       all-the-icons-ivy-icon-args
;;       (list :face 'all-the-icons-ivy-dir-face))))
;;    (t
;;     ;; TODO Create a cache, if necessary.
;;     (let* ((icon-path "~/.config/doom/vscode-icons/icons")
;;           (icon-file (format "%s/file_type_%s.svg" icon-path
;;                              (file-name-extension s)))
;;           (real-icon-file (if (file-exists-p icon-file) icon-file
;;                             (format "%s/default_file.svg" icon-path))))
;;       (create-image real-icon-file 'svg nil :scale 0.1)))))

(map! :mnv "go" #'avy-goto-char)

;; Launch programs directly from an Emacs prompt.
(use-package! app-launcher
  :bind (:map doom-leader-map
         ("o o" . app-launcher-run-app)))

;; Sync my org agenda entries to my calendar, so I can see these entries on my
;; phone and get reminders there.
(use-package! org-caldav
  :defer 5
  :config
  (setq org-caldav-url "https://dav.mailbox.org/caldav"
        org-caldav-calendar-id "Y2FsOi8vMC8zMQ"
        org-caldav-inbox "~/org/inbox.org"
        org-caldav-files '("~/org/me.org" "~/org/todo.org" "~/org/spring-2021.org" "~/org/dailp.org")
        org-caldav-delete-calendar-entries 'always
        org-caldav-sync-direction 'twoway
        org-caldav-resume-aborted 'never
        ;; org-icalendar-include-todo 'unblocked
        ;; TODOs don't work with org-caldav yet, so just make events for the deadline.
        org-icalendar-use-deadline '(event-if-not-todo event-if-todo-not-done)
        org-icalendar-timezone "UTC"
        ;; Alert me 20 minutes before events on my phone.
        org-icalendar-alarm-time 20)

  ;; Don't interrupt me when syncing calendars!
  (defun +org-caldav-sync-quiet ()
    "Sync calendars without showing the results."
    (let ((org-caldav-show-sync-results nil))
      (org-caldav-sync)))

  ;; Sync my calendars every hour or so.
  ;;(run-with-timer 5 3600 #'+org-caldav-sync-quiet)
  )

(after! org
  (setq org-timer-countdown-timer-title "Timer finished"))

;; Give full state names to make learning the names easier.
(after! evil
  (setq-default evil-kill-on-visual-paste nil
                evil-move-cursor-back t
                evil-visual-region-expanded t)
  (setq evil-normal-state-tag " NORMAL "
        evil-insert-state-tag " INSERT "
        evil-visual-state-tag " VISUAL "
        evil-visual-line-tag " VLINE "
        evil-visual-block-tag " VBLOCK "
        evil-emacs-state-tag " EMACS "
        evil-operator-state-tag " OPERATOR "
        evil-replace-state-tag " REPLACE "))

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-persp-name t
        doom-modeline-icon nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-modal-icon nil
        doom-modeline-height 26
        doom-modeline-bar-width +snead/frame-border-width)
  (doom-modeline-def-segment exwm-title '(:eval (or exwm-title (doom-modeline-segment--buffer-info-simple))))
  (doom-modeline-def-segment major-mode 'mode-name)
  (doom-modeline-def-segment buffer-position '(" " mode-line-percent-position))
  (doom-modeline-def-segment ace-window '(:eval (and (featurep 'ace-window)
                                                     (propertize (concat " " (upcase (window-parameter (selected-window) 'ace-window-path)) " ")
                                                                 'face
                                                                 (if (doom-modeline--active)
                                                                     'doom-modeline-bar
                                                                   'doom-modeline-bar-inactive)))))
  (doom-modeline-def-segment ranger '(:eval (ranger-header-line)))
  (doom-modeline-def-segment buffer-info-revised
    "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
    (concat
     (doom-modeline--buffer-mode-icon)
     (doom-modeline--buffer-state-icon)
     (doom-modeline--buffer-name)))

  (doom-modeline-def-segment vertical-pad (list (propertize " " 'display '(raise +0.25))
                                                (propertize " " 'display '(raise -0.25))))

  (doom-modeline-def-modeline 'main
    '(ace-window modals buffer-info-revised buffer-position " " matches vertical-pad)
    '(misc-info input-method major-mode vcs lsp checker " "))
  (doom-modeline-def-modeline 'project
    '(ace-window buffer-default-directory vertical-pad)
    '(misc-info irc mu4e github debug major-mode process " "))
  (doom-modeline-def-modeline 'vcs
    '(ace-window buffer-info-simple vertical-pad)
    '(misc-info vcs " "))
  (doom-modeline-def-modeline 'simple
    '(ace-window "  " exwm-title vertical-pad)
    '(misc-info major-mode " "))
  (doom-modeline-def-modeline 'pdf
    '(ace-window " " matches buffer-info-simple pdf-pages vertical-pad)
    '(misc-info major-mode process vcs " "))
  (doom-modeline-def-modeline 'dashboard
    '(ace-window window-number buffer-default-directory-simple vertical-pad)
    '(misc-info irc mu4e github debug minor-modes input-method major-mode process " "))
  (doom-modeline-def-modeline 'ranger
    '(ace-window " " ranger vertical-pad)
    '())
  (add-hook! '(ranger-mode-hook)
    (defun doom-modeline-set-ranger-modeline ()
      (setq-local mode-line-format nil
                  header-line-format (doom-modeline 'ranger))))
  ;; (doom-modeline-set-modeline 'upper t)
  ;; Add a mini-modeline with: git, workspace, time, battery, exwm tray

  )

(after! evil-escape
  (setq evil-escape-delay 0.04))

(defvar +snead/volume nil)
;;(defun +snead/volume-update ()
  ;;(setq +snead/volume (list (+svg-icon-string "material" "volume-high")
                            ;;(propertize " " 'display '(space :width 0.5))
                            ;;(concat (desktop-environment-volume-get) "%"))))
;;(after! desktop-environment
  ;;(run-with-timer 1 2 #'+snead/volume-update))

(use-package! mini-modeline
  :if (equal "t" (getenv "EMACS_EXWM"))
  :after doom-modeline
  :hook (doom-modeline-mode . mini-modeline-mode)
  :config
  ;; Avoid putting time in global-mode-string, instead explicitly showing time.
  ;; (doom-modeline-def-segment time 'display-time-string)
  ;; (doom-modeline-def-segment wifi )
  ;; Make a custom doom-modeline to sit in the echo area.
  ;; (doom-modeline-def-modeline 'lower
  ;;   '()
  ;;   '(mu4e persp-name battery " " time))

  (defvar +snead/wifi-name nil)
  (defun +snead/wifi-update ()
    (let* ((network-name (string-trim (shell-command-to-string "iwctl station wlan0 show | rg Connected | cut -d' ' -f17-")))
           (connected (not (string-empty-p network-name))))
      (setq +snead/wifi-name
            (propertize "--"
                        'display (svg-icon "material" (if connected "wifi" "wifi-off") "white")
                        'help-echo (if connected network-name "Disconnected")))))
  ;;(run-with-timer 1 5 #'+snead/wifi-update)

  (let ((half-space (propertize " " 'display '(space :width 0.5))))
    (setq mini-modeline-r-format `(
                                   (:eval (doom-modeline-segment--mu4e))
                                   ;;(:eval (+svg-icon-string "material" "folder"))
                                   ,half-space
                                   (:eval (+workspace-current-name))
                                   "  "
                                   (:eval +snead/wifi-name)
                                   "  "
                                   (:eval +snead/volume)
                                   "  "
                                   (:eval (let ((status doom-modeline--battery-status))
                                            (list (car status) (cdr status))))
                                   "  "
                                   ;;(:eval (+svg-icon-string "material" "clock-outline"))
                                   ,half-space
                                   display-time-string)
          ;; Make room for an external system tray on the right side.
          mini-modeline-right-padding 10
          ;; Don't apply extra faces.
          mini-modeline-enhance-visual nil
          mini-modeline-update-interval 0.5)
    )
  ;; Show battery life and current time in the mini-modeline.
  (display-battery-mode)
  (display-time-mode)
  ;; Remove time from misc-info, so that can go into the header.
  ;; Then, the time segment uses display-time-string directly.
  (setq-default global-mode-string (remq 'display-time-string global-mode-string))
  ;; Remove load from the time string, it was adding too much.
  (setq display-time-string-forms (remove 'load display-time-string-forms))
  ;; Specialized header lines instead of mode lines.
  (defun doom-modeline-set-modeline (key &optional default)
    "Set the modeline format. Does nothing if the modeline KEY doesn't exist.
If DEFAULT is non-nil, set the default mode-line for all buffers.

Redefined to change the header-line instead of the mode-line.
If there's a local header-line-format, don't step on its feet!
Move it to the mode-line."
    (when-let ((modeline (doom-modeline key)))
      (if default
          (setf (default-value 'mode-line-format) (list "%e" modeline))
        (progn
          (when (and (local-variable-p 'header-line-format) (not (equal "%e" (car header-line-format))))
            (setq-local mode-line-format header-line-format))
          (setq-local header-line-format (list "%e" modeline))))
      ))

  (defun doom-modeline-unfocus ()
    "Unfocus mode-line."
    (setq doom-modeline-remap-face-cookie
          (face-remap-add-relative 'header-line 'mode-line-inactive))))

;; Add extra line spacing for some modes.
;; Not in programming modes because indent guides look a bit funny spaced out.
;; (setq-hook! '(olivetti-mode-hook
;;               mu4e-headers-mode-hook)
;;   line-spacing 2)

(use-package! svg-icon
	      :disabled
  :after all-the-icons doom-modeline
  :config
  (defun +svg-icon-string (collection name)
    (propertize "--" 'display (svg-icon collection name (face-attribute 'default :foreground))))
  (memoize 'svg-icon)
  ;; Redefine battery icon display using svg-icon.
  (defun doom-modeline-update-battery-status ()
    "Update battery status."
    (setq doom-modeline--battery-status
          (when (bound-and-true-p display-battery-mode)
            (let* ((data (and (bound-and-true-p battery-status-function)
                              (funcall battery-status-function)))
                   (charging? (string-equal "AC" (cdr (assoc ?L data))))
                   (percentage (car (read-from-string (or (cdr (assq ?p data)) "ERR"))))
                   (valid-percentage? (and (numberp percentage)
                                           (>= percentage 0)
                                           (<= percentage battery-mode-line-limit)))
                   (face (if valid-percentage?
                             (cond (charging? 'doom-modeline-battery-charging)
                                   ((< percentage battery-load-critical) 'doom-modeline-battery-critical)
                                   ((< percentage 25) 'doom-modeline-battery-warning)
                                   ((< percentage 95) 'doom-modeline-battery-normal)
                                   (t 'doom-modeline-battery-full))
                           'doom-modeline-battery-error))
                   (icon (if valid-percentage?
                             (cond (charging?
                                    (+svg-icon-string "material" "battery-charging-100")
                                    ;; (doom-modeline-icon 'alltheicon "battery-charging" "🔋" "+"
                                    ;;                     :face face :height 1.4 :v-adjust -0.1)
                                    )
                                   ((> percentage 95)
                                    (+svg-icon-string "material" "battery")
                                    ;; (doom-modeline-icon 'faicon "battery-full" "🔋" "-"
                                    ;;                     :face face :v-adjust -0.0575)
                                    )
                                   ((> percentage 70)
                                    (+svg-icon-string "material" "battery-70")
                                    ;; (doom-modeline-icon 'faicon "battery-three-quarters" "🔋" "-"
                                    ;;                     :face face :v-adjust -0.0575)
                                    )
                                   ((> percentage 40)
                                    (+svg-icon-string "material" "battery-40")
                                    ;; (doom-modeline-icon 'faicon "battery-half" "🔋" "-"
                                    ;;                     :face face :v-adjust -0.0575)
                                    )
                                   ((> percentage battery-load-critical)
                                    (+svg-icon-string "material" "battery-10")
                                    ;; (doom-modeline-icon 'faicon "battery-quarter" "🔋" "-"
                                    ;;                     :face face :v-adjust -0.0575)
                                    )
                                   (t ;; (doom-modeline-icon 'faicon "battery-empty" "🔋" "!"
                                    ;;                     :face face :v-adjust -0.0575)
                                    (+svg-icon-string "material" "battery-alert")
                                    ))
                           (+svg-icon-string "material" "battery-unknown");; (doom-modeline-icon 'faicon "battery-empty" "⚠" "N/A"
                           ;;                     :face face :v-adjust -0.0575)
                           ))
                   (text (if valid-percentage? (format "%d%%%%" percentage) ""))
                   (help-echo (if (and battery-echo-area-format data valid-percentage?)
                                  (battery-format battery-echo-area-format data)
                                "Battery status not available")))
              (cons (propertize icon 'help-echo help-echo)
                    (propertize text 'face face 'help-echo help-echo))))))
  ;;(defun all-the-icons-material (icon-name &rest args)
  ;;(propertize "--" 'display (svg-icon "material" icon-name)))
  ;; (defun all-the-icons-faicon (icon-name &rest args)
  ;;   (propertize "--" 'display (svg-icon "")))
  )

(map! :leader "fa" (cmd! (consult-find "~")))

(map! :leader "oe" #'proced)
(after! proced
  (map! :map proced-mode-map
        :n "gr" #'proced-update))

(after! which-key
  (setq which-key-idle-delay 0.4
        which-key-show-prefix nil))

;; Make which-key prettier with groups and command descriptions.
(use-package! pretty-which-key
  :after which-key
  :config
  ;; Add groups and command descriptions to several modes.
  (require 'pretty-which-key-modes))

(use-package! hercules
  :disabled
  :after pretty-which-key
  :config
  ;; (hercules-def
  ;;  :toggle-funs #'+mu4e-show-help
  ;;  :keymap 'mu4e-headers-mode-map
  ;;  :transient t)
  )

(defun +which-key-show-evil-major (&optional all mode)
  (interactive "P")
  (if (which-key--popup-showing-p)
      ;; Hide the existing popup.
      (progn ;; (setq-local which-key-persistent-popup nil)
        (which-key--hide-popup))
    ;; Show the popup!
    (let* ((map-sym (intern (format "%s-map" (or mode major-mode))))
           (value (and (boundp map-sym) (symbol-value map-sym)))
           (evil-value (or (evil-get-auxiliary-keymap value evil-state)
                           (evil-get-auxiliary-keymap value 'normal)
                           value)))
      (if (and value (keymapp value))
          (progn (which-key--show-keymap
                  "Major-mode bindings"
                  evil-value
                  (apply-partially #'which-key--map-binding-p evil-value)
                  all
                  t)
                 ;; (setq-local which-key-persistent-popup t)
                 )
        (message "which-key: No map named %s" map-sym)))))

;; Show help menus in evil-bound modes.
(map! :after mu4e
      :map mu4e-headers-mode-map
      :mn "?" #'+which-key-show-evil-major)

(map! :leader "o -" #'deer)
(map! :after ranger
      :map ranger-mode-map
      :mn "?" #'+which-key-show-evil-major)

(after! pdf-view
  (map! :map pdf-view-mode-map
        :mn "?" #'+which-key-show-evil-major))


;; Center the minibuffer to make it easier to read quickly.
;; (defvar +snead/max-minibuffer-width 130)
;; (defun +snead/center-minibuffer ()
;;   (let ((margin (max 0 (/ (- (window-width) +snead/max-minibuffer-width) 2))))
;;     (unless (and (featurep 'mini-frame) mini-frame-mode)
;;       (set-window-fringes nil margin margin))))
;; (remove-hook 'minibuffer-setup-hook #'+snead/center-minibuffer)

(map! :after envrc
      :leader "e" envrc-command-map)

;; Benchmark startup if Emacs is launched with --debug-init
(use-package! benchmark-init
  :if doom-debug-p
  :config
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))

(use-package! disk-usage
  :commands (disk-usage disk-usage-here))

(custom-set-faces!
  '(doom-modeline-spc-face :inherit nil)
  '(header-line :inherit mode-line))

(after! lsp-mode
  (setq lsp-signature-function #'lsp-lv-message))

(use-package! eldoc-box
  :hook ((prog-mode) . eldoc-box-hover-mode)
  :config
  ;; TODO Avoid point when calculating the box position. (Useful for small windows)
  (defun +eldoc-box--upper-corner-position-function (width _)
    "Place the box at the upper-right corner of the selected window,
rather than the default which places it relative to the whole frame.
Position is calculated base on WIDTH and HEIGHT of childframe text window"
    (cons (+ (window-pixel-left) (- (window-pixel-width) width 8))
          ;; y position + a little padding (16)
          (+ (window-pixel-top) (window-header-line-height))))
  (setq eldoc-box-position-function #'+eldoc-box--upper-corner-position-function)
  ;; Remove the header-line in the eldoc-box.

  (after! mini-modeline
    (add-hook 'eldoc-box-buffer-hook 'mini-modeline--no-header)))

;; Highlight regions of operation for slightly longer than default.
(after! evil-goggles
  (setq evil-goggles-duration 0.2))

;; Exclude org-mode from company, b/c most of the time I don't need completion there.
(after! company
  (setq company-global-modes '(not erc-mode message-mode help-mode gud-mode org-mode)))

;; I don't need smartparens in org-mode.
;; (add-hook 'org-mode-hook 'turn-off-smartparens-mode)

(after! ws-butler
  (setq ws-butler-keep-whitespace-before-point t))

(use-package! plantuml-mode)

(setq-default inferior-lisp-program "common-lisp.sh")

(use-package! sly-asdf
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-asdf 'append))
