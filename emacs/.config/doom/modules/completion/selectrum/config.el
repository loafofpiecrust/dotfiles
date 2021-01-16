;;; completion/selectrum/config.el -*- lexical-binding: t; -*-

(use-package! selectrum
  :hook (doom-first-input . selectrum-mode)
  :config
  (setq selectrum-extend-current-candidate-highlight t
        selectrum-fix-minibuffer-height t
        projectile-completion-system 'default
        selectrum-count-style 'current/matches)

  (setq completion-styles '(substring partial-completion))

  (map! :leader "'" #'selectrum-repeat)

  ;; For those used to evil bindings.
  (map! :map selectrum-minibuffer-map
        "C-j" #'selectrum-next-candidate
        "C-k" #'selectrum-previous-candidate)

  (autoload 'ffap-guesser "ffap")
  (setq minibuffer-default-add-function
        (defun minibuffer-default-add-function+ ()
          (with-selected-window (minibuffer-selected-window)
            (delete-dups
             (delq nil
                   (list (thing-at-point 'symbol)
                         (thing-at-point 'list)
                         (ffap-guesser)
                         (thing-at-point-url-at-point))))))))

(defun +selectrum-refresh ()
  "Refresh the candidate list in the current selectrum buffer."
  (setq selectrum--previous-input-string nil))

;; There are a few things that helm is loaded for.
;; Automatically replace them with built-in completion functions.
(after! helm
  (fset 'helm-find-files #'find-file))

;; Sorts and filters results, remembering the most selected results between sessions.
(use-package! selectrum-prescient
  :if (featurep! +prescient)
  :after selectrum
  :config
  (selectrum-prescient-mode)
  (prescient-persist-mode))

;; Add more useful completion commands and provide nicer UX for some built-in ones.
(use-package! consult
  :after selectrum
  :init
  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur)
  (define-key!
    [remap switch-to-buffer] #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap imenu] #'consult-imenu
    [remap apropos] #'consult-apropos
    [remap recentf-open-files] #'consult-recent-file)

  :config
  ;; Previews are too heavy and don't work the best in EXWM, so disable them.
  (setq consult-preview-key nil)

  (setq consult-project-root-function #'projectile-project-root
        consult-find-command '("fd" "--color=never" "--full-path")
        consult-ripgrep-command '("rg" "--null" "--line-buffered" "--color=always" "--max-columns=500" "--no-heading" "--line-number" "--hidden" "-g!.git" "-S" "." "-e"))

  (map! :leader
        "sb" #'consult-line
        "se" #'consult-error
        "iy" #'consult-yank-pop
        "sp" #'consult-ripgrep
        "/" #'consult-ripgrep
        ">" #'consult-find))

;; Integrate some commands slightly better with selectrum, especially ones with
;; dynamic collections, like consult-find.
(use-package! consult-selectrum
  :after consult selectrum)

;; Provide categories for each type of completion.
(use-package! marginalia
  :init
  (marginalia-mode))

(after! which-key
  (push '(("^[0-9-]\\|kp-[0-9]\\|kp-subtract\\|C-u\\|C-g$" . nil) . ignore)
        which-key-replacement-alist)

  ;; We only use which-key in minibuffer mode for embark actions, where we need
  ;; a line break at the start to see the first row of bindings.
  (defun +which-key--echo (orig-fun text)
    "Echo TEXT to minibuffer without logging."
    (let (message-log-max)
      (message "\n%s" text)))
  (advice-add 'which-key--echo :around #'+which-key--echo))

;; Integrate a few consult commands as embark actions!
;; Also, provide action descriptions until that's resolved upstream in embark.
(map! :after (embark consult)
      :map embark-file-map
      :desc "Open externally" "x" #'consult-file-externally
      :desc "Copy" "c" #'copy-file
      :desc "Load" "l" #'load-file
      :desc "Rename" "r" #'rename-file
      :desc "Open in other window" "o" #'find-file-other-window
      :desc "Delete" "d" #'delete-file
      :desc "Open shell here" "e" #'embark-eshell-in-directory
      :desc "Diff" "=" #'ediff-files
      :desc "Make directory" "+" #'make-directory
      :desc "Execute command" "!" #'shell-command
      :desc "Execute command (async)" "&" #'async-shell-command
      :desc "Delete directory" "D" #'delete-directory
      :desc "Insert relative path" "I" #'embark-insert-relative-path
      :desc "Copy relative path" "W" #'embark-save-relative-path
      "b" nil
      "B" nil)

(use-package! embark
  :bind (:map minibuffer-local-map
         ("C-o" . #'embark-act)
         ("C-e" . #'embark-collect-snapshot)
         :map embark-collect-mode-map
         ("C-o" . #'embark-act)
         :map embark-general-map
         ;; ESC should act the same as C-g in all minibuffer operations.
         ("<escape>" . #'ignore))
  :config
  (setq embark-prompt-style 'default
        embark-action-indicator (defun +embark-which-key (map)
                                  "Show key hints in the same minibuffer as actions."
                                  (setq-local which-key-popup-type 'minibuffer
                                              which-key-show-prefix nil
                                              which-key-replacement-alist
                                              (cons '(("^C-h\\|SPC$" . nil) . ignore)
                                                    which-key-replacement-alist))
                                  (which-key--show-keymap "Embark" map nil nil 'no-paging)
                                  #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)

  ;; Integrate with helpful.
  ;; (defun embark-describe-symbol ()
  ;;   (interactive)
  ;;   (helpful-symbol (intern (embark-target))))

  (set-popup-rule! "^\\*Embark Collect" :size 0.35 :ttl 0 :quit nil)
  (add-hook 'embark-pre-action-hook #'+selectrum-refresh))

(after! projectile
  ;; FIXME Why is this broken when ivy is disabled?
  (setq projectile-switch-project-action (lambda ()
                                           (+workspaces-set-project-action-fn)
                                           (+workspaces-switch-to-project-h))))

(after! selectrum
  (defun doom-project-find-file (dir)
    "Jump to a file in DIR (searched recursively).

If DIR is not a project, it will be indexed (but not cached)."
    (unless (file-directory-p dir)
      (error "Directory %S does not exist" dir))
    (unless (file-readable-p dir)
      (error "Directory %S isn't readable" dir))
    (let* ((default-directory (file-truename dir))
           (projectile-project-root (doom-project-root dir))
           (projectile-enable-caching projectile-enable-caching))
      (cond (projectile-project-root
             (unless (doom-project-p default-directory)
               ;; Disable caching if this is not a real project; caching
               ;; non-projects easily has the potential to inflate the projectile
               ;; cache beyond reason.
               (setq projectile-enable-caching nil))
             (if (doom-module-p :completion 'ivy)
                 ;; Intentionally avoid `helm-projectile-find-file', because it runs
                 ;; asynchronously, and thus doesn't see the lexical
                 ;; `default-directory'
                 (call-interactively #'counsel-projectile-find-file)
               ;; THIS IS WHAT I CHANGED! projectile comes with a function for
               ;; this exact purpose!
               (projectile-find-file-in-directory default-directory)))
            ((fboundp 'consult-fdfind)
             (call-interactively #'consult-fdfind))
            ((fboundp 'counsel-file-jump) ; ivy only
             (call-interactively #'counsel-file-jump))
            ((project-current nil dir)
             (project-find-file-in nil nil dir))
            ((fboundp 'helm-find-files)
             (call-interactively #'helm-find-files))
            ((call-interactively #'find-file))))))

;; Reroute all minibuffer completion to a child frame, including all built-in
;; completing-read, read-string, etc. functionality.
(use-package! mini-frame
  :if (featurep! +childframe)
  :hook (doom-first-input . mini-frame-mode)

  :custom
  ;; Immediately instantiate the mini-frame so there's no waiting when we first
  ;; want to use it.
  (mini-frame-create-lazy . nil)

  :config
  ;; Some modes use a pop-up window above the echo area to show contents,
  ;; allowing input via the minibuffer. We don't want to move those inputs
  ;; into the mini-frame, because that would be jarring.
  (setq mini-frame-ignore-commands '(eval-expression
                                     ;; Debugging
                                     "edebug-eval-expression" debugger-eval-expression
                                     ;; Org-mode scheduling
                                     org-schedule org-time-stamp org-deadline org-time-stamp-inactive
                                     org-agenda-deadline
                                     org-agenda-schedule
                                     ;; calc prompts
                                     calcDigit-start))

  ;; Keep the background color the same as normal frames.
  (defun +mini-frame-header-bg (&optional frame)
    (face-attribute 'mode-line :background frame))
  (setq mini-frame-background-color-function #'+mini-frame-header-bg)

  ;; Make it pretty and automatically resize based on the prompt.
  (setq mini-frame-show-parameters `((left . 0.5)
                                     (top . 38)
                                     (width . 0.55)
                                     (height . 1)
                                     (internal-border-width . 0)
                                     (left-fringe . 10)
                                     (right-fringe . 10)))

  ;; Workaround for EXWM compatibility to show the mini-frame on top of any X window.
  (when (featurep 'exwm)
    (appendq! mini-frame-show-parameters '((parent-frame . nil)))))
