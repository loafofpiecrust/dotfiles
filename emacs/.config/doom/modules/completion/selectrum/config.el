;;; completion/selectrum/config.el -*- lexical-binding: t; -*-

(use-package! selectrum
  :hook (doom-first-input . selectrum-mode)
  :config
  (setq selectrum-extend-current-candidate-highlight t
        selectrum-fix-minibuffer-height t
        projectile-completion-system 'default)
  (map! :leader
        "'" #'selectrum-repeat)
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
                         (thing-at-point-url-at-point)))))))


  ;; If an action changes the results, refresh the candidate list.
  (defun +selectrum-refresh ()
    (setq selectrum--previous-input-string nil))

  )

;; There are a few things that helm is picked up for.
;; Automatically replace them with default completion.
(after! helm
  (fset 'helm-find-files #'find-file))

(use-package! selectrum-prescient
  :if (featurep! +prescient)
  :after selectrum
  :config
  (selectrum-prescient-mode)
  (prescient-persist-mode))

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
  (setq consult-project-root-function #'projectile-project-root
        consult--fdfind-cmd '("fd" "--color=never" "--full-path"))
  (map! :n "?" #'consult-line)
  (map! :leader
        "sb" #'consult-line
        "se" #'consult-error
        "iy" #'consult-yank-pop
        "sp" #'consult-ripgrep
        "/" #'consult-ripgrep
        ">" #'consult-fdfind))

(use-package! consult-selectrum
  :after consult selectrum)

(defun consult-more-chars (input minimum)
  (unless (>= (length input) minimum)
    (list (format "Type at least %d characters..." minimum))))

(defvar consult-locate--results '())
(defun consult-locate ()
  (interactive)
  (let* ((default-directory "~")
         (choice (completing-read
                  "Find File "
                  (lambda (input pred type)
                    (async-start-process "consult-locate"
                                         "fd"
                                         (lambda (proc)
                                           (with-current-buffer (process-buffer proc)
                                             (setq consult-locate--results (s-lines (buffer-substring-no-properties (point-min) (point-max))))
                                             )
                                           (+selectrum-refresh)
                                           )
                                         input
                                         "~")
                    (or consult-locate--results '(input))
                    ))))
    (find-file fn)))

(use-package! marginalia
  :init
  (marginalia-mode))

(after! which-key
  (push '(("^[0-9-]\\|kp-[0-9]\\|kp-subtract\\|C-u\\|C-g$" . nil) . ignore)
        which-key-replacement-alist)

  ;; We only use which-key in minibuffer mode for embark actions, where we need
  ;; a line break at the start to see the first row of bindings.
  (defun which-key--echo (text)
    "Echo TEXT to minibuffer without logging."
    (let (message-log-max)
      (message "\n%s" text))))

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
  ;; Replicate default ivy-occur keybinding.
  :bind (:map minibuffer-local-map
         ("C-o" . #'embark-act)
         ("C-e" . #'embark-occur)
         :map embark-occur-mode-map
         ("C-o" . #'embark-act)
         :map embark-general-map
         ;; ESC should act the same as C-g in all minibuffer operations.
         ("<escape>" . #'ignore))
  :config
  (setq embark-prompt-style 'default
        embark-action-indicator
        (defun embark-which-key-setup ()
          (let ((which-key-show-transient-maps t)
                (which-key-replacement-alist
                 (cons '(("^C-h\\|ESC\\|SPC\\|<escape>$" . nil) . ignore)
                       which-key-replacement-alist)))
            (setq-local which-key-popup-type 'minibuffer
                        which-key-show-prefix nil)
            (which-key--update)))
        embark-become-indicator embark-action-indicator)

  ;; Integrate with helpful.
  (defun embark-describe-symbol ()
    (interactive)
    (helpful-symbol (intern (embark-target))))

  (set-popup-rule! "^\\*Embark Occur" :size 0.35 :ttl 0 :quit nil)
  (add-hook 'embark-pre-action-hook #'+selectrum-refresh)

  ;; Integrate embark with selectrum.
  (add-hook 'embark-target-finders 'selectrum-get-current-candidate)

  (add-hook 'embark-candidate-collectors
            (defun embark-selectrum-candidates+ ()
              (when selectrum-active-p
                (selectrum-get-current-candidates
                 ;; Pass relative file names for dired.
                 minibuffer-completing-file-name))))

  ;; No unnecessary computation delay after injection.
  (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate)

  (add-hook 'embark-input-getters
            (defun embark-selectrum-input-getter+ ()
              (when selectrum-active-p
                (let ((input (selectrum-get-current-input)))
                  (if minibuffer-completing-file-name
                      ;; Only get the input used for matching.
                      (file-name-nondirectory input)
                    input))))))

(after! projectile
  ;; FIXME Why is this broken when ivy is disabled?
  (setq projectile-switch-project-action (lambda ()
                                           (+workspaces-set-project-action-fn)
                                           (+workspaces-switch-to-project-h))))

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
          ((call-interactively #'find-file)))))

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

  ;; Make it pretty and automatically resize based on the prompt.
  (setq mini-frame-show-parameters `((left . 0.5)
                                     (top . 38)
                                     (width . 0.55)
                                     (height . 1)
                                     (internal-border-width . 0)
                                     (left-fringe . 10)
                                     (right-fringe . 10))
        mini-frame-resize 'grow-only)

  ;; Workaround for EXWM compatibility to show the mini-frame on top of any X window.
  (when (fboundp 'exwm--root)
    (appendq! mini-frame-show-parameters '((parent-frame . nil))))

  ;; Add padding to the minibuffer prompt, making it easier to read.
  ;; This is especially helpful for single-line prompts like passwords.
  (custom-set-faces!
    `(minibuffer-prompt :box (:line-width 3 :color ,(funcall mini-frame-background-color-function)))))
