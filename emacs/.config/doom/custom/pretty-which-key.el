;;; ../../dotfiles/emacs/.config/doom/custom/pretty-which-key.el -*- lexical-binding: t; -*-
;;;
;;; Why this package?
;;; matcha provides a really nice UI for a bunch of modes based on transient.
;;; The core problem in my mind is that to rebind, redescribe, or regroup a
;;; command within a particular transient map you have to use
;;; transient-suffix-put rather than built-in keymap manipulation methods.
;;;
;;; pretty-which-key extends the idea of which-key: to provide a UI showing you
;;; all the *already bound* keys in the current keymap.
;;; pretty-which-key adds the idea of binding groups to arrange bindings in a
;;; similar fashion to transient or hydra.
;;;
;;; The difference is that pretty-which-key is totally additive. You can install
;;; this, make no other code changes, and use the same which-key UI as before
;;; with your simpler keymaps.
;;; For larger keymaps, like a leader key, you can assign groups to each command
;;; separately from the bindings themselves.
;;; Reassigning an existing binding can then happen in any vanilla Emacs
;;; context using =define-key= (if =which-key-enable-extended-define-key= is =t=
;;;before loading which-key):
;;;
;;;     (define-key doom-leader-map (kbd ".") '("Jump to///file" . find-file))
;;;
;;; You may provide only a group but no description (to see "Find File" under
;;; the "Jump to" group):
;;;
;;;     (define-key doom-leader-map (kbd ".") '("Jump to///" . find-file))
;;;
;;; You may also provide only a description with no group:
;;;
;;;     (define-key doom-leader-map (kbd ".") '("Jump to file" . find-file))
;;;
;;; This exposes the internal separator used by pretty-which-key for grouping: "///".
;;; Now, my bindings are decentralized. I don't have to use transient-suffix-put
;;;to modify a specific position in a transient. I can simply use =define-key=
;;;or =general= or DOOM's =map!=.
;;;
;;; Caveat: replacing an existing binding with a different command is easy as
;;;that, but replacing existing bindings with a different *key* is more work than
;;;with transient. You have to unbind the original key and bind your own.
;;; I suspect that this is a somewhat less common case than simply adding your
;;;own bindings.
;;;
;;; With selectrum, consult, and embark coming into view as alternatives to ivy
;;;and helm that are closer to Emacs core and make use of built-in facilities, I
;;;felt that a solution to discoverable command UI using purely vanilla keymaps
;;;was in order.
;;;
;;; Don't get me wrong, transient is amazing! I love using transient with magit
;;;and a few other complex modes, but I'm not sure that I need its power when it
;;;comes to most modes. Plus, =evil-collection= and =doom-emacs= and other
;;;starter kits pride themselves on having community collections of keybindings
;;;that we can put to use directly in pretty UIs instead of fragmenting into a new
;;;set of community binding efforts.

(require 'which-key)
(require 'seq)
(require 's)

;; Change some which-key settings to make things prettier.
(setq which-key-separator " "
      which-key-min-display-lines 7
      which-key-add-column-padding 3)

;; TODO Option to hide uncategorized bindings in a particular map.
;; TODO Combine duplicate entries for the same function with comma separation.
;; TODO Add prefix keys to certain groups.

(defconst pretty-which-key--group-separator "///")

(defun pretty-which-key--last-part (binding-desc)
  (car (last (s-split pretty-which-key--group-separator binding-desc))))

(defun pretty-which-key--add-keymap-replacements (keymap key replacement &rest more)
  (while key
    (let* ((needs-desc (s-suffix-p pretty-which-key--group-separator replacement))
           (pseudo (which-key--pseudo-key key))
           ;; Keep current command description if none provided and there's an
           ;; existing one in the keymap.
           (existing-binding (and needs-desc (lookup-key keymap pseudo)))
           (desc (if (and existing-binding (consp existing-binding))
                     (concat replacement (pretty-which-key--last-part (caadr existing-binding)))
                   replacement))
           (command nil))
      (define-key keymap pseudo
        (list 'which-key (cons desc command))))
    (setq key (pop more)
          replacement (pop more))))

(defun pretty-which-key-add-command-replacements (keymap replacements)
  (mapc (lambda (replacement)
          (mapc (lambda (binding) (pretty-which-key--add-keymap-replacements
                                   keymap
                                   binding
                                   (cdr replacement)))
                (where-is-internal (car replacement) keymap)))
        replacements))

(defun pretty-which-key-add-command-groups (keymap groups)
  "Groups are of the form '(GROUP . ((COMMAND . DESCRIPTION)))
Adds an entry of the form '((DESCRIPTION . GROUP) . COMMAND) for each key."
  ;; for each group
  (mapc (lambda (group)
          ;; for each binding in the group
          (mapc (lambda (binding)
                  (mapc (lambda (kb)
                          (pretty-which-key--add-keymap-replacements
                           keymap
                           ;; command symbol
                           kb
                           (concat (car group) pretty-which-key--group-separator (or (cdr-safe binding) ""))))
                        (where-is-internal (or (car-safe binding) binding) keymap)))
                (cdr group)))
        groups))

;; Add plain descriptions to evil commands!
;; This makes checking out the g- and z- prefixes easier.
(with-eval-after-load 'evil
  ;; Give names to the most common evil prefixes that will almost definitely not change.
  (which-key-add-keymap-based-replacements
    evil-normal-state-map
    "z" "display"
    "g" "goto"
    "c" "change"
    "[" "previous"
    "]" "next")

  (pretty-which-key-add-command-groups
   doom-leader-map
   '(("Jump to" . ((consult-buffer . "buffer")
                   (persp-switch-to-buffer . "workspace buffer")
                   (ace-window . "window")
                   (consult-ripgrep . "search in project")
                   (find-file . "file")
                   (projectile-find-file . "file in project")
                   (bookmark-jump . "bookmark")
                   (evil-switch-to-windows-last-buffer . "last buffer")
                   (doom/open-scratch-buffer . "scratch buffer")
                   (+default/search-project-for-symbol-at-point . "symbol in project")
                   (consult-find . "some file")))
     ("Workspace" . ((+workspace/switch-to-other . "Switch")
                     (+workspace/delete . "Delete")))
     ("Input" . (set-input-method
                 toggle-input-method
                 selectrum-repeat))
     ("Actions" . (org-capture
                   execute-extended-command
                   +popup/toggle
                   universal-argument))))

  ;; TODO Integrate this into DOOM's map! so that one can provide groups directly
  ;; while mapping.
  ;; TODO Allow nil command description, so it *only* adds a group to the keymap entry.
  (pretty-which-key-add-command-groups
   doom-leader-project-map
   '(("Find" . ((projectile-find-file . "File")
                (projectile-find-file-in-known-projects . "File in other project")
                (doom/find-file-in-other-project . "File in other project")
                (projectile-recentf . "Recent file")
                (projectile-find-other-file . "Other file")
                (+default/browse-project . "Browse project")
                (doom/browse-in-other-project . "Browse other project")))
     ("Buffers" . ((projectile-switch-to-buffer . "Switch")
                   (projectile-save-project-buffers . "Save all")
                   (projectile-kill-buffers . "Kill all")
                   (doom/open-project-scratch-buffer . "Pop up scratch")
                   (doom/switch-to-project-scratch-buffer . "Switch to scratch")))
     ("Actions" . ((projectile-compile-project . "Compile project")
                   (projectile-run-project . "Run project")
                   (projectile-test-project . "Test project")
                   (projectile-run-shell-command-in-root . "Run shell command")
                   (projectile-repeat-last-command . "Repeat last command")
                   (magit-todos-list . "TODO list")))
     ("Manage" . ((projectile-switch-project . "Other project")
                  (projectile-invalidate-cache . "Invalidate cache")
                  (projectile-add-known-project . "Add project")
                  (projectile-remove-known-project . "Remove project")
                  (projectile-discover-projects-in-directory . "Discover projects here")
                  (+default/discover-projects . "Discover new projects")
                  (projectile-edit-dir-locals . "Edit project .dir-locals")
                  (projectile-configure-project . "Configure")))))

  (pretty-which-key-add-command-groups
   evil-normal-state-map
   '(("Operators" . ((evil-insert-resume . "Resume insert")
                     (evil-visual-restore . "Resume visual")
                     (evil-exchange . "Exchange")
                     (evilnc-comment-operator . "Comment")
                     (evil-upcase . "Upcase")
                     (evil-fill . "Fill")
                     (evil-downcase . "Downcase")
                     (evil-rot13 . "Encrypt text")
                     (evil-fill-and-move . "Fill and move")
                     (evil-invert-case . "Invert case")
                     (+eval:region . "Eval Region")
                     (+eval/buffer . "Eval Buffer")
                     (+format:region . "Format region")
                     (evil-numbers/dec-at-pt . "Decrement Number")
                     (evil-numbers/inc-at-pt . "Increment Number")
                     (evil-join-whitespace . "Join with whitespace")
                     (+evil:yank-unindented . "Copy unindented")
                     (evil-lion-left)
                     (evil-lion-right)))
     ("Navigate" . ((+lookup/references . "Find references")
                    (+lookup/definition . "Go to definition")
                    (+lookup/file . "Go to file")
                    (evil-find-file-at-point-with-line . "Go to file and line")
                    (evil-goto-first-line . "Go to top")
                    (goto-last-change . "Last change")
                    (goto-last-change-reverse . "More recent change")))
     ("Folds" . ((evil-toggle-fold . "Toggle fold")
                 (evil-close-fold . "Close fold")
                 (evil-close-folds . "Close all folds")
                 (evil-open-fold . "Open fold")
                 (evil-open-fold-rec . "Open folds below")
                 (evil-open-folds . "Open all folds")))
     ("Words" . ((what-cursor-position . "Print cursor position")
                 (+spell/add-word . "Learn spelling")
                 (+spell/remove-word . "Unlearn spelling")
                 (ispell-word . "Spellcheck")
                 (count-words . "Count words")))
     ("Sections" . ((+evil:narrow-buffer . "Narrow buffer")
                    (doom/widen-indirectly-narrowed-buffer . "Widen buffer")
                    (kill-current-buffer . "Close buffer")
                    ))))

  (pretty-which-key-add-command-groups
   outline-mode-map
   '(("Outline" . ((outline-hide-body . "Hide body")
                   (outline-show-entry . "Show entry")
                   (outline-show-branches . "Show branches")
                   (outline-hide-other . "Hide other")
                   (outline-up-heading . "Up heading")))))

  (pretty-which-key-add-command-groups
   evil-motion-state-map
   '(("Folds" . ((vimish-fold-delete . "Delete fold")
                 (vimish-fold-delete-all . "Delete all folds")
                 (evil-vimish-fold/create . "Create fold")
                 (evil-vimish-fold/create-line . "Create fold of lines")))
     ("Navigate" . ((evil-first-non-blank-of-visual-line . "First non-blank of visual line")
                    (evil-last-non-blank . "Last non-blank")
                    (evil-next-visual-line . "Next visual line")
                    (evil-previous-visual-line . "Previous visual line")
                    (evil-middle-of-visual-line . "Middle of visual line")
                    (evil-beginning-of-visual-line . "Beginning of visual line")
                    (evil-end-of-visual-line . "End of visual line")
                    (evil-backward-word-end . "End of last word")
                    (evil-backward-WORD-end . "End of last WORD")
                    (evil-scroll-column-left . "Scroll 1 left")
                    (evil-scroll-column-right . "Scroll 1 right")
                    (evil-scroll-right . "Scroll right")
                    (evil-scroll-left . "Scroll left")
                    (evil-scroll-line-to-center . "Scroll line to center")
                    (evil-scroll-line-to-top . "Scroll line to top")
                    (evil-scroll-line-to-bottom . "Scroll line to bottom")
                    (evil-scroll-top-line-to-bottom . "Scroll top line to bottom")
                    (evil-scroll-bottom-line-to-top . "Scroll bottom line to top")
                    (+fold/next . "Next fold")
                    (+fold/previous . "Previous fold")

                    ;; Next
                    (outline-next-visible-heading . "Next visible heading")
                    (+evil/insert-newline-below . "Newline below")
                    (+workspace/switch-right . "Next workspace")
                    (evil-next-close-paren . "Next close paren")
                    (evil-next-close-brace . "Next close brace")
                    (evil-forward-section-end . "Next section end")
                    (evil-forward-section-begin . "Next section start")
                    (evil-forward-arg . "Next argument")
                    (next-buffer . "Next buffer")
                    (+evil/next-comment . "Next comment")
                    (git-gutter:next-hunk . "Next git change")
                    (next-error . "Next error")
                    (+evil/next-file . "Next file")
                    (+evil/next-frame . "Next frame")
                    (+evil/next-beginning-of-method . "Next method start")
                    (+evil/next-end-of-method . "Next method end")
                    (+spell/next-error . "Next misspelling")
                    (hl-todo-next . "Next TODO")
                    (+evil:url-encode . "URL Encode")
                    (+web:encode-html-entities . "Encode HTML entities")
                    (+evil:c-string-encode . "Encode C string")
                    )))))



(defvar pretty-which-key-prefixes '("proced-"
                                    "evil-"
                                    "+evil/"
                                    "+evil:"
                                    "projectile-"
                                    "npm-mode-"
                                    "consult-"
                                    "counsel-"
                                    "doom/"
                                    "doom-"
                                    "which-key-"
                                    "desktop-environment-"
                                    "magit-"
                                    "mu4e-headers-"
                                    "mu4e-"
                                    "mu4e~"
                                    "+mu4e/"
                                    "org-"))


(defun pretty-which-key-strip-prefix (command-name)
  (let ((prefix (seq-find (lambda (s) (s-prefix? s command-name))
                          pretty-which-key-prefixes)))
    (if prefix
        (s-chop-prefix prefix command-name)
      command-name)))

(defun pretty-which-key-unlispify (symbol)
  (let ((s (string-trim (replace-regexp-in-string "[-:/+]+" " " symbol))))
    (concat (upcase (substring s 0 1)) (substring s 1))))

(defun pretty-which-key-backup-format (kb)
  (cons (car kb)
        ;; Use the built-in conversion from symbol name to pretty string
        ;; provided by "custom".
        (pretty-which-key-unlispify (pretty-which-key-strip-prefix (cdr kb)))))

;; Last resort replacement turning function names into space-delimited strings.
;; Since this is at the end of the list, other replacements take precedent,
;; especially those embedded in the relevant keymap. This just gives a
;; consistent look to commands without manual descriptions.
(add-to-list 'which-key-replacement-alist
             (cons '(nil . "^\\+?\\w+[-/~]")
                   #'pretty-which-key-backup-format)
             t
             (lambda (a b) (equal (car a) (car b))))

;; Describe commands for mode-specific bindings.
(with-eval-after-load 'proced
  (pretty-which-key-add-command-replacements
   proced-mode-map
   '((proced-sort-interactive . "Order interactively")
     (proced-sort-pid . "Order by PID")
     (proced-sort-pcpu . "Order by %CPU")
     (proced-sort-pmem . "Order by %MEM")
     (proced-mark . "Mark")
     (proced-mark-all . "Mark all"))))

(with-eval-after-load 'magit
  (pretty-which-key-add-command-groups
   magit-status-mode-map
   '(("Magit Sections" . (magit-jump-to-staged
                          magit-jump-to-stashes
                          magit-jump-to-tracked
                          magit-jump-to-unstaged
                          magit-jump-to-untracked
                          magit-jump-to-todos)))))

(with-eval-after-load 'mu4e
  (which-key-add-keymap-based-replacements
    mu4e-headers-mode-map
    "c" "compose")
  (pretty-which-key-add-command-groups
   mu4e-headers-mode-map
   '(("Actions" . (mu4e-headers-mark-for-flag
                   mu4e-headers-mark-for-unread
                   mu4e-headers-mark-for-unmark
                   mu4e-headers-mark-for-trash
                   mu4e-headers-mark-for-untrash
                   mu4e-headers-mark-for-unflag
                   mu4e-headers-mark-for-refile
                   mu4e-headers-mark-for-read
                   mu4e-headers-mark-for-move
                   mu4e-headers-mark-for-delete
                   mu4e-headers-mark-for-action
                   mu4e-headers-mark-for-something
                   mu4e-headers-mark-subthread
                   mu4e-headers-mark-custom
                   mu4e-headers-mark-pattern
                   mu4e-headers-action
                   (mu4e-mark-execute-all . "Execute marks")
                   mu4e-mark-unmark-all))
     ("Compose" . (mu4e-compose-reply
                   mu4e-compose-new
                   mu4e-compose-forward
                   mu4e-compose-edit))
     ("Navigate Mail" . (mu4e~headers-jump-to-maildir
                         mu4e-headers-next-unread
                         mu4e-headers-prev-unread
                         mu4e-headers-next
                         mu4e-headers-prev
                         (mu4e-headers-search-narrow . "Narrow results")
                         (mu4e-headers-search-bookmark . "Goto bookmark")
                         (mu4e-headers-search-bookmark-edit . "Edit bookmark")
                         mu4e~headers-quit-buffer
                         mu4e-headers-view-message
                         (mu4e-headers-search . "New search")
                         (mu4e-headers-search-edit . "Edit search")))
     ("Options" . (mu4e-headers-toggle-threading
                   mu4e-headers-toggle-include-related
                   mu4e-headers-toggle-skip-duplicates
                   (mu4e-headers-change-sorting . "Sort order")
                   (mu4e-context-switch . "Switch account"))))))

(defun pretty-which-key--build-groups (keys)
  (seq-sort-by #'car
               #'string<
               (seq-group-by (lambda (kb)
                               (let* ((command (last kb))
                                      (str (if (consp command) (car command) command)))
                                 (get-text-property 0 'pretty-which-key-group str)))
                             keys)))

(defun pretty-which-key--partition-list (orig-fun n list)
  "Partition LIST into N-sized sublists."
  ;; If all bindings are uncategorized, then don't show any category names.
  (let ((grouped-keys (pretty-which-key--build-groups list)))
    (if (and (= 1 (length grouped-keys))
             (null (caar grouped-keys)))
        (apply orig-fun (list n (cdar grouped-keys)))
      ;; Otherwise, if there are any explicit categories then show those as title
      ;; headings. Uncategorized bindings are in the last section.
      (let ((content-n (- n 1)))
        (apply #'seq-concatenate
               'list
               (mapcar (lambda (x)
                         ;; Ensure that each sublist is the max length.
                         (let ((sublists (mapcar (lambda (sublist) (cons `("" ,which-key-separator "") (if (length< sublist content-n)
                                                                                                           (seq-concatenate 'list sublist (make-list (- content-n (length sublist)) `("" ,which-key-separator "")))
                                                                                                         sublist)))
                                                 (apply orig-fun (list content-n (cdr x))))))
                           (setf (caar sublists) `("" ,which-key-separator ,(or (car x) "Other")))
                           sublists))
                       grouped-keys))))))

(advice-add 'which-key--partition-list :around #'pretty-which-key--partition-list)


(defun pretty-which-key--split-desc (binding-desc)
  "Split an internal key replacement into its constituent parts of
a binding group and command description. This should only apply to
key replacements bound by pretty-which-key functions."
  (cond
   ;; If the replacement contains our obscure separator, then it should be
   ;; treated as a group + description combination.
   ((and binding-desc (s-contains-p pretty-which-key--group-separator
                                    binding-desc))
    (reverse (mapcar (lambda (x) (and (not (string-empty-p x)) x))
                     (split-string binding-desc pretty-which-key--group-separator))))
   ;; Otherwise, it must be a plain description with no group.
   ;; In that case, just return the binding as-is.
   (binding-desc (list binding-desc))
   (t nil)))

(defun pretty-which-key--maybe-replace-pseudo (key-binding &optional prefix)
  "Use `which-key--replacement-alist' to maybe replace KEY-BINDING.
KEY-BINDING is a cons cell of the form \(KEY . BINDING\) each of
which are strings. KEY is of the form produced by `key-binding'."
  (which-key--get-pseudo-binding key-binding prefix))

(defun pretty-which-key--maybe-replace-regex (key-binding &optional prefix)
  "Use `which-key--replacement-alist' to maybe replace KEY-BINDING.
KEY-BINDING is a cons cell of the form \(KEY . BINDING\) each of
which are strings. KEY is of the form produced by `key-binding'."
  (let* ((replacer (if which-key-allow-multiple-replacements
                       #'which-key--replace-in-repl-list-many
                     #'which-key--replace-in-repl-list-once)))
    (pcase
        (apply replacer
               (list key-binding
                     (cdr-safe (assq major-mode which-key-replacement-alist))))
      (`(replaced . ,repl)
       (if which-key-allow-multiple-replacements
           (pcase (apply replacer (list repl which-key-replacement-alist))
             (`(replaced . ,repl) repl)
             ('() repl))
         repl))
      ('()
       (pcase (apply replacer (list key-binding which-key-replacement-alist))
         (`(replaced . ,repl) repl)
         ('() key-binding))))))

;; Add the corresponding group name to each binding, if any.
(defun which-key--format-and-replace (unformatted &optional prefix preserve-full-key)
  "Take a list of (key . desc) cons cells in UNFORMATTED, add
faces and perform replacements according to the three replacement
alists. Returns a list (key separator description)."
  (let* ((sep-w-face
          (which-key--propertize which-key-separator
                                 'face 'which-key-separator-face))
         (local-map (current-local-map))
         new-list)
    (dolist (key-binding unformatted)
      (let* ((key (car key-binding))
             (orig-desc (cdr key-binding))
             (orig-desc-sym (intern orig-desc))
             (group (which-key--group-p orig-desc))
             ;; At top-level prefix is nil
             (keys (if prefix
                       (concat (key-description prefix) " " key)
                     key))
             (local (eq (which-key--safe-lookup-key local-map (kbd keys))
                        orig-desc-sym))
             (hl-face (which-key--highlight-face orig-desc))
             ;; This is of the form '(KEY . DESCRIPTION)
             ;; Maybe we change DESCRIPTION to get '(KEY . (DESC . CATEGORY)) and
             ;; be compatible with define-key still.
             (key-binding-pseudo (pretty-which-key--maybe-replace-pseudo (cons keys orig-desc) prefix))
             ;; This var is of the form '(GROUP DESC) or just DESC
             (key-binding-parts (pretty-which-key--split-desc (cdr key-binding-pseudo)))
             (binding-group (nth 1 key-binding-parts))
             ;; If there's only a group name in the pseudo-binding, then allow
             ;; further replacements to fill in the command description.
             (key-binding (if (first key-binding-parts)
                              key-binding-pseudo
                            (pretty-which-key--maybe-replace-regex (cons keys orig-desc) prefix)))
             (key-binding-desc (or (first key-binding-parts) (cdr key-binding) orig-desc))
             (final-desc (which-key--propertize-description
                          key-binding-desc group local hl-face orig-desc)))
        (when final-desc
          (setq final-desc
                (which-key--truncate-description
                 (which-key--maybe-add-docstring final-desc orig-desc)))
          ;; ADDED
          (when binding-group
            (add-text-properties 0 (length final-desc)
                                 `(pretty-which-key-group ,binding-group)
                                 final-desc)))

        (when (consp key-binding)
          (push
           (list (which-key--propertize-key
                  (if preserve-full-key
                      (car key-binding)
                    (which-key--extract-key (car key-binding))))
                 sep-w-face
                 final-desc)
           new-list))))
    (nreverse new-list)))


(provide 'pretty-which-key)
