;;; ../../dotfiles/emacs/.config/doom/custom/pretty-which-key-modes.el -*- lexical-binding: t; -*-

(require 'pretty-which-key)

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

  (which-key-add-keymap-based-replacements
   doom-leader-map
   "p" "Project..."
   "m" "Media..."
   "a" "Authentication...")

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
                 (selectrum-repeat . "Resume search")
                 (execute-extended-command . "Execute command")
                 "i"))
     ("Actions" . (org-capture
                   (+popup/toggle . "Toggle last popup")
                   universal-argument
                   "p" "s" "o" "g" "f" "w" "t" "b" "c"))
     ("System" . ("a" "m" "r" "q" "h" "n"))))

  (pretty-which-key-add-command-groups
   doom-leader-map
   '(("Media Player" . ((playerctl-next . "Next")
                        (playerctl-previous . "Previous")
                        (playerctl-play-pause . "Toggle pause")))
     ("Screen capture" . ((+wm/screenshot . "Take screenshot")
                          (desktop-environment-screenshot-part . "Take partial screenshot")))))

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
                   ("x" . "Execute marks")
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

(provide 'pretty-which-key-modes)
