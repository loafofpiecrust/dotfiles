;;; desktop/polybar/config.el -*- lexical-binding: t; -*-

(use-package! doom-modeline
  :after anzu evil exwm
  :config
  (setq-default mode-line-format ""
                ;; header-line-format '(" " (:eval (or (buffer-file-name) mode-line-buffer-identification)))
                doom-modeline-buffer-file-name-style 'relative-to-project
                doom-modeline-height 20
))

;; Show no mode-line contents, use polybar instead.
(after! (anzu evil exwm)
  (setq window-divider-default-right-width 5
        window-divider-default-bottom-width 0)

  ;; Highlight the current window using a thin mode-line that matches the
  ;; window-divider size.
  (custom-set-faces!
    '(mode-line :height 30 :background nil :inherit internal-border)
    '(mode-line-inactive :height 30 :background nil :inherit vertical-border)
    '(header-line :inherit vertical-border))


  (defun +snead/show-file-header ()
    (let ((aligner (propertize " " 'display '(space :align-to left))))
      (setq-local header-line-format
                  `(,aligner
                    (:eval (doom-modeline-segment--buffer-info))
                    (:eval (doom-modeline-segment--buffer-position))
                    ))))

  ;; Use a general hook for all file-visiting buffers.
  (add-hook! '(prog-mode-hook text-mode-hook) #'+snead/show-file-header)

  ;; Some modes hide the mode-line, but we actually want it to show which window
  ;; is selected. Namely, terminal windows need a mode-line, but bottom pop-ups
  ;; don't necessarily.
  ;; (remove-hook! 'vterm-mode-hook #'hide-mode-line-mode)

  (defcustom polybar-update-interval 0.2
    "How long to wait for Emacs to be idle before updating polybar.")

  (defcustom polybar-format '()
    "Modeline format for displaying within a polybar module.")

  (setq polybar-format
        `("%e"
          "%%{B" (:eval (ewal-get-color 'green -9))
          " F" (:eval (ewal-get-color 'brightwhite)) "}"
          (:eval (when anzu--state
                   `(" " ,anzu--mode-line-format " ")))
          (:eval (when evil-this-macro
                   `("󰃨" ,(help-key-description (vector evil-this-macro) nil) " ")))
          "%%{B" (:eval (ewal-get-color 'green -6)) "}"
          (:eval (doom-modeline--evil))
          "%%{B" (:eval (ewal-get-color 'red -9)) "} "
          (:eval (+workspace-current-name))
          " %%{B" (:eval (ewal-get-color 'red -6)) "}"
          ;; " " mode-line-buffer-identification " "
          (:eval (when current-input-method-title
                   `("%%{B" (ewal-get-color 'red -3) "} "
                     current-input-method-title " ")))
          "%%{B" (:eval (ewal-get-color 'red 0)) "} "
          mode-line-modified
          mode-line-remote " "
          mode-line-position
          " %%{B" (:eval (ewal-get-color 'red +3))
          " F" (:eval (ewal-get-color 'black)) "}"
          (vc-mode vc-mode)
          " %%{B- F-}"))

  (defun polybar--contents ()
    (substring-no-properties (format-mode-line polybar-format)))

  (defun polybar--update ()
    (let ((inhibit-message t)
          (default-directory "~"))
      (start-process "polybar-msg" nil "polybar-msg" "hook" "exwm" "1")))

  (run-with-idle-timer polybar-update-interval t #'polybar--update)
  )
