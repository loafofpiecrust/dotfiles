;;; desktop/polybar/config.el -*- lexical-binding: t; -*-

;; Show no modeline, use polybar instead.
(after! (anzu evil exwm)
  (setq-default mode-line-format nil)

  (defcustom polybar-update-interval 0.2
    "How long to wait for Emacs to be idle before updating polybar.")

  (defcustom polybar-format
    `("%e"
      (:eval (format "%%%%{B%s} " (ewal-get-color 'green -6)))
      ,anzu--mode-line-format " "
      (:eval (format "%%%%{B%s} " (ewal-get-color 'red -9)))
      (:eval (+workspace-current-name))
      (:eval (format " %%%%{B%s} " (ewal-get-color 'red -6)))
      mode-line-buffer-identification
      (:eval (format " %%%%{B%s}" (ewal-get-color 'red -3)))
      evil-mode-line-tag
      (:eval (when evil-this-macro
               (format "󰃨%s " (help-key-description (vector evil-this-macro) nil))))
      current-input-method-title
      (:eval (format "%%%%{B%s} " (ewal-get-color 'red 0)))
      mode-line-modified
      mode-line-remote " "
      mode-line-position
      (:eval (format " %%%%{B%s F%s}" (ewal-get-color 'red +3) (ewal-get-color 'background)))
      (vc-mode vc-mode)
      " %%{B- F-}")
    "Modeline format for displaying within a polybar module.")

  (defun polybar--contents ()
    (substring-no-properties (format-mode-line polybar-format)))

  (defun polybar--update ()
    (let ((inhibit-message t) (default-directory "~"))
      (start-process "polybar-msg" nil "polybar-msg" "hook" "exwm" "1")))

  (run-with-idle-timer polybar-update-interval t #'polybar--update))
