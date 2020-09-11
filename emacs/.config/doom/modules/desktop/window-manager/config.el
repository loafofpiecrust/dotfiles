;;; desktop/exwm/config.el -*- lexical-binding: t; -*-

(use-package! exwm
  :if (getenv "EMACS_EXWM")
  :hook (after-init . exwm-enable)
  :init
  (setq exwm-input-global-keys `((,(kbd "s-SPC") . ,doom-leader-map)
                                 (,(kbd "s-p") . counsel-linux-app)
                                 ;; TODO Launch programs in new window.
                                 ;; Select windows.
                                 (,(kbd "s-h") . evil-window-left)
                                 (,(kbd "s-l") . evil-window-right)
                                 (,(kbd "s-j") . evil-window-down)
                                 (,(kbd "s-k") . evil-window-up)
                                 ;; Move windows around.
                                 (,(kbd "s-H") . +evil/window-move-left)
                                 (,(kbd "s-J") . +evil/window-move-down)
                                 (,(kbd "s-K") . +evil/window-move-up)
                                 (,(kbd "s-L") . +evil/window-move-right)
                                 (,(kbd "s-u") . winner-undo)
                                 (,(kbd "s-U") . winner-redo)
                                 ;; TODO hydra for moving windows.
                                 ;; Split the frame, and follow.
                                 (,(kbd "s-s") . ,(cmd! (evil-window-vsplit) (other-window 1)))
                                 (,(kbd "s-v") . ,(cmd! (evil-window-split) (other-window 1)))
                                 ;; TODO delete buffer too if it's an app.
                                 (,(kbd "C-w") . kill-current-buffer)
                                 (,(kbd "s-w") . kill-current-buffer)
                                 ;; Workspace commands
                                 (,(kbd "s-q") . +workspace/close-window-or-workspace)
                                 (,(kbd "s-f") . exwm-floating-toggle-floating)
                                 (,(kbd "<s-tab>") . +workspace/switch-to-other)
                                 (,(kbd "<s-backspace>") . +workspace/delete)
                                 (,(kbd "s-n") . +workspace/new)
                                 (,(kbd "s-[") . +workspace/switch-left)
                                 (,(kbd "s-]") . +workspace/switch-right)
                                 (,(kbd "s-1") . +workspace/switch-to-0)
                                 (,(kbd "s-2") . +workspace/switch-to-1)
                                 (,(kbd "s-3") . +workspace/switch-to-2)
                                 (,(kbd "s-4") . +workspace/switch-to-3)
                                 (,(kbd "s-5") . +workspace/switch-to-4)
                                 (,(kbd "s-r") . exwm-reset)
                                 (,(kbd "s-<") . ivy-switch-buffer)
                                 (,(kbd "s-,") . ivy-switch-buffer-same-type)
                                 (,(kbd "<XF86MonBrightnessDown>") . desktop-environment-brightness-decrement)
                                 (,(kbd "<XF86MonBrightnessUp>") . desktop-environment-brightness-increment)
                                 (,(kbd "<XF86AudioMute>") . desktop-environment-toggle-mute)
                                 (,(kbd "<XF86AudioRaiseVolume>") . desktop-environment-volume-increment)
                                 (,(kbd "<XF86AudioLowerVolume>") . desktop-environment-volume-decrement)
                                 (,(kbd "<print>") . desktop-environment-screenshot)
                                 ;; App Shortcuts
                                 (,(kbd "<s-return>") . eshell)
                                 (,(kbd "M-x") . counsel-M-x)
                                 (,(kbd "s-`") . +eshell/toggle)))

  (setq exwm-input-prefix-keys (list ?\M-\  ?\C-s ?\M-x))
  (setq exwm-input-simulation-keys '())
  ;; Show all buffers on all displays since we have DOOM workspaces.
  (setq exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t)
  ;; Pass all keys directly to windows.
  (setq exwm-manage-configurations '((t char-mode t)))

  :config
  ;; I must be able to copy stuff!
  (map! :map exwm-mode-map "C-c" nil)

  ;; Fix exwm buffer switching for DOOM.
  (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)

  ;; Pass keys directly to windows.
  (add-hook 'exwm-mode-hook #'evil-emacs-state)

  ;; Rename buffers to their window title.
  (defun exwm-rename-buffer ()
    (interactive)
    (exwm-workspace-rename-buffer
     (concat exwm-class-name ": "
             (if (<= (length exwm-title) 60) exwm-title
               (substring exwm-title 0 59)))))

  ;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
  (add-hook! '(exwm-update-class-hook exwm-update-title-hook)
             #'exwm-rename-buffer)

  ;; Fix issues with persp not saving X windows.
  (defun exwm--update-utf8-title-advice (oldfun id &optional force)
    "Only update the exwm-title when the buffer is visible."
    (when (get-buffer-window (exwm--id->buffer id))
      (funcall oldfun id force)))
  (advice-add #'exwm--update-utf8-title :around #'exwm--update-utf8-title-advice)

  (define-ibuffer-column exwm-class (:name "Class")
    (if (bound-and-true-p exwm-class-name)
        exwm-class-name
      "")))

(use-package! exwm-xim
  :after exwm
  :config
  ;; These variables are required for X programs to pick up Emacs IM.
  (setenv "XMODIFIERS" "@im=exwm-xim")
  (setenv "GTK_IM_MODULE" "xim")
  (setenv "QT_IM_MODULE" "xim")
  (setenv "CLUTTER_IM_MODULE" "xim")
  (setenv "QT_QPA_PLATFORM" "xcb")
  (setenv "SDL_VIDEODRIVER" "x11")
  (exwm-xim-enable))

(after! (exwm persp-mode)
  (defun +workspace/display ()
    "Do nothing. EXWM is enabled so I'm showing workspaces in polybar."
    (interactive)))

(after! (exwm ivy)
  (defun ivy-switch-buffer-prefiltered (prompt predicate)
    (ivy-read (or prompt "Switch to buffer: ") #'internal-complete-buffer
              :keymap ivy-switch-buffer-map
              :predicate predicate
              :preselect (buffer-name (other-buffer (current-buffer)))
              :action #'ivy--switch-buffer-action
              :matcher #'ivy--switch-buffer-matcher
              :caller 'ivy-switch-buffer-prefiltered))

  (defun ivy-switch-buffer-same-type ()
    (interactive)
    (ivy-switch-buffer-prefiltered
     (format "Switch to %s buffer: " exwm-class-name)
     (lambda (b) (equal (buffer-local-value 'exwm-class-name (cdr b)) exwm-class-name)))))

(use-package! desktop-environment
  :after exwm
  :config
  (setq desktop-environment-brightness-normal-decrement "5%-"
        desktop-environment-brightness-normal-increment "5%+"))
