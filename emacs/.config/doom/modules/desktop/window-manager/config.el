;;; desktop/exwm/config.el -*- lexical-binding: t; -*-

(defun +wm/screenshot ()
  "Take a screenshot."
  (interactive)
  (desktop-environment-screenshot)
  (message "Screenshot saved to %s" desktop-environment-screenshot-directory))

(use-package! exwm
  :if (getenv "EMACS_EXWM")
  :hook (after-init . exwm-enable)
  :init
  (setq exwm-input-global-keys `((,(kbd "s-SPC") . ,doom-leader-map)
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
                                 ;; I don't want a big message just for muting
                                 ;; the volume. I have a bar to show me if it worked.
                                 (,(kbd "<XF86AudioMute>") . ,(cmd! (shell-command-to-string desktop-environment-volume-toggle-command)))
                                 (,(kbd "<XF86AudioRaiseVolume>") . desktop-environment-volume-increment)
                                 (,(kbd "<XF86AudioLowerVolume>") . desktop-environment-volume-decrement)
                                 (,(kbd "<print>") . +wm/screenshot)
                                 ;; App Shortcuts
                                 (,(kbd "<s-return>") . eshell)
                                 (,(kbd "M-x") . counsel-M-x)))

  ;; Show all buffers on all displays since we have DOOM workspaces.
  (setq exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t)

  ;; Pass all keys directly to windows.
  ;; TODO Leverage exwm line and char modes for evil keybindings.
  (setq exwm-manage-configurations '((t char-mode t)))

  ;; FIXME May not need this given the above.
  (setq exwm-input-prefix-keys (list ?\M-\  ?\C-s ?\M-x))
  (setq exwm-input-simulation-keys '())

  :config
  ;; Let me copy things from other programs.
  (map! :map exwm-mode-map "C-c" nil)

  ;; Fix exwm buffer switching for DOOM.
  (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)

  ;; Pass keys directly to windows.
  (add-hook 'exwm-mode-hook #'evil-emacs-state)

  (defun exwm-rename-buffer ()
    "Rename the current buffer to match its corresponding window title."
    (interactive)
    (exwm-workspace-rename-buffer
     (concat "[" exwm-class-name "] "
             (if (<= (length exwm-title) 60) exwm-title
               (substring exwm-title 0 59)))))

  ;; Update each exwm buffer name to match the window class and title.
  (add-hook! '(exwm-update-class-hook exwm-update-title-hook)
             #'exwm-rename-buffer)

  (defun exwm--update-utf8-title-advice (oldfun id &optional force)
    "Only update the window title when the buffer is visible."
    (when (get-buffer-window (exwm--id->buffer id))
      (funcall oldfun id force)))
  ;; Allow persp-mode to restore window configurations involving exwm buffers by
  ;; only changing names of visible buffers.
  (advice-add #'exwm--update-utf8-title :around #'exwm--update-utf8-title-advice)

  ;; Show the program title of exwm buffers in ibuffer.
  (define-ibuffer-column exwm-class (:name "Class")
    (if (bound-and-true-p exwm-class-name)
        exwm-class-name
      "")))

;; Use emacs input methods in any application.
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

;; Automatically handle multiple monitors.
;; Each monitor corresponds to an Emacs frame, and each frame can focus on a
;; different workspace. Workspaces are always shared between all frames.
(use-package! exwm-randr
  :after exwm
  :config
  (setq exwm-randr-workspace-monitor-plist '(0 "eDP1"
                                               1 "HDMI1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda () (exec "xrandr --output HDMI1 --right-of eDP1 --auto")))
  (exwm-randr-enable))

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
