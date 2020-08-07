;;; ~/dotfiles/emacs/.config/doom/custom/my-exwm-config.el -*- lexical-binding: t; -*-

;; Make sure autostart only happens once.
(defvar my-autostart-done nil)
(defun my-autostart ()
  (unless my-autostart-done
    (call-process "autostart.sh" nil 0 nil)
    (call-process "wpg" nil 0 nil "-m")
    (call-process "picom" nil 0 nil "--experimental-backends")
    (call-process "polybar" nil 0 nil "main")
    (call-process "davmail" nil 0 nil "/home/snead/.config/davmail/.properties")
    (setq my-autostart-done t)))

;; Autostart stuff.
(add-hook 'after-init-hook #'my-autostart)

(use-package! desktop-environment
  :after exwm
  :config
  (setq! desktop-environment-brightness-normal-decrement "5%-"
         desktop-environment-brightness-normal-increment "5%+")
  ;; (desktop-environment-mode)
  ;; (map! :map (desktop-environment-mode-map)
  ;;  "s-l" nil)
  )

(use-package! exwm
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
                                 ;; TODO hydra for moving windows.
                                 ;; Split the frame, and follow.
                                 (,(kbd "s-s") . ,(cmd! (evil-window-vsplit) (other-window 1)))
                                 (,(kbd "s-v") . ,(cmd! (evil-window-split) (other-window 1)))
                                 ;; TODO delete buffer too if it's an app.
                                 (,(kbd "s-w") . kill-current-buffer)
                                 ;; Workspace commands
                                 (,(kbd "s-q") . +workspace/close-window-or-workspace)
                                 (,(kbd "s-f") . exwm-floating-toggle-floating)
                                 (,(kbd "<s-tab>") . +workspace/switch-to)
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
                                 (,(kbd "s-\\") . set-input-method)
                                 (,(kbd "s-;") . toggle-input-method)
                                 (,(kbd "s-,") . ivy-switch-buffer)
                                 (,(kbd "<XF86MonBrightnessDown>") . desktop-environment-brightness-decrement)
                                 (,(kbd "<XF86MonBrightnessUp>") . desktop-environment-brightness-increment)
                                 (,(kbd "<XF86AudioMute>") . desktop-environment-toggle-mute)
                                 (,(kbd "<XF86AudioRaiseVolume>") . desktop-environment-volume-increment)
                                 (,(kbd "<XF86AudioLowerVolume>") . desktop-environment-volume-decrement)
                                 (,(kbd "<print>") . desktop-environment-screenshot)
                                 ;; (,(kbd "C-SPC") . ,doom-leader-map)
                                 ;; App Shortcuts
                                 (,(kbd "<s-return>") . eshell)
                                 (,(kbd "s-o") . ,doom-leader-open-map)
                                 (,(kbd "s-x") . counsel-M-x)
                                 (,(kbd "s-m") . =mu4e)
                                 (,(kbd "s-b") . ,(cmd! (call-process "firefox" nil "firefox")))
                                 (,(kbd "s-c") . ,(cmd! (start-process "playerctl" nil "playerctl" "play-pause")))
                                 (,(kbd "s-`") . +eshell/toggle)
                                 ))
  (setq exwm-input-prefix-keys (list ?\M-\  ?\C-s ?\M-x))
  (setq exwm-input-simulation-keys '())
  ;; Show all buffers on all displays since we have DOOM workspaces.
  (setq exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t)
  ;; Pass all keys directly to windows. May break input methods?
  ;; (setq exwm-manage-configurations '((t char-mode t)))

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
     (concat exwm-class-name ":"
             (if (<= (length exwm-title) 50) exwm-title
               (concat (substring exwm-title 0 49) "...")))))

  ;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
  (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook 'exwm-rename-buffer)
  ;; Fix issues with persp not saving X windows.
  (defun exwm--update-utf8-title-advice (oldfun id &optional force)
    "Only update the exwm-title when the buffer is visible."
    (when (get-buffer-window (exwm--id->buffer id))
      (funcall oldfun id force)))
  (advice-add #'exwm--update-utf8-title :around #'exwm--update-utf8-title-advice))

(use-package! exwm-x
  :after exwm
  :config
  (add-hook 'exwm-manage-finish-hook #'exwmx-grocery--manage-finish-function))

(use-package! exwm-xim
  :hook (exwm-init . exwm-xim-enable)
  :config
  (setenv "XMODIFIERS" "@im=exwm-xim")
  (setenv "GTK_IM_MODULE" "xim")
  (setenv "QT_IM_MODULE" "xim")
  (setenv "CLUTTER_IM_MODULE" "xim")
  (setenv "QT_QPA_PLATFORM" "xcb")
  (setenv "SDL_VIDEODRIVER" "x11"))

;; Replace the modeline with a bar up top.
(use-package! mini-modeline
  :after exwm
  :config
  (mini-modeline-mode))

;; (use-package! hide-mode-line
;;   :after exwm
;;   :config
;;   (global-hide-mode-line-mode))

;; Show no mode line in any window.
(after! exwm
  (setq-default mode-line-format nil))

(after! (exwm persp-mode)
  (defun +workspace/display ()
    "Do nothing. EXWM is enabled so I'm showing workspaces in polybar."
    (interactive)))

(after! exwm
  (defun my-bar-workspaces ()
    (let ((names (+workspace-list-names))
          (current (+workspace-current-name)))
      (mapconcat 'identity
                 (cl-loop for name in names
                          for i to (length names)
                          collect
                          (if (equal current name)
                              (format "%%{R} %s %%{R}" name)
                            (format " %s " name)))
                 "")))

  (defun my-bar-evil-state ()
    (string-trim (substring-no-properties (or (buffer-local-value
                                               'evil-mode-line-tag
                                               (window-buffer))
                                              "<*>"))))

  ;; TODO saved state, buffer title, input method.
  (defun my-bar-contents ()
    (mapconcat 'identity
               (list (my-bar-evil-state)
                     default-input-method
                     (+workspace-current-name)
                     (buffer-name (window-buffer)))
               " / "))

  (defun my-bar-update ()
    (let ((inhibit-message t))
      (call-process "polybar-msg" nil 0 nil "hook" "exwm" "1")))

  (add-hook! '(window-configuration-change-hook
               input-method-activate-hook
               evil-normal-state-entry-hook
               evil-insert-state-entry-hook
               evil-visual-state-entry-hook
               evil-emacs-state-entry-hook)
             #'my-bar-update))

;; TODO try out golden-ratio/zoom to highlight active window.

;; Move all minibuffer interaction to a child frame.
(use-package! mini-frame
  :after exwm
  :config
  (setq! mini-frame-resize t
         mini-frame-show-parameters '((top . 0.4)
                                      (width . 200)
                                      (left . 0.5)
                                      (parent-frame . nil)))
  (mini-frame-mode))

(provide 'my-exwm-config)
