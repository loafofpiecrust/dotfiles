;;; ~/dotfiles/emacs/.config/doom/custom/my-exwm-config.el -*- lexical-binding: t; -*-

(defun exec (command)
  "Executes the given command as a separate process without waiting.
Intended to replicate the functionality of `exec' from i3 config."
  (let ((args (split-string command)))
    (apply #'call-process (first args) nil 0 nil (rest args))))

;; Make sure autostart only happens once.
(defvar my-autostart-done nil)
(defun my-autostart ()
  (unless my-autostart-done
    (exec "autostart.sh")
    (exec "picom --experimental-backends")
    (exec "davmail /home/snead/.config/davmail/.properties")
    (setq my-autostart-done t)))

;; Autostart stuff.
(add-hook 'exwm-init-hook #'my-autostart)

(use-package! exwm
  :no-require t
  :init
  (appendq! exwm-input-global-keys `(;;(,(kbd "s-,") . +ivy/switch-workspace-buffer)
                                     ;; (,(kbd "s-\\") . set-input-method)
                                     ;; (,(kbd "s-;") . toggle-input-method)
                                     (,(kbd "<f19>") . ,doom-leader-map)
                                     (,(kbd "<XF86MonBrightnessDown>") . desktop-environment-brightness-decrement)
                                     (,(kbd "<XF86MonBrightnessUp>") . desktop-environment-brightness-increment)
                                     ;; (,(kbd "<Multi_key>") . ,doom-leader-map)
                                     ;; App Shortcuts
                                     ;; (,(kbd "s-`") . +vterm/toggle)
                                     ;; (,(kbd "s-a") . org-agenda)
                                     ;; (,(kbd "s-o") . ,doom-leader-open-map)
                                     ;; (,(kbd "s-p") . ,doom-leader-project-map)
                                     ;; (,(kbd "s-b") . ,(cmd! (exec "firefox")))
                                     ;; (,(kbd "s-c") . ,(cmd! (exec "playerctl play-pause")))
                                     )))

(use-package! anzu
  :after evil
  :config
  (setq! anzu-cons-mode-line-p nil)
  (global-anzu-mode))

(use-package! evil-anzu :after (evil anzu))

;; TODO try out golden-ratio/zoom to highlight active window.

;; Custom ivy prompt to connect to network.
(defun counsel-iwd-connect (&optional arg)
  (interactive "P")
  (ivy-read "Connect to a network: " (my-iwd-network-list)
            :predicate (lambda (x) (not (string-prefix-p "> " (cdr x))))
            :action #'my-iwd-connect
            :caller 'counsel-iwd-connect))

;; TODO Prompt for password if necessary.
(defun my-iwd-connect (network)
  (async-shell-command (format "iwctl station wlan0 connect \"%s\"" (cdr network))
                       "*help:iwctl*"))

(defun my-iwd-network-list ()
  (let ((networks (nthcdr 4 (split-string
                             (ansi-color-apply
                              (shell-command-to-string "iwctl station wlan0 get-networks"))
                             "[\r\n]+"
                             t
                             "[ \t]+"))))
    (mapcar (lambda (network) `(,network . ,(car (split-string (substring-no-properties network)
                                                               "[ \t]\\{3,\\}"))))
            networks)))

(defun counsel-logout (&optional arg)
  "Perform some system action related to logins."
  (interactive "P")
  (ivy-read "Log out: " '(("log out" . "kill -9 -1")
                          ("reboot" . "systemctl reboot")
                          ("shutdown" . "systemctl poweroff")
                          ("sleep" . "systemctl suspend"))
            :action (lambda (x) (call-process-shell-command (cdr x)))
            :caller 'counsel-logout))

(defun counsel-vpn-connect (&optional arg)
  "Connect to a VPN server."
  (interactive "P")
  (ivy-read "Connect to a VPN: " '(("panama" . "openvpn-panama"))
            :action #'my-vpn-connect
            :caller 'counsel-vpn-connect))

(defun my-vpn-connect (vpn)
  (shell-command "pgrep openvpn && systemctl stop \"openvpn-*\"")
  (async-shell-command (format "systemctl start \"%s\"" (cdr vpn))
                       "*help:iwctl*"))

(after! (evil evil-collection)
  (map! :leader
        "ri" #'counsel-iwd-connect
        "rv" #'counsel-vpn-connect
        "qo" #'counsel-logout))

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
               (completing-read "Switch to workspace: " (+workspace-list-names))))))
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

;; Place ivy frames above X windows.
(after! ivy-posframe
  (setq! ivy-posframe-parameters '((min-width . 90)
                                   (min-height . 17)
                                   (parent-frame . nil))))

(use-package mini-frame
  :hook (ivy-mode . mini-frame-mode)
  :config
  (setq! mini-frame-show-parameters '((left . 0.5)
                                      (top . 28)
                                      (width . 0.55)
                                      (height . 1)
                                      (left-fringe . 8)
                                      (right-fringe . 8)
                                      (parent-frame . nil))
         mini-frame-resize 'grow-only))

(after! counsel
  (defun counsel-linux-app-format-function-custom (name comment exec icon)
    (format "% -35s: %s"
            (propertize (ivy--truncate-string name 35)
                        'face 'counsel-application-name)
            (or comment "")))
  (setq counsel-linux-app-format-function #'counsel-linux-app-format-function-custom)
  (defun counsel-linux-app--parse-file (file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((start (re-search-forward "^\\[Desktop Entry\\] *$" nil t))
            (end (re-search-forward "^\\[" nil t))
            (visible t)
            name comment exec icon)
        (catch 'break
          (unless start
            (push file counsel-linux-apps-faulty)
            (message "Warning: File %s has no [Desktop Entry] group" file)
            (throw 'break nil))

          (goto-char start)
          (when (re-search-forward "^\\(Hidden\\|NoDisplay\\) *= *\\(1\\|true\\) *$" end t)
            (setq visible nil))
          (setq name (match-string 1))

          (goto-char start)
          (unless (re-search-forward "^Type *= *Application *$" end t)
            (throw 'break nil))
          (setq name (match-string 1))

          (goto-char start)
          (unless (re-search-forward "^Name *= *\\(.+\\)$" end t)
            (push file counsel-linux-apps-faulty)
            (message "Warning: File %s has no Name" file)
            (throw 'break nil))
          (setq name (match-string 1))

          (goto-char start)
          (when (re-search-forward "^Comment *= *\\(.+\\)$" end t)
            (setq comment (match-string 1)))

          (goto-char start)
          (unless (re-search-forward "^Exec *= *\\(.+\\)$" end t)
            ;; Don't warn because this can technically be a valid desktop file.
            (throw 'break nil))
          (setq exec (match-string 1))

          ;; (goto-char start)
          ;; (unless (re-search-forward "^Icon *= *\\(.+\\)$" end t)
          ;;   (throw 'break nil))
          ;; (let* ((icon-entry (match-string 1))
          ;;       (icon-path (first (split-string (shell-command-to-string
          ;;                                  (format "find /run/current-system/sw/share/icons -name \"%s.png\""
          ;;                                          icon-entry))
          ;;                                       "\n"))))
          ;;   (setq icon (if (s-blank? icon-path) icon-entry icon-path)))


          (goto-char start)
          (when (re-search-forward "^TryExec *= *\\(.+\\)$" end t)
            (let ((try-exec (match-string 1)))
              (unless (locate-file try-exec exec-path nil #'file-executable-p)
                (throw 'break nil))))
          (propertize
           (funcall counsel-linux-app-format-function name comment exec icon)
           'visible visible))))))


(provide 'my-exwm-config)
