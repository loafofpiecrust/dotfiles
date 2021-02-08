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
    ;; System tray over the right side of the echo area
    (exec "stalonetray")
    (setq my-autostart-done t)))

;; Autostart stuff.
(add-hook 'exwm-init-hook #'my-autostart)

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
                                   ))

;; (use-package! evil-anzu
;;   :after evil
;;   :config
;;   (setq! anzu-cons-mode-line-p nil)
;;   (global-anzu-mode))

;; TODO try out golden-ratio/zoom to highlight active window.

;; Custom ivy prompt to connect to network.
(defun counsel-iwd-connect ()
  (interactive)
  (let* ((networks (my-iwd-network-list))
         (network (completing-read
                   "Connect to network: "
                   networks
                   (lambda (x) (not (string-prefix-p "> " (cdr x)))))))
    (my-iwd-connect (assoc network networks))))

;; TODO Prompt for password if necessary.
(defun my-iwd-connect (network)
  ;; (async-start (lambda ()
  ;;                (shell-command-to-string (format "iwctl station wlan0 connect \"%s\"" (cdr network))))
  ;;              (lambda (output)
  ;;                (unless (string-blank-p output)
  ;;                  (message (string-trim output)))))
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

(defun +snead/logout (&optional arg)
  "Perform some system action related to logins."
  (interactive "P")
  (let* ((options '(("log out" . "kill -9 -1")
                    ("reboot" . "systemctl reboot")
                    ("shutdown" . "systemctl poweroff")
                    ("sleep" . "systemctl suspend")))
         (pick (assoc (completing-read "Log out: " options)
                      options)))
    (call-process-shell-command (cdr pick))))

(defun +snead/vpn-connect (vpn)
  (interactive "P")
  (let* ((options '(("Panama" . "openvpn-panama")))
         (vpn (assoc (completing-read "Connect to VPN: " options)
                     options)))
    (shell-command "pgrep openvpn && systemctl stop \"openvpn-*\"")
    (async-shell-command (format "systemctl start \"%s\"" (cdr vpn))
                         "*help:iwctl*")))

(defun +snead/random-wallpaper ()
  (interactive)
  (exec "wpg -m"))

(map! :leader
      :prefix ("r" . "system")
      :desc "Connect to Network" "i" #'counsel-iwd-connect
      :desc "Connect to VPN" "v" #'+snead/vpn-connect
      "w" #'+snead/random-wallpaper
      :desc "Log out" "o" #'+snead/logout)


;; (after! (evil-owl mini-frame)
;;   (appendq! evil-owl-extra-posframe-args `(:background-color ,(mini-frame-get-background-color))))

(after! counsel
  (defun counsel-linux-app-clear-cache ()
    (interactive)
    (counsel-linux-apps-list)
    nil)

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

(after! exwm
  (defun exwm-insert (orig-fn &rest args)
    (if (eq major-mode 'exwm-mode)
        (progn (evil-set-register ?+ (first args))
               (exwm-input--fake-key ?\C-v))
      (apply orig-fn args)))
  ;; (advice-add #'insert :around #'exwm-insert)
  )

(after! exwm
  (setenv "SUDO_ASKPASS" "emacsclient -e '(read-passwd \"sudo password: \")' | xargs"))

(define-minor-mode inhibit-screensaver-mode
  "Inhibit the screensaver from turning the screen dark"
  :lighter " CAF!"
  :init-value nil
  :global t
  (if inhibit-screensaver-mode
      (exec "xset s off")
    (exec "xset s on")))

(provide 'my-exwm-config)
