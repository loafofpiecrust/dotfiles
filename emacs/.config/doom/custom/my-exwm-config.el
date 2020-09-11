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
    (exec "wpg -m")
    (exec "picom --experimental-backends")
    (exec "polybar main")
    (exec "davmail /home/snead/.config/davmail/.properties")
    (setq my-autostart-done t)))

;; Autostart stuff.
(my-autostart)

(use-package! exwm
  :no-require t
  :init
  (appendq! exwm-input-global-keys `((,(kbd "s-,") . ivy-switch-buffer-same-type)
                                     (,(kbd "s-\\") . set-input-method)
                                     (,(kbd "s-;") . toggle-input-method)
                                     ;; App Shortcuts
                                     (,(kbd "s-b") . ,(cmd! (exec "firefox")))
                                     (,(kbd "s-c") . ,(cmd! (exec "playerctl play-pause"))))))

;; Replace the modeline with a bar up top.
(use-package! mini-modeline
  :after exwm
  :defer 1
  :config
  (setq! mini-modeline-r-format `("%e"
                                  mode-line-buffer-identification " "
                                  current-input-method-title
                                  ;; ,anzu--mode-line-format
                                  evil-mode-line-tag
                                  mode-line-modified
                                  mode-line-remote " "
                                  mode-line-position)
         mini-modeline-update-interval 0.2
         mini-modeline-face-attr nil)
  (mini-modeline-mode))

(after! exwm
  ;; TODO saved state, buffer title, input method.
  ;; TODO anzu in mini-modeline
  (defun my-bar-contents ()
    (mapconcat 'identity
               (list (+workspace-current-name))
               " / "))

  (defun my-bar-update ()
    (let ((inhibit-message t) (default-directory "~"))
      (call-process "polybar-msg" nil 0 nil "hook" "exwm" "1")))

  (add-hook! '(window-configuration-change-hook
               evil-normal-state-entry-hook
               evil-insert-state-entry-hook
               evil-visual-state-entry-hook
               evil-emacs-state-entry-hook)
             #'my-bar-update))

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

(defun counsel-vpn-connect (&optional arg)
  (interactive "P")
  (ivy-read "Connect to a VPN: " (my-vpn-list)
            :action #'my-vpn-connect
            :caller 'counsel-vpn-connect))

(defun my-vpn-list ()
  '(("panama" . "openvpn-panama")))

(defun my-vpn-connect (vpn)
  (shell-command "pgrep openvpn && systemctl stop \"openvpn-*\"")
  (async-shell-command (format "systemctl start %s" (cdr vpn))
                       "*help:iwctl*"))

(after! (evil evil-collection)
  (map! :leader
        "ri" #'counsel-iwd-connect
        "rv" #'counsel-vpn-connect))

;; (after! ivy-posframe
;;   (setq ivy-posframe-parameters '((parent-frame nil))))

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

(provide 'my-exwm-config)
