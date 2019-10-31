;; bootstrap package manager (straight.el)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default 1)

(straight-use-package 'use-package)

;; Base packages
(use-package helm) ; narrowing completion?
(use-package evil)
(use-package magit)
(use-package company) ; completions
(use-package dtrt-indent)
(use-package smart-hungry-delete
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
	 ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ; dont defer so we can add our functions to hooks 
  :config (smart-hungry-delete-add-default-hooks))

;; Languages
(use-package nix-mode)

;; Rust stuff
(use-package rustic) 

;; evil mode by default while I adjust to emacs!
(evil-mode 1)

;; Enable completion everywhere
(add-hook 'after-init-hook 'global-company-mode)
