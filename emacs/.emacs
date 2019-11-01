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

;; Make sure to use :commands and :hook to defer package load until its used
;; This is mainly relevant for language packages, where we don't need them all at once.

;; Essential packages
;;(use-package helm) ; narrowing completion?
(use-package evil)
(use-package magit)
(use-package yasnippet)
(use-package projectile :commands projectile-mode)

;; Completion and LSP setup
(use-package lsp-mode
  :hook ((go-mode rust-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)) ; language server protocol

(use-package lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; autocomplete
(use-package company
  :hook (prog-mode . company-mode))
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)
(setq company-require-match -1)

;; show docs in popup!
(use-package pos-tip)
(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

(use-package company-lsp)
(push 'company-lsp company-backends)

;; Editing
(setq-default indent-tabs-mode nil) ; spaces by default
(use-package dtrt-indent) ; auto-detect indentation
(use-package move-text)
(use-package smart-hungry-delete
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
		 ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ; dont defer so we can add our functions to hooks 
  :config (smart-hungry-delete-add-default-hooks))

;; Languages
(use-package nix-mode :commands nix-mode)
(use-package bazel-mode :commands bazel-mode)
(use-package yaml-mode :commands yaml-mode)

;; golang
(use-package go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

;; Rust
(use-package rust-mode)
(add-hook 'before-save-hook 'rust-format-buffer)

;; evil mode by default while I adjust to emacs!
(evil-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere 1)
(ido-mode 1)

;; Disable toolbar, I don't use it.
(menu-bar-mode -1)
(setq-default tab-width 4)
(show-paren-mode 1)

;; Enable completion, pair matching, line numbers
(add-hook 'after-init-hook (lambda ()
							 (electric-pair-mode)
							 (global-display-line-numbers-mode)
							 (column-number-mode)))

(custom-set-faces
 `(company-tooltip ((t :background "white"
					   :foreground "black")))
 `(lsp-ui-doc-background ((t :background "black"))))

;; Custom key bindings
(global-set-key (kbd "M-SPC") 'company-complete)
;; TODO: Rebind commands for marking??
(global-set-key (kbd "C-s") 'save-buffer)
;; TODO: Rebind isearch-forward
(global-set-key (kbd "C-]") 'tab-to-tab-stop)
;;(global-set-key (kbd "C-<tab>") 'indent-relative) ; TODO: rebind, C-tab doesn't work
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
;;(global-set-key (kbd "C-_") 'comment-line)
;; rebind undo-tree-undo for undoing stuff!
;; Generally need to pick bindings for undo/redo
;; TODO: Backspace works weird in terminal...?!
