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

;; Essential packages
;;(use-package helm) ; narrowing completion?
(use-package evil)
(use-package magit)
(use-package yasnippet)

;; Completion and LSP setup
(use-package lsp-mode
  :commands (lsp lsp-deferred)) ; language server protocol
(use-package lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(use-package company)
(use-package pos-tip)
(use-package company-quickhelp) ; show docs in popup!
(company-quickhelp-mode)

(use-package company-lsp)
(push 'company-lsp company-backends)

;; Editing
(use-package dtrt-indent) ; auto-detect indentation
(use-package move-text)
(use-package smart-hungry-delete
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
	 ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ; dont defer so we can add our functions to hooks 
  :config (smart-hungry-delete-add-default-hooks))

;; Languages
(use-package nix-mode)
(use-package bazel-mode)

;; golang
(use-package go-mode)
(add-hook 'go-mode-hook 'lsp-deferred) ; language server
(add-hook 'before-save-hook 'gofmt-before-save)

;; Rust
(use-package rustic) 

;; evil mode by default while I adjust to emacs!
(evil-mode 1)

;; Disable toolbar, I don't use it.
(menu-bar-mode -1)
(setq-default tab-width 4)
(show-paren-mode 1)

;; Enable completion, pair matching, line numbers
(add-hook 'after-init-hook (lambda ()
							 (global-company-mode)
							 (electric-pair-mode)
							 (global-display-line-numbers-mode)
							 (column-number-mode)))


;; Custom key bindings
;;(global-set-key (kbd "C-SPC") 'company-complete)
;; TODO: Rebind commands for marking??
(global-set-key (kbd "C-s") 'save-buffer)
;; TODO: Rebind isearch-forward
(global-set-key (kbd "C-]") 'tab-to-tab-stop)
(global-set-key (kbd "C-<tab>") 'indent-relative)
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "C-_") 'comment-line)
;; rebind undo-tree-undo for undoing stuff!
;; Generally need to pick bindings for undo/redo
(global-set-key (kbd "C-h <backspace>") 'kill-whole-line)
;; TODO: Backspace works weird in terminal...?!
