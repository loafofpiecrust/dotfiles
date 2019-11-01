
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

;; Colors config (supposedly this should be at the top of the file)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t :background "white" :foreground "black")))
 '(line-number-current-line ((t :background "white" :foreground "black")))
 '(lsp-ui-doc-background ((t :background "black"))))


;; Essential packages
(use-package evil)
(use-package magit)
(use-package forge :after magit)
(use-package yasnippet)
(use-package projectile)
(use-package emojify)


;; mini-buffer tab completion
(use-package ivy)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-re-builders-alist
      '((ivy-switch-buffer . ivy--regex-plus)
        ;; Use fuzzy matching for most cases
        (t . ivy--regex-fuzzy)))
(setq projectile-completion-system 'ivy)


;; code completion and LSP setup
(use-package lsp-mode
  :hook ((go-mode rust-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)) ; language server protocol

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;; auto-complete
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
(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))
(use-package smart-hungry-delete
  :bind (("<backspace>" . smart-hungry-delete-backward-char))
  :defer nil ; dont defer so we can add our functions to hooks 
  :config (smart-hungry-delete-add-default-hooks))

;; Languages
(use-package nix-mode :commands nix-mode)
(use-package bazel-mode :commands bazel-mode)
(use-package yaml-mode :commands yaml-mode)

;; golang
(use-package go-mode :commands go-mode)

(add-hook 'lsp-mode-hook (lambda ()
                           (add-hook 'before-save-hook 'lsp-format-buffer)))

;; Rust
(use-package rust-mode :commands rust-mode)

;; typesetting
(use-package markdown-mode)
;; TODO: latex plugins!

;; ui packages
(use-package diminish) ; hide minor mode lines
(use-package smart-mode-line)
(sml/setup)

;; evil mode by default while I adjust to emacs!
(evil-mode 1)
;;(setq ido-enable-flex-matching t)
;;(setq ido-everywhere 1)
;;(ido-mode 1)


;; Enable spellcheck in comments and strings (requires ispell)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(setq flyspell-issue-message-flag nil)

;; Disable toolbar, I don't use it.
(menu-bar-mode -1)
(setq-default tab-width 4)
(show-paren-mode 1)


;; Backup settings
(setq backup-by-copying t ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.cache/emacs")) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)


;; Enable completion, pair matching, line numbers
(add-hook 'after-init-hook (lambda ()
                             (ivy-mode 1)
                             (electric-pair-mode)
							 (global-display-line-numbers-mode)
                             (global-emojify-mode)
                             (column-number-mode)))


;; Custom key bindings
(global-set-key (kbd "M-SPC") 'company-complete)
;; TODO: Rebind commands for marking??
(global-set-key (kbd "C-s") 'save-buffer)
;; TODO: Rebind isearch-forward
(global-set-key (kbd "C-]") 'tab-to-tab-stop)
;;(global-set-key (kbd "C-<tab>") 'indent-relative) ; TODO: rebind, C-tab doesn't work
;;(global-set-key (kbd "C-_") 'comment-line)
;; rebind undo-tree-undo for undoing stuff!
;; Generally need to pick bindings for undo/redo
;; TODO: Backspace works weird in terminal...?!

(load-theme 'tango-dark)

;;(if (eq (gentenv "TERM") "xt"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))))
