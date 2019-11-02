
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

;; General config
(menu-bar-mode -1)
(setq-default tab-width 4)
(show-paren-mode 1)
;; Custom theme to use terminal colors best
(load-theme 'terminal-wal t)

;; Make sure to use :commands and :hook to defer package load until its used
;; This is mainly relevant for language packages, where we don't need them all at once.

;; ui packages
(use-package diminish) ; hide minor mode lines
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; Editing
;; evil mode by default while I adjust to emacs!
(use-package evil
  :hook (after-init . evil-mode))
(setq-default indent-tabs-mode nil) ; spaces by default
(cua-mode t) ; normal copy-paste bindings
(use-package dtrt-indent) ; auto-detect indentation
(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))
(use-package whole-line-or-region)


;; Essential packages
(use-package yasnippet)
(use-package projectile)
(use-package emojify
  :hook (after-init . global-emojify-mode))
(use-package flycheck
  :hook (after-init . global-flycheck-mode))


;; mini-buffer tab completion
(use-package ivy :hook (after-init . ivy-mode))
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-re-builders-alist
      '((ivy-switch-buffer . ivy--regex-plus)
        ;; Use fuzzy matching for most cases
        (t . ivy--regex-fuzzy)))
(setq projectile-completion-system 'ivy)

;; in-buffer auto-complete
(use-package company
  :hook (prog-mode . company-mode))
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)
(setq company-require-match -1)

;; show docs in popup!
(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

;; code completion and LSP setup
(use-package lsp-mode
  :hook (((go-mode rust-mode) . lsp-deferred)
         ;; Format code on save
         (lsp-mode . (lambda ()
                       (add-hook 'before-save-hook 'lsp-format-buffer))))
  :commands (lsp lsp-deferred)) ; language server protocol

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp)
(push 'company-lsp company-backends)


;; version control
(use-package magit)
(use-package forge :after magit)
;; Show changed lines in the margin
(use-package diff-hl
  :hook ((after-init . (lambda ()
                         (global-diff-hl-mode)
                         (diff-hl-margin-mode)))
         (magit-post-refresh . diff-hl-magit-post-refresh)))


(setq frame-background-mode 'dark)

;; Enable spellcheck in comments and strings (requires ispell)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(setq flyspell-issue-message-flag nil)


;; Backup settings
(setq backup-by-copying t ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.cache/emacs")) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)


;; Enable completion, pair matching, line numbers
(add-hook 'after-init-hook (lambda ()
                             (electric-pair-mode)
							 (global-display-line-numbers-mode)
                             (column-number-mode)))

(use-package smart-hungry-delete)
(smart-hungry-delete-add-default-hooks)

;; Custom key bindings
(global-set-key (kbd "M-SPC") 'company-complete)
(global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)
(global-set-key (kbd "C-s") 'save-buffer)
;; TODO: Rebind isearch-forward
(global-set-key (kbd "C-]") 'tab-to-tab-stop)
;; rebind undo-tree-undo for undoing stuff!
;; Generally need to pick bindings for undo/redo
;; TODO: Backspace works weird in terminal...?!


;; Languages
(use-package nix-mode :commands nix-mode)
(use-package bazel-mode :commands bazel-mode)
(use-package yaml-mode :commands yaml-mode)
(use-package json-mode :commands json-mode)
(use-package go-mode :commands go-mode)
(use-package rust-mode :commands rust-mode)

(use-package rainbow-mode
  :hook (after-init . 'rainbow-mode))

;; javascript and typescript
(defun custom-tide-setup ()
  (tide-setup)
  (tide-hl-identifier-mode 1))

(use-package tide
 :hook ((typescript-mode . custom-tide-setup)
        (before-save . tide-format-before-save)))

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (let ((ext (file-name-extension buffer-file-name)))
              (when (or (string-equal ext "tsx")
                        (string-equal ext "jsx"))
                (custom-tide-setup)))))

;; Use eslint for js/ts
;; FIXME!
;; (flycheck-disable-checker 'typescript-tslint)
;;(flycheck-disable-checker 'javascript-jshint)
;;(flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-mode 'javascript-eslint 'typescript-mode)


;; typesetting
(use-package markdown-mode :commands markdown-mode)
(use-package poly-markdown :after markdown-mode)
;; TODO: latex plugins!


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("eabaa2ba26896ab0253f87c1a3ba62fe137a44f22965ccd04f89644bead32e75" "4f87a907299c237ec58c634647b44aca5ee636fb7861da19a9defa0b0658b26e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
