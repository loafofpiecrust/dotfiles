(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval cl-flet
           ((enhance-imenu-lisp
             (&rest keywords)
             (dolist
                 (keyword keywords)
               (add-to-list 'lisp-imenu-generic-expression
                            (list
                             (purecopy
                              (concat
                               (capitalize keyword)
                               (if
                                   (string=
                                    (substring-no-properties keyword -1)
                                    "s")
                                   "es" "s")))
                             (purecopy
                              (concat "^\\s-*("
                                      (regexp-opt
                                       (list
                                        (concat "define-" keyword))
                                       t)
                                      "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                             2)))))
           (enhance-imenu-lisp "bookmarklet-command" "class" "command" "ffi-method" "function" "mode" "parenscript" "user-class"))))
 '(warning-suppress-types '((comp) (emacs) (lsp-mode) (:warning))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-spc-face ((t (:inherit nil))))
 '(fixed-pitch-serif ((t (:family nil))))
 '(header-line ((t (:inherit mode-line))))
 '(line-number ((t (:foreground nil :inherit org-tag))))
 '(markdown-header-face-1 ((t (:inherit outline-1))))
 '(markdown-header-face-2 ((t (:inherit outline-2))))
 '(markdown-header-face-3 ((t (:inherit outline-3))))
 '(markdown-header-face-4 ((t (:inherit outline-4))))
 '(markdown-header-face-5 ((t (:inherit outline-5))))
 '(minibuffer-prompt ((t (:family nil))))
 '(mixed-pitch-variable-pitch ((t (:family "Times New Roman" :height 1.25))))
 '(org-document-title ((t (:weight extra-bold :height 1.5))))
 '(org-level-1 ((t (:inherit outline-1))))
 '(org-level-2 ((t (:inherit outline-2))))
 '(org-level-3 ((t (:inherit outline-3))))
 '(org-level-4 ((t (:inherit outline-4))))
 '(org-level-5 ((t (:inherit outline-5))))
 '(org-level-6 ((t (:inherit outline-6))))
 '(outline-1 ((t (:weight extra-bold :height 1.3))))
 '(outline-2 ((t (:weight bold :height 1.2))))
 '(outline-3 ((t (:weight bold :height 1.1))))
 '(outline-4 ((t (:weight semi-bold :height 1.08))))
 '(outline-5 ((t (:weight semi-bold :height 1.06))))
 '(outline-6 ((t (:weight semi-bold :height 1.03))))
 '(outline-7 ((t (:weight semi-bold))))
 '(outline-8 ((t (:weight semi-bold))))
 '(outline-9 ((t (:weight semi-bold)))))
