;;; ../../dotfiles/emacs/.config/doom/custom/pretty-which-key.el -*- lexical-binding: t; -*-
;;;
;;; Why this package?
;;; matcha provides a really nice UI for a bunch of modes based on transient.
;;; The core problem in my mind is that to rebind, redescribe, or regroup a
;;; command within a particular transient map you have to use
;;; transient-suffix-put rather than built-in keymap manipulation methods.
;;;
;;; pretty-which-key extends the idea of which-key: to provide a UI showing you
;;; all the *already bound* keys in the current keymap.
;;; pretty-which-key adds the idea of binding groups to arrange bindings in a
;;; similar fashion to transient or hydra.
;;;
;;; The difference is that pretty-which-key is totally additive. You can install
;;; this, make no other code changes, and use the same which-key UI as before
;;; with your simpler keymaps.
;;; For larger keymaps, like a leader key, you can assign groups to each command
;;; separately from the bindings themselves.
;;; Reassigning an existing binding can then happen in any vanilla Emacs
;;; context using =define-key= (if =which-key-enable-extended-define-key= is =t=
;;;before loading which-key):
;;;
;;;     (define-key doom-leader-map (kbd ".") '("Jump to///file" . find-file))
;;;
;;; You may provide only a group but no description (to see "Find File" under
;;; the "Jump to" group):
;;;
;;;     (define-key doom-leader-map (kbd ".") '("Jump to///" . find-file))
;;;
;;; You may also provide only a description with no group:
;;;
;;;     (define-key doom-leader-map (kbd ".") '("Jump to file" . find-file))
;;;
;;; This exposes the internal separator used by pretty-which-key for grouping: "///".
;;; Now, my bindings are decentralized. I don't have to use transient-suffix-put
;;;to modify a specific position in a transient. I can simply use =define-key=
;;;or =general= or DOOM's =map!=.
;;;
;;; Caveat: replacing an existing binding with a different command is easy as
;;;that, but replacing existing bindings with a different *key* is more work than
;;;with transient. You have to unbind the original key and bind your own.
;;; I suspect that this is a somewhat less common case than simply adding your
;;;own bindings.
;;;
;;; With selectrum, consult, and embark coming into view as alternatives to ivy
;;;and helm that are closer to Emacs core and make use of built-in facilities, I
;;;felt that a solution to discoverable command UI using purely vanilla keymaps
;;;was in order.
;;;
;;; Don't get me wrong, transient is amazing! I love using transient with magit
;;;and a few other complex modes, but I'm not sure that I need its power when it
;;;comes to most modes. Plus, =evil-collection= and =doom-emacs= and other
;;;starter kits pride themselves on having community collections of keybindings
;;;that we can put to use directly in pretty UIs instead of fragmenting into a new
;;;set of community binding efforts.

(require 'which-key)
(require 'seq)
(require 's)

;; Change some which-key settings to make things prettier.
(setq which-key-separator " "
      which-key-min-display-lines 7
      which-key-add-column-padding 3)

;; TODO Option to hide uncategorized bindings in a particular map.
;; TODO Combine duplicate entries for the same function with comma separation.

(defconst pretty-which-key--group-separator "//")

(defun pretty-which-key--last-part (binding-desc)
  "Extract the command description part of BINDING-DESC,
which may include a group name at the beginning that will be dropped."
  (car (last (s-split pretty-which-key--group-separator binding-desc))))

;; (defun pretty-which-key-binding-is-grouped (binding)
;;   "Has the given BINDING been assigned an explicit group?"
;;   (let ((desc (cdr-safe binding)))
;;     (and desc (s-contains? pretty-which-key--group-separator desc))))

(defun pretty-which-key--blank-separator ()
  "Returns a string of spaces the same width as WHICH-KEY-SEPARATOR"
  (string-pad "" (string-width which-key-separator)))

(defun pretty-which-key--add-keymap-replacements (keymap key replacement &rest more)
  (let ((current-bindings (which-key--get-keymap-bindings keymap t)))
    (while key
      (let* ((needs-desc (string-suffix-p pretty-which-key--group-separator replacement))
             ;; Keep current command description if none provided and there's an
             ;; existing one in the keymap.
             (command (lookup-key keymap key))
             (existing-binding (cdr-safe (assoc (key-description key) current-bindings)))
             (existing-has-group (and existing-binding (s-contains-p pretty-which-key--group-separator existing-binding)))
             (desc (cond ((and needs-desc existing-has-group)
                          (concat replacement (car (pretty-which-key--split-desc existing-binding))))
                         ((and needs-desc existing-binding)
                          (concat replacement existing-binding))
                         (t replacement))))
        ;; Don't bother with unbound keys.
        (when existing-binding
          (define-key keymap key
            `(,desc . ,command))))
      (setq key (pop more)
            replacement (pop more)))))

(defun pretty-which-key-add-command-replacements (keymap replacements)
  (mapc (lambda (replacement)
          (mapc (lambda (binding) (pretty-which-key--add-keymap-replacements
                                   keymap
                                   binding
                                   (cdr replacement)))
                (where-is-internal (car replacement) keymap)))
        replacements))

(defun pretty-which-key-add-command-groups (keymap groups)
  "Groups are of the form '(GROUP . ((COMMAND . DESCRIPTION)))
Adds an entry of the form '((DESCRIPTION . GROUP) . COMMAND) for each key."
  ;; for each group
  (mapc (lambda (group)
          ;; for each binding in the group
          (mapc (lambda (binding)
                  (let ((command (or (car-safe binding) binding)))
                    (mapc (lambda (kb)
                            (pretty-which-key--add-keymap-replacements
                             keymap
                             ;; command symbol
                             kb
                             (concat (car group) pretty-which-key--group-separator (or (cdr-safe binding) ""))))
                          (if (stringp command)
                              (list (kbd command))
                            (where-is-internal command keymap)))))
                (cdr group)))
        groups))

(setq pretty-which-key-prefixes '("proced-"
                                  "dired-"
                                  "ranger-"
                                  "evil-"
                                  "+evil/"
                                  "+evil:"
                                  "projectile-"
                                  "npm-mode-"
                                  "consult-"
                                  "counsel-"
                                  "doom/"
                                  "doom-"
                                  "which-key-"
                                  "desktop-environment-"
                                  "magit-"
                                  "mu4e-headers-"
                                  "mu4e-"
                                  "mu4e~"
                                  "+mu4e/"
                                  "org-"))


(defun pretty-which-key-strip-prefix (command-name)
  (let ((prefix (seq-find (lambda (s) (string-prefix-p s command-name))
                          pretty-which-key-prefixes)))
    (if prefix
        (s-chop-prefix prefix command-name)
      command-name)))

(defun pretty-which-key-unlispify (symbol)
  (let ((s (string-trim (replace-regexp-in-string "[-:/+]+" " " symbol))))
    (concat (upcase (substring s 0 1)) (substring s 1))))

(defun pretty-which-key-backup-format (kb)
  (cons (car kb)
        ;; Use the built-in conversion from symbol name to pretty string
        ;; provided by "custom".
        (pretty-which-key-unlispify (pretty-which-key-strip-prefix (cdr kb)))))

;; Last resort replacement turning function names into space-delimited strings.
;; Since this is at the end of the list, other replacements take precedent,
;; especially those embedded in the relevant keymap. This just gives a
;; consistent look to commands without manual descriptions.
(add-to-list 'which-key-replacement-alist
             (cons '(nil . "^\\+?\\w+[-/~]")
                   #'pretty-which-key-backup-format)
             t
             (lambda (a b) (equal (car a) (car b))))


(defun pretty-which-key--build-groups (keys)
  "Take a list of key bindings KEYS and return a list of groups
based on group definitions added by pretty-which-key-add-command-groups"
  (cl-sort (seq-group-by (lambda (kb)
                           (let* ((command (last kb))
                                  (str (if (consp command) (car command) command)))
                             (get-text-property 0 'pretty-which-key-group str)))
                         (cl-reduce (lambda (a b) (let ((matching-bind (cl-find-if (lambda (x) (equal (cdr-safe x) (cdr-safe b)))
                                                                                   a)))
                                                    (cond
                                                     ;; Unlabeled prefix keys
                                                     ;; should not be grouped.
                                                     ((and (listp b)
                                                           (equal (nth 2 b) "+prefix"))
                                                      (cons b a))
                                                     ;; Don't add the same key twice.
                                                     ((and (car-safe b)
                                                           matching-bind
                                                           (or (equal (car b) (car matching-bind))
                                                               (s-contains-p (format ",%s" (car b)) (car matching-bind))))
                                                      a)
                                                     ;; Group keys bound to the
                                                     ;; same command.
                                                     (matching-bind
                                                      (setf (car matching-bind) (format "%s,%s" (car matching-bind) (car-safe b)))
                                                      a)
                                                     ;; Otherwise, just add the binding.
                                                     (t (cons b a)))))
                                    keys
                                    :initial-value nil))
           #'string<
           :key #'car-safe))

(defun pretty-which-key--partition-list (orig-fun n list)
  "Partition LIST into N-sized sublists."
  (let ((grouped-keys (pretty-which-key--build-groups list))
        (blank-sep (pretty-which-key--blank-separator)))
    ;; If all bindings are uncategorized, then don't show any category names.
    (if (and (= 1 (length grouped-keys))
             (null (caar grouped-keys)))
        (apply orig-fun (list n (cdar grouped-keys)))

      ;; Otherwise, if there are any explicit categories then show those as title
      ;; headings. Uncategorized bindings are in the last section.
      (let* ((content-n (- n 1))
             (inner-mapper (lambda (sublist) (cons `("" ,blank-sep "") (if (length< sublist content-n)
                                                                           (append sublist (make-list (- content-n (length sublist)) `("" ,blank-sep "")))
                                                                         sublist)))))
        (mapcan (lambda (x)
                  ;; Ensure that each sublist is the max length.
                  (let ((sublists (mapcar inner-mapper
                                          (apply orig-fun (list content-n (cdr-safe x))))))
                    (setf (caar sublists) `("" ,blank-sep ,(or (car-safe x) "Other")))
                    sublists))
                grouped-keys)))))

(advice-add 'which-key--partition-list :around #'pretty-which-key--partition-list)


(defun pretty-which-key--split-desc (binding-desc)
  "Split an internal key replacement into its constituent parts of
a binding group and command description. This should only apply to
key replacements bound by pretty-which-key functions."
  (cond
   ;; If the replacement contains our obscure separator, then it should be
   ;; treated as a group + description combination.
   ((and binding-desc (s-contains-p pretty-which-key--group-separator
                                    binding-desc))
    (reverse (mapcar (lambda (x) (and (not (string-empty-p x)) x))
                     (split-string (s-chop-prefix "group:" binding-desc) pretty-which-key--group-separator))))
   ;; Otherwise, it must be a plain description with no group.
   ;; In that case, just return the binding as-is.
   (binding-desc (list binding-desc))
   (t nil)))

;; Add the corresponding group name to each binding, if any.
(defun pretty-which-key--format-and-replace (orig-fn unformatted &optional prefix preserve-full-key)
  "Take a list of (key . desc) cons cells in UNFORMATTED, add
faces and perform replacements according to the three replacement
alists. Returns a list (key separator description)."
  (let* ((sep-w-face
          (which-key--propertize which-key-separator
                                 'face 'which-key-separator-face))
         (local-map (current-local-map))
         new-list)
    (dolist (key-binding unformatted)
      (let* ((key (car key-binding))
             (orig-desc (cdr key-binding))
             (group (and (stringp orig-desc)
                         (which-key--group-p orig-desc)))
             ;; Split the description into '(name group)
             (key-binding-parts (and (stringp orig-desc) (pretty-which-key--split-desc orig-desc)))
             (binding-group (and key-binding-parts (nth 1 key-binding-parts)))
             (ungrouped-desc (or (car-safe key-binding-parts) orig-desc))
             ;; Check whether the description is a prefix or plain command name.
             (orig-desc-sym (and (stringp ungrouped-desc)
                                 (intern ungrouped-desc)))
             (is-command-name (and orig-desc-sym (fboundp orig-desc-sym)))
             ;; At top-level prefix is nil
             (keys (if prefix
                       (concat (key-description prefix) " " key)
                     key))
             (local (eq (which-key--safe-lookup-key local-map (kbd keys))
                        orig-desc-sym))
             (hl-face (which-key--highlight-face orig-desc))
             (key-binding-so-far (cons keys ungrouped-desc))
             ;; If there's only a group name in the pseudo-binding, then allow
             ;; further replacements to fill in the command description.
             (key-binding (if (or is-command-name group)
                              (which-key--maybe-replace key-binding-so-far prefix)
                            key-binding-so-far))
             (key-binding-desc (cdr key-binding))
             (final-desc (which-key--propertize-description
                          key-binding-desc group local hl-face orig-desc)))
        (when (and final-desc (not (string-empty-p final-desc)))
          (setq final-desc
                (which-key--truncate-description
                 (which-key--maybe-add-docstring final-desc orig-desc)))
          ;; ADDED
          (when binding-group
            (add-text-properties 0 (length final-desc)
                                 `(pretty-which-key-group ,binding-group)
                                 final-desc)))

        (when (and (consp key-binding) (stringp final-desc))
          (push
           (list (which-key--propertize-key
                  (if preserve-full-key
                      (car key-binding)
                    (which-key--extract-key (car key-binding))))
                 sep-w-face
                 final-desc)
           new-list))))
    (nreverse new-list)))

(advice-add 'which-key--format-and-replace :around #'pretty-which-key--format-and-replace)

;; (which-key--get-current-bindings (kbd "SPC"))

(provide 'pretty-which-key)
