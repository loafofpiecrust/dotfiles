;;; ~/dotfiles/emacs/.config/doom/custom/md-msg.el -*- lexical-binding: t; -*-
;;; md-msg.el --- Org mode to send and reply to email in HTML. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Taylor Snead

;; Author: Taylor Snead <taylorsnead@gmail.com>
;; Created: August 2020
;; Keywords: extensions mail
;; Homepage: https://github.com/loafofpiecrust/md-msg
;; Package-Version: 2.8
;; Package-Requires: ((emacs "24.4") (htmlize "1.54"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; OrgMsg is a GNU/Emacs global minor mode mixing up Org mode and your
;; Mail User Agent Mode to compose and reply to emails in a HTML
;; friendly style.

;; * Presentation

;; By default, if the original message is in text form OrgMsg keeps it
;; that way and does not activate itself.  It allows to reply to
;; developer mailing list seamlessly.  If the original message is in
;; the HTML form, it activates the OrgMsg mode on the reply buffer.

;; OrgMsg provides a `md-msg-edit-mode' which is an derivation of Org
;; mode in which some functionality of your Mail User Agent are
;; imported or replicated.  For instance, a OrgMsg buffer uses the
;; same `font-lock-keywords' than Message mode or the `TAB' key while
;; the cursor is in the header calls the `message-tab' function.

;; For convenience, the original message is quoted below the
;; `--citation follows this line (read-only)--' marker.  So you can
;; easily refer to the original message.  However, the entire quoted
;; text is read-only because OrgMsg does not support modification of
;; the original content.

;; OrgMsg has a mechanism to support different Mail User Agents
;; (message, mu4e, notmuch ...).  Each function which depends on the Mail User
;; Agent calls the `md-msg-mua-call' which is an indirection to the
;; OrgMsg Mail User Agent specific function.

;; * Keys and interactive functions

;; The OrgMsg mode keys are the usual key combination used in either
;; Org mode or Message mode.
;; - C-c C-e - calls `md-msg-preview', it generates the final HTML
;;   email, save it into a temporary file and call the `browse-url'
;;   function on that file.
;; - C-c C-k - calls `message-kill-buffer'
;; - C-c C-s - calls `message-goto-subject' (same as in Message
;;   mode)
;; - C-c C-b - calls `md-msg-goto-body' (similar to
;;   `message-goto-body' in Message mode)
;; - C-c C-a - calls `md-msg-attach', very similar to the
;;   `org-attach' function.  It lets you add or delete attachment for
;;   this email.  Attachment list is stored in the `:attachment:'
;;   property.
;; - C-c C-c - calls `org-ctrl-c-ctrl-c'.  OrgMsg configures
;;   `md-msg-ctrl-c-ctrl-c' as a final hook of Org mode.  When
;;   `md-msg-ctrl-c-ctrl-c' is called in a OrgMsg buffer it
;;   generates the MIME message and send it.

;; The `md-msg-mode' interactive function can be called to
;; enable/disable OrgMsg.  By default, once the module is loaded, it
;; is disabled.  If you want to reply to an email without making use
;; of OrgMsg, you should call that function before you call the
;; reply-to function.

;; To start composing a new OrgMsg email, you can call the interactive
;; `message-mail' function.  If your `mail-user-agent' is
;; `message-user-agent' (which is the by default Emacs configuration),
;; `compose-mail' calls `message-mail' and is bound to [C-x m] by
;; default.

;; * Configuration

;; The following is my configuration as an Example

;; (require 'md-msg)
;; (setq md-msg-greeting-fmt "\nHi %s,\n\n")
;; (setq md-msg-greeting-fmt-mailto t)
;; (setq md-msg-signature "
;;
;; Regards,
;;
;; #+begin_signature
;; -- *Jeremy* \\\\
;; /One Emacs to rule them all/
;; #+end_signature")
;; (md-msg-mode)

;; The `md-msg-greeting-fmt' can be customized to configure the
;; default greeting message.  If this format contains a `%s' token it
;; is automatically replaced with the first name of the person you are
;; replying to.  If `md-msg-greeting-fmt-mailto' is t, the first name
;; it is formatted as mailto link.

;; In order to avoid CSS conflict, OrgMsg performs inline replacement
;; when it generates the final HTML message.  See the
;; `md-msg-enforce-css' variable to customize the style (and the
;; default `md-msg-default-style' variable for reference).

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'cl-seq)
(require 'gnus-art)
(require 'gnus-msg)
(require 'htmlize)
(require 'message)
(require 'mml)
(require 'markdown-mode)
(require 'url-parse)
(require 'xml)
(require 'mu4e)

(defgroup md-msg nil
  "Org Message group."
  :group 'applications)

(defvar md-msg-text-plain nil
  "Temporary variable to pass the text/plain version of the email.")

(defvar md-msg-export-in-progress nil
  "Internal use only.
It is used by function advice.")

(defcustom md-msg-separator "--citation follows this line (read-only)--"
  "String separating the reply area and the original mail."
  :type '(string))

(defcustom md-msg-text-plain-alternative t
  "Include an ASCII export as a text/plain alternative.")

(defcustom md-msg-greeting-fmt nil
  "Mail greeting format.
If it contains a '%s' format, '%s' is replaced with the first
name of the person you are replying to.

Example: \"\nHi %s,\n\n\""
  :type '(string))

(defcustom md-msg-greeting-name-limit 1
  "Maximum number of recipient first name for the greeting format.
If replying to an email for which the 'To' field contains more
than one recipient and the `md-msg-greeting-fmt' contains a '%s'
format, this variable limits the number of recipient first name
used as a replacement of the '%s' format.  nil means unlimited."
  :type '(integer))

(defcustom md-msg-greeting-fmt-mailto nil
  "Define the format behavior for recipient greeting.
If t and `md-msg-greeting-fmt' contains a '%s' the first name is
formatted as a mailto link."
  :type '(boolean))

(defcustom md-msg-signature nil
  "Mail signature string appended if not nil.
The part in the signature block gets applied the \"signature\"
CSS style.

Example:
\"\n\nRegards,\n\n#+begin_signature\n-- *Your name*\n#+end_signature\""
  :type '(string))

(defconst md-msg-default-style
  (let* ((font-size '(font-size . "10pt"))
         (line-height '(line-height . "10pt"))
         (bold '(font-weight . "bold"))
         (theme-color "#0071c5")
         (color `(color . ,theme-color))
         (table `((margin-top . "0px")))
         (ftl-number `(,color ,bold (text-align . "left")))
         (inline-modes '(asl c c++ conf cpp csv diff ditaa emacs-lisp
                             fundamental ini json makefile man org plantuml
                             python sh xml))
         (inline-src `((color . ,(face-foreground 'default))
                       (background-color . ,(face-background 'default))))
         (code-src
          (mapcar (lambda (mode)
                    `(code ,(intern (concat "src src-" (symbol-name mode)))
                           ,inline-src))
                  inline-modes)))
    `(;; (del nil ((color . "grey") (border-left . "none")
      ;;           (text-decoration . "line-through") (margin-bottom . "0px")
      ;;           (margin-top . "10px") (line-height . "11pt")))
      ;; (a nil (,color))
      (a reply-header ((color . "black") (text-decoration . "none")))
      (div reply-header ((padding . "3.0pt 0in 0in 0in")
                         (border-top . "solid #e1e1e1 1.0pt")
                         (margin-bottom . "20px")))
      (span underline ((text-decoration . "underline")))
      (nil signature ((margin-bottom . "20px")))
      ;; (blockquote nil ((margin . "0px 0px 0px 0.8ex")
      ;;                  (padding-left . "1ex")
      ;;                  (border-left . "1px solid rgb(204,204,204)")))
      (code nil (,font-size (font-family . "monospace")))
      ,@code-src
      (nil linenr ((padding-right . "1em")
                   (color . "black")
                   (background-color . "#aaaaaa")))
      (pre nil ((line-height . "12pt")
                ,@inline-src
                (margin . "0px")
                (font-size . "9pt")
                (font-family . "monospace")))
      (nil figure-number ,ftl-number)
      ;; (nil table-number)
      (caption nil ((text-align . "left")
                    ,bold))
      (nil t-above ((caption-side . "top")))
      (nil t-bottom ((caption-side . "bottom")))
      (nil listing-number ,ftl-number)
      (nil figure ,ftl-number)

      (table nil (,@table ,line-height (border-collapse . "collapse")))
      (th nil ((border . "1px solid white")
               (background-color . ,theme-color)
               (color . "white")
               (padding-left . "10px") (padding-right . "10px")))
      (td nil (,@table (padding-left . "10px") (padding-right . "10px")
                       (background-color . "#f9f9f9") (border . "1px solid white")))
      (td org-left ((text-align . "left")))
      (td org-right ((text-align . "right")))
      (td org-center ((text-align . "center")))

      ;; (div outline-text-4 ((margin-left . "15px")))
      ;; (div outline-4 ((margin-left . "10px")))
      (h4 nil ((margin-bottom . "0px")))
      (h3 nil ((margin-bottom . "0px")))
      (h2 nil ((margin-top . "20px") (margin-bottom . "5px")))
      (h1 nil ((margin-top . "20px")
               (margin-bottom . "5px"))))))

(defcustom md-msg-enforce-css md-msg-default-style
  "Define how to handle CSS style:
- list - style definition: see `md-msg-default-style' for
  example.
- string - path to a CSS file: same as t but use this file
  definitions."
  :type '(choice (file :must-match t)
                 (list (list symbol symbol
                             (alist :value-type string)))))

(defcustom md-msg-reply-header-class 'reply-header
  "Default CSS class for reply header tags."
  :type '(symbol))

(defcustom md-msg-supported-mua '((gnus-user-agent . "gnus")
                                  (message-user-agent . "gnus")
                                  (mu4e-user-agent . "mu4e")
                                  (notmuch-user-agent . "notmuch"))
  "Supported Mail User Agents."
  :type '(alist :value-type string))

(defun md-msg-mua-call (sym &rest arg)
  "Call the specific MUA function for SYM with ARG parameters."
  (let ((mua (assoc-default mail-user-agent md-msg-supported-mua)))
    (if mua
        (let ((fun (intern (format "md-msg-%s-%s" sym mua))))
          (when (functionp fun)
            (apply fun arg)))
      (error "Backend not found"))))

(defun md-msg-mml-recursive-support ()
  (fboundp 'mml-expand-all-html-into-multipart-related))

;; Works perfect!
(defun md-msg-save-article-for-reply-mu4e ()
  "Export the currently visited mu4e article as HTML."
  (let* ((msg mu4e-compose-parent-message)
         (html (mu4e-message-field msg :body-html))
         (file (concat "/tmp/" (mu4e-message-field msg :message-id))))
    (cl-flet* ((mails2str (l)
                          (mapconcat (lambda (m)
                                       (format "%S &lt;%s&gt;" (car m) (cdr m)))
                                     l ", "))
               (field2str (f)
                          (let ((value (funcall (cdr f)
                                                (mu4e-message-field msg (car f)))))
                            (when value
                              (format "%s: %s<br>\n"
                                      (capitalize (substring (symbol-name (car f)) 1))
                                      value)))))
      (with-temp-buffer
        (save-excursion
          (insert html))
        ;; Remove everything before html tag
        (save-excursion
          (if (re-search-forward "^<html\\(.*?\\)>" nil t)
              (delete-region (point-min) (match-beginning 0))
            ;; Handle malformed HTML
            (insert "<html><body>")
            (goto-char (point-max))
            (insert "</body></html>")))
        ;; Insert reply header after body tag
        ;; (when (re-search-forward "<body\\(.*?\\)>" nil t)
        ;;   (goto-char (match-end 0))
        ;;   (insert "<div>\n"
        ;;           (mapconcat #'field2str
        ;;                      `((:from . ,#'mails2str)
        ;;                        (:subject . identity)
        ;;                        (:to . ,#'mails2str)
        ;;                        (:cc . ,#'mails2str)
        ;;                        (:date . message-make-date))
        ;;                      "")
        ;;           "</div>\n<hr>\n"))
        (write-file file))
      (list file))))

(defun md-msg-attrs-str (attr)
  "Convert ATTR list of attributes into a string."
  (cl-flet ((attr-str (x)
                      (concat " " (symbol-name (car x)) "=\""
                              (xml-escape-string (cdr x)) "\"")))
    (if attr
        (apply 'concat (mapcar #'attr-str attr))
      "")))

(defun md-msg-xml-escape-string (string)
  "Convert STRING into a string containing valid XML character data.
This is a reduction of `xml-escape-string' to work-around a bug
during email generation where '&apos;' is turned into
'&amp;apos;'."
  (with-temp-buffer
    (insert string)
    (dolist (substitution '(("&" . "&amp;")
                            ("<" . "&lt;")
                            (">" . "&gt;")
                            ("\"" . "&quot;")))
      (goto-char (point-min))
      (while (search-forward (car substitution) nil t)
        (replace-match (cdr substitution) t t nil)))
    (buffer-string)))

(defun md-msg-xml-to-str (xml)
  "Convert the XML tree into a HTML string."
  (cond ((and (listp xml) (equal xml '(p nil " ")))
         "<o:p>&nbsp;</o:p>")
        ((and (listp xml) (equal xml '(p nil)))
         "<o:p>\n</o:p>")
        ((stringp xml) (md-msg-xml-escape-string xml))
        ((eq (car xml) 'comment)
         (format "<!--%s-->" (caddr xml)))
        ((eq (car xml) 'style)
         (format "<style>%s</style>" (caddr xml)))
        ((cddr xml)
         (format "<%s%s>%s</%s>"
                 (symbol-name (car xml))
                 (md-msg-attrs-str (cadr xml))
                 (apply 'concat (mapcar 'md-msg-xml-to-str (cddr xml)))
                 (symbol-name (car xml))))
        ((format "<%s%s/>" (symbol-name (car xml))
                 (md-msg-attrs-str (cadr xml))))))

(defun md-msg-css-to-list ()
  "Convert the current buffer CSS content into a list.
\((tag class ((prop1 . val1) ...)) ...)."
  (let ((l))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\\([a-zA-Z0-9, \-\\\._]+\\\) *{" nil t)
        (let ((selectors (split-string (match-string 1) "," nil " +"))
              (start (point))
              (props '()))
          (backward-char 1)
          (forward-sexp)
          (let ((text-props (buffer-substring start (1- (point)))))
            (dolist (p (split-string text-props ";" t "[\n ]*"))
              (cl-multiple-value-bind (prop val) (split-string p ":" t "[\n ]*")
                (push (cons (intern prop) val) props)))
            (dolist (sel selectors)
              (cl-multiple-value-bind (tag class) (split-string sel "\\\.")
                (push (list (if (string= tag "") nil (intern tag))
                            (if (stringp class) (intern class) nil)
                            props)
                      l)))))))
    l))

(defun md-msg-css-file-to-list (file)
  "Convert FILE CSS content into a list representation.
See `md-msg-css-to-list'."
  (with-temp-buffer
    (insert-file-contents file)
    (md-msg-css-to-list)))

(defun md-msg-props-to-style (props)
  "Convert PROPS properties to a CSS style string."
  (cl-flet ((css-str (css)
                     (concat (symbol-name (car css)) ":"
                             (cdr css) ";")))
    (apply 'concat (mapcar #'css-str props))))

(defun md-msg-build-style (tag class css)
  "Given a TAG and CLASS selector, it builds a CSS style string.
This string can be used as a HTML style attribute value."
  (cl-flet ((css-match-p (css)
                         (or (and (eq tag (car css))
                                  (eq class (cadr css)))
                             (and (not (car css))
                                  (eq class (cadr css)))
                             (and (not (cadr css))
                                  (eq tag (car css))))))
    (let* ((sel (cl-remove-if-not #'css-match-p css))
           (props (apply 'append (mapcar 'caddr sel))))
      (when props
        (md-msg-props-to-style props)))))

;; TODO: Make use of `mail-extract-address-components' from 'mail-extr
;; package
(defun md-msg-str-to-mailto (str css)
  "Convert a string of mail addresses into mailto anchor links.
Takes a string STR as a parameter and build a list of string and
mailto anchor link.  If a CSS style list is provided and a 'a
selectors on class `md-msg-reply-header-class', it sets the
style mailto anchor link style appropriately."
  (with-temp-buffer
    (insert str)
    (let ((name-regexp "\\\([a-zA-Z\"][0-9a-zA-Z ,\"\(\)@\./\-]+\\\)")
          (mail-regexp "<\\\([A-Za-z0-9@\.]+\\\)>")
          (cursor (goto-char (point-min)))
          (style (md-msg-build-style 'a md-msg-reply-header-class css))
          (res))
      (while (re-search-forward (concat name-regexp " " mail-regexp) nil t)
        (unless (= (match-beginning 0) cursor)
          (push (buffer-substring cursor (match-beginning 0))
                res)
          (setq cursor (match-end 0)))
        (let ((anchor `(a ((href . ,(concat "mailto:" (match-string 0))))
                          ,(delete ?\" (match-string 1)))))
          (when style
            (push `(style . ,style) (cadr anchor)))
          (push anchor res)))
      (nreverse res))))

(defmacro md-msg-list-foreach (spec &rest body)
  "Loop over a list.
Evaluate BODY with VAR bound to each cons from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

\(fn (VAR LIST) BODY...)"
  (declare (indent 1))
  `(let ((,(car spec) ,(cadr spec)))
     (while ,(car spec)
       ,@body
       (let ((temp ,(car spec)))
         (setq ,(car spec) (cdr temp))))))

(defun md-msg-improve-reply-header (xml css)
  "Aesthetically improve the reply header.
The reply header (From, Subject, Date, ...) generated by
`gnus-article-browse-html-article' does not look very nice.  XML
is the XML tree and CSS the style."
  (let ((div (assq 'div (assq 'body xml))))
    ;; Delete unnecessary line break
    (let ((e (cdr div)))
      (while e
        (if (and (stringp (car e))
                 (eq (cl-caadr e) 'br)
                 (and (stringp (caddr e))
                      (string-prefix-p "\n " (caddr e))))
            (progn
              (setcar e (replace-regexp-in-string "\n +" " "
                                                  (concat (car e) (cl-caddr e))))
              (setcdr e (cl-cdddr e)))
          (setf e (cdr e)))))
    ;; Add a bold property to the prefixes like "From", "Date",
    ;; "Subject", ...
    (md-msg-list-foreach (e (cdr div))
      (when (stringp (cadr e))
        (let ((prefix (car (split-string (cadr e) ":"))))
          (setcar (cdr e) (replace-regexp-in-string prefix "" (cadr e)))
          (setcdr e (cons `(b nil ,(capitalize prefix)) (cdr e)))
          (setf e (cdr e)))))
    ;; Transform mail addresses into "mailto" links
    (md-msg-list-foreach (e (cdr div))
      (when (stringp (cadr e))
        (let ((mailto (md-msg-str-to-mailto (cadr e) css)))
          (when mailto
            (setf mailto (append mailto (cddr e)))
            (setcdr e mailto)))))
    (when css
      (assq-delete-all 'hr (assq 'body xml))
      (assq-delete-all 'align (cadr div))
      (setf (cadr div) (assq-delete-all 'style (cadr div)))
      (let ((div-style (md-msg-build-style 'div
                                           md-msg-reply-header-class css))
            (p-style (md-msg-build-style 'p md-msg-reply-header-class css)))
        (when div-style
          (push `(style . ,div-style) (cadr div)))
        (when p-style
          (setf (cddr div) `((p ((style . ,p-style)) ,@(cddr div)))))))))

(defun md-msg-xml-walk (xml fun)
  "Recursively walk a XML tree and call FUN on each node."
  (when (listp xml)
    (funcall fun xml)
    (dolist (e (cddr xml))
      (md-msg-xml-walk e fun))))

(defun md-msg-html-buffer-to-xml (&optional base)
  "Return the XML tree of the current HTML buffer.
BASE is the path used to convert the IMG SRC relative paths to
absolute paths.  Base is also used to locate SVG objects tag file
and include the SVG content into the email XML tree."
  (let ((dirs (list base (temporary-file-directory))))
    (cl-flet* ((get-file-path (file)
                              (let ((paths (mapcar* 'concat dirs
                                                    (make-list (length dirs) file))))
                                (car (delete-if-not 'file-exists-p paths))))
               (make-img-abs (xml)
                             (when (eq (car xml) 'img)
                               (let ((src (assq 'src (cadr xml))))
                                 (unless (url-type (url-generic-parse-url (cdr src)))
                                   (when src
                                     (unless (file-name-absolute-p (cdr src))
                                       (let* ((file (cdr src))
                                              (path (get-file-path file)))
                                         (if path
                                             (setcdr src path)
                                           (unless (y-or-n-p (format "'%s' Image is missing,\
 do you want to continue ?" file))
                                             (error "'%s' Image is missing" file))))))))))
               (inline-svg (xml)
                           (when (and (eq (car xml) 'object)
                                      (string= (cdr (assq 'type (cadr xml)))
                                               "image/svg+xml"))
                             (let ((file (get-file-path (assoc-default 'data (cadr xml)))))
                               (when file
                                 (let ((svg (with-temp-buffer
                                              (insert-file file)
                                              (when (search-forward "<svg " nil t)
                                                (libxml-parse-xml-region (match-beginning 0)
                                                                         (point-max))))))
                                   (setcar xml (car svg))
                                   (setcdr xml (cdr svg))))))))
      (let ((xml (libxml-parse-html-region (point-min) (point-max) base)))
        (when base
          ;; (md-msg-xml-walk xml #'make-img-abs)
          (md-msg-xml-walk xml #'inline-svg))
        (assq-delete-all 'title (assq 'head xml))
        xml))))

(defun md-msg-load-html-file (file)
  "Return the XML tree of a HTML FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (md-msg-html-buffer-to-xml (file-name-directory file))))

(defun md-msg-markdown-to-xml (str &optional base)
  "Transform the STR Markdown string into a XML tree.
BASE is the path used to convert the IMG SRC relative paths to
absolute paths."
  (save-window-excursion
    (with-temp-buffer
      (insert str)
      (let ((md-msg-export-in-progress t))
        ;; Replace the contents of this temporary buffer with the html output.
        (shell-command-on-region (point-min) (point-max)
                                 ;; TODO use markdown here, not gfm.
                                 "pandoc -f gfm -t html --wrap=preserve --columns=80"
                                 nil t)
        (let ((xml (md-msg-html-buffer-to-xml base)))
          (kill-buffer)
          xml)))))

(defun md-msg-markdown-to-text-plain ()
  "Transform the current Md-Msg buffer into a text plain form."
  ;; (let ((end (md-msg-end)))
  ;;   ;; Delete the citation separator.
  ;;   (md-msg-delete-separator)
  ;;   )
  (buffer-substring-no-properties (md-msg-start) (md-msg-end))
  ;; (save-window-excursion
  ;;   (let ((str ))
  ;;     str))
  )

(defun md-msg-load-css ()
  "Load the CSS definition according to `md-msg-enforce-css'."
  (cond ((listp md-msg-enforce-css) md-msg-enforce-css)
        ((stringp md-msg-enforce-css)
         (md-msg-css-file-to-list md-msg-enforce-css))))

(defmacro md-msg-with-match-prop (prop &rest body)
  "Look for the markdown PROP property and call @BODY on match."
  (declare (indent 1))
  `(save-excursion
     (goto-char (point-min))
     (when (re-search-forward (org-re-property ,prop nil t) nil t)
       (progn ,@body))))

(defun md-msg-get-prop (prop)
  "Return the markdown PROP property value, nil if undefined."
  (md-msg-with-match-prop prop
    (read (match-string-no-properties 3))))

(defun md-msg-set-prop (prop val)
  "Set the markdown PROP property value to VAL."
  (md-msg-with-match-prop prop
    (replace-match (format "%S" val) nil nil nil 3)))

(defun md-msg-citation-line ()
  (save-excursion
    (goto-char (md-msg-end))
    (forward-line)
    (let ((start (point)))
      (forward-line)
      (buffer-substring-no-properties start (point)))))

(defun md-msg-build ()
  "Build and return the XML tree for current markdownMsg buffer."
  (let ((css (md-msg-load-css)))
    (cl-flet ((enforce (xml)
                       (let* ((tag (car xml))
                              (tmp (assq 'class (cadr xml)))
                              (class (when tmp
                                       (intern (cdr tmp))))
                              (style (md-msg-build-style tag class css)))
                         (when style
                           (setf (cadr xml) (assq-delete-all 'style (cadr xml)))
                           (setf (cadr xml) (assq-delete-all 'class (cadr xml)))
                           (push `(style . ,style) (cadr xml)))))
              (fix-img-src (xml)
                           (let ((src (assq 'src (cadr xml))))
                             (when (string-prefix-p "file://" (cdr src))
                               (setcdr src (substring (cdr src) (length "file://")))))))
      (let* ((org (md-msg-markdown-to-text-plain))
             (citation-line (md-msg-citation-line))
             (reply (md-msg-markdown-to-xml org default-directory))
             (temp-file (md-msg-reply-to-temp))
             (original (when temp-file
                         (md-msg-load-html-file temp-file)))
             (original-body (when original
                              (assq 'body original)))
             ;; TODO Check if there exists the "In-reply-to" header.
             (is-forward t)
             (should-cite (or original is-forward)))
        ;; (assq-delete-all 'h1 (assq 'div (assq 'body reply)))
        (md-msg-xml-walk (assq 'body reply) #'fix-img-src)
        (when css
          (assq-delete-all 'style (assq 'head reply))
          (md-msg-xml-walk (assq 'body reply) #'enforce))
        (if (not should-cite)
            ;; Remove all scripts from replies?
            (assq-delete-all 'script (assq 'head reply))
          ;; FIXME Add defcustom for this, because some reply styles should have
          ;; the headers. Outlook, for example, does this.
          ;; (md-msg-improve-reply-header original css)
          ;; each one looks like '(body attrs a b c d)

          ;; Add the original message to the end of the reply.
          (setf (cddr (assq 'body reply))
                (append (cddr (assq 'body reply))
                        ;; Mimic Gmail quote structure.
                        (list (list 'div '((class . "gmail_quote"))
                                    (list 'div '((class . "gmail_attr"))
                                          citation-line
                                          '(br nil))
                                    (append `(blockquote ((type . "cite")
                                                          (class . "gmail_quote")))
                                            (if original-body
                                                (cddr original-body)
                                              (cddr original))))))))
        reply))))

(defun md-msg-preview (arg)
  "Create a temporary mail and open it with `browse-url'.
With the prefix argument ARG set, it calls
`xwidget-webkit-browse-url' instead of `browse-url'."
  (interactive "P")
  (save-window-excursion
    (let ((browse-url-browser-function (if arg
                                           'xwidget-webkit-browse-url
                                         browse-url-browser-function))
          (tmp-file (make-temp-file "md-msg" nil ".html"))
          (mail (md-msg-build)))
      (with-temp-buffer
        (insert (md-msg-xml-to-str mail))
        (write-file tmp-file))
      (browse-url (concat "file://" tmp-file)))))

(defun md-msg-prepare-to-send ()
  "Convert the current markdownMsg buffer into `mml' content.
This function is a hook for `message-send-hook'."
  (save-window-excursion
    (when (or (eq major-mode 'md-msg-edit-mode) (eq major-mode 'mu4e-compose-mode))
      (let ((mail (md-msg-build)))
        ;; (when md-msg-text-plain-alternative
        ;;   (setq md-msg-text-plain (md-msg-markdown-to-text-plain)))
        (message-goto-body)
        ;; (delete-region (md-msg-start) (point-max))
        (insert "<#multipart type=alternative><#part type=text/plain>\n")
        ;; (mml-insert-multipart "alternative")
        ;; (mml-insert-part "text/plain")
        (md-msg-delete-separator)
        (goto-char (point-max))
        ;; (insert md-msg-text-plain)
        (forward-line)
        ;; (mml-insert-part "text/html")
        (insert "<#part type=text/html>\n<!DOCTYPE html>")
        (insert (md-msg-xml-to-str mail))
        (insert "<#/multipart>\n")))))

(defun md-msg-file-mime-type (file)
  "Return FILE mime type based on FILE extension.
If FILE does not have an extension, \"text/plain\" is returned."
  (let ((extension (file-name-extension file)))
    (if extension
        (mailcap-extension-to-mime extension)
      "text/plain")))

(defun md-msg-message-fetch-field (field-name)
  "Return the value of the header field whose type is FIELD-NAME."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (message-fetch-field field-name))))

(defun md-msg-reply-to-temp ()
  (let ((msg (md-msg-message-fetch-field "In-reply-to")))
    (when msg (concat "/tmp/" (substring msg 1 -1)))))

(defun md-msg-get-to-first-name ()
  "Return the first name of the recipient.
It parses the 'To:' field of the current `md-msg-edit-mode'
buffer to extract and return the first name.  It is used to
automatically greet the right name, see `md-msg-greeting-fmt'."
  (cl-flet ((recipient2name (r)
                            (cl-multiple-value-bind (name mail) r
                              (when name
                                (let* ((split (split-string name ", " t))
                                       (first-name (if (= (length split) 2)
                                                       (cadr split)
                                                     (car (split-string name " " t)))))
                                  (setf first-name (capitalize first-name))
                                  (if md-msg-greeting-fmt-mailto
                                      (format "[[mailto:%s][%s]]" mail first-name)
                                    first-name))))))
    (save-excursion
      (let ((to (md-msg-message-fetch-field "to")))
        (if to
            (let ((recipients (mail-extract-address-components to t)))
              (when md-msg-greeting-name-limit
                (setf recipients (seq-take recipients md-msg-greeting-name-limit)))
              (mapconcat #'recipient2name recipients ", "))
          "")))))

(defun md-msg-header (reply-to)
  "Build the markdown OPTIONS and PROPERTIES blocks.
REPLY-TO is the file path of the original email export in HTML."
  (format ":PROPERTIES:\n:reply-to: %S\n:attachment: nil\n:END:\n"
          reply-to))

(defun md-msg-article-htmlp-gnus ()
  "Return t if the current gnus article is HTML article.
If the currently visited article (`gnus-article-buffer') contains
a html mime part, it returns t, nil otherwise."
  (let* ((parts (with-current-buffer gnus-article-buffer
		  (set-buffer gnus-original-article-buffer)
		  (mm-dissect-buffer t t)))
	 (str (format "%s" parts)))
    (string-match-p "text/html" str)))

(defun md-msg-article-htmlp-mu4e ()
  "Return t if the current mu4e article is HTML article."
  (when (mu4e-message-field mu4e-compose-parent-message :body-html) t))

(defun md-msg-article-htmlp-notmuch ()
  "Return t if the current notmuch reply is an HTML article."
  ;; Seems like never the case for notmuch but we want to use md-msg
  t)

(defun md-msg-post-setup (&rest _args)
  "Transform the current `message' buffer into a markdownMsg buffer.
If the current `message' buffer is a reply, the
`md-msg-separator' string is inserted at the end of the editing
area."
  (unless (eq major-mode 'md-msg-edit-mode)
    (md-msg-edit-mode)
    (message-goto-body)
    (let ((new (not (md-msg-message-fetch-field "subject")))
          (with-original (not (= (point) (point-max))))
          (reply-to))
      (when (or new (md-msg-mua-call 'article-htmlp))
        (unless new
          (setq reply-to (md-msg-mua-call 'save-article-for-reply)))
        ;; (insert (md-msg-header reply-to))
        (when md-msg-greeting-fmt
          (insert (format md-msg-greeting-fmt
                          (if new
                              ""
                            (md-msg-get-to-first-name)))))
        (save-excursion
          (when with-original
            (save-excursion
              (insert "\n")
              (funcall message-citation-line-function)
              (insert "\n" md-msg-separator "\n")
              (delete-region (line-beginning-position)
                             (1+ (line-end-position)))
              ;; (dolist (rep '(("^>+ *" . "") ("___+" . "---")))
              ;;   (save-excursion
              ;;     (while (re-search-forward (car rep) nil t)
              ;;       (replace-match (cdr rep)))))
              ))
          ;; (when md-msg-signature
          ;;   (insert md-msg-signature))
          )
        (set-buffer-modified-p nil))
      (if (md-msg-message-fetch-field "to")
          (md-msg-goto-body)
        (message-goto-to)))))

(defun md-msg-post-setup--if-not-reply (&rest _args)
  "Helper for new mail setup vs reply in notmuch"
  (unless (md-msg-message-fetch-field "subject")
    (md-msg-post-setup _args)))

(defun md-msg-ctrl-c-ctrl-c ()
  "Send message like `message-send-and-exit'.
If the current buffer is markdownMsg buffer and markdownMsg is enabled (see
`md-msg-toggle'), it calls `message-send-and-exit'."
  (when (eq major-mode 'mu4e-compose-mode)
    (message-send-and-exit)))

(defun md-msg-tab ()
  "Complete names or markdown mode visibility cycle.
If `point' is in the mail header region, the `message-tab'
function is called.  `org-cycle' is called otherwise."
  (interactive)
  (if (message-in-body-p)
      (markdown-cycle)
    (message-tab)))

(defun md-msg-start ()
  "Return the point of the beginning of the message body."
  (save-excursion
    (message-goto-body)
    (line-beginning-position)))

(defun md-msg-end ()
  "Return the point of the end of the message body."
  (save-excursion
    (goto-char (point-min))
    (or (when (re-search-forward
               (concat "^" (regexp-quote md-msg-separator) "$") nil t)
          (match-beginning 0))
        (point-max))))

(defun md-msg-delete-separator ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^" (regexp-quote md-msg-separator) "$") nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun md-msg-goto-body ()
  "Move point to the beginning of the message body."
  (interactive)
  (goto-char (point-min))
  (if md-msg-signature
      (when (search-forward md-msg-signature nil t)
        (goto-char (match-beginning 0)))
    (message-goto-body)))

(defun md-msg-font-lock-make-header-matcher (regexp)
  "Create a function which look for REGEXP."
  `(lambda (limit)
     (save-restriction
       (widen)
       (let ((start (point))
             (citation-start (md-msg-end)))
         (when (< start citation-start)
           (goto-char citation-start))
         (re-search-forward ,regexp (point-max) t)))))

(defun md-msg-kill-buffer ()
  "Delete temporary files."
  (let ((files (md-msg-get-prop "reply-to")))
    (dolist (file files)
      (when (and (not (string= "" file)) (file-exists-p file))
        (cond ((file-directory-p file) (delete-directory file t))
              ((delete-file file)))))))

(defun md-msg-mode-mu4e ()
  "Setup the hook for mu4e mail user agent."
  (if md-msg-mode
      (progn
        (add-hook 'mu4e-view-mode-hook 'md-msg-view-mode)
        (add-hook 'mu4e-compose-mode-hook 'md-msg-post-setup))
    (progn
      (remove-hook 'mu4e-view-mode-hook 'md-msg-view-mode)
      (remove-hook 'mu4e-compose-mode-hook 'md-msg-post-setup))))

(defun md-msg-select-other-view (&optional orig-fn)
  "When the headers view is selected, select the message view (if
that has a live window), and vice versa."
  (interactive)
  (let* ((other-buf
          (cond
           ((eq major-mode 'mu4e-headers-mode)
            (mu4e-get-view-buffer))
           ((or (eq major-mode 'mu4e-view-mode) (eq major-mode 'md-msg-view-mode))
            (mu4e-get-headers-buffer))))
         (other-win (and other-buf (get-buffer-window other-buf))))
    (if (window-live-p other-win)
        (select-window other-win)
      (mu4e-message "No window to switch to"))))

;;;###autoload
(define-minor-mode md-msg-mode
  "Toggle MdMsg mode.
With a prefix argument ARG, enable Delete Selection mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

When MdMsg mode is enabled, the Message mode behavior is
modified to make use of markdown Mode for mail composition and build
HTML emails."
  :global t
  (md-msg-mua-call 'mode)
  (if md-msg-mode
      (progn
        ;; Edit emails in a markdown buffer.
        (add-hook 'message-mode-hook #'md-msg-post-setup)
        ;; Convert them to multipart plain text and html for sending.
        (add-hook 'message-send-hook #'md-msg-prepare-to-send)
        ;; Revert to the original message after sending.
        (add-hook 'message-sent-hook #'undo)
        (advice-add 'mu4e~view-quit-buffer :around 'md-msg-view-quit-buffer)
        (advice-add 'mu4e-select-other-view :around 'md-msg-select-other-view)
        ;; FIXME
        ;; (add-hook 'org-ctrl-c-ctrl-c-final-hook 'md-msg-ctrl-c-ctrl-c)
        (add-to-list 'message-syntax-checks '(invisible-text . disabled))
        ;; (unless (md-msg-mml-recursive-support)
        ;;   (advice-add 'mml-expand-html-into-multipart-related
        ;;               :around #'md-msg-mml-into-multipart-related))
        ;; (advice-add 'org-html--todo :around #'md-msg-html--todo)
        ;; (advice-add 'message-mail :after #'md-msg-post-setup)
        (when (boundp 'bbdb-mua-mode-alist)
          (add-to-list 'bbdb-mua-mode-alist '(message md-msg-edit-mode))))
    (remove-hook 'message-send-hook #'md-msg-prepare-to-send)
    (remove-hook 'message-sent-hook #'undo)
    (remove-hook 'message-mode-hook #'md-msg-post-setup)
    (advice-remove 'mu4e~view-quit-buffer 'md-msg-view-quit-buffer)
    (advice-remove 'mu4e-select-other-view 'md-msg-select-other-view)
    ;; (remove-hook 'org-ctrl-c-ctrl-c-final-hook 'md-msg-ctrl-c-ctrl-c)
    (setq message-syntax-checks (delete '(invisible-text . disabled)
                                        message-syntax-checks))
    ;; (unless (md-msg-mml-recursive-support)
    ;;   (advice-remove 'mml-expand-html-into-multipart-related
    ;;                  #'md-msg-mml-into-multipart-related))
    ;; (advice-remove 'org-html--todo #'md-msg-html--todo)
    ;; (advice-remove 'message-mail #'md-msg-post-setup)
    (when (boundp 'bbdb-mua-mode-alist)
      (setq bbdb-mua-mode-alist (delete '(message md-msg-edit-mode)
                                        bbdb-mua-mode-alist)))))

(defvar md-msg-font-lock-keywords
  (let ((content "[ \t]*\\(.+\\(\n[ \t].*\\)*\\)\n?"))
    `((,(md-msg-font-lock-make-header-matcher
         (concat "^\\([Tt]o:\\)" content))
       (1 'message-header-name)
       (2 'message-header-to nil t))
      (,(md-msg-font-lock-make-header-matcher
         (concat "^\\(^[GBF]?[Cc][Cc]:\\|^[Rr]eply-[Tt]o:\\)" content))
       (1 'message-header-name)
       (2 'message-header-cc nil t))
      (,(md-msg-font-lock-make-header-matcher
         (concat "^\\([Ss]ubject:\\)" content))
       (1 'message-header-name)
       (2 'message-header-subject nil t))
      (,(md-msg-font-lock-make-header-matcher
         (concat "^\\([A-Z][^: \n\t]+:\\)" content))
       (1 'message-header-name)
       (2 'message-header-other nil t))
      ,@(if (and md-msg-separator
                 (not (equal md-msg-separator "")))
            `((,(concat "^\\(" (regexp-quote md-msg-separator) "\\)$")
               1 'message-separator))
          nil)))
  "Additional expressions to highlight in OrgMsg mode.")

;; Composing messages in Markdown requires the header to remain in mu4e-compose-mode
;; for address completion, adding attachments, etc.
;; Then, the body can simply switch to markdown-mode, giving its full editing power.
;; (define-hostmode poly-mu4e-compose-hostmode
;;   :mode 'mu4e-compose-mode)

;; (define-innermode poly-markdown-msg-headers-innermode
;;   :mode 'markdown-mode
;;   :head-matcher "^--text follows this line--\n"
;;   :tail-matcher "\\'"
;;   :head-mode 'host
;;   :tail-mode 'host)

;; (define-polymode md-msg-edit-mode
;;   :hostmode 'poly-mu4e-compose-hostmode
;;   :innermodes '(poly-markdown-msg-headers-innermode))


;; md-msg-view-mode
;; (define-hostmode poly-mu4e-view-hostmode
;;   :mode 'mu4e-view-mode)

;; (define-innermode poly-markdown-msg-content-innermode
;;   :mode 'md-msg-view-mode
;;   ;; Message headers never contain two newlines, as that indicates the start of content.
;;   :head-matcher "^$"
;;   :tail-matcher "\\'"
;;   :head-mode 'host
;;   :tail-mode 'host)

;; (define-polymode md-msg-view-polymode
;;   :hostmode 'poly-mu4e-view-hostmode
;;   :innermodes '(poly-markdown-msg-content-innermode))

(defun md-msg-view-quit-buffer (&optional orig-fn)
  "Quit the mu4e-view buffer.
This is a rather complex function, to ensure we don't disturb
other windows."
  (interactive)
  (if (eq mu4e-split-view 'single-window)
      (when (buffer-live-p (mu4e-get-view-buffer))
        (kill-buffer (mu4e-get-view-buffer)))
    (unless (or (eq major-mode 'md-msg-view-mode) (eq major-mode 'mu4e-view-mode))
      (mu4e-error "Must be in md-msg-view-mode (%S)" major-mode))
    (let ((curbuf (current-buffer))
          (curwin (selected-window))
          (headers-win))
      (walk-windows
       (lambda (win)
         ;; check whether the headers buffer window is visible
         (when (eq (mu4e-get-headers-buffer) (window-buffer win))
           (setq headers-win win))
         ;; and kill any _other_ (non-selected) window that shows the current
         ;; buffer
         (when
             (and
              (eq curbuf (window-buffer win)) ;; does win show curbuf?
              (not (eq curwin win))         ;; but it's not the curwin?
              (not (one-window-p))) ;; and not the last one on the frame?
           (delete-window win))))  ;; delete it!
      ;; now, all *other* windows should be gone.
      ;; if the headers view is also visible, kill ourselves + window; otherwise
      ;; switch to the headers view
      (if (window-live-p headers-win)
          ;; headers are visible
          (progn
            (kill-buffer-and-window) ;; kill the view win
            (setq mu4e~headers-view-win nil)
            (select-window headers-win)) ;; and switch to the headers win...
        ;; headers are not visible...
        (progn
          (kill-buffer)
          (setq mu4e~headers-view-win nil)
          (when (buffer-live-p (mu4e-get-headers-buffer))
            (switch-to-buffer (mu4e-get-headers-buffer))))))))


(defvar md-msg-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mu4e-view-mode-map)
    (define-key map (kbd "<tab>") 'md-msg-tab)
    (define-key map (kbd "C-c C-s") 'message-goto-subject)
    (define-key map (kbd "C-c C-b") 'md-msg-goto-body)
    ;; (define-key map (kbd "q") 'md-msg-view-quit-buffer)
    ;; (evil-define-key 'normal map "q" 'md-msg-view-quit-buffer)
    map)
  "Keymap for `md-msg-view-mode'.")

(define-derived-mode md-msg-view-mode markdown-view-mode "mdmsg:view"
  "Major mode to read email using Markdown highlighting.

\\{md-msg-view-mode-map}"
  (use-local-map md-msg-view-mode-map)
  ;; Highlight header fields.
  (setq-local markdown-mode-font-lock-keywords
              (append markdown-mode-font-lock-keywords message-font-lock-keywords
                      md-msg-font-lock-keywords))
  ;; TODO Figure out how to hide a particular message header.
  ;; If we can, then hide the subject line to put it only in the header-line.

  ;; (let ((subject (concat " " (mu4e-message-field-at-point :subject) " ")))
  ;;   (add-text-properties 0 1 '(display (space :align-to left)) subject)
  ;;   (add-text-properties 1 (length subject) '(face magit-header-line) subject)
  ;;   (add-text-properties (- (length subject) 1) (length subject) '(display
  ;;   (space :align-to right)) subject)
  ;;   ;; TODO Add a face to this so we can make it big.
  ;;   (setq-local header-line-format subject))

  ;; Hide line numbers.
  (setq-local display-line-numbers-type nil)
  ;; Display images embedded in the email, but remote ones take too long.
  (setq-local markdown-display-remote-images nil)
  (markdown-display-inline-images))

(defun md-msg-edit-mode-mu4e ()
  "Setup mu4e faces, addresses completion and run mu4e."
  (mu4e~compose-remap-faces)
  (mu4e~start)
  (when mu4e-compose-complete-addresses
    (mu4e~compose-setup-completion))
  ;; the following code is verbatim from mu4e-compse.el, mu4e-compose-mode
  ;; this will setup fcc (saving sent messages) and handle flags
  ;; (e.g. replied to)
  (add-hook 'message-send-hook
            (lambda () ;; mu4e~compose-save-before-sending
              ;; when in-reply-to was removed, remove references as well.
              (when (eq mu4e-compose-type 'reply)
                (mu4e~remove-refs-maybe))
              (when use-hard-newlines
                (mu4e-send-harden-newlines))
              ;; for safety, always save the draft before sending
              (set-buffer-modified-p t)
              (save-buffer)
              (mu4e~compose-setup-fcc-maybe)
              (widen)) nil t)
  ;; when the message has been sent.
  (add-hook 'message-sent-hook
            (lambda () ;;  mu4e~compose-mark-after-sending
              (setq mu4e-sent-func 'mu4e-sent-handler)
              (mu4e~proc-sent (buffer-file-name))) nil t)
  (define-key md-msg-edit-mode-map (kbd "C-c C-k") 'mu4e-message-kill-buffer))

(defalias 'md-msg-edit-kill-buffer-gnus 'message-kill-buffer)
(defalias 'md-msg-edit-kill-buffer-notmuch 'message-kill-buffer)
(defalias 'md-msg-edit-kill-buffer-mu4e 'mu4e-message-kill-buffer)

(defun md-msg-edit-kill-buffer ()
  (interactive)
  (md-msg-mua-call 'edit-kill-buffer))

(defvar md-msg-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map markdown-mode-map)
    (define-key map (kbd "<tab>") 'md-msg-tab)
    (define-key map (kbd "TAB") 'md-msg-tab)
    (define-key map [remap markdown-preview] 'md-msg-preview)
    (define-key map (kbd "C-c C-k") 'md-msg-edit-kill-buffer)
    ;; (define-key map (kbd "C-c C-s") 'message-goto-subject)
    ;; (define-key map (kbd "C-c C-b") 'md-msg-goto-body)
    (define-key map [remap org-attach] 'md-msg-attach)
    map)
  "Keymap for `md-msg-edit-mode'.")

(define-derived-mode md-msg-edit-mode markdown-mode "mdmsg:compose"
  "Major mode to compose email using Org mode.
Like Org Mode but with these additional/changed commands:
Type \\[org-ctrl-c-ctrl-c] to send the message if the cursor is
  not a C-c C-c Org mode controlled region (Org babel for
  example).
Type \\[md-msg-preview] to preview the final email with
  `browse-url'.
Type \\[message-kill-buffer] to kill the current OrgMsg buffer.
Type \\[message-goto-subject] to move the point to the Subject
  header.
Type \\[md-msg-goto-body] to move the point to the beginning of
  the message body.
Type \\[md-msg-attach] to call the dispatcher for attachment
  commands.

\\{md-msg-edit-mode-map}"
  (use-local-map md-msg-edit-mode-map)
  (set (make-local-variable 'message-sent-message-via) nil)
  (set (make-local-variable 'message-signature) mu4e-compose-signature)
  (set (make-local-variable 'message-send-mail-real-function) nil)
  (make-local-variable 'message-default-charset)
  (set (make-variable-buffer-local 'comment-use-syntax) nil)
  (setq default-directory (mu4e~get-attachment-dir))

  (add-hook 'completion-at-point-functions 'message-completion-function nil t)
  (setq markdown-mode-font-lock-keywords
	(append markdown-mode-font-lock-keywords message-font-lock-keywords
		md-msg-font-lock-keywords))
  (toggle-truncate-lines)
  (md-msg-mua-call 'edit-mode)
  (setq-local kill-buffer-hook 'md-msg-kill-buffer)
  (unless (= (md-msg-end) (point-max))
    (add-text-properties (1- (md-msg-end)) (point-max) '(read-only t))))

(provide 'md-msg)

;;; md-msg.el ends here
