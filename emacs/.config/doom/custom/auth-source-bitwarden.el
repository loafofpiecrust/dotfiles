;;; ../../dotfiles/emacs/.config/doom/custom/auth-source-bitwarden.el -*- lexical-binding: t; -*-

(require 'seq)
(eval-when-compile (require 'subr-x))
(require 'cl-lib)
(require 'auth-source)
(require 'url-parse)
(require 'bitwarden)

;;;###autoload
(defun auth-source-bitwarden-enable ()
  "Enable auth-source-password-store."
  ;; To add password-store to the list of sources, evaluate the following:
  (add-to-list 'auth-sources 'bitwarden)
  ;; clear the cache (required after each change to #'auth-source-bitwarden-search)
  (auth-source-forget-all-cached))

(cl-defun auth-source-bitwarden-search (&rest spec
                                              &key backend type host user port
                                              &allow-other-keys)
  "Given some search query, return matching credentials.
See `auth-source-search' for details on the parameters SPEC, BACKEND, TYPE,
HOST, USER and PORT."
  (unless (bitwarden-unlocked-p) (bitwarden-unlock))
  (cl-assert (or (null type) (eq type (oref backend type)))
             t "Invalid bitwarden search: %s %s")
  (cond ((eq host t)
         (warn "auth-source-bitwarden does not handle host wildcards.")
         nil)
        ((null host)
         ;; Do not build a result, as none will match when HOST is nil
         nil)
        (t
         (when-let ((result (auth-source-bitwarden--build-result host port user)))
           (list result)))))

(defun auth-source-bitwarden--build-result (hosts port user)
  "Build auth-source-pass entry matching HOSTS, PORT and USER.
HOSTS can be a string or a list of strings."
  (let* ((entry-data (auth-source-bitwarden--find-match hosts user port))
         (login (gethash "login" entry-data)))
    (when entry-data
      (list
       :host (url-host (url-generic-parse-url (gethash "uri" (aref (gethash "uris" login) 0))))
       :port port
       :user (or (gethash "username" login) user)
       :secret (gethash "password" login)))))

(defun auth-source-bitwarden--find-match (hosts user port)
  "Return password-store entry data matching HOSTS, USER and PORT.
Disambiguate between user provided inside HOSTS (e.g., user@server.com) and
inside USER by giving priority to USER.  Same for PORT.
HOSTS can be a string or a list of strings."
  (seq-some (lambda (host)
              (let ((results (bitwarden-search host "uri")))
                ;; Search for both "user" and "user@host" for the username.
                (or (first (append (bitwarden-search-filter-username results
                                                                     user) nil))
                    (first (append (bitwarden-search-filter-username results
                                                                     (format
                                                                      "%s@%s" user host)) nil)))))
            (if (listp hosts)
                hosts
              (list hosts))))

(cl-defun auth-source-bitwarden-create (&rest spec
                                              &key backend host port create
                                              &allow-other-keys)
  ;; Encode a json object with the entry.
  ;; TODO Add to existing entry if possible.
  ;; TODO Support adding to folder.
  (let* ((username (read-string (format "[%s] Username: " host)))
         (password (read-passwd (format "[%s] Password: " host)))
         (json (json-encode-alist `((name . ,host)
                                    (login . ((uris . ,host)
                                              (username . ,username)
                                              (password . ,password)))))))
    (start-process "bitwarden-encode" "*bitwarden-encode*" "bw" "encode")
    (process-send-string "bitwarden-encode" json)
    (with-current-buffer "*bitwarden-encode*"
      (bitwarden-runcmd "create" "item" (buffer-string)))))

(defvar auth-source-bitwarden-backend
  (auth-source-backend
   :source "." ;; not used
   :type 'bitwarden
   :search-function #'auth-source-bitwarden-search
   :create-function #'auth-source-bitwarden-create)
  "Auth-source backend for password-store.")

(provide 'auth-source-bitwarden)
