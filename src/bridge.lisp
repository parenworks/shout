(in-package #:shout.bridge)

;;; Multiposter Bridge
;;; Interfaces between SHOUT's TUI and multiposter's API

(defvar *multiposter* nil
  "The current multiposter instance")

;;; ============================================================
;;; Client info - read-only view of a multiposter client
;;; ============================================================

(defclass client-info ()
  ((name :initarg :name :accessor client-name)
   (type-name :initarg :type-name :accessor client-type-name)
   (enabled-p :initarg :enabled-p :accessor client-enabled-p)
   (ready-p :initarg :ready-p :accessor client-ready-p)
   (client-object :initarg :client-object :accessor client-object
                  :documentation "The actual multiposter client instance"))
  (:documentation "Read-only snapshot of a multiposter client's state"))

(defmethod print-object ((ci client-info) stream)
  (print-unreadable-object (ci stream :type t)
    (format stream "~A (~A) ~:[disabled~;~:[not ready~;ready~]~]"
            (client-name ci) (client-type-name ci)
            (client-enabled-p ci) (client-ready-p ci))))

;;; ============================================================
;;; Posting result
;;; ============================================================

(defclass posting-result ()
  ((client-name :initarg :client-name :accessor result-client-name)
   (success-p :initarg :success-p :accessor result-success-p :initform nil)
   (url :initarg :url :accessor result-url :initform nil)
   (error-message :initarg :error :accessor result-error :initform nil))
  (:documentation "Result of posting to a single client"))

;;; ============================================================
;;; Client setup field definitions
;;; ============================================================

(defparameter *client-setup-fields*
  '(("bluesky"
     ((:handle "Handle" "Your Bluesky handle (e.g. user.bsky.social)")
      (:password "App Password" "Create one at bsky.app/settings/app-passwords")
      (:pds "PDS URL" "https://bsky.social" t)))
    ("mastodon"
     ((:base "Server" "Mastodon server (e.g. mastodon.social)")
      (:name "Client Name" "Name for the OAuth app" t)))
    ("discord"
     ((:url "Webhook URL" "Discord webhook URL from channel settings")
      (:server-id "Channel ID" "Channel ID (optional, needed for undo)" t)
      (:username "Username" "Custom webhook username (optional)" t)))
    ("file"
     ((:path "Directory" "Directory to save posts in (e.g. ~/posts/)")))
    ("git"
     ((:path "Repository" "Path to git repository (e.g. ~/blog/)")))
    ("webdav"
     ((:base-url "Base URL" "WebDAV server URL (e.g. https://dav.example.com/posts)")
      (:username "Username" "WebDAV username (leave empty for no auth)" t)
      (:password "Password" "WebDAV password" t)
      (:authorization "Auth Header" "Full Authorization header (alternative to basic auth)" t)))
    ("tumblr"
     ((:blog "Blog Name" "Your Tumblr blog name" t)
      (:key "OAuth Key" "Tumblr OAuth consumer key")
      (:secret "OAuth Secret" "Tumblr OAuth consumer secret")))
    ("lichat"
     ((:hostname "Hostname" "Lichat server hostname (e.g. chat.tymoon.eu)")
      (:port "Port" "Server port (default: 1111)" t)
      (:username "Username" "Lichat username")
      (:password "Password" "Lichat password (if any)" t)
      (:channel "Channel" "Channel to post in")))
    ("pixiv"
     ((:cookie-string "Cookies" "Paste cookie string from browser console (see help)")))
    ("reader"
     ((:base-url "Base URL" "Reader instance URL (e.g. https://reader.tymoon.eu)")
      (:key "OAuth Key" "Reader OAuth app key")
      (:secret "OAuth Secret" "Reader OAuth app secret")))
    ("studio"
     ((:api-base "API Base" "Studio API URL (e.g. https://studio.tymoon.eu/api)")
      (:key "OAuth Key" "Studio OAuth app key")
      (:secret "OAuth Secret" "Studio OAuth app secret")))
    ("cohost"
     ((:email "Email" "Cohost email address (NOTE: Cohost shut down in 2024)")
      (:password "Password" "Cohost password")
      (:page "Page Handle" "Page handle to post on" t))))
  "Setup fields for each client type: (type-name ((keyword label help-text &optional optional-p)...))")

(defun client-setup-fields (type-name)
  (let ((entry (find type-name *client-setup-fields*
                     :key #'first :test #'string-equal)))
    (when entry (second entry))))

;;; ============================================================
;;; Available client types (all defined in multiposter)
;;; ============================================================

(defparameter *known-client-types*
  '(("bluesky" . "Bluesky (AT Protocol)")
    ("mastodon" . "Mastodon (ActivityPub)")
    ("discord" . "Discord (Webhook)")
    ("git" . "Git Repository")
    ("webdav" . "WebDAV")
    ("file" . "Local File")
    ("lichat" . "Lichat")
    ("reader" . "Reader")
    ("studio" . "Studio")
    ("tumblr" . "Tumblr")
    ("cohost" . "Cohost")
    ("pixiv" . "Pixiv")))

(defun get-available-client-types ()
  *known-client-types*)

;;; ============================================================
;;; SHOUT config directory and tag persistence
;;; ============================================================

(defun bridge-getenv (name)
  #+sbcl (sb-ext:posix-getenv name)
  #+ccl (ccl:getenv name)
  #-(or sbcl ccl) (uiop:getenv name))

(defun shout-config-dir ()
  (let ((xdg (ignore-errors (bridge-getenv "XDG_CONFIG_HOME"))))
    (merge-pathnames "shout/"
                     (if (and xdg (> (length xdg) 0))
                         (pathname (format nil "~A/" xdg))
                         (merge-pathnames ".config/" (user-homedir-pathname))))))

(defun shout-tags-file ()
  (merge-pathnames "tags.lisp" (shout-config-dir)))

(defun load-saved-tags ()
  (let ((file (shout-tags-file)))
    (when (probe-file file)
      (handler-case
          (with-open-file (s file :direction :input)
            (read s nil nil))
        (error () nil)))))

(defun save-tags (tags)
  (let ((dir (shout-config-dir))
        (file (shout-tags-file)))
    (ensure-directories-exist dir)
    (with-open-file (s file :direction :output :if-exists :supersede)
      (let ((*print-pretty* t))
        (write tags :stream s)
        (terpri s)))))

;;; ============================================================
;;; Config loading
;;; ============================================================

(defun load-multiposter-config ()
  (setf *multiposter*
        (handler-bind ((error #'continue))
          (org.shirakumo.multiposter:load-config nil)))
  *multiposter*)

;;; ============================================================
;;; Client discovery
;;; ============================================================

(defun get-configured-clients ()
  (unless *multiposter*
    (load-multiposter-config))
  (when *multiposter*
    (let ((clients nil))
      (maphash (lambda (name client)
                 (push (make-instance 'client-info
                                      :name name
                                      :type-name (string-downcase
                                                  (symbol-name (type-of client)))
                                      :enabled-p (org.shirakumo.multiposter::enabled-p client)
                                      :ready-p (ignore-errors
                                                 (org.shirakumo.multiposter:ready-p client))
                                      :client-object client)
                       clients))
               (org.shirakumo.multiposter:clients *multiposter*))
      (sort clients #'string< :key #'client-name))))

;;; ============================================================
;;; Client configuration
;;; ============================================================

(defun add-client-to-config (type-name client-name field-values)
  "Add a new client to the multiposter config.
TYPE-NAME is the client type (e.g. \"bluesky\").
CLIENT-NAME is the user-chosen name for this client.
FIELD-VALUES is an alist of (keyword . string-value)."
  (unless *multiposter*
    (load-multiposter-config))
  (when *multiposter*
    (let* ((type-sym (find-symbol (string-upcase type-name)
                                  (find-package :org.shirakumo.multiposter)))
           (client (when type-sym
                     (apply #'make-instance type-sym
                            (loop for (key . val) in field-values
                                  append (list key val))))))
      (when client
        (org.shirakumo.multiposter:add-client client *multiposter*)
        ;; Rename: multiposter uses the name as hash key
        (setf (gethash client-name (org.shirakumo.multiposter:clients *multiposter*))
              client)
        ;; Try setup
        (ignore-errors (org.shirakumo.multiposter:setup client))
        ;; Save config
        (ignore-errors (org.shirakumo.multiposter:save-config *multiposter*))
        ;; Return new client-info
        (make-instance 'client-info
                       :name client-name
                       :type-name type-name
                       :enabled-p t
                       :ready-p (ignore-errors (org.shirakumo.multiposter:ready-p client))
                       :client-object client)))))

;;; ============================================================
;;; Posting
;;; ============================================================

(defun post-to-clients (text tags selected-client-names &key callback)
  (unless *multiposter*
    (load-multiposter-config))
  (let ((results nil)
        (post (make-instance 'org.shirakumo.multiposter:text-post
                             :description text
                             :tags tags)))
    (dolist (client-name selected-client-names)
      (let ((client (org.shirakumo.multiposter:find-client client-name *multiposter*)))
        (when client
          (when callback
            (funcall callback :posting client-name))
          (handler-case
              (let ((result (org.shirakumo.multiposter:post post client :verbose nil)))
                (let ((pr (make-instance 'posting-result
                                         :client-name client-name
                                         :success-p (not (org.shirakumo.multiposter:failed-p result))
                                         :url (org.shirakumo.multiposter:url result))))
                  (push pr results)
                  (when callback
                    (funcall callback :success client-name (result-url pr)))))
            (error (e)
              (let ((pr (make-instance 'posting-result
                                       :client-name client-name
                                       :success-p nil
                                       :error (format nil "~A" e))))
                (push pr results)
                (when callback
                  (funcall callback :error client-name (format nil "~A" e)))))))))
    ;; Save config after posting (tokens may have refreshed)
    (ignore-errors (org.shirakumo.multiposter:save-config *multiposter*))
    (nreverse results)))

;;; ============================================================
;;; Character limits per client type
;;; ============================================================

(defun client-char-limit (type-name)
  (cond
    ((string-equal type-name "bluesky") 300)
    ((string-equal type-name "mastodon") 500)
    ((string-equal type-name "discord") 2000)
    ((string-equal type-name "tumblr") 4096)
    ((string-equal type-name "cohost") 3000)
    ((string-equal type-name "lichat") 1024)
    ((string-equal type-name "pixiv") 140)
    (t nil)))

(defun client-tags-in-body-p (type-name)
  "Return T if this client type appends tags to the post body text.
Bluesky sends tags as facets (metadata), not in the body."
  (cond
    ((string-equal type-name "bluesky") nil)
    (t t)))
