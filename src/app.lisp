(in-package #:shout)

;;; SHOUT - Social Herald Over Unix Terminals
;;; Main application entry point and screen composition

(defparameter *version* "0.1.0")
(defvar *demo-mode* nil "When T, use fake clients for all known types")

;;; ============================================================
;;; Application class
;;; ============================================================

(defclass shout-app ()
  ((screen :accessor app-screen :initform nil)
   (running :accessor app-running :initform nil)
   ;; Widgets
   (client-list :accessor app-client-list :initform nil)
   (compose-area :accessor app-compose-area :initform nil)
   (tag-list :accessor app-tag-list :initform nil)
   (status-display :accessor app-status-display :initform nil)
   (preview-panel :accessor app-preview-panel :initform nil)
   (progress-bars :accessor app-progress-bars :initform nil)
   (help-bar :accessor app-help-bar :initform nil)
   ;; State
   (clients :accessor app-clients :initform nil
            :documentation "List of client-info objects"))
  (:documentation "Main SHOUT application"))

;;; ============================================================
;;; Application setup
;;; ============================================================

(defgeneric setup-app (app)
  (:documentation "Initialize the application, load config, create widgets"))

(defgeneric run-app (app)
  (:documentation "Run the main event loop"))

(defgeneric teardown-app (app)
  (:documentation "Clean up before exit"))

(defgeneric do-post (app)
  (:documentation "Execute the post to selected clients"))

(defgeneric refresh-layout (app)
  (:documentation "Recalculate widget positions for current terminal size"))

(defgeneric refresh-preview (app)
  (:documentation "Update the preview panel with current char counts"))

(defun item-type-name (item-key clients)
  "Extract the client type name from a checkbox item key.
For configured clients, look up the type from app-clients.
For unconfigured items like 'discord:unconfigured', extract the prefix."
  (let ((ci (find item-key clients :key #'client-name :test #'string=)))
    (if ci
        (client-type-name ci)
        ;; Unconfigured: key is 'typename:unconfigured'
        (let ((pos (position #\: item-key)))
          (if pos (subseq item-key 0 pos) item-key)))))

(defun make-demo-clients ()
  "Create fake client-info objects for all known client types."
  (loop for (type-name . display) in (get-available-client-types)
        for i from 1
        collect (make-instance 'client-info
                               :name (format nil "demo-~A" type-name)
                               :type-name type-name
                               :enabled-p t
                               :ready-p t
                               :client-object nil)))

(defmethod setup-app ((app shout-app))
  ;; Load multiposter config (or demo clients)
  (if *demo-mode*
      (setf (app-clients app) (make-demo-clients))
      (progn
        (load-multiposter-config)
        (setf (app-clients app) (get-configured-clients))))

  ;; Create screen
  (setf (app-screen app) (make-instance 'screen))

  ;; Build client items: configured clients first, then unconfigured types
  (let ((all-items nil)
        (configured-type-names (mapcar #'client-type-name (app-clients app))))
    ;; Configured clients
    (dolist (ci (app-clients app))
      (push (cons (client-name ci)
                  (format nil "~A (~A)~:[~; ✗~]"
                          (client-name ci)
                          (client-type-name ci)
                          (not (client-ready-p ci))))
            all-items))
    ;; Unconfigured client types (greyed out)
    (dolist (ct (get-available-client-types))
      (let ((type-name (car ct))
            (display (cdr ct)))
        (unless (find type-name configured-type-names :test #'string-equal)
          (push (cons (format nil "~A:unconfigured" type-name)
                      (format nil "~A (not configured)" display))
                all-items))))
    (setf (app-client-list app)
          (make-instance 'checkbox-list
                         :title "Clients"
                         :items (nreverse all-items))))

  ;; Pre-check enabled & ready clients
  (dolist (ci (app-clients app))
    (when (and (client-enabled-p ci) (client-ready-p ci))
      (setf (gethash (client-name ci)
                     (checkbox-list-checked (app-client-list app)))
            t)))

  (setf (app-compose-area app)
        (make-instance 'text-area
                       :title "Compose"
                       :placeholder "Type your message..."))

  ;; Load saved tags
  (let ((saved-tags (load-saved-tags)))
    (setf (app-tag-list app)
          (make-instance 'tag-list
                         :title "Tags"))
    (dolist (tag saved-tags)
      (add-tag (app-tag-list app) tag)
      ;; Saved tags start disabled; user toggles them per post
      (setf (gethash tag (tag-list-enabled (app-tag-list app))) nil)))

  (setf (app-status-display app)
        (make-instance 'status-display
                       :title "Status"))

  (setf (app-preview-panel app)
        (make-instance 'panel
                       :title "Preview"))

  (setf (app-progress-bars app) nil)

  (setf (app-help-bar app)
        (make-instance 'help-bar))

  ;; Add widgets to screen (render order: back to front)
  (let ((s (app-screen app)))
    (add-widget s (app-client-list app) :focusable t)
    (add-widget s (app-tag-list app) :focusable t)
    (add-widget s (app-status-display app) :focusable nil)
    (add-widget s (app-compose-area app) :focusable t)
    (add-widget s (app-preview-panel app) :focusable nil)
    (add-widget s (app-help-bar app) :focusable nil)
    (setf (shout.layout:screen-help s) (app-help-bar app))

    ;; Initial focus on compose area
    (setf (shout.layout:screen-focus-ring s)
          (list (app-compose-area app)
                (app-client-list app)
                (app-tag-list app)))
    (setf (shout.layout:screen-focus-index s) 0)
    (focus (app-compose-area app))
    (update-help-bar s))

  ;; Initial status
  (let ((n-ready (count-if #'client-ready-p (app-clients app)))
        (n-total (length (app-clients app))))
    (add-status (app-status-display app) "✓" :success
                (format nil "~D/~D client~:P ready" n-ready n-total)))

  ;; Layout
  (refresh-layout app))

(defmethod refresh-layout ((app shout-app))
  (let* ((s (app-screen app))
         (size (terminal-size))
         (tw (or (first size) 80))
         (th (or (second size) 24))
         ;; Reserve 1 row for help bar at bottom
         (usable-h (1- th))
         ;; Left column: ~30% width, min 24
         (left-w (max 24 (min 35 (floor (* tw 0.3)))))
         ;; Right column: remaining width (extends to edge)
         (right-w (- tw left-w 0))
         ;; Left column: 3 panels stacked vertically, no overlap
         ;; client-h: enough for all items + border, capped at 60% of usable
         (n-client-items (length (checkbox-list-items (app-client-list app))))
         (client-h (max 5 (min (+ 2 n-client-items) (floor (* usable-h 0.55)))))
         ;; tag-h: smaller, just enough for a few tags
         (left-remaining (- usable-h client-h))
         (tag-h (max 4 (min 8 (floor left-remaining 2))))
         (status-h (max 3 (- usable-h client-h tag-h)))
         ;; Right column: preview height based on checked clients
         (checked (checkbox-list-checked (app-client-list app)))
         (n-preview-clients (loop for item in (checkbox-list-items (app-client-list app))
                                  when (gethash (car item) checked)
                                  count t))
         (preview-h (+ 2 (max 1 n-preview-clients)))
         (compose-h (- usable-h preview-h)))
    (setf (shout.layout:screen-width s) tw
          (shout.layout:screen-height s) th)

    ;; Left column: stack top-to-bottom, no gaps, no overlap
    (setf (widget-x (app-client-list app)) 1
          (widget-y (app-client-list app)) 1
          (widget-width (app-client-list app)) left-w
          (widget-height (app-client-list app)) client-h)

    (let ((tag-y (1+ client-h)))
      (setf (widget-x (app-tag-list app)) 1
            (widget-y (app-tag-list app)) tag-y
            (widget-width (app-tag-list app)) left-w
            (widget-height (app-tag-list app)) tag-h)

      (let ((status-y (+ tag-y tag-h)))
        (setf (widget-x (app-status-display app)) 1
              (widget-y (app-status-display app)) status-y
              (widget-width (app-status-display app)) left-w
              (widget-height (app-status-display app)) (- usable-h status-y -1))))

    ;; Right column: compose on top, preview at bottom, no gap
    (setf (widget-x (app-compose-area app)) (1+ left-w)
          (widget-y (app-compose-area app)) 1
          (widget-width (app-compose-area app)) right-w
          (widget-height (app-compose-area app)) compose-h)

    (setf (widget-x (app-preview-panel app)) (1+ left-w)
          (widget-y (app-preview-panel app)) (1+ compose-h)
          (widget-width (app-preview-panel app)) right-w
          (widget-height (app-preview-panel app)) preview-h)

    ;; Help bar: last row
    (setf (widget-x (app-help-bar app)) 1
          (widget-y (app-help-bar app)) th
          (widget-width (app-help-bar app)) tw
          (widget-height (app-help-bar app)) 1)

    (setf (shout.layout:screen-dirty s) t)))

(defmethod refresh-preview ((app shout-app))
  (let* ((panel (app-preview-panel app))
         (text (text-area-content (app-compose-area app)))
         (tags (tag-list-active-tags (app-tag-list app)))
         (text-len (length text))
         ;; Tag text length for clients that include tags in body
         (tag-text (format nil "~{#~A~^ ~}" tags))
         (tag-len (if (> (length tags) 0)
                      (+ 2 (length tag-text))
                      0))
         (inner-x (+ (widget-x panel) 2))
         (inner-y (+ (widget-y panel) 1))
         (inner-w (- (widget-width panel) 4))
         (inner-h (- (widget-height panel) 2))
         (row 0))
    ;; Clear inner area (border already rendered by render-screen)
    (clear-region inner-x inner-y inner-w inner-h)
    ;; Show char counts for each checked client
    (let ((checked (checkbox-list-checked (app-client-list app))))
      (dolist (item (checkbox-list-items (app-client-list app)))
        (let ((item-key (car item)))
          (when (and (gethash item-key checked)
                     (< row inner-h))
            (let* ((type-name (item-type-name item-key (app-clients app)))
                   (limit (client-char-limit type-name))
                   (total-len (if (client-tags-in-body-p type-name)
                                  (+ text-len tag-len)
                                  text-len)))
            (cursor-to (+ inner-y row) inner-x)
            (if limit
                ;; Client with char limit: show progress bar
                (let* ((bar-w (max 8 (- inner-w 20)))
                       (ratio (min 1.0 (/ total-len (max 1 limit))))
                       (filled (round (* bar-w ratio)))
                       (empty (- bar-w filled)))
                  (cond
                    ((> ratio 0.9) (fg :error))
                    ((> ratio 0.7) (fg :warning))
                    (t (fg :accent)))
                  (loop repeat (max 0 filled) do (write-string "█" *terminal-io*))
                  (fg :muted)
                  (loop repeat (max 0 empty) do (write-string "░" *terminal-io*))
                  (write-char #\Space *terminal-io*)
                  (cond
                    ((> total-len limit) (fg :error))
                    ((> ratio 0.9) (fg :warning))
                    (t (fg :white)))
                  (format *terminal-io* "~D/~D " total-len limit))
                ;; Client without char limit: show count + no limit
                (progn
                  (fg :success)
                  (write-string "✓ " *terminal-io*)
                  (fg :white)
                  (format *terminal-io* "~D chars " total-len)
                  (fg :muted)
                  (write-string "no limit " *terminal-io*)))
            (fg :muted)
            (write-string item-key *terminal-io*)
            (reset)
            (incf row))))))))

;;; ============================================================
;;; Posting
;;; ============================================================

(defun demo-post (app selected)
  "Simulate posting to selected clients with fake delays and results."
  (dolist (name selected)
    (add-status (app-status-display app)
                (string (next-spinner-frame)) :posting
                (format nil "Posting to ~A..." name))
    (setf (shout.layout:screen-dirty (app-screen app)) t)
    (render-screen (app-screen app))
    (force-output *terminal-io*)
    (sleep 0.5)
    (add-status (app-status-display app)
                "✓" :success
                (format nil "~A: https://example.com/~A/12345" name name))
    (setf (shout.layout:screen-dirty (app-screen app)) t)
    (render-screen (app-screen app))
    (force-output *terminal-io*)))

(defmethod do-post ((app shout-app))
  (let* ((text (text-area-content (app-compose-area app)))
         (tags (tag-list-active-tags (app-tag-list app)))
         (checked (checkbox-list-checked (app-client-list app)))
         (selected (loop for ci in (app-clients app)
                         when (gethash (client-name ci) checked)
                         collect (client-name ci))))
    (when (and (> (length text) 0) selected)
      (clear-status (app-status-display app))
      (add-status (app-status-display app) "⠋" :posting "Posting...")
      (setf (shout.layout:screen-dirty (app-screen app)) t)
      (render-screen (app-screen app))

      (if *demo-mode*
          (demo-post app selected)
          (let ((results (post-to-clients text tags selected
                           :callback (lambda (event client-name &optional detail)
                                       (case event
                                         (:posting
                                          (add-status (app-status-display app)
                                                      (string (next-spinner-frame)) :posting
                                                      (format nil "Posting to ~A..." client-name))
                                          (setf (shout.layout:screen-dirty (app-screen app)) t)
                                          (render-screen (app-screen app)))
                                         (:success
                                          (add-status (app-status-display app)
                                                      "✓" :success
                                                      (format nil "~A: ~A" client-name
                                                              (or detail "posted")))
                                          (setf (shout.layout:screen-dirty (app-screen app)) t)
                                          (render-screen (app-screen app)))
                                         (:error
                                          (add-status (app-status-display app)
                                                      "✗" :error
                                                      (format nil "~A: ~A" client-name
                                                              (or detail "failed")))
                                          (setf (shout.layout:screen-dirty (app-screen app)) t)
                                          (render-screen (app-screen app))))))))
            (declare (ignore results))))
      ;; Final status
      (add-status (app-status-display app) "✓" :success "Done!")
      ;; Clear compose area for next post
      (setf (text-area-content (app-compose-area app)) "")
      (setf (text-area-cursor-row (app-compose-area app)) 0)
      (setf (text-area-cursor-col (app-compose-area app)) 0)
      (setf (text-area-scroll-offset (app-compose-area app)) 0)
      ;; Disable all tags for next post
      (dolist (tag (tag-list-tags (app-tag-list app)))
        (setf (gethash tag (tag-list-enabled (app-tag-list app))) nil))
      (setf (shout.layout:screen-dirty (app-screen app)) t))))

;;; ============================================================
;;; Main event loop
;;; ============================================================

(defmethod run-app ((app shout-app))
  (setf (app-running app) t)
  (loop while (app-running app) do
    (refresh-layout app)
    (render-screen (app-screen app))
    (refresh-preview app)
    (force-output *terminal-io*)
    (let ((key (read-key)))
      (when key
        (let ((result (dispatch-key (app-screen app) key)))
          (case result
            (:quit
             (setf (app-running app) nil))
            (:post
             (do-post app))
            (t
             (setf (shout.layout:screen-dirty (app-screen app)) t))))))))

(defmethod teardown-app ((app shout-app))
  ;; Save tags for next session
  (when (app-tag-list app)
    (save-tags (tag-list-tags (app-tag-list app))))
  (setf (app-running app) nil))

;;; ============================================================
;;; Header rendering
;;; ============================================================

(defun draw-header (width)
  (cursor-to 1 1)
  (fg :accent)
  (bold)
  (write-string "SHOUT" *terminal-io*)
  (reset)
  (fg :muted)
  (format *terminal-io* " v~A" *version*)
  (reset)
  ;; Right-align the tagline
  (let* ((tagline "Social Herald Over Unix Terminals")
         (right-col (- width (length tagline))))
    (when (> right-col 10)
      (cursor-to 1 right-col)
      (fg :muted)
      (italic)
      (write-string tagline *terminal-io*)
      (reset))))

;;; ============================================================
;;; Entry points
;;; ============================================================

(defun shout ()
  (let ((app (make-instance 'shout-app)))
    (with-raw-terminal
      (setup-app app)
      (unwind-protect
           (run-app app)
        (teardown-app app)))))

(defun print-help ()
  (format t "SHOUT - Social Herald Over Unix Terminals~%~%")
  (format t "Usage: shout [OPTIONS]~%~%")
  (format t "Options:~%")
  (format t "  -h, --help       Show this help message~%")
  (format t "  -v, --version    Show version~%")
  (format t "      --demo       Demo mode (fake clients, no real posting)~%")
  (format t "~%")
  (format t "Keybindings:~%")
  (format t "  Tab / S-Tab      Cycle focus between panels~%")
  (format t "  F5 / C-s         Post to selected clients~%")
  (format t "  C-q              Quit~%")
  (format t "  Space            Toggle item (clients/tags)~%")
  (format t "  a                Add new tag~%")
  (format t "  d                Delete selected tag~%")
  (format t "  ↑↓               Navigate lists~%")
  (format t "~%")
  (format t "Configuration:~%")
  (format t "  Tags saved to ~~/.config/shout/tags.lisp~%")
  (format t "  Clients configured via multiposter (~~/.config/multiposter/)~%"))

(defun main ()
  (let ((args #+sbcl (rest sb-ext:*posix-argv*) #-sbcl nil))
    (cond
      ((or (find "--help" args :test #'string=)
           (find "-h" args :test #'string=))
       (print-help))
      ((or (find "--version" args :test #'string=)
           (find "-v" args :test #'string=))
       (format t "shout ~A~%" *version*))
      (t
       (when (find "--demo" args :test #'string=)
         (setf *demo-mode* t))
       (handler-case
           (shout)
         (error (e)
           (format *error-output* "~&SHOUT error: ~A~%" e)
           #+sbcl (sb-ext:exit :code 1)))))))
