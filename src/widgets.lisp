(in-package #:shout.widgets)

;;; CLOS Widget System for SHOUT
;;; All UI components are extensible classes with generic methods

;;; ============================================================
;;; Box drawing characters
;;; ============================================================

(defparameter *box-chars*
  '(:top-left     #\╭
    :top-right    #\╮
    :bottom-left  #\╰
    :bottom-right #\╯
    :horizontal   #\─
    :vertical     #\│
    :t-down       #\┬
    :t-up         #\┴
    :t-right      #\├
    :t-left       #\┤
    :cross        #\┼))

(defun box-char (name)
  (getf *box-chars* name))

;;; ============================================================
;;; Spinner for async operations
;;; ============================================================

(defparameter *spinner-frames* "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏")
(defparameter *spinner-index* 0)

(defun next-spinner-frame ()
  (prog1 (char *spinner-frames* *spinner-index*)
    (setf *spinner-index* (mod (1+ *spinner-index*) (length *spinner-frames*)))))

;;; ============================================================
;;; Base widget class
;;; ============================================================

(defclass widget ()
  ((x :initarg :x :accessor widget-x :initform 1)
   (y :initarg :y :accessor widget-y :initform 1)
   (width :initarg :width :accessor widget-width :initform 40)
   (height :initarg :height :accessor widget-height :initform 10)
   (focused :initarg :focused :accessor widget-focused :initform nil)
   (visible :initarg :visible :accessor widget-visible :initform t)
   (help-keys :initarg :help-keys :accessor widget-help-keys :initform nil
              :documentation "Alist of (key . description) for context help"))
  (:documentation "Base class for all SHOUT UI widgets"))

(defgeneric render (widget)
  (:documentation "Draw the widget to the terminal"))

(defgeneric handle-key (widget key-event)
  (:documentation "Handle a key event. Returns T if consumed, NIL if not."))

(defgeneric focus (widget)
  (:documentation "Called when widget gains focus"))

(defgeneric blur (widget)
  (:documentation "Called when widget loses focus"))

(defmethod focus ((w widget))
  (setf (widget-focused w) t))

(defmethod blur ((w widget))
  (setf (widget-focused w) nil))

(defmethod handle-key ((w widget) key-event)
  (declare (ignore key-event))
  nil)

;;; ============================================================
;;; Drawing primitives
;;; ============================================================

(defun draw-text (row col text &key max-width)
  (cursor-to row col)
  (let ((display-text (if (and max-width (> (length text) max-width))
                          (concatenate 'string (subseq text 0 (- max-width 1)) "…")
                          text)))
    (write-string display-text *terminal-io*)))

(defun draw-box (x y width height &key title (focused nil) (info nil))
  (let ((x2 (+ x width -1))
        (y2 (+ y height -1))
        (tl (box-char :top-left))
        (tr (box-char :top-right))
        (bl (box-char :bottom-left))
        (br (box-char :bottom-right))
        (hz (box-char :horizontal))
        (vt (box-char :vertical)))
    (if focused
        (fg :accent)
        (fg :muted))
    ;; Top border with title
    (cursor-to y x)
    (write-char tl *terminal-io*)
    (cond
      (title
       (write-char hz *terminal-io*)
       (write-char #\Space *terminal-io*)
       (if focused (fg :bright-white) (fg :white))
       (when focused (bold))
       (let ((max-title (- width 6)))
         (write-string (if (> (length title) max-title)
                           (subseq title 0 max-title)
                           title)
                       *terminal-io*))
       (reset)
       (if focused (fg :accent) (fg :muted))
       (write-char #\Space *terminal-io*)
       (let ((title-len (min (length title) (- width 6))))
         (loop repeat (- width 5 title-len) do (write-char hz *terminal-io*))))
      (t
       (loop repeat (- width 2) do (write-char hz *terminal-io*))))
    (write-char tr *terminal-io*)
    ;; Side borders
    (loop for row from (1+ y) below y2 do
      (cursor-to row x)
      (write-char vt *terminal-io*)
      (cursor-to row x2)
      (write-char vt *terminal-io*))
    ;; Bottom border with optional info
    (cursor-to y2 x)
    (write-char bl *terminal-io*)
    (cond
      (info
       (let* ((info-str (format nil "~A" info))
              (info-len (length info-str))
              (left-border (- width 4 info-len)))
         (loop repeat (max 0 left-border) do (write-char hz *terminal-io*))
         (write-char #\Space *terminal-io*)
         (fg :muted)
         (write-string info-str *terminal-io*)
         (if focused (fg :accent) (fg :muted))
         (write-char #\Space *terminal-io*)))
      (t
       (loop repeat (- width 2) do (write-char hz *terminal-io*))))
    (write-char br *terminal-io*)
    (reset)))

(defun clear-region (x y width height)
  (loop for row from y below (+ y height) do
    (cursor-to row x)
    (loop repeat width do (write-char #\Space *terminal-io*))))

(defun word-wrap (text width)
  (let ((lines nil)
        (current-line (make-string-output-stream))
        (col 0))
    (loop for char across text do
      (cond
        ((char= char #\Newline)
         (push (get-output-stream-string current-line) lines)
         (setf current-line (make-string-output-stream))
         (setf col 0))
        ((>= col width)
         (push (get-output-stream-string current-line) lines)
         (setf current-line (make-string-output-stream))
         (setf col 0)
         (write-char char current-line)
         (incf col))
        (t
         (write-char char current-line)
         (incf col))))
    (let ((remaining (get-output-stream-string current-line)))
      (when (> (length remaining) 0)
        (push remaining lines)))
    (nreverse lines)))

;;; ============================================================
;;; Panel - bordered container with title
;;; ============================================================

(defclass panel (widget)
  ((title :initarg :title :accessor panel-title :initform nil)
   (border-color :initarg :border-color :accessor panel-border-color :initform :muted)
   (focused-color :initarg :focused-color :accessor panel-focused-color :initform :accent))
  (:documentation "A bordered panel that can contain content"))

(defmethod render ((p panel))
  (when (widget-visible p)
    (draw-box (widget-x p) (widget-y p)
              (widget-width p) (widget-height p)
              :title (panel-title p)
              :focused (widget-focused p))))

;;; ============================================================
;;; Text Area - multi-line text editing
;;; ============================================================

(defclass text-area (panel)
  ((content :initarg :content :accessor text-area-content :initform "")
   (cursor-row :initarg :cursor-row :accessor text-area-cursor-row :initform 0)
   (cursor-col :initarg :cursor-col :accessor text-area-cursor-col :initform 0)
   (scroll-offset :initarg :scroll-offset :accessor text-area-scroll-offset :initform 0)
   (placeholder :initarg :placeholder :accessor text-area-placeholder
                :initform "Type your message..."))
  (:documentation "Multi-line text editing widget"))

(defmethod initialize-instance :after ((ta text-area) &key)
  (setf (widget-help-keys ta)
        '(("Type" . "compose") ("Enter" . "newline")
          ("Backspace" . "delete") ("C-a" . "home") ("C-e" . "end"))))

(defun text-area-lines (ta)
  (let ((content (text-area-content ta)))
    (if (string= content "")
        (list "")
        (cl-ppcre:split "\\n" content :limit most-positive-fixnum))))

(defun text-area-inner-width (ta)
  (- (widget-width ta) 4))

(defun text-area-inner-height (ta)
  (- (widget-height ta) 2))

(defun text-area-visible-lines (ta)
  (let* ((raw-lines (text-area-lines ta))
         (inner-w (text-area-inner-width ta))
         (wrapped nil))
    (dolist (line raw-lines)
      (if (string= line "")
          (push "" wrapped)
          (dolist (wl (word-wrap line inner-w))
            (push wl wrapped))))
    (nreverse wrapped)))

(defmethod render ((ta text-area))
  (call-next-method)
  (let* ((inner-x (+ (widget-x ta) 2))
         (inner-y (+ (widget-y ta) 1))
         (inner-w (text-area-inner-width ta))
         (inner-h (text-area-inner-height ta))
         (lines (text-area-visible-lines ta))
         (offset (text-area-scroll-offset ta)))
    ;; Clear inner area
    (clear-region inner-x inner-y inner-w inner-h)
    (if (and (string= (text-area-content ta) "")
             (not (widget-focused ta)))
        ;; Show placeholder
        (progn
          (cursor-to inner-y inner-x)
          (fg :muted)
          (italic)
          (write-string (subseq (text-area-placeholder ta)
                                0 (min (length (text-area-placeholder ta)) inner-w))
                        *terminal-io*)
          (reset))
        ;; Show content
        (loop for i from 0 below inner-h
              for line-idx = (+ i offset)
              when (< line-idx (length lines))
              do (cursor-to (+ inner-y i) inner-x)
                 (let* ((line (nth line-idx lines))
                        (display (if (> (length line) inner-w)
                                     (subseq line 0 inner-w)
                                     line)))
                   (write-string display *terminal-io*))))
    ;; Show cursor when focused
    (when (widget-focused ta)
      (let* ((cursor-screen-row (- (text-area-cursor-row ta) offset))
             (cursor-screen-col (text-area-cursor-col ta)))
        (when (and (>= cursor-screen-row 0)
                   (< cursor-screen-row inner-h))
          (cursor-to (+ inner-y cursor-screen-row)
                     (+ inner-x (min cursor-screen-col inner-w)))
          (cursor-show)
          (force-output *terminal-io*))))))

(defun text-area-insert-char (ta char)
  (let* ((lines (text-area-lines ta))
         (row (text-area-cursor-row ta))
         (col (text-area-cursor-col ta))
         (line (or (nth row lines) "")))
    (setf (nth row lines)
          (concatenate 'string
                       (subseq line 0 (min col (length line)))
                       (string char)
                       (subseq line (min col (length line)))))
    (setf (text-area-content ta)
          (format nil "~{~A~^~%~}" lines))
    (incf (text-area-cursor-col ta))))

(defun text-area-insert-newline (ta)
  (let* ((lines (text-area-lines ta))
         (row (text-area-cursor-row ta))
         (col (text-area-cursor-col ta))
         (line (or (nth row lines) ""))
         (before (subseq line 0 (min col (length line))))
         (after (subseq line (min col (length line)))))
    (setf (nth row lines) before)
    (setf lines (append (subseq lines 0 (1+ row))
                        (list after)
                        (when (< (1+ row) (length lines))
                          (subseq lines (1+ row)))))
    (setf (text-area-content ta)
          (format nil "~{~A~^~%~}" lines))
    (incf (text-area-cursor-row ta))
    (setf (text-area-cursor-col ta) 0)
    ;; Auto-scroll
    (let ((inner-h (text-area-inner-height ta)))
      (when (>= (- (text-area-cursor-row ta) (text-area-scroll-offset ta)) inner-h)
        (incf (text-area-scroll-offset ta))))))

(defun text-area-backspace (ta)
  (let* ((lines (text-area-lines ta))
         (row (text-area-cursor-row ta))
         (col (text-area-cursor-col ta)))
    (cond
      ;; Delete char in current line
      ((> col 0)
       (let ((line (nth row lines)))
         (setf (nth row lines)
               (concatenate 'string
                            (subseq line 0 (1- col))
                            (subseq line col)))
         (setf (text-area-content ta)
               (format nil "~{~A~^~%~}" lines))
         (decf (text-area-cursor-col ta))))
      ;; Merge with previous line
      ((> row 0)
       (let* ((prev-line (nth (1- row) lines))
              (curr-line (nth row lines))
              (new-col (length prev-line)))
         (setf (nth (1- row) lines)
               (concatenate 'string prev-line curr-line))
         (setf lines (append (subseq lines 0 row)
                             (when (< (1+ row) (length lines))
                               (subseq lines (1+ row)))))
         (setf (text-area-content ta)
               (format nil "~{~A~^~%~}" lines))
         (decf (text-area-cursor-row ta))
         (setf (text-area-cursor-col ta) new-col)
         (when (< (text-area-cursor-row ta) (text-area-scroll-offset ta))
           (decf (text-area-scroll-offset ta))))))))

(defmethod handle-key ((ta text-area) key)
  (cond
    ;; Printable character
    ((key-event-char key)
     (cond
       ;; Ctrl+A - beginning of line
       ((and (key-event-ctrl-p key) (char= (key-event-char key) #\a))
        (setf (text-area-cursor-col ta) 0)
        t)
       ;; Ctrl+E - end of line
       ((and (key-event-ctrl-p key) (char= (key-event-char key) #\e))
        (let* ((lines (text-area-lines ta))
               (line (or (nth (text-area-cursor-row ta) lines) "")))
          (setf (text-area-cursor-col ta) (length line)))
        t)
       ;; Ctrl+K - kill to end of line
       ((and (key-event-ctrl-p key) (char= (key-event-char key) #\k))
        (let* ((lines (text-area-lines ta))
               (row (text-area-cursor-row ta))
               (col (text-area-cursor-col ta))
               (line (or (nth row lines) "")))
          (setf (nth row lines) (subseq line 0 (min col (length line))))
          (setf (text-area-content ta)
                (format nil "~{~A~^~%~}" lines)))
        t)
       ;; Regular character (not ctrl)
       ((not (key-event-ctrl-p key))
        (text-area-insert-char ta (key-event-char key))
        t)
       (t nil)))
    ;; Special keys
    ((key-event-code key)
     (case (key-event-code key)
       (:enter
        (text-area-insert-newline ta)
        t)
       (:backspace
        (text-area-backspace ta)
        t)
       (:up
        (when (> (text-area-cursor-row ta) 0)
          (decf (text-area-cursor-row ta))
          (let* ((lines (text-area-lines ta))
                 (line (or (nth (text-area-cursor-row ta) lines) "")))
            (setf (text-area-cursor-col ta)
                  (min (text-area-cursor-col ta) (length line))))
          (when (< (text-area-cursor-row ta) (text-area-scroll-offset ta))
            (decf (text-area-scroll-offset ta))))
        t)
       (:down
        (let ((lines (text-area-lines ta)))
          (when (< (text-area-cursor-row ta) (1- (length lines)))
            (incf (text-area-cursor-row ta))
            (let ((line (or (nth (text-area-cursor-row ta) lines) "")))
              (setf (text-area-cursor-col ta)
                    (min (text-area-cursor-col ta) (length line))))
            (let ((inner-h (text-area-inner-height ta)))
              (when (>= (- (text-area-cursor-row ta) (text-area-scroll-offset ta)) inner-h)
                (incf (text-area-scroll-offset ta))))))
        t)
       (:left
        (when (> (text-area-cursor-col ta) 0)
          (decf (text-area-cursor-col ta)))
        t)
       (:right
        (let* ((lines (text-area-lines ta))
               (line (or (nth (text-area-cursor-row ta) lines) "")))
          (when (< (text-area-cursor-col ta) (length line))
            (incf (text-area-cursor-col ta))))
        t)
       (:home
        (setf (text-area-cursor-col ta) 0)
        t)
       (:end
        (let* ((lines (text-area-lines ta))
               (line (or (nth (text-area-cursor-row ta) lines) "")))
          (setf (text-area-cursor-col ta) (length line)))
        t)
       (t nil)))
    (t nil)))

(defmethod focus :after ((ta text-area))
  (cursor-show))

(defmethod blur :after ((ta text-area))
  (cursor-hide))

;;; ============================================================
;;; Checkbox List - toggleable items
;;; ============================================================

(defclass checkbox-list (panel)
  ((items :initarg :items :accessor checkbox-list-items :initform nil
          :documentation "List of (name . display-string) pairs")
   (selected :initarg :selected :accessor checkbox-list-selected :initform 0
             :documentation "Currently highlighted index")
   (checked :initarg :checked :accessor checkbox-list-checked :initform nil
            :documentation "Hash table of name -> checked-p")
   (scroll-offset :initarg :scroll-offset :accessor checkbox-list-scroll-offset :initform 0))
  (:documentation "A list of toggleable checkbox items"))

(defmethod initialize-instance :after ((cl checkbox-list) &key)
  (unless (checkbox-list-checked cl)
    (setf (checkbox-list-checked cl) (make-hash-table :test 'equal)))
  (setf (widget-help-keys cl)
        '(("Space" . "toggle") ("↑↓" . "navigate") ("Enter" . "configure"))))

(defun toggle-checked (cl &optional (index (checkbox-list-selected cl)))
  (let* ((item (nth index (checkbox-list-items cl)))
         (name (car item)))
    (setf (gethash name (checkbox-list-checked cl))
          (not (gethash name (checkbox-list-checked cl))))))

(defmethod render ((cl checkbox-list))
  (call-next-method)
  (let* ((inner-x (+ (widget-x cl) 2))
         (inner-y (+ (widget-y cl) 1))
         (inner-h (- (widget-height cl) 2))
         (inner-w (- (widget-width cl) 4))
         (items (checkbox-list-items cl))
         (offset (checkbox-list-scroll-offset cl)))
    (loop for i from 0 below inner-h
          for item-idx = (+ i offset)
          when (< item-idx (length items))
          do (let* ((item (nth item-idx items))
                    (name (car item))
                    (display (cdr item))
                    (checked-p (gethash name (checkbox-list-checked cl)))
                    (selected-p (= item-idx (checkbox-list-selected cl))))
               (cursor-to (+ inner-y i) inner-x)
               ;; Clear only the inner width, not to end of line
               (loop repeat inner-w do (write-char #\Space *terminal-io*))
               (cursor-to (+ inner-y i) inner-x)
               ;; Selection highlight
               (when (and selected-p (widget-focused cl))
                 (inverse))
               ;; Checkbox
               (if checked-p
                   (progn (fg :success) (write-string "● " *terminal-io*))
                   (progn (fg :muted) (write-string "○ " *terminal-io*)))
               ;; Label
               (if checked-p (fg :white) (fg :muted))
               (let ((max-label (- inner-w 2)))
                 (write-string (if (> (length display) max-label)
                                   (subseq display 0 max-label)
                                   display)
                               *terminal-io*))
               (reset)))))

(defmethod handle-key ((cl checkbox-list) key)
  (let ((items (checkbox-list-items cl)))
    (cond
      ((and (key-event-code key) (eq (key-event-code key) :up))
       (when (> (checkbox-list-selected cl) 0)
         (decf (checkbox-list-selected cl))
         (when (< (checkbox-list-selected cl) (checkbox-list-scroll-offset cl))
           (decf (checkbox-list-scroll-offset cl))))
       t)
      ((and (key-event-code key) (eq (key-event-code key) :down))
       (when (< (checkbox-list-selected cl) (1- (length items)))
         (incf (checkbox-list-selected cl))
         (let ((inner-h (- (widget-height cl) 2)))
           (when (>= (- (checkbox-list-selected cl) (checkbox-list-scroll-offset cl)) inner-h)
             (incf (checkbox-list-scroll-offset cl)))))
       t)
      ((and (key-event-char key) (char= (key-event-char key) #\Space))
       (toggle-checked cl)
       t)
      ;; Enter - signal configure request
      ((and (key-event-code key) (eq (key-event-code key) :enter))
       :configure)
      (t nil))))

;;; ============================================================
;;; Tag List - add/remove tags
;;; ============================================================

(defclass tag-list (panel)
  ((tags :initarg :tags :accessor tag-list-tags :initform nil
         :documentation "List of all saved tag strings")
   (enabled :initarg :enabled :accessor tag-list-enabled :initform nil
            :documentation "Hash table of tag -> enabled-p for this post")
   (selected :initarg :selected :accessor tag-list-selected :initform 0)
   (editing :initarg :editing :accessor tag-list-editing :initform nil
            :documentation "When non-nil, currently editing a new tag")
   (edit-buffer :initarg :edit-buffer :accessor tag-list-edit-buffer :initform "")
   (scroll-offset :initarg :scroll-offset :accessor tag-list-scroll-offset :initform 0))
  (:documentation "A toggleable list of saved tags with add/remove capability"))

(defmethod initialize-instance :after ((tl tag-list) &key)
  (unless (tag-list-enabled tl)
    (setf (tag-list-enabled tl) (make-hash-table :test 'equal)))
  (setf (widget-help-keys tl)
        '(("Space" . "toggle") ("a" . "add tag") ("d" . "delete") ("↑↓" . "navigate"))))

(defun add-tag (tl tag)
  (unless (find tag (tag-list-tags tl) :test #'string-equal)
    (setf (tag-list-tags tl)
          (append (tag-list-tags tl) (list tag)))
    ;; New tags are enabled by default
    (setf (gethash tag (tag-list-enabled tl)) t)))

(defun remove-tag (tl &optional (index (tag-list-selected tl)))
  (when (and (tag-list-tags tl) (< index (length (tag-list-tags tl))))
    (let ((tag (nth index (tag-list-tags tl))))
      (remhash tag (tag-list-enabled tl)))
    (setf (tag-list-tags tl)
          (append (subseq (tag-list-tags tl) 0 index)
                  (when (< (1+ index) (length (tag-list-tags tl)))
                    (subseq (tag-list-tags tl) (1+ index)))))
    (when (>= (tag-list-selected tl) (max 1 (length (tag-list-tags tl))))
      (setf (tag-list-selected tl) (max 0 (1- (length (tag-list-tags tl))))))))

(defun toggle-tag (tl &optional (index (tag-list-selected tl)))
  (when (and (tag-list-tags tl) (< index (length (tag-list-tags tl))))
    (let ((tag (nth index (tag-list-tags tl))))
      (setf (gethash tag (tag-list-enabled tl))
            (not (gethash tag (tag-list-enabled tl)))))))

(defun tag-list-active-tags (tl)
  "Return only the tags that are currently enabled."
  (loop for tag in (tag-list-tags tl)
        when (gethash tag (tag-list-enabled tl))
        collect tag))

(defmethod render ((tl tag-list))
  (call-next-method)
  (let* ((inner-x (+ (widget-x tl) 2))
         (inner-y (+ (widget-y tl) 1))
         (inner-h (- (widget-height tl) 2))
         (inner-w (- (widget-width tl) 4))
         (tags (tag-list-tags tl))
         (offset (tag-list-scroll-offset tl)))
    ;; Draw tags with toggle indicators
    (loop for i from 0 below (1- inner-h)
          for tag-idx = (+ i offset)
          when (< tag-idx (length tags))
          do (let* ((tag (nth tag-idx tags))
                    (enabled-p (gethash tag (tag-list-enabled tl)))
                    (selected-p (= tag-idx (tag-list-selected tl))))
               (cursor-to (+ inner-y i) inner-x)
               (when (and selected-p (widget-focused tl) (not (tag-list-editing tl)))
                 (inverse))
               ;; Checkbox indicator
               (if enabled-p
                   (progn (fg :accent) (write-string "● " *terminal-io*))
                   (progn (fg :muted) (write-string "○ " *terminal-io*)))
               ;; Tag text
               (if enabled-p (fg :cyan) (fg :muted))
               (write-string "#" *terminal-io*)
               (if enabled-p (fg :white) (fg :muted))
               (let ((max-tag (- inner-w 3)))
                 (write-string (if (> (length tag) max-tag)
                                   (subseq tag 0 max-tag)
                                   tag)
                               *terminal-io*))
               (reset)))
    ;; Draw add-tag line
    (let ((add-row (min (+ inner-y (length tags) (- offset))
                        (+ inner-y inner-h -1))))
      (cursor-to add-row inner-x)
      (cond
        ((tag-list-editing tl)
         (fg :accent)
         (write-string "● #" *terminal-io*)
         (fg :bright-white)
         (write-string (tag-list-edit-buffer tl) *terminal-io*)
         (write-string "▎" *terminal-io*)
         (reset))
        (t
         (fg :muted)
         (write-string "+ add tag..." *terminal-io*)
         (reset))))))

(defmethod handle-key ((tl tag-list) key)
  (cond
    ;; Editing mode
    ((tag-list-editing tl)
     (cond
       ((and (key-event-code key) (eq (key-event-code key) :enter))
        (let ((tag (string-trim '(#\Space) (tag-list-edit-buffer tl))))
          (when (> (length tag) 0)
            (add-tag tl tag)))
        (setf (tag-list-editing tl) nil)
        (setf (tag-list-edit-buffer tl) "")
        t)
       ((and (key-event-code key) (eq (key-event-code key) :escape))
        (setf (tag-list-editing tl) nil)
        (setf (tag-list-edit-buffer tl) "")
        t)
       ((and (key-event-code key) (eq (key-event-code key) :backspace))
        (when (> (length (tag-list-edit-buffer tl)) 0)
          (setf (tag-list-edit-buffer tl)
                (subseq (tag-list-edit-buffer tl) 0 (1- (length (tag-list-edit-buffer tl))))))
        t)
       ((and (key-event-char key) (not (key-event-ctrl-p key)))
        (setf (tag-list-edit-buffer tl)
              (concatenate 'string (tag-list-edit-buffer tl) (string (key-event-char key))))
        t)
       (t nil)))
    ;; Normal mode
    (t
     (cond
       ((and (key-event-code key) (eq (key-event-code key) :up))
        (when (> (tag-list-selected tl) 0)
          (decf (tag-list-selected tl))
          (when (< (tag-list-selected tl) (tag-list-scroll-offset tl))
            (decf (tag-list-scroll-offset tl))))
        t)
       ((and (key-event-code key) (eq (key-event-code key) :down))
        (when (< (tag-list-selected tl) (1- (length (tag-list-tags tl))))
          (incf (tag-list-selected tl))
          (let ((inner-h (- (widget-height tl) 2)))
            (when (>= (- (tag-list-selected tl) (tag-list-scroll-offset tl)) (1- inner-h))
              (incf (tag-list-scroll-offset tl)))))
        t)
       ;; Space - toggle tag enabled/disabled
       ((and (key-event-char key) (char= (key-event-char key) #\Space))
        (toggle-tag tl)
        t)
       ;; a - add new tag
       ((and (key-event-char key) (char= (key-event-char key) #\a))
        (setf (tag-list-editing tl) t)
        (setf (tag-list-edit-buffer tl) "")
        t)
       ;; d - permanently delete tag
       ((and (key-event-char key) (char= (key-event-char key) #\d))
        (remove-tag tl)
        t)
       (t nil)))))

;;; ============================================================
;;; Progress Bar
;;; ============================================================

(defclass progress-bar (widget)
  ((value :initarg :value :accessor progress-bar-value :initform 0)
   (max-value :initarg :max-value :accessor progress-bar-max-value :initform 100)
   (label :initarg :label :accessor progress-bar-label :initform ""))
  (:documentation "A horizontal progress bar"))

(defmethod render ((pb progress-bar))
  (when (widget-visible pb)
    (let* ((bar-width (- (widget-width pb) 2 (length (progress-bar-label pb)) 8))
           (filled (if (> (progress-bar-max-value pb) 0)
                       (round (* bar-width (/ (progress-bar-value pb)
                                              (progress-bar-max-value pb))))
                       0))
           (empty (- bar-width filled)))
      (cursor-to (widget-y pb) (widget-x pb))
      ;; Filled portion
      (fg :accent)
      (loop repeat (max 0 filled) do (write-string "█" *terminal-io*))
      ;; Empty portion
      (fg :muted)
      (loop repeat (max 0 empty) do (write-string "░" *terminal-io*))
      ;; Count
      (write-char #\Space *terminal-io*)
      (let ((ratio (/ (progress-bar-value pb) (max 1 (progress-bar-max-value pb)))))
        (cond
          ((> ratio 0.9) (fg :error))
          ((> ratio 0.7) (fg :warning))
          (t (fg :white))))
      (format *terminal-io* "~D/~D" (progress-bar-value pb) (progress-bar-max-value pb))
      ;; Label
      (write-char #\Space *terminal-io*)
      (fg :muted)
      (write-string (progress-bar-label pb) *terminal-io*)
      (reset))))

;;; ============================================================
;;; Status Display - posting progress and results
;;; ============================================================

(defclass status-display (panel)
  ((entries :initarg :entries :accessor status-display-entries :initform nil
            :documentation "List of (icon color message) entries"))
  (:documentation "Displays posting status and results"))

(defmethod initialize-instance :after ((sd status-display) &key)
  (setf (widget-help-keys sd) nil))

(defun add-status (sd icon color message)
  (push (list icon color message) (status-display-entries sd)))

(defun clear-status (sd)
  (setf (status-display-entries sd) nil))

(defmethod render ((sd status-display))
  (call-next-method)
  (let* ((inner-x (+ (widget-x sd) 2))
         (inner-y (+ (widget-y sd) 1))
         (inner-h (- (widget-height sd) 2))
         (inner-w (- (widget-width sd) 4))
         (entries (reverse (status-display-entries sd))))
    (loop for i from 0 below inner-h
          for entry in entries
          do (destructuring-bind (icon color message) entry
               (cursor-to (+ inner-y i) inner-x)
               (fg color)
               (format *terminal-io* "~A " icon)
               (fg :white)
               (write-string (if (> (length message) (- inner-w 3))
                                 (subseq message 0 (- inner-w 3))
                                 message)
                             *terminal-io*)
               (reset)))))

;;; ============================================================
;;; Help Bar - context-sensitive key bindings
;;; ============================================================

(defclass help-bar (widget)
  ((bindings :initarg :bindings :accessor help-bar-bindings :initform nil
             :documentation "List of (key . description) pairs to display"))
  (:documentation "Bottom bar showing context-sensitive key bindings"))

(defmethod render ((hb help-bar))
  (when (widget-visible hb)
    (cursor-to (widget-y hb) (widget-x hb))
    ;; Clear the line first
    (clear-line)
    ;; Render as a simple status line with no box corners
    (let ((col 1)
          (max-col (widget-width hb)))
      (write-char #\Space *terminal-io*)
      (incf col)
      (dolist (binding (help-bar-bindings hb))
        (let* ((key-str (car binding))
               (desc-str (cdr binding))
               (entry-len (+ (length key-str) (length desc-str) 5)))
          (when (< (+ col entry-len) max-col)
            (fg :bright-cyan)
            (bold)
            (write-string key-str *terminal-io*)
            (reset)
            (fg :muted)
            (write-string ": " *terminal-io*)
            (fg :white)
            (write-string desc-str *terminal-io*)
            (fg :muted)
            (write-string " │ " *terminal-io*)
            (incf col entry-len)))))
    (reset)))

;;; ============================================================
;;; Config Form - modal form for client configuration
;;; ============================================================

(defclass config-form (panel)
  ((client-type :initarg :client-type :accessor config-form-client-type :initform nil)
   (fields :initarg :fields :accessor config-form-fields :initform nil
           :documentation "List of (keyword label help-text optional-p)")
   (values :accessor config-form-values :initform (make-hash-table :test 'equal)
           :documentation "Hash of keyword -> string value")
   (selected :accessor config-form-selected :initform 0)
   (editing :accessor config-form-editing :initform t
            :documentation "Always editing the selected field")
   (submitted :accessor config-form-submitted :initform nil
              :documentation "Set to :submit or :cancel when done")
   (scroll-offset :accessor config-form-scroll-offset :initform 0))
  (:documentation "Modal form for configuring a multiposter client"))

(defmethod initialize-instance :after ((cf config-form) &key)
  (setf (widget-help-keys cf)
        '(("↑↓" . "field") ("Enter" . "next/save") ("C-g" . "cancel")
          ("C-u" . "clear") ("Type" . "edit"))))

(defun config-form-field-value (cf index)
  (let ((field (nth index (config-form-fields cf))))
    (when field
      (gethash (first field) (config-form-values cf) ""))))

(defun config-form-set-value (cf index value)
  (let ((field (nth index (config-form-fields cf))))
    (when field
      (setf (gethash (first field) (config-form-values cf)) value))))

(defun config-form-result (cf)
  "Return an alist of (keyword . value) for non-empty fields."
  (loop for field in (config-form-fields cf)
        for key = (first field)
        for val = (gethash key (config-form-values cf) "")
        when (> (length val) 0)
        collect (cons key val)))

(defmethod render ((cf config-form))
  (call-next-method)
  (let* ((inner-x (+ (widget-x cf) 2))
         (inner-y (+ (widget-y cf) 1))
         (inner-w (- (widget-width cf) 4))
         (inner-h (- (widget-height cf) 2))
         (fields (config-form-fields cf))
         (offset (config-form-scroll-offset cf))
         (row 0))
    ;; Clear inner area
    (clear-region inner-x inner-y inner-w inner-h)
    ;; Render fields: each field takes 2 rows (label, then input)
    (loop for i from offset below (length fields)
          while (< row (1- inner-h))
          do (let* ((field (nth i fields))
                    (label (second field))
                    (help (third field))
                    (optional-p (fourth field))
                    (value (gethash (first field) (config-form-values cf) ""))
                    (selected-p (= i (config-form-selected cf))))
               ;; Label row
               (cursor-to (+ inner-y row) inner-x)
               (if selected-p (fg :accent) (fg :muted))
               (when selected-p (bold))
               (write-string label *terminal-io*)
               (reset)
               (when optional-p
                 (fg :muted)
                 (write-string " (optional)" *terminal-io*)
                 (reset))
               (incf row)
               ;; Input row
               (when (< row inner-h)
                 (cursor-to (+ inner-y row) inner-x)
                 (if selected-p
                     (progn
                       (fg :accent)
                       (write-string "▸ " *terminal-io*)
                       (reset)
                       (fg :white)
                       (let ((display (if (> (length value) (- inner-w 4))
                                         (subseq value (max 0 (- (length value) (- inner-w 4))))
                                         value)))
                         (write-string display *terminal-io*))
                       ;; Cursor indicator
                       (fg :accent)
                       (write-string "▏" *terminal-io*)
                       (reset))
                     (progn
                       (fg :muted)
                       (write-string "  " *terminal-io*)
                       (if (> (length value) 0)
                           (progn
                             (fg :white)
                             (let ((display (if (> (length value) (- inner-w 4))
                                               (subseq value 0 (- inner-w 4))
                                               value)))
                               (write-string display *terminal-io*)))
                           (progn
                             (fg :muted)
                             (italic)
                             (let ((hint (if (> (length help) (- inner-w 4))
                                            (subseq help 0 (- inner-w 4))
                                            help)))
                               (write-string hint *terminal-io*))))
                       (reset)))
                 (incf row))
               ;; Spacer row
               (when (< row inner-h)
                 (incf row))))))

(defmethod handle-key ((cf config-form) key)
  (let ((n-fields (length (config-form-fields cf))))
    (cond
      ;; Ctrl+G - cancel
      ((and (key-event-char key) (key-event-ctrl-p key)
            (char= (key-event-char key) #\g))
       (setf (config-form-submitted cf) :cancel)
       t)
      ;; Up - previous field
      ((and (key-event-code key) (eq (key-event-code key) :up))
       (when (> (config-form-selected cf) 0)
         (decf (config-form-selected cf))
         (when (< (config-form-selected cf) (config-form-scroll-offset cf))
           (decf (config-form-scroll-offset cf))))
       t)
      ;; Down - next field
      ((and (key-event-code key) (eq (key-event-code key) :down))
       (when (< (config-form-selected cf) (1- n-fields))
         (incf (config-form-selected cf))
         (let ((inner-h (- (widget-height cf) 2))
               (rows-per-field 3))
           (when (> (* (- (config-form-selected cf) (config-form-scroll-offset cf)) rows-per-field)
                    (- inner-h rows-per-field))
             (incf (config-form-scroll-offset cf)))))
       t)
      ;; Enter - next field or submit if on last
      ((and (key-event-code key) (eq (key-event-code key) :enter))
       (if (>= (config-form-selected cf) (1- n-fields))
           (setf (config-form-submitted cf) :submit)
           (progn
             (incf (config-form-selected cf))
             (let ((inner-h (- (widget-height cf) 2))
                   (rows-per-field 3))
               (when (> (* (- (config-form-selected cf) (config-form-scroll-offset cf)) rows-per-field)
                        (- inner-h rows-per-field))
                 (incf (config-form-scroll-offset cf))))))
       t)
      ;; Backspace - delete char from current field
      ((and (key-event-code key) (eq (key-event-code key) :backspace))
       (let ((val (config-form-field-value cf (config-form-selected cf))))
         (when (> (length val) 0)
           (config-form-set-value cf (config-form-selected cf)
                                  (subseq val 0 (1- (length val))))))
       t)
      ;; Printable character - append to current field
      ((and (key-event-char key) (not (key-event-ctrl-p key)))
       (let ((val (config-form-field-value cf (config-form-selected cf))))
         (config-form-set-value cf (config-form-selected cf)
                                (concatenate 'string val (string (key-event-char key)))))
       t)
      ;; Ctrl+U - clear field
      ((and (key-event-char key) (key-event-ctrl-p key)
            (char= (key-event-char key) #\u))
       (config-form-set-value cf (config-form-selected cf) "")
       t)
      (t nil))))
