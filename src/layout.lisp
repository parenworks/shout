(in-package #:shout.layout)

;;; Screen Layout Engine
;;; Manages widget placement, focus cycling, and rendering

(defclass screen ()
  ((widgets :initarg :widgets :accessor screen-widgets :initform nil
            :documentation "Ordered list of all widgets")
   (focus-ring :initarg :focus-ring :accessor screen-focus-ring :initform nil
               :documentation "Ordered list of focusable widgets")
   (focus-index :initarg :focus-index :accessor screen-focus-index :initform 0)
   (help :initarg :help :accessor screen-help :initform nil
         :documentation "The help bar widget")
   (width :initarg :width :accessor screen-width :initform 80)
   (height :initarg :height :accessor screen-height :initform 24)
   (dirty :initarg :dirty :accessor screen-dirty :initform t
          :documentation "Whether screen needs full redraw"))
  (:documentation "Manages the overall screen layout and widget orchestration"))

(defgeneric add-widget (screen widget &key focusable)
  (:documentation "Add a widget to the screen"))

(defgeneric focus-next (screen)
  (:documentation "Move focus to the next widget in the focus ring"))

(defgeneric focus-prev (screen)
  (:documentation "Move focus to the previous widget in the focus ring"))

(defgeneric focused-widget (screen)
  (:documentation "Return the currently focused widget"))

(defgeneric layout-screen (screen)
  (:documentation "Calculate widget positions based on screen dimensions"))

(defgeneric render-screen (screen)
  (:documentation "Render all visible widgets"))

(defgeneric dispatch-key (screen key-event)
  (:documentation "Dispatch a key event to the focused widget or handle globally"))

(defmethod add-widget ((s screen) (w widget) &key (focusable t))
  (push w (screen-widgets s))
  (when focusable
    (setf (screen-focus-ring s)
          (append (screen-focus-ring s) (list w))))
  (setf (screen-dirty s) t))

(defmethod focused-widget ((s screen))
  (when (screen-focus-ring s)
    (nth (screen-focus-index s) (screen-focus-ring s))))

(defmethod focus-next ((s screen))
  (let ((ring (screen-focus-ring s)))
    (when ring
      (let ((current (focused-widget s)))
        (when current (blur current)))
      (setf (screen-focus-index s)
            (mod (1+ (screen-focus-index s)) (length ring)))
      (let ((new-focus (focused-widget s)))
        (when new-focus
          (focus new-focus)
          (update-help-bar s)))
      (setf (screen-dirty s) t))))

(defmethod focus-prev ((s screen))
  (let ((ring (screen-focus-ring s)))
    (when ring
      (let ((current (focused-widget s)))
        (when current (blur current)))
      (setf (screen-focus-index s)
            (mod (1- (screen-focus-index s)) (length ring)))
      (let ((new-focus (focused-widget s)))
        (when new-focus
          (focus new-focus)
          (update-help-bar s)))
      (setf (screen-dirty s) t))))

(defun update-help-bar (screen)
  (let ((hb (screen-help screen))
        (fw (focused-widget screen)))
    (when (and hb fw)
      (setf (help-bar-bindings hb)
            (append (widget-help-keys fw)
                    '(("Tab" . "next") ("S-Tab" . "prev")
                      ("F5" . "post") ("C-s" . "post") ("C-q" . "quit")))))))

(defmethod layout-screen ((s screen))
  (let* ((size (terminal-size))
         (w (or (first size) 80))
         (h (or (second size) 24)))
    (setf (screen-width s) w)
    (setf (screen-height s) h)
    ;; Layout is calculated by the app layer which knows the widget roles
    ;; This method just updates dimensions
    (setf (screen-dirty s) t)))

(defmethod render-screen ((s screen))
  (begin-sync-update)
  (when (screen-dirty s)
    (clear-screen))
  ;; Render all widgets in order (back to front)
  (dolist (w (reverse (screen-widgets s)))
    (when (widget-visible w)
      (cursor-hide)
      (render w)))
  ;; Show cursor for focused text-area
  (let ((fw (focused-widget s)))
    (when (and fw (typep fw 'text-area) (widget-focused fw))
      (cursor-show)))
  (force-output *terminal-io*)
  (end-sync-update)
  (setf (screen-dirty s) nil))

(defmethod dispatch-key ((s screen) key)
  ;; Global keys first
  (cond
    ;; Ctrl+Q - quit
    ((and (key-event-char key) (key-event-ctrl-p key)
          (char= (key-event-char key) #\q))
     :quit)
    ;; Tab - next focus
    ((and (key-event-code key) (eq (key-event-code key) :tab)
          (not (key-event-ctrl-p key)))
     (focus-next s)
     :handled)
    ;; Shift-Tab (often ESC [ Z) - prev focus
    ((and (key-event-code key) (eq (key-event-code key) :tab)
          (key-event-ctrl-p key))
     (focus-prev s)
     :handled)
    ;; F5 - post
    ((and (key-event-code key) (eq (key-event-code key) :f5))
     :post)
    ;; Ctrl+S - post
    ((and (key-event-char key) (key-event-ctrl-p key)
          (char= (key-event-char key) #\s))
     :post)
    ;; Dispatch to focused widget
    (t
     (let ((fw (focused-widget s)))
       (if (and fw (handle-key fw key))
           :handled
           nil)))))
