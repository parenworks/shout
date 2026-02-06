(in-package #:shout.terminal)

;;; Raw Terminal Input/Output - CLOS-based
;;; Handles putting terminal in raw mode and reading key events

;;; Key event class

(defclass key-event ()
  ((char :initarg :char :accessor key-event-char :initform nil)
   (code :initarg :code :accessor key-event-code :initform nil)
   (ctrl-p :initarg :ctrl-p :accessor key-event-ctrl-p :initform nil)
   (alt-p :initarg :alt-p :accessor key-event-alt-p :initform nil))
  (:documentation "Represents a keyboard input event"))

(defmethod print-object ((key key-event) stream)
  (print-unreadable-object (key stream :type t)
    (format stream "~@[char=~S~]~@[ code=~S~]~@[ ctrl~]~@[ alt~]"
            (key-event-char key) (key-event-code key)
            (key-event-ctrl-p key) (key-event-alt-p key))))

(defun make-key-event (&key char code ctrl-p alt-p)
  (make-instance 'key-event :char char :code code :ctrl-p ctrl-p :alt-p alt-p))

;;; Special key codes

(defconstant +key-up+ :up)
(defconstant +key-down+ :down)
(defconstant +key-left+ :left)
(defconstant +key-right+ :right)
(defconstant +key-enter+ :enter)
(defconstant +key-escape+ :escape)
(defconstant +key-tab+ :tab)
(defconstant +key-backspace+ :backspace)
(defconstant +key-delete+ :delete)
(defconstant +key-home+ :home)
(defconstant +key-end+ :end)
(defconstant +key-page-up+ :page-up)
(defconstant +key-page-down+ :page-down)
(defconstant +key-f1+ :f1)
(defconstant +key-f2+ :f2)
(defconstant +key-f3+ :f3)
(defconstant +key-f4+ :f4)
(defconstant +key-f5+ :f5)
(defconstant +key-f6+ :f6)
(defconstant +key-f7+ :f7)
(defconstant +key-f8+ :f8)
(defconstant +key-f9+ :f9)
(defconstant +key-f10+ :f10)
(defconstant +key-f11+ :f11)
(defconstant +key-f12+ :f12)

;;; Environment configuration

(defparameter *stty-path*
  (or #+sbcl (sb-ext:posix-getenv "SHOUT_STTY_PATH")
      (ignore-errors (probe-file "/run/current-system/sw/bin/stty"))
      (ignore-errors (probe-file "/bin/stty"))
      (ignore-errors (probe-file "/usr/bin/stty"))
      (ignore-errors (probe-file "/usr/local/bin/stty"))
      "/bin/stty"))

(defparameter *tty-path*
  (or #+sbcl (sb-ext:posix-getenv "SHOUT_TTY_PATH")
      (let ((candidates '("/dev/tty" "/dev/pts/0" "/dev/console" "/dev/tty0")))
        (loop for path in candidates
              when (ignore-errors (open path :direction :input :if-does-not-exist nil))
              return path
              finally (return "/dev/tty")))))

(defparameter *escape-timeout*
  (or (ignore-errors
        (let ((timeout-str #+sbcl (sb-ext:posix-getenv "SHOUT_ESCAPE_TIMEOUT")))
          (when timeout-str
            (let ((parsed (read-from-string timeout-str)))
              (if (numberp parsed) parsed nil)))))
      (let ((term #+sbcl (sb-ext:posix-getenv "TERM"))
            (alacritty-socket #+sbcl (sb-ext:posix-getenv "ALACRITTY_SOCKET")))
        (cond
          (alacritty-socket 0.02)
          ((and term (search "alacritty" term)) 0.02)
          (t 0.05)))))

;;; Terminal mode controller class

(defclass terminal-mode ()
  ((raw-p :initarg :raw-p :accessor terminal-raw-p :initform nil)
   (original-settings :accessor terminal-original-settings :initform nil))
  (:documentation "Manages terminal mode state"))

(defgeneric enable-raw-mode (mode)
  (:documentation "Put terminal in raw mode"))

(defgeneric disable-raw-mode (mode)
  (:documentation "Restore terminal to normal mode"))

(defgeneric query-size (mode)
  (:documentation "Query terminal dimensions"))

(defmethod enable-raw-mode ((mode terminal-mode))
  (unless (terminal-raw-p mode)
    (handler-case
        #+sbcl
        (sb-ext:run-program (namestring *stty-path*) '("-echo" "raw" "-icanon")
                            :input t :output nil :error nil)
      (error (e)
        (handler-case
            #+sbcl
            (sb-ext:run-program "sh" (list "-c" "stty -echo raw -icanon")
                                :input t :output nil :error nil)
          (error (e2)
            (warn "Failed to enable raw mode: ~A / ~A" e e2)))))
    (setf (terminal-raw-p mode) t)))

(defmethod disable-raw-mode ((mode terminal-mode))
  (when (terminal-raw-p mode)
    (handler-case
        #+sbcl
        (sb-ext:run-program (namestring *stty-path*) '("echo" "-raw" "icanon")
                            :input t :output nil :error nil)
      (error (e)
        (handler-case
            #+sbcl
            (sb-ext:run-program "sh" (list "-c" "stty echo -raw icanon")
                                :input t :output nil :error nil)
          (error (e2)
            (declare (ignore e2))
            (warn "Failed to disable raw mode: ~A" e)))))
    (setf (terminal-raw-p mode) nil)))

(defmethod query-size ((mode terminal-mode))
  (declare (ignore mode))
  (handler-case
      (let* ((size-str (with-output-to-string (s)
                         #+sbcl
                         (sb-ext:run-program (namestring *stty-path*) '("size")
                                             :input t :output s :error nil)))
             (parts (cl-ppcre:split "\\s+" (string-trim '(#\Newline #\Space) size-str))))
        (when (= (length parts) 2)
          (list (parse-integer (second parts))
                (parse-integer (first parts)))))
    (error (e)
      (warn "Failed to query terminal size: ~A" e)
      '(80 24))))

;;; Global terminal mode instance

(defparameter *terminal-mode* (make-instance 'terminal-mode))

;;; Convenience functions

(defun terminal-size ()
  (query-size *terminal-mode*))

(defun enter-alternate-screen ()
  (format *terminal-io* "~C[?1049h" *escape*)
  (force-output *terminal-io*))

(defun leave-alternate-screen ()
  (format *terminal-io* "~C[?1049l" *escape*)
  (force-output *terminal-io*))

(defmacro with-raw-terminal (&body body)
  `(progn
     (enter-alternate-screen)
     (enable-raw-mode *terminal-mode*)
     (cursor-hide)
     (unwind-protect
          (progn ,@body)
       (close-tty-stream)
       (cursor-show)
       (disable-raw-mode *terminal-mode*)
       (leave-alternate-screen)
       (reset))))

(defun setup-terminal ()
  (enable-raw-mode *terminal-mode*)
  (cursor-hide))

(defun restore-terminal ()
  (cursor-show)
  (disable-raw-mode *terminal-mode*))

;;; Input reader class

(defclass input-reader ()
  ((stream :initarg :stream :accessor reader-stream :initform nil)
   (tty-path :initarg :tty-path :accessor reader-tty-path :initform *tty-path*))
  (:documentation "Reads and parses keyboard input from TTY"))

(defgeneric reader-open (reader)
  (:documentation "Open the TTY stream for reading"))

(defgeneric reader-close (reader)
  (:documentation "Close the TTY stream"))

(defgeneric read-key-event (reader)
  (:documentation "Read a key event from the input stream"))

(defmethod reader-open ((reader input-reader))
  (unless (and (reader-stream reader) (open-stream-p (reader-stream reader)))
    (setf (reader-stream reader)
          (open (reader-tty-path reader)
                :direction :input
                :element-type '(unsigned-byte 8)
                :if-does-not-exist :error))
    #+sbcl
    (let ((fd (sb-sys:fd-stream-fd (reader-stream reader))))
      (sb-posix:fcntl fd sb-posix:f-setfl
                      (logior (sb-posix:fcntl fd sb-posix:f-getfl)
                              sb-posix:o-nonblock))))
  reader)

(defmethod reader-close ((reader input-reader))
  (when (and (reader-stream reader) (open-stream-p (reader-stream reader)))
    (close (reader-stream reader))
    (setf (reader-stream reader) nil))
  reader)

(defun read-byte-nonblocking (stream)
  (handler-case
      (read-byte stream nil nil)
    #+sbcl (sb-int:simple-stream-error () nil)))

(defun wait-for-escape-sequence (stream timeout)
  (let ((start (get-internal-real-time))
        (timeout-ticks (* timeout internal-time-units-per-second)))
    (loop
      (let ((b (read-byte-nonblocking stream)))
        (when b (return b)))
      (when (>= (- (get-internal-real-time) start) timeout-ticks)
        (return nil))
      (sleep 0.0005))))

(defmethod read-key-event ((reader input-reader))
  (let* ((stream (reader-stream reader))
         (byte (read-byte-nonblocking stream)))
    (unless byte
      (return-from read-key-event nil))
    (cond
      ;; Escape sequence or bare escape
      ((= byte 27)
       (let ((next (wait-for-escape-sequence stream *escape-timeout*)))
         (cond
           ((null next)
            (make-key-event :code +key-escape+))
           ((= next 91)
            ;; CSI sequence: ESC [
            (let ((params nil)
                  (final-byte nil))
              (loop
                (setf final-byte (read-byte stream nil nil))
                (unless final-byte (return))
                (cond
                  ((and (>= final-byte 48) (<= final-byte 57))
                   (push (code-char final-byte) params))
                  ((= final-byte 59)
                   (push #\; params))
                  (t (return))))
              (let ((param-str (coerce (nreverse params) 'string)))
                (case final-byte
                  (65 (make-key-event :code +key-up+))
                  (66 (make-key-event :code +key-down+))
                  (67 (make-key-event :code +key-right+))
                  (68 (make-key-event :code +key-left+))
                  (72 (make-key-event :code +key-home+))
                  (70 (make-key-event :code +key-end+))
                  (126
                   (cond
                     ((string= param-str "3") (make-key-event :code +key-delete+))
                     ((string= param-str "5") (make-key-event :code +key-page-up+))
                     ((string= param-str "6") (make-key-event :code +key-page-down+))
                     ((string= param-str "11") (make-key-event :code +key-f1+))
                     ((string= param-str "12") (make-key-event :code +key-f2+))
                     ((string= param-str "13") (make-key-event :code +key-f3+))
                     ((string= param-str "14") (make-key-event :code +key-f4+))
                     ((string= param-str "15") (make-key-event :code +key-f5+))
                     ((string= param-str "17") (make-key-event :code +key-f6+))
                     ((string= param-str "18") (make-key-event :code +key-f7+))
                     ((string= param-str "19") (make-key-event :code +key-f8+))
                     ((string= param-str "20") (make-key-event :code +key-f9+))
                     ((string= param-str "21") (make-key-event :code +key-f10+))
                     ((string= param-str "23") (make-key-event :code +key-f11+))
                     ((string= param-str "24") (make-key-event :code +key-f12+))
                     (t (make-key-event :code :unknown))))
                  (t (make-key-event :code :unknown))))))
           ;; Alt+Enter (ESC + CR)
           ((= next 13)
            (make-key-event :code +key-enter+ :alt-p t))
           ;; Alt+Enter (ESC + LF)
           ((= next 10)
            (make-key-event :code +key-enter+ :alt-p t))
           (t
            ;; Alt + key
            (make-key-event :char (code-char next) :alt-p t)))))
      ;; Control characters
      ((< byte 32)
       (cond
         ((= byte 13) (make-key-event :code +key-enter+))
         ((= byte 10) (make-key-event :char #\Newline))
         ((= byte 9) (make-key-event :code +key-tab+))
         ((= byte 8) (make-key-event :code +key-backspace+))
         (t (make-key-event :char (code-char (+ byte 96)) :ctrl-p t))))
      ;; DEL character (127)
      ((= byte 127)
       (make-key-event :code +key-backspace+))
      ;; Multi-byte UTF-8 character (emoji, accented chars, etc.)
      ((>= byte 192)
       (let* ((utf8-bytes (cond
                            ((< byte 224) 2)   ; 110xxxxx = 2 bytes
                            ((< byte 240) 3)   ; 1110xxxx = 3 bytes
                            (t 4)))             ; 11110xxx = 4 bytes
              (bytes (make-array utf8-bytes :element-type '(unsigned-byte 8)))
              (valid t))
         (setf (aref bytes 0) byte)
         (loop for i from 1 below utf8-bytes
               for b = (read-byte stream nil nil)
               do (if (and b (= (logand b #xC0) #x80))
                      (setf (aref bytes i) b)
                      (setf valid nil)))
         (if valid
             (let ((str (handler-case
                            (babel:octets-to-string bytes :encoding :utf-8)
                          (error () "?"))))
               (when (> (length str) 0)
                 (make-key-event :char (char str 0))))
             (make-key-event :char #\?))))
      ;; Regular ASCII character
      (t
       (make-key-event :char (code-char byte))))))

;;; Global input reader instance

(defparameter *input-reader* (make-instance 'input-reader))

(defun close-tty-stream ()
  (reader-close *input-reader*))

(defun read-key ()
  (reader-open *input-reader*)
  (loop
    (let ((key (read-key-event *input-reader*)))
      (when key (return key)))
    (sleep 0.01)))

(defun read-key-with-timeout (timeout-ms)
  (reader-open *input-reader*)
  (let ((start-time (get-internal-real-time))
        (timeout-ticks (* timeout-ms (/ internal-time-units-per-second 1000))))
    (loop
      (let ((key (read-key-event *input-reader*)))
        (when key (return key)))
      (when (> (- (get-internal-real-time) start-time) timeout-ticks)
        (return nil))
      (sleep 0.01))))
