;;; Build script for SHOUT executable

(require :asdf)

;; Load Quicklisp
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

;; Add project and multiposter to ASDF search path
(push (truename ".") asdf:*central-registry*)
;; Multiposter must be available - check common locations
(dolist (path '(#P"../multiposter/"
               #P"~/SourceCode/multiposter/"
               #P"~/common-lisp/multiposter/"
               #P"~/quicklisp/local-projects/multiposter/"))
  (let ((resolved (ignore-errors (truename path))))
    (when (and resolved (probe-file (merge-pathnames "multiposter.asd" resolved)))
      (pushnew resolved asdf:*central-registry* :test #'equal))))

;; Load the system
(format t "Loading SHOUT...~%")
(force-output)
(ql:quickload :shout)
(format t "System loaded. Building executable...~%")
(force-output)

;; Build the executable
(sb-ext:save-lisp-and-die "shout"
                          :toplevel #'shout:main
                          :executable t
                          :compression 1
                          :save-runtime-options t)
