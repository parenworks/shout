(asdf:defsystem shout
  :version "0.1.0"
  :license "zlib"
  :author "Glenn Skinner"
  :description "Social Herald Over Unix Terminals - A TUI for Multiposter"
  :depends-on (:multiposter
               :cl-ppcre
               :alexandria
               :babel)
  :serial T
  :components ((:module "src"
                :components ((:file "packages")
                             (:file "ansi")
                             (:file "terminal")
                             (:file "widgets")
                             (:file "layout")
                             (:file "bridge")
                             (:file "app"))))
  :build-operation "program-op"
  :build-pathname "shout"
  :entry-point "shout:main")
