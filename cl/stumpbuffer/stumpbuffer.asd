(in-package #:asdf-user)

(defsystem stumpbuffer
  :author "juki"
  :version "0.1"
  :license "GPLv3"

  :depends-on (:stumpwm)

  :serial t
  :components ((:file "package")
               (:file "stumpbuffer")))
