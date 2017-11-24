(in-package #:cl-user)

(defpackage #:stumpbuffer
  (:use #:cl #:stumpwm)
  
  ;; Grab some internal functions from stumpwm. This is kinda ugly,
  ;; but there's no point reimplementing everything.
  (:import-from #:stumpwm
                #:pull-window
                #:window-frame
                #:window-name
                #:frame-number
                #:kill-group
                #:find-free-hidden-group-number
                #:find-free-group-number
                #:find-group
                #:next-group
                #:non-hidden-groups)
  
  (:export #:stumpbuffer-kill-group
           #:stumpbuffer-get-data
           #:stumpbuffer-pull-window
           #:stumpbuffer-rename-window
           #:stumpbuffer-kill-window
           #:stumpbuffer-switch-to-group
           #:stumpbuffer-create-group
           #:stumpbuffer-throw-window-to-group
           #:stumpbuffer-throw-window
           #:stumpbuffer-focus-window
           #:stumpbuffer-rename-group))

