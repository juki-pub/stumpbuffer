(in-package #:cl-user)

(defpackage #:stumpbuffer
  (:use #:cl #:stumpwm #:alexandria)
  
  ;; Grab some internal functions from stumpwm. Using internals can't
  ;; really be avoided since we have to be able to control things that
  ;; aren't exposed by stumpwm.
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
                #:non-hidden-groups
                #:frame-windows
                #:group-frames)
  
  (:export #:*window-data-fields*
           #:*frame-data-fields*
           #:*group-data-fields*

           #:stumpbuffer-kill-group
           #:stumpbuffer-get-data
           #:stumpbuffer-pull-window
           #:stumpbuffer-rename-window
           #:stumpbuffer-kill-window
           #:stumpbuffer-switch-to-group
           #:stumpbuffer-create-group
           #:stumpbuffer-throw-window-to-group
           #:stumpbuffer-throw-window
           #:stumpbuffer-focus-window
           #:stumpbuffer-rename-group
           #:stumpbuffer-kill-frame
           #:stumpbuffer-throw-window-to-frame
           #:stumpbuffer-split-frame
           #:stumpbuffer-focus-frame))

