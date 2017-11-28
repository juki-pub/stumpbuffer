(in-package #:cl-user)

(defpackage #:stumpbuffer
  (:use #:cl #:stumpwm #:alexandria)
  
  ;; Grab some internal functions from stumpwm. Using internals can't
  ;; really be avoided since we have to be able to control things that
  ;; aren't exposed by stumpwm.
  (:import-from #:stumpwm
                #:choose-new-frame-window
                #:find-free-group-number
                #:find-free-hidden-group-number
                #:find-group
                #:float-group
                #:focus-frame
                #:frame-head
                #:frame-number
                #:frame-window
                #:frame-windows
                #:funcall-on-node
                #:group-frames
                #:kill-group
                #:list-splice-replace
                #:migrate-frame-windows
                #:next-group
                #:non-hidden-groups
                #:pull-window
                #:remove-split
                #:split-frame-h
                #:split-frame-v
                #:sync-frame-windows
                #:tile-group-current-frame
                #:tile-group-frame-head
                #:tile-group-last-frame
                #:tree-split-type
                #:unhide-window
                #:window-by-id
                #:window-frame
                #:window-hidden-p
                #:window-id
                #:window-name
                #:float-window
                #:tile-window
                )
  
  (:export #:*window-data-fields*
           #:*frame-data-fields*
           #:*group-data-fields*

           #:with-simple-error-handling
           #:find-window-by-id
           #:find-group-by-number
           #:find-frame-by-group-and-number

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
           #:stumpbuffer-focus-frame
           #:stumpbuffer-delete-window
           #:stumpbuffer-renumber-group
           #:stumpbuffer-renumber-window
           #:stumpbuffer-delete-frame
           #:stumpbuffer-echo-args
           #:stumpbuffer-delete-group
           #:stumpbuffer-dump-group
           #:stumpbuffer-hide-window
           ))

