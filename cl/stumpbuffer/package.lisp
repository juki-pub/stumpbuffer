;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
                #:window-transient-p
                #:window-modal-p
                #:dump-to-file
                #:dump-group
                #:read-dump-from-file
                #:restore-group
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
           #:stumpbuffer-renumber-frame
           #:stumpbuffer-only
           #:stumpbuffer-dump-group
           #:stumpbuffer-restore-group
           ))

