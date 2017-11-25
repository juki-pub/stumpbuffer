(in-package #:stumpbuffer)

(defmacro with-simple-error-handling (&body body)
  `(handler-case
       (progn ,@body)
     (error (e) (message "(:error \"~a\")" e))))

(defun find-group-by-number (num)
  (or (find num (screen-groups (current-screen)) :key #'group-number)
      (error "No such group.")))

(defun find-window-by-group-and-number (group number)
  (or (find number (group-windows group) :key #'window-number)
      (error "No such window.")))

(defun find-frame-by-group-and-number (group number)
  (or (find number (group-frames group) :key #'frame-number)
      (error "No such frame.")))

(defun find-window-by-id (id)
  (or (stumpwm::window-by-id id)
      (error "No such window.")))

;; This is just copy/paste of the same function in STUMPWM, except
;; that this takes the frame as an argument instead of using the
;; current frame.
(defun split-frame (group frame how &optional (ratio 1/2))
  (check-type how (member :row :column))
  (let ((head (stumpwm::frame-head group frame)))
    ;; don't create frames smaller than the minimum size
    (when (or (and (eq how :row)
                   (>= (frame-height frame) (* *min-frame-height* 2)))
              (and (eq how :column)
                   (>= (frame-width frame) (* *min-frame-width* 2))))
      (multiple-value-bind (f1 f2) (funcall (if (eq how :column)
                                                'stumpwm::split-frame-h
                                                'stumpwm::split-frame-v)
                                            group frame ratio)
        (setf (stumpwm::tile-group-frame-head group head)
              (if (atom (stumpwm::tile-group-frame-head group head))
                  (list f1 f2)
                  (stumpwm::funcall-on-node
                   (stumpwm::tile-group-frame-head group head)
                   (lambda (tree)
                     (if (eq (stumpwm::tree-split-type tree) how)
                         (stumpwm::list-splice-replace frame tree f1 f2)
                         (substitute (list f1 f2) frame tree)))
                   (lambda (tree)
                     (unless (atom tree)
                       (find frame tree))))))
        (stumpwm::migrate-frame-windows group frame f1)
        (stumpwm::choose-new-frame-window f2 group)
        (if (eq (stumpwm::tile-group-current-frame group)
                frame)
            (setf (stumpwm::tile-group-current-frame group) f1))
        (setf (stumpwm::tile-group-last-frame group) f2)
        (stumpwm::sync-frame-windows group f1)
        (stumpwm::sync-frame-windows group f2)
        ;; we also need to show the new window in the other frame
        (when (stumpwm::frame-window f2)
          (stumpwm::unhide-window (stumpwm::frame-window f2)))
        (frame-number f2)))))

(defcommand stumpbuffer-switch-to-group (group-num)
    ((:number "Group number: "))
  (with-simple-error-handling
    (gselect (find-group-by-number group-num))))

(defcommand stumpbuffer-focus-frame (group-num frame-num)
    ((:number "Group number: ")
     (:number "Frame number: "))
  (with-simple-error-handling
    (let ((g (find-group-by-number group-num)))
      (gselect g)
      (stumpwm::focus-frame g (find-frame-by-group-and-number g frame-num)))))

(defcommand stumpbuffer-focus-window (group-num window-id)
    ((:number "Group number: ")
     (:number "Window ID: "))
  (with-simple-error-handling
    (let ((g (find-group-by-number group-num)))
      (gselect g)
      (group-focus-window g (find-window-by-id window-id)))))

(defcommand stumpbuffer-pull-window (window-id)
    ((:number "Window ID: "))
  (with-simple-error-handling
    (let ((window (find-window-by-id window-id)))
      (move-window-to-group window (current-group))
      (pull-window window))))

(defcommand stumpbuffer-throw-window-to-group (from-window-id to-group-num)
    ((:number "From window ID: ")
     (:number "To group number: "))
  (with-simple-error-handling
    (let ((from-window (find-window-by-id from-window-id))
           (to-group (find-group-by-number to-group-num)))
      (move-window-to-group from-window to-group))))

(defcommand stumpbuffer-throw-window-to-frame (from-window-id
                                               to-group-num
                                               to-frame-num)
    ((:number "From window ID: ")
     (:number "To group number: ")
     (:number "To frame number: "))
  (with-simple-error-handling
    (let* ((from-window (find-window-by-id from-window-id))
           (to-group (find-group-by-number to-group-num))
           (to-frame (find-frame-by-group-and-number to-group to-frame-num)))
      (save-frame-excursion
        (move-window-to-group from-window to-group)
        (pull-window from-window to-frame)))))

(defcommand stumpbuffer-throw-window (from-window-id
                                      to-group-num
                                      to-window-id)
    ((:number "From window ID: ")
     (:number "To group number: ")
     (:number "To window ID: "))
  (with-simple-error-handling
    (let* ((from-window (find-window-by-id from-window-id))
           (to-group (find-group-by-number to-group-num))
           (to-window (find-window-by-id to-window-id)))
      (save-frame-excursion
        (move-window-to-group from-window to-group)
        (pull-window from-window (window-frame to-window))))))

(defcommand stumpbuffer-rename-group (group-num name)
    ((:number "Group number: ")
     (:string "New name: "))
  (with-simple-error-handling
    (let* ((group (find-group-by-number group-num)))
      ;; This is largely copied from the standard GRENAME command.
      (cond ((find-group (current-screen) name)
             (error "Name already exists."))
            ((or (zerop (length name))
                 (string= name "."))
             (error "Name can't be empty."))
            (t (cond
                 ((and (char= (char name 0) #\.)
                       (not (char= (char (group-name group) 0) #\.)))
                  (setf (group-number group) (find-free-hidden-group-number
                                              (current-screen))))
                 ((and (not (char= (char name 0) #\.))
                       (char= (char (group-name group) 0) #\.))
                  (setf (group-number group) (find-free-group-number
                                              (current-screen)))))
               (setf (group-name group) name))))))

(defcommand stumpbuffer-create-group (name)
    ((:string "Group name: "))
  (with-simple-error-handling
    (add-group (current-screen) name :background t)))

(defcommand stumpbuffer-kill-group (group-num)
    ((:number "Group number: "))
  (with-simple-error-handling
    (let* ((groups (screen-groups (current-screen)))
           (group (find-group-by-number group-num))
           (to-group (if (eq group (current-group))
                         (or (next-group group (non-hidden-groups groups))
                             (next-group group groups))
                         (current-group))))
      (if (null to-group)
          (error "Only one group left.")
          (progn
            (when (eql group (current-group))
              (gselect to-group))
            (kill-group group to-group))))))

(defcommand stumpbuffer-rename-window (id new-name)
    ((:number "Window ID: ")
     (:string "New name: "))
  (with-simple-error-handling
    (setf (window-user-title (find-window-by-id id)) new-name)))

(defcommand stumpbuffer-kill-window (window-id)
    ((:number "Window ID: "))
  (with-simple-error-handling
    (let* ((window (find-window-by-id window-id)))
      (delete-window window))))

(defcommand stumpbuffer-kill-frame (group-num frame-num)
    ((:number "Group number: ")
     (:number "Frame number: "))
  (with-simple-error-handling
    (let* ((group (find-group-by-number group-num))
           (frame (find-frame-by-group-and-number group frame-num)))
      (stumpwm::remove-split group frame))))

(defcommand stumpbuffer-split-frame (group-num frame-num direction)
    ((:number "Group number: ")
     (:number "Frame number: ")
     (:number "Direction: "))
  "Split a frame. 

DIRECTION can be 1 or 2, corresponding to column and row
respectively."
  (with-simple-error-handling
    (let* ((group (find-group-by-number group-num))
           (frame (find-frame-by-group-and-number group frame-num)))
      ;; Same as STUMPWM::SPLIT-FRAME-IN-DIR, but that doesn't take
      ;; the frame as an argument.
      (if (split-frame group frame
                       (ecase direction
                         (1 :column)
                         (2 :row))
                       1/2)
          (when (stumpwm::frame-window frame)
            (update-decoration (stumpwm::frame-window frame)))
          (error "Cannot split smaller than minimum size.")))))

(defcommand stumpbuffer-get-data () ()
  "Retrieve information about groups and windows for StumpBuffer."
  (labels ((number (plist) (getf plist :number))
           (window-plist (window)
             (list :number (window-number window)
                   :frame (frame-number
                           (window-frame window))
                   :title (window-name window)
                   :class (window-class window)
                   :role (window-role window)
                   :instance (window-res window)
                   :id (stumpwm::window-id window)))
           (frame-plist (group frame)
             (list :number (frame-number frame)
                   :windows (sort
                             (mapcar #'window-plist (frame-windows group frame))
                             #'< :key #'number)))
           (group-plist (group)
             (let ((type (if (typep group 'stumpwm::float-group)
                             :floating
                             :tiling)))
               (list* :number (group-number group)
                      :name (group-name group)
                      :type type
                      (if (eql type :tiling)
                          (list :frames
                                (sort (mapcar (lambda (frame)
                                                (frame-plist group frame))
                                              (group-frames group))
                                      #'< :key #'number)))))))
    (with-simple-error-handling
      (let ((*print-case* :downcase))
        (message "~s" (let ((groups (screen-groups (current-screen))))
                        (sort (mapcar #'group-plist groups)
                              #'< :key #'number)))))))
