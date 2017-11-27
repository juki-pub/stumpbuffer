(in-package #:stumpbuffer)

(defmacro with-simple-error-handling (&body body)
  `(handler-case
       (progn ,@body)
     (error (e) (message "(:error ~s)" (princ-to-string e)))))

(defun find-group-by-number (num)
  (or (find num (screen-groups (current-screen)) :key #'group-number)
      (error "No such group.")))

(defun find-frame-by-group-and-number (group number)
  (or (find number (group-frames group) :key #'frame-number)
      (error "No such frame.")))

(defun find-window-by-id (id)
  (or (window-by-id id)
      (error "No such window.")))

;; This is just copy/paste of the same function in STUMPWM, except
;; that this takes the frame as an argument instead of using the
;; current frame.
(defun split-frame (group frame how &optional (ratio 1/2))
  (check-type how (member :row :column))
  (let ((head (frame-head group frame)))
    ;; don't create frames smaller than the minimum size
    (when (or (and (eq how :row)
                   (>= (frame-height frame) (* *min-frame-height* 2)))
              (and (eq how :column)
                   (>= (frame-width frame) (* *min-frame-width* 2))))
      (multiple-value-bind (f1 f2) (funcall (if (eq how :column)
                                                'split-frame-h
                                                'split-frame-v)
                                            group frame ratio)
        (setf (tile-group-frame-head group head)
              (if (atom (tile-group-frame-head group head))
                  (list f1 f2)
                  (funcall-on-node
                   (tile-group-frame-head group head)
                   (lambda (tree)
                     (if (eq (tree-split-type tree) how)
                         (list-splice-replace frame tree f1 f2)
                         (substitute (list f1 f2) frame tree)))
                   (lambda (tree)
                     (unless (atom tree)
                       (find frame tree))))))
        (migrate-frame-windows group frame f1)
        (choose-new-frame-window f2 group)
        (if (eq (tile-group-current-frame group)
                frame)
            (setf (tile-group-current-frame group) f1))
        (setf (tile-group-last-frame group) f2)
        (sync-frame-windows group f1)
        (sync-frame-windows group f2)
        ;; we also need to show the new window in the other frame
        (when (frame-window f2)
          (unhide-window (frame-window f2)))
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
      (focus-frame g (find-frame-by-group-and-number g frame-num)))))

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
                                      to-window-id)
    ((:number "From window ID: ")
     (:number "To window ID: "))
  (with-simple-error-handling
    (let* ((from-window (find-window-by-id from-window-id))
           (to-window (find-window-by-id to-window-id))
           (to-group (window-group to-window)))
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
    (message "~d" (group-number (add-group (current-screen) name :background t)))))

(defcommand stumpbuffer-delete-group (group-num)
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

(defcommand stumpbuffer-delete-window (window-id)
    ((:number "Window ID: "))
  (with-simple-error-handling
    (let* ((window (find-window-by-id window-id)))
      (delete-window window))))

(defcommand stumpbuffer-delete-frame (group-num frame-num)
    ((:number "Group number: ")
     (:number "Frame number: "))
  (with-simple-error-handling
    (let* ((group (find-group-by-number group-num))
           (frame (find-frame-by-group-and-number group frame-num)))
      (remove-split group frame))))

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
          (when (frame-window frame)
            (update-decoration (frame-window frame)))
          (error "Cannot split smaller than minimum size.")))))

(defcommand stumpbuffer-renumber-window (window-id new-number)
    ((:number "Window ID: ")
     (:number "New number: "))
  (with-simple-error-handling
    (let* ((window (find-window-by-id window-id))
           (current-number (window-number window))
           (group (window-group window))
           (old-window (find-if (lambda (win)
                                  (= (window-number win) new-number))
                                (group-windows group))))
      (if (null old-window)
          (setf (window-number window) new-number)
          (setf (window-number old-window) current-number
                (window-number window) new-number)))))

(defcommand stumpbuffer-renumber-group (group-num new-number)
    ((:number "Group number: ")
     (:number "New number: "))
  (with-simple-error-handling
    (let* ((group (find-group-by-number group-num))
           (old-group (ignore-errors
                       (find-group-by-number new-number))))
      (if (null old-group)
          (setf (group-number group) new-number)
          (setf (group-number old-group) group-num
                (group-number group) new-number)))))

(defvar *window-data-fields* nil)
(defvar *group-data-fields* nil)
(defvar *frame-data-fields* nil)

(defcommand stumpbuffer-get-data () ()
  "Retrieve information about groups and windows for StumpBuffer."
  (labels ((number (plist) (getf plist :number))
           (process-custom-fields (alist &rest args)
             (mappend (lambda (field)
                        (destructuring-bind (key . fn) field
                          (list key (apply fn args))))
                      alist))
           (window-plist (window)
             (list* :number (window-number window)
                    :frame (frame-number
                            (window-frame window))
                    :title (window-name window)
                    :class (window-class window)
                    :role (window-role window)
                    :instance (window-res window)
                    :id (window-id window)
                    :hiddenp (window-hidden-p window)
                    :visiblep (window-visible-p window)
                    :fullscreenp (window-fullscreen window)
                    (process-custom-fields *window-data-fields* window)))
           (frame-plist (group frame)
             (list* :number (frame-number frame)
                    :width (frame-width frame)
                    :height (frame-height frame)
                    :windows (sort
                              (mapcar #'window-plist (frame-windows group frame))
                              #'< :key #'number)
                    (process-custom-fields *frame-data-fields* group frame)))
           (group-plist (group)
             (let ((type (if (typep group 'float-group)
                             :float
                             :tile)))
               (list* :number (group-number group)
                      :name (group-name group)
                      :type type
                      :frames (if (eql type :tile)
                                  (sort (mapcar (lambda (frame)
                                                  (frame-plist group frame))
                                                (group-frames group))
                                        #'< :key #'number))
                      :hiddenp (char= (char (group-name group) 0)
                                      #\.)
                      (process-custom-fields *group-data-fields* group)))))
    (with-simple-error-handling
      (let ((*print-case* :downcase))
        (message "~s" (let ((groups (screen-groups (current-screen))))
                        (sort (mapcar #'group-plist groups)
                              #'< :key #'number)))))))
