(in-package #:stumpbuffer)

(defun find-group-by-number (num)
  (dolist (group (screen-groups (current-screen)))
    (when (= num (group-number group))
      (return group))))

(defun find-window-by-group-and-number (group number)
  (dolist (window (group-windows group))
    (when (= number (window-number window))
      (return window))))

(defcommand stumpbuffer-switch-to-group (group-num)
    ((:number "Group number: "))
  (let ((group (find-group-by-number group-num)))
    (when group (gselect group))))

(defcommand stumpbuffer-focus-window (group-num window-num)
    ((:number "Group number: ")
     (:number "Window number: "))
  (let ((g (find-group-by-number group-num)))
    (gselect g)
    (group-focus-window g (find-window-by-group-and-number g window-num))))

(defcommand stumpbuffer-pull-window (group-num window-num)
    ((:number "Group number: ")
     (:number "Window number: "))
  (let* ((group (find-group-by-number group-num))
         (window (find-window-by-group-and-number group window-num)))
    (move-window-to-group window (current-group))
    (stumpwm::pull-window window)))

(defcommand stumpbuffer-throw-window (from-group-num from-window-num
                                      to-group-num to-window-num)
    ((:number "From group number: ")
     (:number "From window number: ")
     (:number "To group number: ")
     (:number "To window number: "))
  (let* ((from-group (find-group-by-number from-group-num))
         (from-window (find-window-by-group-and-number
                       from-group from-window-num))
         (to-group (find-group-by-number to-group-num))
         (to-window (find-window-by-group-and-number
                     to-group to-window-num)))
    (move-window-to-group from-window to-group)
    (stumpwm::pull-window from-window (stumpwm::window-frame to-window))))

(defcommand stumpbuffer-rename-group (group-num name)
    ((:number "Group number: ")
     (:string "New name: "))
  (let* ((group (find-group-by-number group-num)))
    ;; This is largely copied from the standard GRENAME command.
    (cond ((stumpwm::find-group (current-screen) name)
           (message "Name already exists."))
          ((or (zerop (length name))
               (string= name "."))
           (message "Empty name."))
          (t (cond
               ((and (char= (char name 0) #\.)
                     (not (char= (char (group-name group) 0) #\.)))
                (setf (group-number group) (stumpwm::find-free-hidden-group-number
                                            (current-screen))))
               ((and (not (char= (char name 0) #\.))
                     (char= (char (group-name group) 0) #\.))
                (setf (group-number group) (stumpwm::find-free-group-number
                                            (current-screen)))))
             (setf (group-name group) name)))))

(defcommand stumpbuffer-rename-window (group-num window-num new-name)
    ((:number "Group number: ")
     (:number "Window number: ")
     (:string "New name: "))
  (let* ((group (find-group-by-number group-num))
         (window (find-window-by-group-and-number group window-num)))
    (setf (window-user-title window) new-name)))

(defcommand stumpbuffer-kill-window (group-num window-num)
    ((:number "Group number: ")
     (:number "Window number: "))
  (let* ((group (find-group-by-number group-num))
         (window (find-window-by-group-and-number group window-num)))
    (kill-window window)))

(defcommand stumpbuffer-get-data () ()
  ""
  (let ((*print-case* :downcase))
    (message "~s"
             (let ((groups (screen-groups (current-screen))))
               (sort (mapcar
                      (lambda (group)
                        (list :number (group-number group)
                              :name (group-name group)
                              :windows (sort (mapcar
                                              (lambda (window)
                                                (list :number (window-number window)
                                                      :frame (stumpwm::frame-number
                                                              (stumpwm::window-frame window))
                                                      :title (stumpwm::window-name window)
                                                      :class (window-class window)
                                                      :role (window-role window)
                                                      :instance (window-res window)))
                                              (group-windows group))
                                             #'< :key (lambda (win) (getf win :number)))))
                      groups)
                     #'< :key (lambda (group) (getf group :number)))))))
