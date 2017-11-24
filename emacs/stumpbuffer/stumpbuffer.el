;;; stumpbuffer.el --- A buffer to control Stumpwm

;; Copyright (C) 2017 juki

;; Author: juki <juki_pub@outlook.com>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A tool to control Stumpwm.

;;; Code:

(require 'cl-lib)

(defgroup stumpbuffer nil
  "A tool to control Stumpwm."
  :version "0.1")

(defcustom stumpbuffer-stumpish-command "stumpish"
  "The name of the command to use to communicate with Stumpwm."
  :type 'string
  :group 'stumpbuffer)

(defcustom stumpbuffer-quit-window-after-command t
  "Should stumpbuffer quit after executing a command like focus window."
  :type 'boolean
  :group 'stumpbuffer)

(defcustom stumpbuffer-mode-hook nil
  "Hook run upon entry into `stumpbuffer-mode'."
  :type 'hook
  :group 'stumpbuffer)

(defcustom stumpbuffer-load-hook nil
  "Hook run when stumpbuffer is loaded."
  :type 'hook
  :group 'stumpbuffer)

(defcustom stumpbuffer-group-face 'font-lock-keyword-face
  "Face used for the group name."
  :type 'face
  :group 'stumpbuffer)

(defcustom stumpbuffer-marked-face 'warning
  "Face used for marked windows."
  :type 'face
  :group 'stumpbuffer)

(defvar stumpbuffer-window-format '((:frame 1 "F")
                                    (:number 3 "N")
                                    (:title 35 "Title")
                                    (:class 10 "Class")
                                    (:role 10 "Role")
                                    (:instance nil "Instance"))
  "Format for displaying windows.")

(defvar stumpbuffer-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "n") 'stumpbuffer-forward-line)
    (define-key map (kbd "p") 'stumpbuffer-backward-line)
    (define-key map (kbd "TAB") 'stumpbuffer-forward-group)
    (define-key map [backtab] 'stumpbuffer-backward-group)
    (define-key map (kbd "g") 'stumpbuffer-update)
    (define-key map (kbd "U") 'stumpbuffer-unmark-all)
    (define-key map (kbd "d") 'stumpbuffer-mark-to-kill)
    (define-key map (kbd "x") 'stumpbuffer-execute-marks)
    (define-key map (kbd "% r") 'stumpbuffer-mark-by-window-title-regex)
    (define-key map (kbd "% f") 'stumpbuffer-mark-frame)
    (define-key map (kbd "% c") 'stumpbuffer-mark-by-window-class)
    (define-key map (kbd "% R") 'stumpbuffer-mark-by-window-role)
    (define-key map (kbd "% i") 'stumpbuffer-mark-by-window-instance)
    (define-key map (kbd "% g") 'stumpbuffer-mark-current-group)
    (define-key map (kbd "P") 'stumpbuffer-pull-windows)
    (define-key map (kbd "C") 'stumpbuffer-create-group)
    map))

(defvar stumpbuffer-mode-group-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'stumpbuffer-mark-group)
    (define-key map (kbd "u") 'stumpbuffer-unmark-group)
    (define-key map (kbd "RET") 'stumpbuffer-switch-to-group)
    (define-key map (kbd "T") 'stumpbuffer-throw-marked-windows-to-group)
    (define-key map (kbd "N") 'stumpbuffer-rename-group)
    (define-key map (kbd "D") 'stumpbuffer-kill-group)
    map))

(defvar stumpbuffer-mode-window-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'stumpbuffer-mark)
    (define-key map (kbd "u") 'stumpbuffer-unmark)
    (define-key map (kbd "RET") 'stumpbuffer-focus-window)
    (define-key map (kbd "D") 'stumpbuffer-kill-and-update)
    (define-key map (kbd "T") 'stumpbuffer-throw-marked-windows)
    (define-key map (kbd "N") 'stumpbuffer-rename-window)
    map))

(defvar stumpbuffer-mark-functions
  '((?D . stumpbuffer-kill))
  "An alist of mark character to functions to execute the mark.")

(defun stumpbuffer-command (command &rest args)
  (apply #'call-process stumpbuffer-stumpish-command
         nil nil nil (concat "stumpbuffer-" command) args))

(defun stumpbuffer-on-group-name ()
  "If point is on a group name, return info about it."
  (when-let ((group (get-text-property (point) 'stumpbuffer-group-number)))
    (list :start (point-at-bol)
          :end (point-at-eol)
          :group-plist (get-text-property (point) 'stumpbuffer-group-plist))))

(defun stumpbuffer-on-window ()
  "If point is on a window row, return info about it."
  (when-let ((group (get-text-property (point) 'stumpbuffer-group))
             (window (get-text-property (point) 'stumpbuffer-window)))
    (list* :group group
           :start (point-at-bol)
           :end (point-at-eol)
           :window-plist (get-text-property (point) 'stumpbuffer-window-plist)
           (when-let ((mark (get-text-property (point) 'stumpbuffer-mark)))
             (list :mark mark)))))

(defun stumpbuffer-map-groups (fn)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when-let ((group (stumpbuffer-on-group-name)))
        (save-excursion
          (funcall fn group)))
      (forward-line))))

(defun stumpbuffer-map-windows (fn)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when-let ((win (stumpbuffer-on-window)))
        (save-excursion
          (funcall fn win)))
      (forward-line))))

(defun stumpbuffer-map-group-windows (fn)
  (save-excursion
    (unless (stumpbuffer-on-group-name)
      (stumpbuffer-backward-group))
    (forward-line)
    (while (not (or (eobp)
                    (stumpbuffer-on-group-name)))
      (when-let ((win (stumpbuffer-on-window)))
        (save-excursion
          (funcall fn win)))
      (forward-line))))

(defun stumpbuffer-map-marked-windows (fn)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (get-text-property (point) 'stumpbuffer-mark)
        (when-let ((win (stumpbuffer-on-window)))
          (save-excursion
            (funcall fn win))))
      (forward-line))))

(defun stumpbuffer-change-window-mark (window mark)
  (unwind-protect
      (save-excursion
        (setq buffer-read-only nil)
        (goto-char (getf window :start))
        (move-to-column 2)
        (delete-char 1)
        (insert-and-inherit (cond ((null mark) ?\s)
                                  ((eql mark t) ?*)
                                  (t mark)))
        (add-text-properties
         (getf window :start)
         (getf window :end)
         (list 'stumpbuffer-mark mark
               'font-lock-face (when mark
                                 stumpbuffer-marked-face))))
    (setq buffer-read-only t)))

(defun stumpbuffer-mark-group (mark)
  "If point is on group, mark all windows in it."
  (interactive (list ?*))
  (when-let ((group (stumpbuffer-on-group-name)))
    (stumpbuffer-map-group-windows
     (lambda (win) 
       (stumpbuffer-change-window-mark win mark)))
    (stumpbuffer-forward-group)))

(defun stumpbuffer-mark (mark)
  "If point is on a window, mark it."
  (interactive (list ?*))
  (when-let ((win (stumpbuffer-on-window)))
    (stumpbuffer-change-window-mark win mark)
    (stumpbuffer-forward-line)))

(defun stumpbuffer-unmark-group ()
  "If point is on group, unmark all windows in it."
  (interactive)
  (when-let ((group (stumpbuffer-on-group-name)))
    (stumpbuffer-map-group-windows
     (lambda (win)
       (stumpbuffer-change-window-mark win nil)))
    (stumpbuffer-forward-group)))

(defun stumpbuffer-unmark ()
  "If point is on a window, unmark it."
  (interactive)
  (when-let ((win (stumpbuffer-on-window)))
    (stumpbuffer-change-window-mark win nil)
    (stumpbuffer-forward-line)))

(defun stumpbuffer-unmark-all ()
  "Remove all marks."
  (interactive)
  (stumpbuffer-map-marked-windows
   (lambda (win)
     (stumpbuffer-change-window-mark win nil))))

(defun stumpbuffer-mark-to-kill ()
  (interactive)
  (stumpbuffer-mark ?D))

(defun stumpbuffer-mark-by-window-title-regex (regex mark)
  (interactive (list (read-string "Regex: ")
                     ?*))
  (stumpbuffer-map-windows
   (lambda (win)
     (when-let ((title (getf (getf win :window-plist) :title)))
       (when (string-match regex title)
         (stumpbuffer-mark mark))))))

(defun stumpbuffer-mark-frame (frame mark)
  (interactive (list (getf (getf (stumpbuffer-on-window) :window-plist) :frame)
                     ?*))
  (stumpbuffer-map-group-windows
   (lambda (win)
     (when-let ((f (getf (getf win :window-plist) :frame)))
       (when (= f frame)
         (stumpbuffer-mark mark))))))

(defun stumpbuffer-mark-by-window-class (class mark)
  (interactive (list (getf (getf (stumpbuffer-on-window) :window-plist) :class)
                     ?*))
  (stumpbuffer-map-windows
   (lambda (win)
     (when (equal class (getf (getf win :window-plist) :class))
       (stumpbuffer-mark mark)))))

(defun stumpbuffer-mark-by-window-role (role mark)
  (interactive (list (getf (getf (stumpbuffer-on-window) :window-plist) :role)
                     ?*))
  (stumpbuffer-map-windows
   (lambda (win)
     (when (equal role (getf (getf win :window-plist) :role))
       (stumpbuffer-mark mark)))))

(defun stumpbuffer-mark-by-window-instance (instance mark)
  (interactive (list (getf (getf (stumpbuffer-on-window) :window-plist)
                           :instance)
                     ?*))
  (stumpbuffer-map-windows
   (lambda (win)
     (when (equal instance (getf (getf win :window-plist) :instance))
       (stumpbuffer-mark mark)))))

(defun stumpbuffer-mark-current-group (mark)
  (interactive (list ?*))
  (stumpbuffer-map-group-windows
   (lambda (win)
     (stumpbuffer-mark mark))))

(defun stumpbuffer-execute-marks ()
  (interactive)
  (stumpbuffer-map-marked-windows
   (lambda (win)
     (when-let ((mark (getf win :mark))
                (fn (cdr (assoc mark stumpbuffer-mark-functions))))
       (funcall fn win)
       (stumpbuffer-unmark))))
  (stumpbuffer-update))

(defun stumpbuffer-kill (win)
  (when-let ((group (getf win :group))
             (win (getf (getf win :window-plist) :number)))
    (stumpbuffer-command "kill-window"
                         (number-to-string group)
                         (number-to-string win))))

(defun stumpbuffer-kill-and-update (win)
  (interactive (list (stumpbuffer-on-window)))
  (when (yes-or-no-p (format "Kill window '%s'? "
                             (getf (getf win :window-plist)
                                   :title)))
    (stumpbuffer-kill win))
  (stumpbuffer-update))

(defun stumpbuffer-create-group (name)
  (interactive (list (read-string "New group name: ")))
  (when name
    (stumpbuffer-command "create-group"
                         name)
    (stumpbuffer-update)))

(defun stumpbuffer-kill-group (group)
  (interactive (list (getf (getf (stumpbuffer-on-group-name) :group-plist)
                           :number)))
  (when (and group
             (yes-or-no-p "Delete group? "))
    (stumpbuffer-command "kill-group"
                         (number-to-string group))
    (stumpbuffer-update)))

(defun stumpbuffer-forward-line ()
  (interactive)
  (beginning-of-line)
  (forward-line 1)
  (when (eobp)
    (goto-char (point-min))))

(defun stumpbuffer-backward-line ()
  (interactive)
  (when (= (point) (point-min))
    (goto-char (point-max)))
  (beginning-of-line)
  (forward-line -1))

(defun stumpbuffer-forward-group ()
  (interactive)
  (when (stumpbuffer-on-group-name)
    (stumpbuffer-forward-line))
  (goto-char (next-single-property-change (point)
                                          'stumpbuffer-group-number
                                          nil
                                          (point-max)))
  (when (= (point) (point-max))
    (goto-char (point-min)))
  (beginning-of-line))

(defun stumpbuffer-backward-group ()
  (interactive)
  (when (stumpbuffer-on-group-name)
    (stumpbuffer-backward-line))
  (goto-char (previous-single-property-change (point)
                                              'stumpbuffer-group-number
                                              nil
                                              (point-min)))
  (when (= (point) (point-min))
    (goto-char (point-max))
    (stumpbuffer-backward-group))
  (beginning-of-line))

(defun stumpbuffer-rename-group (group new-name)
  (interactive (list (get-text-property (point) 'stumpbuffer-group-number)
                     (read-string (format "Rename '%s': "
                                          (getf (get-text-property
                                                 (point) 'stumpbuffer-group-plist)
                                                :name)))))
  (when group
    (stumpbuffer-command "rename-group"
                         (number-to-string group)
                         new-name))
  (stumpbuffer-update))

(defun stumpbuffer-rename-window (group window new-name)
  (interactive (list (get-text-property (point) 'stumpbuffer-group)
                     (get-text-property (point) 'stumpbuffer-window)
                     (read-string (format "Rename '%s': "
                                          (getf (get-text-property
                                                 (point) 'stumpbuffer-window-plist)
                                                :title)))))
  (when (and group window)
    (stumpbuffer-command "rename-window"
                         (number-to-string group)
                         (number-to-string window)
                         new-name))
  (stumpbuffer-update))

(defun stumpbuffer-switch-to-group (group)
  (interactive (list (get-text-property (point) 'stumpbuffer-group-number)))
  (when group
    (stumpbuffer-command "switch-to-group"
                         (number-to-string group))
    (when stumpbuffer-quit-window-after-command
      (quit-window))))

(defun stumpbuffer-focus-window (group window)
  (interactive (list (get-text-property (point) 'stumpbuffer-group)
                     (get-text-property (point) 'stumpbuffer-window)))
  (when (and group window)
    (stumpbuffer-command "focus-window"
                         (number-to-string group)
                         (number-to-string window))
    (when stumpbuffer-quit-window-after-command
      (quit-window))))

(defun stumpbuffer-pull-windows ()
  (interactive)
  (cl-flet ((pull-window (win)
                         (stumpbuffer-command
                          "pull-window"
                          (number-to-string (getf win :group))
                          (number-to-string (getf (getf win :window-plist)
                                                  :number)))))
    (let (marksp)
      (stumpbuffer-map-marked-windows (lambda (win)
                                        (setq marksp t)
                                        (pull-window win)))
      (unless marksp
        (when-let ((win (stumpbuffer-on-window)))
          (pull-window win)))
      (when stumpbuffer-quit-window-after-command
        (quit-window)))))

(defun stumpbuffer-throw-marked-windows-to-group ()
  (interactive)
  (when-let ((group (get-text-property (point) 'stumpbuffer-group-number))
             (target-group (number-to-string group)))
    (stumpbuffer-map-marked-windows
     (lambda (win)
       (stumpbuffer-command "throw-window-to-group"
                            (number-to-string (getf win :group))
                            (number-to-string (getf (getf win :window-plist) :number))
                            target-group))))
  (stumpbuffer-update))

(defun stumpbuffer-throw-marked-windows ()
  (interactive)
  (when-let ((on-window (stumpbuffer-on-window))
             (target-group (number-to-string (getf on-window :group)))
             (target-window (number-to-string (getf (getf on-window :window-plist)
                                                    :number))))
    (stumpbuffer-map-marked-windows
     (lambda (win)
       (stumpbuffer-command "throw-window"
                            (number-to-string (getf win :group))
                            (number-to-string (getf (getf win :window-plist) :number))
                            target-group
                            target-window)))
    (stumpbuffer-update)))

(defun stumpbuffer-get-data ()
  (when (get-buffer "*stumpbuffer-data*")
    (kill-buffer "*stumpbuffer-data*"))
  (let ((output (get-buffer-create "*stumpbuffer-data*")))
          (call-process stumpbuffer-stumpish-command
                        nil output nil
                        "stumpbuffer-get-data")
          (let ((m (set-marker (make-marker) 1 output)))
            (prog1 (read m)
              (kill-buffer "*stumpbuffer-data*")))))

(defun stumpbuffer-set-header ()
  (let ((header (with-output-to-string
                  (princ "  M ")
                  (dolist (field stumpbuffer-window-format)
                    (destructuring-bind (field &optional width title)
                        field
                      (let* ((title (or title (symbol-name field)))
                             (len (length title))
                             (width (or width len)))
                        (princ (store-substring (make-string width ?\s)
                                                0
                                                (substring title 0 (min len width))))))
                    (princ " ")))))
    (setq header-line-format `(:eval (substring ,header
                                                (min (length ,header)
                                                     (window-hscroll)))))))

(defun stumpbuffer-insert-window (window-plist)
  (add-text-properties
   (point)
   (progn (insert "    ")
          (dolist (field stumpbuffer-window-format)
            (destructuring-bind (field &optional width title)
                field
              (let* ((entry (format "%s" (or (getf window-plist field) "")))
                     (len (length entry)))
                (insert (if width
                            (if (> len width)
                                (store-substring (make-string width ?.)
                                                 0
                                                 (substring entry 0
                                                            (- width 3)))
                              (store-substring (make-string width ?\s)
                                               0
                                               (substring entry 0 len)))
                          entry))))
            (insert " "))
          (point))
   `(keymap ,stumpbuffer-mode-window-map
            stumpbuffer-window ,(getf window-plist :number)
            stumpbuffer-window-plist ,window-plist))
  (insert "\n"))

(defun stumpbuffer-insert-group (group-plist)
  (destructuring-bind (&key number name windows &allow-other-keys)
      group-plist
    (add-text-properties
     (point)
     (progn (insert "[ ")
            (when number
              (insert (number-to-string number) " "))
            (when name
              (insert name))
            (insert " ]")
            (point))
     `(keymap ,stumpbuffer-mode-group-map
              font-lock-face ,stumpbuffer-group-face
              stumpbuffer-group-number ,number
              stumpbuffer-group-plist ,group-plist))
    (insert "\n")
    (unless (null windows)
      (put-text-property
       (point)
       (progn (mapc #'stumpbuffer-insert-window windows)
              (point))
       'stumpbuffer-group number))))

(defun stumpbuffer-update ()
  (interactive)
  (unwind-protect
      (progn
        (setq buffer-read-only nil)
        (erase-buffer)
        (stumpbuffer-set-header)
        (mapc #'stumpbuffer-insert-group (stumpbuffer-get-data)))
    (setq buffer-read-only t)
    (goto-char (point-min))))

(defun stumpbuffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*StumpBuffer*"))
  (stumpbuffer-mode)
  (stumpbuffer-update)
  (unwind-protect
      (progn
        (setq buffer-read-only nil)
        (run-hooks 'stumpbuffer-hook))
    (setq buffer-read-only t)))

(define-derived-mode stumpbuffer-mode special-mode "StumpBuffer"
  "A major mode for controlling Stumpwm."
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (hl-line-mode)
  )

(provide 'stumpbuffer)
(run-hooks 'stumpbuffer-load-hook)
;;; stumpbuffer.el ends here
