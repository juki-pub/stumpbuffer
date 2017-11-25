;;; stumpbuffer.el --- A buffer to control Stumpwm

;; Copyright (C) 2017 juki

;; Author: juki <juki_pub@outlook.com>
;; Version: 0.1

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; A tool to control Stumpwm.

;;; Code:

(require 'cl-lib)

(defgroup stumpbuffer nil
  "A tool to control Stumpwm."
  :version "0.1"
  :group 'external)

(defcustom stumpbuffer-stumpish-command "stumpish"
  "The name of the command to use to communicate with Stumpwm."
  :type 'string
  :group 'stumpbuffer)

(defcustom stumpbuffer-quit-window-after-command t
  "Should stumpbuffer quit after executing a command like focus window."
  :type 'boolean
  :group 'stumpbuffer)

(defcustom stumpbuffer-show-frames-p t
  "Should frames be shown?"
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

(defcustom stumpbuffer-frame-face 'font-lock-function-name-face
  "Face used for the frame name."
  :type 'face
  :group 'stumpbuffer)

(defcustom stumpbuffer-marked-face 'warning
  "Face used for marked windows."
  :type 'face
  :group 'stumpbuffer)

(defcustom stumpbuffer-window-field-t-character ?✓
  "The character to use for `t` values in window fields."
  :type 'character
  :group 'stumpbuffer)

(defvar stumpbuffer-kill-frame-on-exit-p nil)

(defvar stumpbuffer-window-format '((:number 3 "N")
                                    (:title 35 "Title")
                                    (:class 10 "Class")
                                    (:role 10 "Role")
                                    (:instance nil "Instance"))
  "Format for displaying windows.")

(defvar stumpbuffer-window-faces
  '((stumpbuffer-window-visible-p . bold)
    (stumpbuffer-window-hidden-p . shadow))
  "A list of (fn . face) pairs used to decide window face.")

(defvar stumpbuffer-group-filters nil
  "A list of functions to filter groups.")
(defvar stumpbuffer-window-filters nil
  "A list of functions to filter windows.")

(defvar stumpbuffer-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "n") 'stumpbuffer-forward-line)
    (define-key map (kbd "p") 'stumpbuffer-backward-line)
    (define-key map (kbd "C-n") 'stumpbuffer-forward-frame)
    (define-key map (kbd "C-p") 'stumpbuffer-backward-frame)
    (define-key map (kbd "TAB") 'stumpbuffer-forward-group)
    (define-key map [backtab] 'stumpbuffer-backward-group)
    (define-key map (kbd "g") 'stumpbuffer-update)
    (define-key map (kbd "U") 'stumpbuffer-unmark-all)
    (define-key map (kbd "d") 'stumpbuffer-mark-to-kill)
    (define-key map (kbd "x") 'stumpbuffer-execute-marks)
    (define-key map (kbd "% r") 'stumpbuffer-mark-by-window-title-regex)
    (define-key map (kbd "% f") 'stumpbuffer-mark-by-frame)
    (define-key map (kbd "% c") 'stumpbuffer-mark-by-window-class)
    (define-key map (kbd "% R") 'stumpbuffer-mark-by-window-role)
    (define-key map (kbd "% i") 'stumpbuffer-mark-by-window-instance)
    (define-key map (kbd "% g") 'stumpbuffer-mark-current-group)
    (define-key map (kbd "C") 'stumpbuffer-create-group)
    (define-key map (kbd "q") 'stumpbuffer-quit-window)
    (define-key map (kbd "P") 'stumpbuffer-pull-windows)
    (define-key map (kbd "*") 'stumpbuffer-change-marks)
    map))

(defvar stumpbuffer-mode-group-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'stumpbuffer-mark-group)
    (define-key map (kbd "u") 'stumpbuffer-unmark-group)
    (define-key map (kbd "RET") 'stumpbuffer-switch-to-group)
    (define-key map (kbd "T") 'stumpbuffer-throw-marked-windows-to-group)
    (define-key map (kbd "N") 'stumpbuffer-rename-group)
    (define-key map (kbd "D") 'stumpbuffer-kill-group)
    (define-key map (kbd "d") 'stumpbuffer-mark-group-for-kill)
    map))

(defvar stumpbuffer-mode-frame-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'stumpbuffer-mark-frame)
    (define-key map (kbd "u") 'stumpbuffer-unmark-frame)
    (define-key map (kbd "d") 'stumpbuffer-mark-frame-for-kill)
    (define-key map (kbd "D") 'stumpbuffer-kill-frame)
    (define-key map (kbd "T") 'stumpbuffer-throw-marked-windows-to-frame)
    (define-key map (kbd "s") 'stumpbuffer-split-frame-vertical)
    (define-key map (kbd "S") 'stumpbuffer-split-frame-horizontal)
    (define-key map (kbd "RET") 'stumpbuffer-focus-frame)
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


;;; Executing commands

(defun stumpbuffer-command (command &rest args)
  "Execute a Stumpwm command.

The command name will be prepended with `stumpbuffer-`. The
command should return something that Emacs can read, which will
be the return value.

As a simple error handling mechanism, the command may return a
two element list `(:error msg)`. An Emacs error will be
signalled with the message."
  (let ((output (get-buffer-create "*stumpbuffer-data*")))
    (apply #'call-process stumpbuffer-stumpish-command
           nil output nil
           (concat "stumpbuffer-" command) args)
    (unless (zerop (buffer-size output))
      (let ((m (set-marker (make-marker) 1 output)))
        (when-let (result (prog1 (read m)
                            (kill-buffer "*stumpbuffer-data*")))
          (if (and (listp result)
                   (eql :error (first result)))
              (error "StumpBuffer error: %s" (second result))
            result))))))


;;; Retrieving info about things

(defun stumpbuffer-on-group-name ()
  "If point is on a group name, return info about it."
  (when-let ((group (get-text-property (point) 'stumpbuffer-group-number)))
    (list :start (point-at-bol)
          :end (point-at-eol)
          :group-plist (get-text-property (point) 'stumpbuffer-group-plist))))

(defun sb--current-group-plist ()
  (getf (stumpbuffer-on-group-name) :group-plist))

(defun stumpbuffer-on-frame-name ()
  "If point is on a frame name, return info about it."
  (when-let ((group (get-text-property (point) 'stumpbuffer-group))
             (frame (get-text-property (point) 'stumpbuffer-frame-number)))
    (list :group group
          :start (point-at-bol)
          :end (point-at-eol)
          :frame-plist (get-text-property (point) 'stumpbuffer-frame-plist))))

(defun sb--current-frame-plist ()
  (getf (stumpbuffer-on-frame-name) :frame-plist))

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

(defun sb--current-window-plist ()
  (getf (stumpbuffer-on-window) :window-plist))

(defun sb--get-window-face (window)
  "Get the appropriate face for window."
  (dolist (pair stumpbuffer-window-faces)
    (destructuring-bind (fn . face) pair
      (when (funcall fn window)
        (return face)))))


;;; Navigating

(defun stumpbuffer-forward-line ()
  "Move forward one line. Wraps around."
  (interactive)
  (beginning-of-line)
  (forward-line 1)
  (when (eobp)
    (goto-char (point-min))))

(defun stumpbuffer-backward-line ()
  "Move backward one line. Wraps around."
  (interactive)
  (when (= (point) (point-min))
    (goto-char (point-max)))
  (beginning-of-line)
  (forward-line -1))

(defun stumpbuffer-forward-frame ()
  "Move to the next frame name. Wraps around."
  (interactive)
  (if (not stumpbuffer-show-frames-p)
      (stumpbuffer-forward-line)
    (when (stumpbuffer-on-frame-name)
      (stumpbuffer-forward-line))
    (goto-char (next-single-property-change (point)
                                            'stumpbuffer-frame-number
                                            nil
                                            (point-max)))
    (when (= (point) (point-max))
      (goto-char (point-min))
      (stumpbuffer-forward-frame))
    (beginning-of-line)))

(defun stumpbuffer-backward-frame ()
  "Move to the previous frame name. Wraps around."
  (interactive)
  (if (not stumpbuffer-show-frames-p)
      (stumpbuffer-backward-line)
    (goto-char (previous-single-property-change (point)
                                                'stumpbuffer-frame-number
                                                nil
                                                (point-min)))
    (when (= (point) (point-min))
      (goto-char (point-max))
      (stumpbuffer-backward-frame))
    (beginning-of-line)))

(defun stumpbuffer-forward-group ()
  "Move to the next group name. Wraps around."
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
  "Move to the previous group name. Wraps around."
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


;;; Iterating things

(defun stumpbuffer-map-groups (fn)
  "Call fn with each group.

The function should take a single argument; the plist returned by
`stumpbuffer-on-group-name'. The function is called with point on
the group name.

The results are discarded."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when-let ((group (stumpbuffer-on-group-name)))
        (save-excursion
          (funcall fn group)))
      (forward-line))))

(defmacro stumpbuffer-do-groups (arglist &rest body)
  "Iterate over all groups.

    (stumpbuffer-do-groups (group) ...)

is short for

    (stumpbuffer-map-groups (lambda (group) ...))
"
  (declare (indent 1))
  `(stumpbuffer-map-groups (lambda ,arglist ,@body)))

(defun stumpbuffer-map-windows (fn)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when-let ((win (stumpbuffer-on-window)))
        (save-excursion
          (funcall fn win)))
      (forward-line))))

(defmacro stumpbuffer-do-windows (arglist &rest body)
  (declare (indent 1))
  `(stumpbuffer-map-windows (lambda ,arglist ,@body)))

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

(defmacro stumpbuffer-do-group-windows (arglist &rest body)
  (declare (indent 1))
  `(stumpbuffer-map-group-windows (lambda ,arglist ,@body)))

(defun stumpbuffer-map-marked-windows (fn)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when-let ((win (stumpbuffer-on-window)))
        (when (getf win :mark)
          (save-excursion
            (funcall fn win))))
      (forward-line))))

(defmacro stumpbuffer-do-marked-windows (arglist &rest body)
  (declare (indent 1))
  `(stumpbuffer-map-marked-windows (lambda ,arglist ,@body)))


;;; Commands

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
               'face (if mark
                         stumpbuffer-marked-face
                       (sb--get-window-face (getf window :window-plist))))))
    (setq buffer-read-only t)))

(defun sb--maybe-prompt-for-mark (default)
  (if current-prefix-arg
      (read-char "Mark character: ")
    default))

(defun stumpbuffer-mark-group (group mark)
  "If point is on group, mark all windows in it."
  (interactive (list (stumpbuffer-on-group-name)
                     (sb--maybe-prompt-for-mark ?*)))
  (when group
    (stumpbuffer-do-group-windows (win)
      (stumpbuffer-change-window-mark win mark))
    (stumpbuffer-forward-group)))

(defun stumpbuffer-mark-frame (mark)
  "If point is on a frame, mark all windows in it."
  (interactive (list (sb--maybe-prompt-for-mark ?*)))
  (when-let ((frame-num (getf (sb--current-frame-plist) :number)))
    (stumpbuffer-do-group-windows (win)
      (when (= frame-num (getf (getf win :window-plist) :frame))
        (stumpbuffer-change-window-mark win mark)))
    (stumpbuffer-forward-frame)))

(defun stumpbuffer-mark (mark)
  "If point is on a window, mark it."
  (interactive (list (sb--maybe-prompt-for-mark ?*)))
  (when-let ((win (stumpbuffer-on-window)))
    (stumpbuffer-change-window-mark win mark)
    (stumpbuffer-forward-line)))

(defun stumpbuffer-unmark-group (group)
  "If point is on group, unmark all windows in it."
  (interactive (list (stumpbuffer-on-group-name)))
  (stumpbuffer-mark-group group nil))

(defun stumpbuffer-unmark-frame ()
  "If point is on group, unmark all windows in it."
  (interactive)
  (stumpbuffer-mark-frame nil))

(defun stumpbuffer-unmark ()
  "If point is on a window, unmark it."
  (interactive)
  (stumpbuffer-mark nil))

(defun stumpbuffer-unmark-all ()
  "Remove all marks."
  (interactive)
  (stumpbuffer-do-marked-windows (win)
    (stumpbuffer-change-window-mark win nil)))

(defun stumpbuffer-mark-to-kill ()
  (interactive)
  (stumpbuffer-mark ?D))

(defun stumpbuffer-mark-by-window-title-regex (regex mark)
  (interactive (list (read-string "Regex: ")
                     (sb--maybe-prompt-for-mark ?*)))
  (stumpbuffer-do-windows (win)
    (when-let ((title (getf (getf win :window-plist) :title)))
      (when (string-match regex title)
        (stumpbuffer-mark mark)))))

(defun stumpbuffer-mark-by-frame (frame mark)
  (interactive (list (getf (sb--current-window-plist) :frame)
                     (sb--maybe-prompt-for-mark ?*)))
  (stumpbuffer-do-group-windows (win)
    (when-let ((f (getf (getf win :window-plist) :frame)))
      (when (= f frame)
        (stumpbuffer-mark mark)))))

(defun stumpbuffer-mark-frame-for-kill (frame)
  (interactive (list (getf (sb--current-frame-plist) :number)))
  (stumpbuffer-mark-by-frame frame ?D)
  (stumpbuffer-forward-frame))

(defun stumpbuffer-mark-group-for-kill (group)
  (interactive (list (getf (sb--current-group-plist) :number)))
  (stumpbuffer-mark-group group ?D))

(defun sb--get-all-window-values (field)
  (let (result)
    (stumpbuffer-do-windows (win)
      (when-let ((val (getf (getf win :window-plist)
                            field)))
        (push val result)))
    result))

(defun stumpbuffer-mark-by-window-class (class mark)
  (interactive (list (or (unless current-prefix-arg
                           (getf (sb--current-window-plist) :class))
                         (completing-read "Class: "
                                          (sb--get-all-window-values :class)))
                     (sb--maybe-prompt-for-mark ?*)))
  (stumpbuffer-do-windows (win)
    (when (equal class (getf (getf win :window-plist) :class))
      (stumpbuffer-mark mark))))

(defun stumpbuffer-mark-by-window-role (role mark)
  (interactive (list (or (unless current-prefix-arg
                           (getf (sb--current-window-plist) :role))
                         (completing-read "Role: "
                                          (sb--get-all-window-values :role)))
                     (sb--maybe-prompt-for-mark ?*)))
  (stumpbuffer-do-windows (win)
    (when (equal role (getf (getf win :window-plist) :role))
      (stumpbuffer-mark mark))))

(defun stumpbuffer-mark-by-window-instance (instance mark)
  (interactive (list (or (unless current-prefix-arg
                           (getf (sb--current-window-plist) :instance))
                         (completing-read "Instance: "
                                          (sb--get-all-window-values :instance)))
                     (sb--maybe-prompt-for-mark ?*)))
  (stumpbuffer-do-windows (win)
    (when (equal instance (getf (getf win :window-plist) :instance))
      (stumpbuffer-mark mark))))

(defun stumpbuffer-mark-current-group (mark)
  (interactive (list (sb--maybe-prompt-for-mark ?*)))
  (stumpbuffer-do-group-windows (win)
    (stumpbuffer-mark mark)))

(defun stumpbuffer-change-marks (mark)
  (interactive (list (read-char "Mark: ")))
  (stumpbuffer-do-marked-windows (win)
    (when (char-equal ?* (getf win :mark))
      (stumpbuffer-mark mark))))

(defun stumpbuffer-execute-marks ()
  (interactive)
  (when (yes-or-no-p "Execute marks? ")
    (stumpbuffer-do-marked-windows (win)
      (when-let ((mark (getf win :mark))
                 (fn (cdr (assoc mark stumpbuffer-mark-functions))))
        (funcall fn win)
        (stumpbuffer-unmark)))
    (stumpbuffer-update)))

(defun stumpbuffer-kill (win)
  (when-let ((win (getf (getf win :window-plist) :id)))
    (stumpbuffer-command "kill-window"
                         (number-to-string win))))

(defun stumpbuffer-kill-and-update (win)
  (interactive (list (stumpbuffer-on-window)))
  (when (yes-or-no-p (format "Kill window '%s'? "
                             (getf (getf win :window-plist)
                                   :title)))
    (stumpbuffer-kill win)
    (stumpbuffer-update)
    (message "Killed windows may take a moment to die. Use `g` to update.")))

(defun stumpbuffer-create-group (name)
  (interactive (list (read-string "New group name: ")))
  (when name
    (stumpbuffer-throw-marked-windows-to-group
     (stumpbuffer-command "create-group" name))
    (stumpbuffer-update)))

(defun stumpbuffer-kill-group (group)
  (interactive (list (getf (sb--current-group-plist) :number)))
  (when (and group
             (yes-or-no-p "Delete group? "))
    (stumpbuffer-command "kill-group"
                         (number-to-string group))
    (stumpbuffer-update)))

(defun stumpbuffer-kill-frame (group frame)
  (interactive (let ((on-frame (stumpbuffer-on-frame-name)))
                 (list (getf on-frame :group)
                       (getf (getf on-frame :frame-plist) :number))))
  (when (and group frame)
    (stumpbuffer-command "kill-frame"
                         (number-to-string group)
                         (number-to-string frame))
    (stumpbuffer-update)))

(defun stumpbuffer-split-frame-vertical (group frame)
  (interactive (let ((on-frame (stumpbuffer-on-frame-name)))
                 (list (getf on-frame :group)
                       (getf (getf on-frame :frame-plist) :number))))
  (when (and group frame)
    (stumpbuffer-command "split-frame"
                         (number-to-string group)
                         (number-to-string frame)
                         "2")
    (stumpbuffer-update)))

(defun stumpbuffer-split-frame-horizontal (group frame)
  (interactive (let ((on-frame (stumpbuffer-on-frame-name)))
                 (list (getf on-frame :group)
                       (getf (getf on-frame :frame-plist) :number))))
  (when (and group frame)
    (stumpbuffer-command "split-frame"
                         (number-to-string group)
                         (number-to-string frame)
                         "1")
    (stumpbuffer-update)))

(defun stumpbuffer-rename-group (group new-name)
  (interactive (let ((gplist (sb--current-group-plist)))
                 (list (getf gplist :number)
                       (read-string (format "Rename '%s': "
                                            (getf gplist :name))))))
  (when group
    (stumpbuffer-command "rename-group"
                         (number-to-string group)
                         new-name))
  (stumpbuffer-update))

(defun stumpbuffer-rename-window (window-id new-name)
  (interactive (let ((wplist (sb--current-window-plist)))
                 (list (getf wplist :id)
                       (read-string (format "Rename '%s': "
                                            (getf wplist :title))))))
  (when window-id
    (stumpbuffer-command "rename-window"
                         (number-to-string window-id)
                         new-name))
  (stumpbuffer-update))

(defun stumpbuffer-switch-to-group (group)
  (interactive (list (getf (sb--current-group-plist) :number)))
  (when group
    (stumpbuffer-command "switch-to-group"
                         (number-to-string group))
    (when stumpbuffer-quit-window-after-command
      (stumpbuffer-quit-window))))

(defun stumpbuffer-focus-frame (group frame)
  (interactive (let ((frame (stumpbuffer-on-frame-name)))
                 (list (getf frame :group)
                       (getf (getf frame :frame-plist) :number))))
  (when (and group frame)
    (stumpbuffer-command "focus-frame"
                         (number-to-string group)
                         (number-to-string frame))
    (when stumpbuffer-quit-window-after-command
      (stumpbuffer-quit-window))))

(defun stumpbuffer-focus-window (group window)
  (interactive (let ((win (stumpbuffer-on-window)))
                 (list (getf win :group)
                       (getf (getf win :window-plist) :id))))
  (when (and group window)
    (stumpbuffer-command "focus-window"
                         (number-to-string group)
                         (number-to-string window))
    (when stumpbuffer-quit-window-after-command
      (stumpbuffer-quit-window))))

(defun stumpbuffer-pull-windows (&optional window)
  "Pull windows to the currently focused frame.

If a window is given as an argument, it will be pulled. Otherwise
all buffers marked with `*` are pulled. If there are no such
marked windows, the window at point will be pulled."
  (interactive (list nil))
  (cl-flet ((pull-window (win)
                         (stumpbuffer-command
                          "pull-window"
                          (number-to-string (getf (getf win :window-plist)
                                                  :id)))))
    (if window
        (pull-window window)
      (let (marksp)
        (stumpbuffer-do-marked-windows (win)
          (when (char-equal ?* (getf win :mark))
            (setq marksp t)
            (pull-window win)))
        (unless marksp
          (when-let ((win (stumpbuffer-on-window)))
            (pull-window win)))
        (when stumpbuffer-quit-window-after-command
          (stumpbuffer-quit-window))))))

(defun stumpbuffer-throw-marked-windows-to-group (group-number &optional followp)
  "Move all `*` marked windows to a group.

With a prefix argument this also switches to the group."
  (interactive (list (getf (sb--current-group-plist) :number)
                     current-prefix-arg))
  (let ((target (number-to-string group-number)))
    (stumpbuffer-do-marked-windows (win)
      (when (char-equal ?* (getf win :mark))
        (stumpbuffer-command "throw-window-to-group"
                             (number-to-string (getf (getf win :window-plist) :id))
                             target))))
  (stumpbuffer-update)
  (when followp
    (stumpbuffer-switch-to-group group-number)))

(defun stumpbuffer-throw-marked-windows-to-frame (group frame &optional followp)
  "Move all `*` marked windows to a frame.

With a prefix argument this also focuses the frame."
  (interactive (let ((frame (stumpbuffer-on-frame-name)))
                 (list (getf frame :group)
                       (getf (getf frame :frame-plist) :number)
                       current-prefix-arg)))
  (when-let ((target-group (number-to-string group))
             (target-frame (number-to-string frame)))
    (stumpbuffer-do-marked-windows (win)
      (when (char-equal ?* (getf win :mark))
        (stumpbuffer-command "throw-window-to-frame"
                             (number-to-string (getf (getf win :window-plist) :id))
                             target-group
                             target-frame))))
  (stumpbuffer-update)
  (when followp
    (stumpbuffer-focus-frame group frame)))

(defun stumpbuffer-throw-marked-windows (target-window-id &optional followp)
  "Move all `*` marked windows to a window.

With a prefix argument this also focuses the window."
  (interactive (list (getf (sb--current-window-plist) :id)
                     current-prefix-arg))
  (when-let ((target-window (number-to-string target-window-id)))
    (stumpbuffer-do-marked-windows (win)
      (when (char-equal ?* (getf win :mark))
        (stumpbuffer-command "throw-window"
                             (number-to-string (getf (getf win :window-plist) :id))
                             target-window)))
    (stumpbuffer-update)
    (when followp
      (let ((twindow (stumpbuffer-on-window)))
        (stumpbuffer-focus-window (getf twindow :group)
                                  target-window-id)))))


;;; Retrieving data and updating

(defun sb--get-data ()
  (stumpbuffer-command "get-data"))

(defun sb--set-header ()
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

(defun sb--filter-window-p (window)
  (some (lambda (filter) (funcall filter window))
        stumpbuffer-window-filters))

(defun sb--insert-window (window-plist)
  (unless (sb--filter-window-p window-plist)
    (add-text-properties
     (point)
     (progn (insert "    ")
            (dolist (field stumpbuffer-window-format)
              (destructuring-bind (field &optional width title)
                  field
                (let* ((entry (format "%s"
                                      (if-let ((value (getf window-plist field)))
                                          (if (eql value t)
                                              (string stumpbuffer-window-field-t-character)
                                            value)
                                        "")))
                       (len (length entry)))
                  (insert (if width
                              (if (> len width)
                                  (format (concat "%-"
                                                  (number-to-string (- width 3))
                                                  "."
                                                  (number-to-string (- width 3))
                                                  "s...")
                                          entry)
                                (format (concat "%-" (number-to-string width) "s")
                                        entry))
                            entry))))
              (insert " "))
            (point))
     `(keymap ,stumpbuffer-mode-window-map
              stumpbuffer-window ,(getf window-plist :number)
              stumpbuffer-window-id ,(getf window-plist :id)
              stumpbuffer-window-plist ,window-plist
              face ,(sb--get-window-face window-plist)))
    (insert "\n")))

(defun sb--insert-frame (frame-plist)
  (destructuring-bind (&key number windows &allow-other-keys)
      frame-plist
    (when stumpbuffer-show-frames-p
      (add-text-properties
       (point)
       (progn (insert "Frame " (number-to-string number))
              (point))
       `(keymap ,stumpbuffer-mode-frame-map
                face ,stumpbuffer-frame-face
                stumpbuffer-frame-number ,number
                stumpbuffer-frame-plist ,frame-plist))
      (insert "\n"))
    (unless (null windows)
      (put-text-property
       (point)
       (progn (mapc #'sb--insert-window windows)
              (point))
       'stumpbuffer-frame number))))

(defun sb--filter-group-p (group)
  (some (lambda (filter) (funcall filter group))
        stumpbuffer-group-filters))

(defun sb--insert-group (group-plist)
  (unless (sb--filter-group-p group-plist)
    (destructuring-bind (&key number name frames type &allow-other-keys)
        group-plist
      (add-text-properties
       (point)
       (progn (insert "[ ")
              (when number
                (insert (number-to-string number) " "))
              (when name
                (insert name))
              (insert " ]")
              (when (eql type :float)
                (insert " Float groups don't work yet!"))
              (point))
       `(keymap ,stumpbuffer-mode-group-map
                face ,stumpbuffer-group-face
                stumpbuffer-group-number ,number
                stumpbuffer-group-plist ,group-plist))
      (insert "\n")
      (unless (null frames)
        (put-text-property
         (point)
         (progn (mapc #'sb--insert-frame frames)
                (point))
         'stumpbuffer-group number)))))

(defun stumpbuffer-update ()
  (interactive)
  (let (active-marks)
    (stumpbuffer-do-windows (win)
      (when-let ((mark (getf win :mark)))
        (push (cons (getf (getf win :window-plist) :id)
                    mark)
              active-marks)))
    (let ((position (count-lines (point-min) (point))))
      (unwind-protect
          (progn
            (setq buffer-read-only nil)
            (erase-buffer)
            (sb--set-header)
            (mapc #'sb--insert-group (sb--get-data)))
        (setq buffer-read-only t)
        (when active-marks
          (stumpbuffer-do-windows (win)
            (when-let ((id (getf (getf win :window-plist) :id))
                       (mark (cdr (assoc id active-marks))))
              (stumpbuffer-mark mark))))
        (goto-char (point-min))
        (dotimes (i position)
          (stumpbuffer-forward-line))))))


;;; Entry / exit

(defun stumpbuffer-quit-window ()
  "Exit the buffer.

Kills the frame if necessary."
  (interactive)
  (when-let ((buf (get-buffer "*StumpBuffer*")))
    (kill-buffer buf))
  (when stumpbuffer-kill-frame-on-exit-p
    (delete-frame)))

(defun stumpbuffer (other-frame-p kill-frame-p)
  "Open a StumpBuffer.

If other-frame-p is true, open in another frame. If kill-frame-p
is true, the frame will be killed upon exiting the buffer.

Something like `emacsclient -c --eval \"(stumpbuffer nil t)\"`
can be used to open a buffer from outside emacs."
  (interactive (list nil nil))
  (let ((buffer (get-buffer-create "*StumpBuffer*")))
    (if other-frame-p
        (switch-to-buffer-other-frame buffer)
      (switch-to-buffer buffer)))
  (setq stumpbuffer-kill-frame-on-exit-p kill-frame-p)
  (stumpbuffer-mode)
  (stumpbuffer-update)
  (setq buffer-read-only t))

(defun stumpbuffer-other-frame ()
  (interactive)
  (stumpbuffer t t))

(define-derived-mode stumpbuffer-mode special-mode "StumpBuffer"
  "A major mode for controlling Stumpwm."
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (hl-line-mode)
  (set (make-local-variable 'stumpbuffer-kill-frame-on-exit-p) nil)
  (make-local-variable 'stumpbuffer-group-filters)
  (make-local-variable 'stumpbuffer-window-filters))


;;; Filter/face utilities

(defun stumpbuffer-group-hidden-p (group)
  (getf group :hiddenp))

(defun stumpbuffer-window-hidden-p (window)
  (getf window :hiddenp))

(defun stumpbuffer-window-visible-p (window)
  (getf window :visiblep))

(defun stumpbuffer-window-fullscreen-p (window)
  (getf window :fullscreenp))


(provide 'stumpbuffer)
(run-hooks 'stumpbuffer-load-hook)
;;; stumpbuffer.el ends here
