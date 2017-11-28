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
(require 'cl-extra)
(require 'subr-x)
(require 'pcase)

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

(defcustom stumpbuffer-marked-face 'warning
  "Face used for marked windows."
  :type 'face
  :group 'stumpbuffer)

(defcustom stumpbuffer-float-title-face 'font-lock-function-name-face
  "Face used for the 'floats' title line."
  :type 'face
  :group 'stumpbuffer)

(defcustom stumpbuffer-window-field-t-character ?✓
  "The character to use for `t` values in window fields."
  :type 'character
  :group 'stumpbuffer)

(defcustom stumpbuffer-stumpish-quote-arguments-with-spaces-p nil
  "Should arguments with spaces be quoted?

Only set to T if your Stumpwm supports that."
  :type 'boolean
  :group 'stumpbuffer)

(defvar sb--kill-frame-on-exit-p nil)

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

(defvar stumpbuffer-frame-name-format
  '((font-lock-function-name-face "Frame " :number)
    (shadow " (" :width " x " :height ")")))

(defvar stumpbuffer-group-name-format
  '((font-lock-keyword-face
     "[ " :number " " :name)
    (warning (:call (lambda (plist)
                      (when (eql (cl-getf plist :type) :float)
                        " Float"))))
    (font-lock-keyword-face " ]")
    ;; (warning (:call (lambda (plist)
    ;;                   (when (eql (cl-getf plist :type) :float)
    ;;                     " Float groups don't work yet!"))))
    ))

(defvar sb--active-filter-group nil)
(defvar sb--active-filter-group-n 0)

(defvar stumpbuffer-filter-groups
  '(("Everything")
    ("No hidden groups"
     (:hide-groups :satisfying stumpbuffer-group-hidden-p))
    ("Only hidden groups"
     (:show-groups :satisfying stumpbuffer-group-hidden-p))))

(defvar stumpbuffer-quick-filter-stack nil)

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
    (define-key map (kbd "f") 'stumpbuffer-toggle-frame-showing)
    (define-key map (kbd "`") 'stumpbuffer-cycle-filter-groups)
    (define-key map (kbd "^") 'stumpbuffer-select-filter-group)
    (define-key map (kbd "\\") 'stumpbuffer-pop-quick-filter)
    (define-key map (kbd "/ h") 'stumpbuffer-push-hide-hidden-groups-filter)
    (define-key map (kbd "/ H") 'stumpbuffer-push-show-hidden-groups-filter)
    (define-key map (kbd "/ r") 'stumpbuffer-push-show-matching-windows-filter)
    (define-key map (kbd "/ c") 'stumpbuffer-push-show-windows-by-class-filter)
    (define-key map (kbd "/ R") 'stumpbuffer-push-show-windows-by-role-filter)
    (define-key map (kbd "/ i") 'stumpbuffer-push-show-windows-by-instance-filter)
    map))

(defvar stumpbuffer-mode-group-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'stumpbuffer-mark-group)
    (define-key map (kbd "u") 'stumpbuffer-unmark-group)
    (define-key map (kbd "RET") 'stumpbuffer-switch-to-group)
    (define-key map (kbd "T") 'stumpbuffer-throw-marked-windows-to-group)
    (define-key map (kbd "N") 'stumpbuffer-rename-group)
    (define-key map (kbd "D") 'stumpbuffer-delete-group)
    (define-key map (kbd "d") 'stumpbuffer-mark-group-for-delete)
    (define-key map (kbd "k") 'stumpbuffer-mark-group-for-kill)
    (define-key map (kbd "r") 'stumpbuffer-renumber-group)
    map))

(defvar stumpbuffer-mode-frame-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'stumpbuffer-mark-frame)
    (define-key map (kbd "u") 'stumpbuffer-unmark-frame)
    (define-key map (kbd "d") 'stumpbuffer-mark-frame-for-delete)
    (define-key map (kbd "k") 'stumpbuffer-mark-frame-for-kill)
    (define-key map (kbd "D") 'stumpbuffer-delete-frame)
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
    (define-key map (kbd "d") 'stumpbuffer-mark-to-delete)
    (define-key map (kbd "D") 'stumpbuffer-delete-and-update)
    (define-key map (kbd "k") 'stumpbuffer-mark-to-kill)
    (define-key map (kbd "K") 'stumpbuffer-kill-and-update)
    (define-key map (kbd "T") 'stumpbuffer-throw-marked-windows)
    (define-key map (kbd "N") 'stumpbuffer-rename-window)
    (define-key map (kbd "s") 'stumpbuffer-split-window-frame-vertical)
    (define-key map (kbd "S") 'stumpbuffer-split-window-frame-horizontal)
    (define-key map (kbd "r") 'stumpbuffer-renumber-window)
    map))

(defvar stumpbuffer-mark-functions
  '((?D . stumpbuffer-delete)
    (?K . stumpbuffer-kill))
  "An alist of mark character to functions to execute the mark.")


;;; Executing commands

(defun sb--process-arg (arg)
  (cl-typecase arg
    (string (if (and stumpbuffer-stumpish-quote-arguments-with-spaces-p
                     (cl-position ?\s arg))
                (format "\"%s\"" (replace-regexp-in-string "\"" "\\\\\"" arg))
              arg))
    (number (number-to-string arg))))

(defun stumpbuffer-command (command &rest args)
  "Execute a Stumpwm command.

The command name will be prepended with `stumpbuffer-`. The
command should return something that Emacs can read, which will
be the return value.

As a simple error handling mechanism, the command may return a
two element list `(:error msg)`. An Emacs error will be
signalled with the message."
  (with-temp-buffer
    (apply #'call-process stumpbuffer-stumpish-command
           nil t nil
           (concat "stumpbuffer-" command)
           (mapcar #'sb--process-arg args))
    (unless (zerop (buffer-size))
      (let ((m (set-marker (make-marker) 1)))
        (when-let (result (prog1 (read m)
                            (kill-buffer)))
          (if (and (listp result)
                   (eql :error (car result)))
              (error "StumpBuffer error: %s" (cl-second result))
            result))))))


;;; Retrieving info about things

(defun stumpbuffer-on-group-name ()
  "If point is on a group name, return info about it."
  (when-let ((group (get-text-property (point) 'stumpbuffer-group-number)))
    (list :start (point-at-bol)
          :end (point-at-eol)
          :group-plist (get-text-property (point) 'stumpbuffer-group-plist))))

(defun sb--current-group-plist ()
  (cl-getf (stumpbuffer-on-group-name) :group-plist))

(defun stumpbuffer-on-frame-name ()
  "If point is on a frame name, return info about it."
  (when-let ((group (get-text-property (point) 'stumpbuffer-group))
             (frame (get-text-property (point) 'stumpbuffer-frame-number)))
    (list :group group
          :start (point-at-bol)
          :end (point-at-eol)
          :frame-plist (get-text-property (point) 'stumpbuffer-frame-plist))))

(defun sb--current-frame-plist ()
  (cl-getf (stumpbuffer-on-frame-name) :frame-plist))

(defun stumpbuffer-on-window ()
  "If point is on a window row, return info about it."
  (when-let ((window (get-text-property (point) 'stumpbuffer-window)))
    (let ((group (get-text-property (point) 'stumpbuffer-group))
          (frame (get-text-property (point) 'stumpbuffer-frame)))
      (cl-list* :group group
                :frame frame
                :start (point-at-bol)
                :end (point-at-eol)
                :window-plist (get-text-property (point) 'stumpbuffer-window-plist)
                (when-let ((mark (get-text-property (point) 'stumpbuffer-mark)))
                  (list :mark mark))))))

(defun sb--current-window-plist ()
  (cl-getf (stumpbuffer-on-window) :window-plist))

(defun sb--get-window-face (window)
  "Get the appropriate face for window."
  (let ((faces '()))
    (dolist (pair stumpbuffer-window-faces faces)
      (cl-destructuring-bind (fn . face) pair
        (when (funcall fn window)
          (push face faces))))))


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
        (when (cl-getf win :mark)
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
        (goto-char (cl-getf window :start))
        (move-to-column 2)
        (delete-char 1)
        (insert-and-inherit (cond ((null mark) ?\s)
                                  ((eql mark t) ?*)
                                  (t mark)))
        (add-text-properties
         (cl-getf window :start)
         (cl-getf window :end)
         (list 'stumpbuffer-mark mark
               'face (if mark
                         stumpbuffer-marked-face
                       (sb--get-window-face (cl-getf window :window-plist))))))
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
  (when-let ((frame-num (cl-getf (sb--current-frame-plist) :number)))
    (stumpbuffer-do-group-windows (win)
      (when (= frame-num (cl-getf (cl-getf win :window-plist) :frame))
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

(defun stumpbuffer-mark-to-delete ()
  (interactive)
  (stumpbuffer-mark ?D))

(defun stumpbuffer-mark-to-kill ()
  (interactive)
  (stumpbuffer-mark ?K))

(defun stumpbuffer-mark-by-window-title-regex (regex mark)
  (interactive (list (read-string "Regex: ")
                     (sb--maybe-prompt-for-mark ?*)))
  (stumpbuffer-do-windows (win)
    (when-let ((title (cl-getf (cl-getf win :window-plist) :title)))
      (when (string-match regex title)
        (stumpbuffer-mark mark)))))

(defun stumpbuffer-mark-by-frame (frame mark)
  (interactive (list (cl-getf (sb--current-window-plist) :frame)
                     (sb--maybe-prompt-for-mark ?*)))
  (stumpbuffer-do-group-windows (win)
    (when-let ((f (cl-getf (cl-getf win :window-plist) :frame)))
      (when (= f frame)
        (stumpbuffer-mark mark)))))

(defun stumpbuffer-mark-frame-for-delete (frame)
  (interactive (list (cl-getf (sb--current-frame-plist) :number)))
  (stumpbuffer-mark-by-frame frame ?D)
  (stumpbuffer-forward-frame))

(defun stumpbuffer-mark-frame-for-kill (frame)
  (interactive (list (cl-getf (sb--current-frame-plist) :number)))
  (stumpbuffer-mark-by-frame frame ?K)
  (stumpbuffer-forward-frame))

(defun stumpbuffer-mark-group-for-delete (group)
  (interactive (list (cl-getf (sb--current-group-plist) :number)))
  (stumpbuffer-mark-group group ?D))

(defun stumpbuffer-mark-group-for-kill (group)
  (interactive (list (cl-getf (sb--current-group-plist) :number)))
  (stumpbuffer-mark-group group ?K))

(defun sb--get-all-window-values (field)
  (let (result)
    (stumpbuffer-do-windows (win)
      (when-let ((val (cl-getf (cl-getf win :window-plist)
                               field)))
        (push val result)))
    result))

(defun stumpbuffer-mark-by-window-class (class mark)
  (interactive (list (or (unless current-prefix-arg
                           (cl-getf (sb--current-window-plist) :class))
                         (completing-read "Class: "
                                          (sb--get-all-window-values :class)))
                     (sb--maybe-prompt-for-mark ?*)))
  (stumpbuffer-do-windows (win)
    (when (equal class (cl-getf (cl-getf win :window-plist) :class))
      (stumpbuffer-mark mark))))

(defun stumpbuffer-mark-by-window-role (role mark)
  (interactive (list (or (unless current-prefix-arg
                           (cl-getf (sb--current-window-plist) :role))
                         (completing-read "Role: "
                                          (sb--get-all-window-values :role)))
                     (sb--maybe-prompt-for-mark ?*)))
  (stumpbuffer-do-windows (win)
    (when (equal role (cl-getf (cl-getf win :window-plist) :role))
      (stumpbuffer-mark mark))))

(defun stumpbuffer-mark-by-window-instance (instance mark)
  (interactive (list (or (unless current-prefix-arg
                           (cl-getf (sb--current-window-plist) :instance))
                         (completing-read "Instance: "
                                          (sb--get-all-window-values :instance)))
                     (sb--maybe-prompt-for-mark ?*)))
  (stumpbuffer-do-windows (win)
    (when (equal instance (cl-getf (cl-getf win :window-plist) :instance))
      (stumpbuffer-mark mark))))

(defun stumpbuffer-mark-current-group (mark)
  (interactive (list (sb--maybe-prompt-for-mark ?*)))
  (stumpbuffer-do-group-windows (win)
    (stumpbuffer-mark mark)))

(defun stumpbuffer-change-marks (mark)
  (interactive (list (read-char "Mark: ")))
  (stumpbuffer-do-marked-windows (win)
    (when (char-equal ?* (cl-getf win :mark))
      (stumpbuffer-mark mark))))

(defun stumpbuffer-execute-marks ()
  (interactive)
  (when (yes-or-no-p "Execute marks? ")
    (unwind-protect
        (stumpbuffer-do-marked-windows (win)
          (when-let ((mark (cl-getf win :mark))
                     (fn (cdr (assoc mark stumpbuffer-mark-functions))))
            (funcall fn win)
            (stumpbuffer-unmark)))
      (stumpbuffer-update))))

(defun stumpbuffer-delete (win)
  (when-let ((win (cl-getf (cl-getf win :window-plist) :id)))
    (stumpbuffer-command "delete-window" win)))

(defun stumpbuffer-kill (win)
  (when-let ((win (cl-getf (cl-getf win :window-plist) :id)))
    (stumpbuffer-command "kill-window" win)))

(defun stumpbuffer-delete-and-update (win)
  (interactive (list (stumpbuffer-on-window)))
  (when (yes-or-no-p (format "Delete window '%s'? "
                             (cl-getf (cl-getf win :window-plist)
                                      :title)))
    (unwind-protect
        (stumpbuffer-delete win)
      (stumpbuffer-update))
    (message "Deleted windows may take a moment to die. Use `g` to update.")))

(defun stumpbuffer-kill-and-update (win)
  (interactive (list (stumpbuffer-on-window)))
  (when (yes-or-no-p (format "Kill window '%s'? "
                             (cl-getf (cl-getf win :window-plist)
                                      :title)))
    (unwind-protect
        (stumpbuffer-kill win)
      (stumpbuffer-update))))

(defun stumpbuffer-create-group (name)
  (interactive (list (read-string "New group name: ")))
  (when name
    (unwind-protect
        (stumpbuffer-throw-marked-windows-to-group
         (stumpbuffer-command "create-group" name))
      (stumpbuffer-update))))

(defun stumpbuffer-delete-group (group)
  (interactive (list (cl-getf (sb--current-group-plist) :number)))
  (when (and group
             (yes-or-no-p "Delete group? "))
    (unwind-protect
        (stumpbuffer-command "delete-group" group)
      (stumpbuffer-update))))

(defun stumpbuffer-delete-frame (group frame)
  (interactive (let ((on-frame (stumpbuffer-on-frame-name)))
                 (list (cl-getf on-frame :group)
                       (cl-getf (cl-getf on-frame :frame-plist) :number))))
  (when (and group frame)
    (unwind-protect
        (stumpbuffer-command "delete-frame" group frame)
      (stumpbuffer-update))))

(defun stumpbuffer-split-frame-vertical (group frame)
  (interactive (let ((on-frame (stumpbuffer-on-frame-name)))
                 (list (cl-getf on-frame :group)
                       (cl-getf (cl-getf on-frame :frame-plist) :number))))
  (when (and group frame)
    (unwind-protect
        (stumpbuffer-command "split-frame" group frame "2")
      (stumpbuffer-update))))

(defun stumpbuffer-split-frame-horizontal (group frame)
  (interactive (let ((on-frame (stumpbuffer-on-frame-name)))
                 (list (cl-getf on-frame :group)
                       (cl-getf (cl-getf on-frame :frame-plist) :number))))
  (when (and group frame)
    (unwind-protect
        (stumpbuffer-command "split-frame" group frame "1")
      (stumpbuffer-update))))

(defun stumpbuffer-split-window-frame-vertical (window)
  (interactive (list (stumpbuffer-on-window)))
  (when window
    (when-let ((group (cl-getf window :group))
               (frame (cl-getf window :frame)))
      (stumpbuffer-split-frame-vertical group frame))))

(defun stumpbuffer-split-window-frame-horizontal (window)
  (interactive (list (stumpbuffer-on-window)))
  (when window
    (when-let ((group (cl-getf window :group))
               (frame (cl-getf window :frame)))
      (stumpbuffer-split-frame-horizontal group frame))))

(defun stumpbuffer-rename-group (group new-name)
  (interactive (let ((gplist (sb--current-group-plist)))
                 (list (cl-getf gplist :number)
                       (read-string (format "Rename '%s': "
                                            (cl-getf gplist :name))))))
  (when group
    (unwind-protect
        (stumpbuffer-command "rename-group" group new-name)
      (stumpbuffer-update))))

(defun stumpbuffer-rename-window (window-id new-name)
  (interactive (let ((wplist (sb--current-window-plist)))
                 (list (cl-getf wplist :id)
                       (read-string (format "Rename '%s': "
                                            (cl-getf wplist :title))))))
  (when window-id
    (unwind-protect
        (stumpbuffer-command "rename-window" window-id new-name)
      (stumpbuffer-update))))

(defun stumpbuffer-renumber-window (window-id new-number)
  (interactive (let ((wplist (sb--current-window-plist)))
                 (list (cl-getf wplist :id)
                       (read-string (format "Renumber '%s' from %d to: "
                                            (cl-getf wplist :title)
                                            (cl-getf wplist :number))))))
  (when (and window-id new-number)
    (unwind-protect
        (stumpbuffer-command "renumber-window" window-id new-number)
      (stumpbuffer-update))))

(defun stumpbuffer-renumber-group (group new-number)
  (interactive (let ((gplist (sb--current-group-plist)))
                 (list (cl-getf gplist :number)
                       (read-string (format "Renumber '%s' from %d to: "
                                            (cl-getf gplist :name)
                                            (cl-getf gplist :number))))))
  (when (and group new-number)
    (unwind-protect
        (stumpbuffer-command "renumber-group" group new-number)
      (stumpbuffer-update))))

(defun stumpbuffer-switch-to-group (group)
  (interactive (list (cl-getf (sb--current-group-plist) :number)))
  (when group
    (stumpbuffer-command "switch-to-group" group)
    (when stumpbuffer-quit-window-after-command
      (stumpbuffer-quit-window))))

(defun stumpbuffer-focus-frame (group frame)
  (interactive (let ((frame (stumpbuffer-on-frame-name)))
                 (list (cl-getf frame :group)
                       (cl-getf (cl-getf frame :frame-plist) :number))))
  (when (and group frame)
    (stumpbuffer-command "focus-frame" group frame)
    (when stumpbuffer-quit-window-after-command
      (stumpbuffer-quit-window))))

(defun stumpbuffer-focus-window (group window)
  (interactive (let ((win (stumpbuffer-on-window)))
                 (list (cl-getf win :group)
                       (cl-getf (cl-getf win :window-plist) :id))))
  (when (and group window)
    (stumpbuffer-command "focus-window" group window)
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
                          (cl-getf (cl-getf win :window-plist) :id))))
    (if window
        (pull-window window)
      (let (marksp)
        (stumpbuffer-do-marked-windows (win)
          (when (char-equal ?* (cl-getf win :mark))
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
  (interactive (list (cl-getf (sb--current-group-plist) :number)
                     current-prefix-arg))
  (unwind-protect
      (stumpbuffer-do-marked-windows (win)
        (when (char-equal ?* (cl-getf win :mark))
          (stumpbuffer-command "throw-window-to-group"
                               (cl-getf (cl-getf win :window-plist) :id)
                               group-number)))
    (stumpbuffer-update))
  (when followp
    (stumpbuffer-switch-to-group group-number)))

(defun stumpbuffer-throw-marked-windows-to-frame (group frame &optional followp)
  "Move all `*` marked windows to a frame.

With a prefix argument this also focuses the frame."
  (interactive (let ((frame (stumpbuffer-on-frame-name)))
                 (list (cl-getf frame :group)
                       (cl-getf (cl-getf frame :frame-plist) :number)
                       current-prefix-arg)))
  (when-let ((target-group group)
             (target-frame frame))
    (unwind-protect
        (stumpbuffer-do-marked-windows (win)
          (when (char-equal ?* (cl-getf win :mark))
            (stumpbuffer-command "throw-window-to-frame"
                                 (cl-getf (cl-getf win :window-plist) :id)
                                 target-group
                                 target-frame)))
      (stumpbuffer-update)))
  (when followp
    (stumpbuffer-focus-frame group frame)))

(defun stumpbuffer-throw-marked-windows (target-window-id &optional followp)
  "Move all `*` marked windows to a window.

With a prefix argument this also focuses the window."
  (interactive (list (cl-getf (sb--current-window-plist) :id)
                     current-prefix-arg))
  (unwind-protect
      (stumpbuffer-do-marked-windows (win)
        (when (char-equal ?* (cl-getf win :mark))
          (stumpbuffer-command "throw-window"
                               (cl-getf (cl-getf win :window-plist) :id)
                               target-window-id)))
    (stumpbuffer-update))
  (when followp
    (let ((twindow (stumpbuffer-on-window)))
      (stumpbuffer-focus-window (cl-getf twindow :group)
                                target-window-id))))

(defun stumpbuffer-toggle-frame-showing ()
  (interactive)
  (setq stumpbuffer-show-frames-p (not stumpbuffer-show-frames-p))
  (stumpbuffer-update))

(defun stumpbuffer-cycle-filter-groups (n)
  (interactive "P")
  (let* ((n (or n 1)))
    (setq sb--active-filter-group-n
          (mod (+ sb--active-filter-group-n n)
               (length stumpbuffer-filter-groups))
          
          sb--active-filter-group
          (nth sb--active-filter-group-n
               stumpbuffer-filter-groups)))
  (stumpbuffer-update))

(defun stumpbuffer-select-filter-group (group-name)
  (interactive (list (completing-read "Filter group: "
                                      stumpbuffer-filter-groups
                                      nil t)))
  (when group-name
    (setq sb--active-filter-group-n
          (or (cl-position group-name stumpbuffer-filter-groups
                           :key #'cl-first
                           :test #'string-equal)
              (error "No such filter group: %s" group-name))
          sb--active-filter-group
          (nth sb--active-filter-group-n
               stumpbuffer-filter-groups)))
  (stumpbuffer-update))

(defun stumpbuffer-pop-quick-filter (n)
  (interactive "p")
  (if (null stumpbuffer-quick-filter-stack)
      (message "Quick filter stack empty.")
    (progn
      (if (>= n 0)
          (message "Popped: %s"
                   (pop stumpbuffer-quick-filter-stack))
        (setq stumpbuffer-quick-filter-stack nil)
        (message "Cleared quick filters."))
      (stumpbuffer-update))))

(defun stumpbuffer-push-quick-filter (filter)
  (cl-pushnew filter stumpbuffer-quick-filter-stack
              :test #'equal))

(defun stumpbuffer-push-hide-hidden-groups-filter ()
  (interactive)
  (stumpbuffer-push-quick-filter
   '(:hide-groups :satisfying stumpbuffer-group-hidden-p))
  (stumpbuffer-update))

(defun stumpbuffer-push-show-hidden-groups-filter ()
  (interactive)
  (stumpbuffer-push-quick-filter
   '(:show-groups :satisfying stumpbuffer-group-hidden-p))
  (stumpbuffer-update))

(defun stumpbuffer-push-show-matching-windows-filter (regex)
  (interactive (list (read-string "Match title: ")))
  (stumpbuffer-push-quick-filter
   `(:show-windows :where :title :matches ,regex))
  (stumpbuffer-update))

(defun stumpbuffer-push-show-windows-by-class-filter (class)
  (interactive (list (or (unless current-prefix-arg
                           (cl-getf (sb--current-window-plist) :class))
                         (completing-read "Class: "
                                          (sb--get-all-window-values :class)))))
  (stumpbuffer-push-quick-filter
   `(:show-windows :where :class :is ,class))
  (stumpbuffer-update))

(defun stumpbuffer-push-show-windows-by-role-filter (role)
  (interactive (list (or (unless current-prefix-arg
                           (cl-getf (sb--current-window-plist) :role))
                         (completing-read "Role: "
                                          (sb--get-all-window-values :role)))))
  (stumpbuffer-push-quick-filter
   `(:show-windows :where :role :is ,role))
  (stumpbuffer-update))

(defun stumpbuffer-push-show-windows-by-instance-filter (instance)
  (interactive (list (or (unless current-prefix-arg
                           (cl-getf (sb--current-window-plist) :instance))
                         (completing-read "Instance: "
                                          (sb--get-all-window-values :instance)))))
  (stumpbuffer-push-quick-filter
   `(:show-windows :where :instance :is ,instance))
  (stumpbuffer-update))


;;; Retrieving data and updating

(defun sb--get-data ()
  (stumpbuffer-command "get-data"))

(defun sb--set-header ()
  (let ((header (with-output-to-string
                  (princ "  M ")
                  (dolist (field stumpbuffer-window-format)
                    (cl-destructuring-bind (field &optional width
                                                  title format-fn)
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

(defun sb--match-filter (how plist)
  (pcase how
    (`(:satisfying ,fn) (funcall fn plist))
    (`(:where ,field :matches ,regex)
     (when-let ((val (cl-getf plist field)))
       (and (stringp val)
            (string-match regex val))))
    (`(:where ,field :is ,value)
     (equal value (cl-getf plist field)))))

(defun sb--filter-window-p (window)
  (cl-flet ((match-filter (filter)
                          (cl-destructuring-bind (what &rest how) filter
                            (cl-case what
                              (:hide-windows (sb--match-filter how window))
                              (:show-windows (not (sb--match-filter how window)))))))
    (or (cl-some #'match-filter
                 (if (stringp (car sb--active-filter-group))
                     (cl-rest sb--active-filter-group)
                   sb--active-filter-group))
        (cl-some #'match-filter
                 stumpbuffer-quick-filter-stack))))

(defmacro sb--with-properties (properties &rest body)
  "Add properties to text inserted by the body."
  (declare (indent 1))
  `(add-text-properties (point)
                        (progn ,@body
                               (point))
                        ,properties))

(defmacro sb--with-property (property value &rest body)
  "Add a property to text inserted by the body."
  (declare (indent 2))
  `(put-text-property (point)
                      (progn ,@body
                             (point))
                      ,property ,value))

(defun sb--insert-window (window-plist)
  (unless (sb--filter-window-p window-plist)
    (sb--with-properties
        (list 'keymap                    stumpbuffer-mode-window-map
              'stumpbuffer-window        (cl-getf window-plist :number)
              'stumpbuffer-window-id     (cl-getf window-plist :id)
              'stumpbuffer-window-plist  window-plist
              'face                      (sb--get-window-face window-plist))
      (insert "    ")
      (dolist (field stumpbuffer-window-format)
        (cl-destructuring-bind (field &optional width title format-fn)
            field
          (let* ((entry (format "%s"
                                (if-let ((value (cl-getf window-plist field)))
                                    (if (null format-fn)
                                        (if (eql value t)
                                            (string stumpbuffer-window-field-t-character)
                                          value)
                                      (funcall format-fn value))
                                  "")))
                 (len (length entry)))
            (insert (if width
                        (format (if (> len width)
                                    (format "%%-%d.%ds..."
                                            (- width 3)
                                            (- width 3))
                                  (format "%%-%ds" width))
                                entry)
                      entry))))
        (insert " ")))
    (insert "\n")))

(defun sb--insert-format (plist format)
  (dolist (part format)
    (cl-destructuring-bind (faces &rest things) part
      (sb--with-property 'face faces
        (dolist (thing things)
          (cl-typecase thing
            (list
             (when (eql (car thing) :call)
               (when-let ((value (funcall (cl-second thing) plist)))
                 (insert value))))
            (string (insert thing))
            (keyword
             (let ((value (cl-getf plist thing "")))
               (cl-typecase value
                 (string (insert value))
                 (number (insert (number-to-string value))))))))))))

(defun sb--insert-frame (frame-plist)
  (cl-destructuring-bind (&key number windows &allow-other-keys)
      frame-plist
    (when stumpbuffer-show-frames-p
      (sb--with-properties
          (list 'keymap                    stumpbuffer-mode-frame-map
                'stumpbuffer-frame-number  number
                'stumpbuffer-frame-plist   frame-plist)
        (sb--insert-format frame-plist stumpbuffer-frame-name-format))
      (insert "\n"))
    (unless (null windows)
      (put-text-property
       (point)
       (progn (mapc #'sb--insert-window windows)
              (point))
       'stumpbuffer-frame number))))

(defun sb--filter-group-p (group)
  (cl-flet ((match-filter (filter)
                          (cl-destructuring-bind (what &rest how) filter
                            (cl-case what
                              (:hide-groups (sb--match-filter how group))
                              (:show-groups (not (sb--match-filter how group)))))))
    (or (cl-some #'match-filter
                 (if (stringp (car sb--active-filter-group))
                     (cl-rest sb--active-filter-group)
                   sb--active-filter-group))
        (cl-some #'match-filter
                 stumpbuffer-quick-filter-stack))))

(defun sb--insert-group (group-plist)
  (unless (sb--filter-group-p group-plist)
    (cl-destructuring-bind (&key number name frames windows type &allow-other-keys)
        group-plist
      (sb--with-properties
          (list 'keymap                    stumpbuffer-mode-group-map
                'stumpbuffer-group-number  number
                'stumpbuffer-group-plist   group-plist)
        (sb--insert-format group-plist stumpbuffer-group-name-format))
      (insert "\n")
      (unless (null frames)
        (sb--with-property 'stumpbuffer-group number
          (mapc #'sb--insert-frame frames)))
      (unless (null windows)
        (when stumpbuffer-show-frames-p
          (sb--with-property 'face stumpbuffer-float-title-face
           (insert "Floats\n")))
        (sb--with-property 'stumpbuffer-group number
          (mapc #'sb--insert-window windows))))))

(defun stumpbuffer-update ()
  (interactive)
  (let (active-marks)
    (stumpbuffer-do-windows (win)
      (when-let ((mark (cl-getf win :mark)))
        (push (cons (cl-getf (cl-getf win :window-plist) :id)
                    mark)
              active-marks)))
    (let ((window-id (cl-getf (sb--current-window-plist) :id))
          (position (count-lines (point-min) (point))))
      (unwind-protect
          (progn (setq buffer-read-only nil)
                 (erase-buffer)
                 (sb--set-header)
                 (mapc #'sb--insert-group (sb--get-data)))
        (setq buffer-read-only t)
        (when active-marks
          (stumpbuffer-do-windows (win)
            (when-let ((id (cl-getf (cl-getf win :window-plist) :id))
                       (mark (cdr (assoc id active-marks))))
              (stumpbuffer-mark mark))))
        (cond
         ((and window-id
               (let (position)
                 (cl-block nil
                   (stumpbuffer-do-windows (win)
                     (when (= window-id (cl-getf (cl-getf win :window-plist) :id))
                       (setq position (point))
                       (cl-return))))
                 (when position (goto-char position)))))
         (t (goto-char (point-min))
            (dotimes (i position)
              (stumpbuffer-forward-line))))))))


;;; Entry / exit

(defun stumpbuffer-quit-window ()
  "Exit the buffer.

Kills the frame if necessary."
  (interactive)
  (when-let ((buf (get-buffer "*StumpBuffer*")))
    (kill-buffer buf))
  (when sb--kill-frame-on-exit-p
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
  (setq sb--kill-frame-on-exit-p kill-frame-p)
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
  (setq truncate-lines t)
  (set (make-local-variable 'sb--kill-frame-on-exit-p) nil)
  (make-local-variable 'stumpbuffer-quick-filter-stack)
  (make-local-variable 'stumpbuffer-show-frames-p)
  (make-local-variable 'stumpbuffer-filter-groups)
  (make-local-variable 'sb--active-filter-group-n)
  (set (make-local-variable 'sb--active-filter-group)
       (nth sb--active-filter-group-n
            stumpbuffer-filter-groups)))


;;; Filter/face utilities

(defun stumpbuffer-group-hidden-p (group)
  (cl-getf group :hiddenp))

(defun stumpbuffer-window-hidden-p (window)
  (cl-getf window :hiddenp))

(defun stumpbuffer-window-visible-p (window)
  (cl-getf window :visiblep))

(defun stumpbuffer-window-fullscreen-p (window)
  (cl-getf window :fullscreenp))


(provide 'stumpbuffer)
(run-hooks 'stumpbuffer-load-hook)
;;; stumpbuffer.el ends here
