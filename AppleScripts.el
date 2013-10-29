;;; AppleScripts.el --- collection of smallish AppleScripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.5.0
;; Keywords: tools, extensions, languages
;; Created: 2013-09-10

;; This program is free software; you can redistribute it and/or modify
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

;; All symbols use prefix osx-.

;;; Code:

(eval-when-compile
  (require 'applescript)
  (require 'cl))

(eval-and-compile
  (or (fboundp 'user-error)
      (defalias 'user-error 'error)))

(defcustom osx-notify-sound nil
  "Default sound for `osx-notify'."
  :type '(choice (const :tag "System default" nil) file)
  :group 'applescript)

(defun osx-emacs-selected-p ()
  "Return t if Emacs is currently selected."
  (equal (applescript "current application is frontmost") "true"))

(defun osx-notify (body &optional title subtitle sound)
  "Post a notification using the Notification Center.
SOUND may be the base name of any sound installed in
Library/Sounds."
  (let ((title (substring-no-properties (or title "Emacs")))
        (subtitle (substring-no-properties (or subtitle "")))
        (sound (substring-no-properties (or sound osx-notify-sound ""))))
    (applescript "display notification #{body} with title #{title} \
subtitle #{subtitle} sound name #{sound}")))

(defun osx-say (text &optional nato)
  "Speak TEXT."
  (interactive
   (let ((text
          (if (use-region-p)
              (buffer-substring-no-properties
               (region-beginning) (region-end))
            (read-string (if (current-word t)
                             (format "Text (default %s): " (current-word t))
                           "Text: ")
                         nil nil (current-word t)))))
     (or (< (length text) 300)
         (yes-or-no-p "Text longer than 300 chars; go ahead? ")
         (user-error "Aborted"))
     (setq deactivate-mark t)
     (list text current-prefix-arg)))
  (let ((text (if nato
                  (with-temp-buffer
                    (insert text)
                    (nato-region (point-min) (point-max))
                    (buffer-string))
                text)))
    (applescript "say #{text} without waiting until completion")))

(defun osx-summarize (beg end &optional number)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let* ((number (or number 1))
         (text (buffer-substring-no-properties beg end))
         (sum (read (applescript "summarize #{text} in #{number}"))))
    (when (called-interactively-p 'interactive)
      (display-message-or-buffer sum))
    sum))

(defun osx-empty-trash (&optional no-confirm)
  (interactive "P")
  (let* ((items (split-string (read (applescript "\
tell application \"Finder\"
  set temp to name of items in trash
  copy (length of temp) to beginning of temp
  if (count temp) > 30 then
    set temp to items 1 thru 31 of temp
  end if
  set AppleScript's text item delimiters to {\"----\"}
  return temp as text
end tell"))
                              "----"))
         (count (string-to-number (pop items))))
    (and (zerop count) (user-error "No trash"))
    (when (or no-confirm
              (let ((split-height-threshold 0))
                ;; See `dired-mark-pop-up' for example.
                (with-temp-buffer-window
                 " *Trashes*"
                 (cons 'display-buffer-below-selected
                       '((window-height . fit-window-to-buffer)))
                 (lambda (window _v)
                   (unwind-protect (yes-or-no-p
                                    (format "Empty Trash (%s %s)? "
                                            count
                                            (if (eq count 1) "item" "items")))
                     (when (window-live-p window)
                       (quit-restore-window window 'kill))))
                 ;; Split items 5-item lines
                 (loop for f in items
                       for c from 1
                       do (princ (format "%d. %s\t" c f))
                       when (zerop (mod c 5))
                       do (princ "\n"))
                 (unless (= (length items) count)
                   (princ "......")))))
      (applescript "ignoring application responses
  tell application \"Finder\" to empty the trash
end ignoring"))))

(let (default-color)
  (defun osx-choose-color ()
    "Allows the user to choose a color from a color picker dialog.
The return value is a list similar to that of `color-values'."
    (interactive)
    (let* ((default (concat "{"
                            (mapconcat #'number-to-string
                                       (or default-color '(65535 65535 65535))
                                       ", ")
                            "}"))
           (color (read (applescript
                         "set myColor to choose color default color #{default#s}"
                         ;; XXX: restore old text item delimiters
                         "set AppleScript's text item delimiters to {\":\"}"
                         "return myColor as text")))
           (result (mapcar 'string-to-number (split-string color ":"))))
      (when (called-interactively-p 'interactive)
        (apply #'message
               "RGB:#%02x%02x%02x  Emacs:%s"
               (append (mapcar (lambda (x) (floor x 256)) result)
                       (list result))))
      (setq default-color result)
      result)))

(defun osx-finder ()
  "Open Finder.app and reveal `buffer-file-name' if any."
  (interactive)
  (let ((dir (expand-file-name
              (or (and (fboundp 'dired-file-name-at-point)
                       (dired-file-name-at-point))
                  (and buffer-file-name
                       (file-exists-p buffer-file-name)
                       buffer-file-name)
                  default-directory))))
    (or (not (file-remote-p dir))
        (error "Remote file/directory not supported"))
    (applescript "set f to POSIX file #{dir}"
                 "tell application \"Finder\""
                 "reveal f"
                 "activate"
                 "end tell")))

(defun osx-terminal ()
  "Open Terminal.app and cd `default-directory'."
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (or (not (file-remote-p dir))
        (error "Remote file/directory not supported"))
    (applescript
     "tell application \"Terminal\"
  if not running then
    launch
    delay 0.5 -- launch can take time
  end if
  set aDir to the quoted form of #{dir}
  try
    set aTab to selected tab of the front window
    -- if selected tab is not busy, reuse it
    if aTab is not busy and processes of aTab doesn't end with \"ssh\" then
      do script \"cd \" & aDir in aTab
    else
      error
    end if
  on error
    do script \"cd \" & aDir
  end try
  activate
end tell")))

(defun osx-finder-or-terminal (&optional arg)
  "Open Terminal.app if ARG else Finder.app."
  (interactive "P")
  (if arg (osx-terminal) (osx-finder)))

(provide 'AppleScripts)
;;; AppleScripts.el ends here
