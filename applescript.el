;;; applescript.el --- applescript wrapper           -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.5.0
;; Keywords: languages, tools
;; Created: 2013-09-08

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

;; Make `do-applescript' easier to use.
;;
;; AppleScript Guide: search 'applescript' in
;; http://developer.apple.com/library/mac/navigation

;;; Code:

(defgroup applescript nil
  "The AppleScript programming language."
  :group 'languages)

(defcustom applescript-debug nil
  "Non-nil to log the AppleScript string."
  :type 'boolean
  :group 'applescript)

(defconst applescript-lisp-start "#{"
  "String marking the start of lisp code.")

(defconst applescript-lisp-end "}"
  "String marking the end of lisp code.")

(defvar applescript-lisp-re
  (concat (regexp-quote applescript-lisp-start)
          "\\(\\(?:.\\|\n\\)*?\\)\\(?:#\\(.\\)\\)?"
          (regexp-quote applescript-lisp-end)))

(defun applescript-parse-line (line)
  (let ((start 0)
        (lisp-code))
    (when (stringp line)
      (while (string-match applescript-lisp-re line start)
        (push (condition-case err
                  (read (match-string 1 line))
                (error (error "%s: %s" (error-message-string err) line)))
              lisp-code)
        (setq start (match-beginning 0))
        ;; XXX: makes a new string every time
        (setq line (replace-match (concat "%" (or (match-string 2 line) "S"))
                                  nil nil line))))
    (if (consp lisp-code)
        (cons 'format (cons line (nreverse lisp-code)))
      line)))

(defun applescript-parse-lines (lines)
  (mapcar #'applescript-parse-line lines))

(defun applescript-strings-p (ss)
  "Return non-nil if SS contains only strings."
  (cond
   ((null ss) t)
   ((stringp (car ss))
    (applescript-strings-p (cdr ss)))))

(defmacro applescript-debug (form)
  (if applescript-debug
      (let ((-value- (make-symbol "-value-")))
        `(let ((,-value- ,form))
           (message "DEBUG [%s]: \n%s"
                    (format-time-string "%Y-%m-%dT%T%z")
                    ,-value-)
           ,-value-))
    form))

;;;###autoload
(defmacro applescript (&rest lines)
  "Like `do-applescript' but allow embedding lisp code.
The value of the lisp code is interpolated in the applescript
string using format control string `%S'. It can also be specified
by appending `#C' where C is one of the chars supported by
`format'. Examples: \"#{fill-column}\" and \"#{fill-column#x}\"."
  (let* ((lines (applescript-parse-lines lines))
         (form (cond
                ((applescript-strings-p lines)
                 (mapconcat 'identity lines "\n"))
                ((not (cdr lines)) (car lines))
                (t `(mapconcat 'identity (list ,@lines) "\n")))))
    ;; Check doc-string of `do-applescript' to see why
    ;; `string-to-multibyte' is needed.
    `(do-applescript (applescript-debug (string-to-multibyte ,form)))))

(provide 'applescript)
;;; applescript.el ends here
