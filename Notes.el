;;; Notes.el --- Notes.app                           -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.8.0
;; Keywords: tools, applescript
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

;; REF: http://www.macosxautomation.com/applescript/notes/index.html

;;; Code:

(eval-when-compile
  (require 'applescript)
  (require 'cl))

(require 'shr)

(require 'org)
(require 'org-element)
(require 'ox-html)

(defcustom Notes-default-folder "Notes"
  "Default folder to create a new note."
  :type 'string
  :group 'applescript)

(defconst Notes-ut-handler "\
-- date string is locale-dependent so converted to unix time
-- to make sure emacs can always parse it
on ut(d) -- convert d to unix time
  if d is missing value
    return -1
  else
    -- Cannot use the locale-dependent value \"1 January 1970\"
    set unixTimeStarts to current date
    set time of unixTimeStarts to 0
    set day of unixTimeStarts to 1
    set month of unixTimeStarts to 1
    set year of unixTimeStarts to 1970
    return (d - unixTimeStarts - (time to GMT))
  end if
end ut")

(defconst Notes-dateFromUT-handler "\
-- http://lists.apple.com/archives/applescript-users/2011/Mar/msg00044.html
on dateFromUT(UTS)
  if UTS is missing value
    return missing value
  end if
  set unixTimeStampStarts to current date -- just any date
  set time of unixTimeStampStarts to 0
  set day of unixTimeStampStarts to 1
  set month of unixTimeStampStarts to 1
  set year of unixTimeStampStarts to 1970
  return unixTimeStampStarts + (UTS as real) + (time to GMT)
end dateFromUT")

(defun Notes-accounts ()
  "Return a list of account names."
  (let ((a (split-string (read (applescript "\
tell application \"Notes\"
  set XX to {}
  set AppleScript's text item delimiters to {\"----\"}
  repeat with a in accounts
    set {name:x1, id:x2} to properties of a
    copy {x1, x2} as text to end of XX
  end repeat
  set AppleScript's text item delimiters to {\"####\"}
  return XX as text
end tell"))
                         "####" t)))
    (mapcar (lambda (x) (split-string x "----")) a)))

(defun Notes-folders (account)
  "Return a list of folder names in ACCOUNT."
  (let ((f (split-string (read (applescript "\
tell application \"Notes\"
  set FF to {}
  set AppleScript's text item delimiters to {\"----\"}
  repeat with f in folders of account #{account}
    set {name:x1, id:x2} to f
    copy {x1,x2} as text to end of FF
  end repeat
  set AppleScript's text item delimiters to {\"####\"}
  return FF as text
end tell"))
                         "####" t)))
    (mapcar (lambda (x) (split-string x "----")) f)))

(defun Notes-to-plist (note &optional sep)
  (let ((sep (or sep "----")))
    (loop for v in (split-string note sep)
          for k in '(:name :note-id :body :creation-date :modification-date)
          collect k collect (if (string-match "date\\'" (symbol-name k))
                                (string-to-number v)
                              v))))

;; (Notes-notes-1 "iCloud" "Notes")
(defun Notes-notes-1 (account folder)
  (let ((notes (split-string (read (applescript Notes-ut-handler
                                                "\
tell application \"Notes\"
  set AppleScript's text item delimiters to {\"----\"}
  set myNotes to {}
  set myNotesRef to a reference to myNotes
  repeat with n in notes of folder #{folder} of account #{account}
    set {name:x1, id:x2, body:x3, creation date:x4, modification date:x5} \
      to properties of n
    copy {x1, x2, x3, my ut(x4), my ut(x5)} as text to end of myNotesRef
  end repeat
  set AppleScript's text item delimiters to {\"####\"}
  return myNotesRef as text
end tell"))
                             "####" t)))
    (mapcar #'Notes-to-plist notes)))

(defun Notes-notes ()                   ; TODO: handle attachment
  (loop for a in (Notes-accounts)
        collect (cons a (loop for f in (Notes-folders (car a))
                              collect (cons f (Notes-notes-1 (car a) (car f)))))))

(defun Notes-normalise (n)            ; same as `Reminders-normalise'.
  (loop for x in n
        collect (cond ((not (stringp x)) x)
                      ((equal x "missing value") nil)
                      ((equal x "true") t)
                      ((equal x "false") nil)
                      (t x))))

(defun Notes-update (data)
  (destructuring-bind (&key note-id name body container modification-date
                            &allow-other-keys)
      data
    (read (applescript Notes-ut-handler
                       Notes-dateFromUT-handler
                       "\
tell application \"Notes\"
  set nil to missing value
  if #{container} is missing value then
    set f to first folder whose name is #{Notes-default-folder}
  else
    set f to first folder whose id is #{container}
  end if

  if #{note-id} is missing value then
    set n to make new note at f
  else
    set n to first note whose id is #{note-id}
    -- Mind the round-off error by org mode
    if #{modification-date} is not missing value and \
          (modification date of n) - my dateFromUT(#{modification-date}) > 59
      error \"Current note is modified outside org\"
    end if
  end if

  set name of n to #{name}
  if #{body} is missing value and body of n is not missing value then
    delete body of n
  else
    set body of n to #{body}
  end if

  set {name:x1, id:x2, body:x3, creation date:x4, modification date:x5} \
    to properties of n
  set AppleScript's text item delimiters to {\"----\"}
  return {x1, x2, x3, my ut(x4), my ut(x5)} as text
end tell"))))

(defun Notes-normalise-org (p)
  (loop for (k v) on p by #'cddr
        collect k
        collect (pcase k
                  ((pred (lambda (x)
                           (and v (string-match-p "-date\\'" (symbol-name x)))))
                   (float-time (apply #'encode-time (org-parse-time-string v))))
                  (t v))))

(defun Notes-from-org-data (data)
  (let ((n (make-symbol "note")))
    (org-element-map data '(headline node-property)
      (lambda (x)
        (pcase (car x)
          (`headline
           (when (eq (org-element-property :level x)
                     (org-reduced-level (org-current-level)))
             (put n :name (org-element-property :raw-value x))))
          (`node-property
           (put n (intern (concat ":" (org-element-property :key x)))
                (org-element-property :value x))))))
    (symbol-plist n)))

(defun Notes-export-org-subtree ()
  (let ((org-html-text-markup-alist (cons '(underline . "<u>%s</u>")
                                          org-html-text-markup-alist)))
    (prog1 (concat (nth 4 (org-heading-components))
                   "<div></br></div>"
                   ;; XXX: why does it recenter the selected window?
                   (org-export-as 'html t nil t))
      ;; Clear the ugly message from `org-cycle-internal-global'.
      (message nil))))

(defun Notes-update-from-org ()
  (interactive)
  (when (= 3 (org-reduced-level (org-current-level)))
    (or (not (org-entry-get (point) "note-id"))
        (fboundp 'libxml-parse-html-region)
        (error "Sync from org to Notes unsafe"))
    (let* ((ws (window-start))  ; org-export-as seems to change this
           (pt (point))
           (folder-id (org-entry-get-with-inheritance "folder-id"))
           (elems (Notes-from-org-data
                   (save-restriction
                     (widen)
                     (org-narrow-to-subtree)
                     (org-element-parse-buffer))))
           (body (Notes-export-org-subtree))
           (data (Notes-update
                  (Notes-normalise-org
                   (plist-put (plist-put elems :container folder-id)
                              :body body)))))
      (Notes-kill-org-subtree)
      (Notes-insert-note (Notes-to-plist data))
      (set-window-start nil ws)
      (set-window-point nil pt))
    (message "Current note updated")
    'synced))

(defun Notes-seconds-to-org (s)
  (format-time-string (cdr org-time-stamp-formats) (seconds-to-time s)))

;;; NOTE: consider using `advice-add' instead once 24.4 is released.
(defmacro Notes-with-fset (funlist &rest body)
  (declare (indent 1))
  (let ((entries (mapcar (lambda (f)
                           (cons (make-symbol (format "-%s-" (car f))) f))
                         funlist)))
    `(let (,@(mapcar (lambda (x)
                       `(,(car x) (symbol-function ',(cadr x))))
                     entries))
       ,@(mapcar (lambda (x)
                   `(fset ',(nth 1 x) (indirect-function #',(nth 2 x))))
                 entries)
       (unwind-protect (progn ,@body)
         ,@(mapcar (lambda (x)
                     `(fset ',(cadr x) ,(car x)))
                   entries)))))

(defun Notes-shr-tag-li (cont)
  (unless (bolp) (insert "\n"))
  (shr-indent)
  (let* ((bullet
          (if (numberp shr-list-mode)
              (prog1
                  (format "%d. " shr-list-mode)
                (setq shr-list-mode (1+ shr-list-mode)))
            "- "))
         (shr-indentation (+ shr-indentation (length bullet))))
    (insert bullet)
    (shr-generic cont)))

(defun Notes-shr-tag-a (cont)
  (let ((url (cdr (assq :href cont)))
	(start (point))
	shr-start)
    (let ((shr-width most-positive-fixnum))
      (shr-generic cont))
    (let ((text (delete-and-extract-region (or shr-start start) (point))))
      (if (equal text url)
          (insert url)
        (insert (format "[[%s][%s]]" url text))))))

(defun Notes-shr-tag-pre (cont)
  (let ((shr-folding-mode 'none))
    (shr-ensure-newline)
    (insert "#+BEGIN_EXAMPLE")
    (shr-indent)
    (shr-generic cont)
    (shr-ensure-newline)
    (insert "#+END_EXAMPLE")))

(autoload 'sgml-pretty-print "sgml-mode" nil t)

(defun Notes-html2org (html)
  "Convert HTML string to org markup."
  (with-temp-buffer
    (insert html)
    (if (fboundp 'libxml-parse-html-region)
        (let ((dom (libxml-parse-html-region (point-min) (point-max))))
          (erase-buffer)
          (Notes-with-fset ((shr-tag-li Notes-shr-tag-li)
                            (shr-tag-a Notes-shr-tag-a)
                            (shr-tag-pre Notes-shr-tag-pre))
            (shr-insert-document dom)))
      (sgml-pretty-print (point-min) (point-max))
      (html2text))
    (let ((delete-trailing-lines t))
      (delete-trailing-whitespace))
    (goto-char (point-min))
    (skip-chars-forward " \t\n")
    (goto-char (line-beginning-position 2))
    (skip-chars-forward " \t\n")
    (delete-region (point-min) (line-beginning-position))
    (let ((markup (lambda (o s)
                    (unless (eq (overlay-start o) (overlay-end o))
                      (goto-char (overlay-start o))
                      (insert s)
                      (goto-char (overlay-end o))
                      (insert s)))))
      (dolist (o (overlays-in (point-min) (point-max)))
        (pcase (overlay-get o 'face)
          (`bold (funcall markup o "*"))
          (`underline (funcall markup o "_"))
          (`italic (funcall markup o "/")))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun Notes-insert-note (n)
  (destructuring-bind (&key name note-id body creation-date modification-date)
      (Notes-normalise n)
    (insert (make-string (1+ (* 2 (org-level-increment))) ?*)
            " " (org-trim name) "\n")
    (when body
      (indent-rigidly (point)
                      (progn
                        (insert (Notes-html2org body))
                        (point))
                      (+ 2 (* 2 (org-level-increment))))
      (or (bolp) (insert "\n")))
    (save-excursion
      (forward-line -1)
      (org-set-property "note-id" note-id)
      (org-set-property "creation-date" (Notes-seconds-to-org creation-date))
      (org-set-property "modification-date"
                        (Notes-seconds-to-org modification-date)))))

(defun Notes-kill-org-subtree ()
  (org-back-to-heading t)
  (kill-region (point) (progn (org-end-of-subtree t t) (point))))

;;; NOTE: with the introduction of nadvice.el in emacs 24.4 it might
;;; be alright to use advice after all.
(defadvice org-kill-line (around delete-note activate)
  (let ((note-id (when (and (bolp)
                            (org-at-heading-p)
                            (= 3 (org-reduced-level (org-current-level)))
                            (org-entry-get (point) "note-id")
                            (yes-or-no-p "Delete this note? "))
                   (org-entry-get (point) "note-id"))))
    (if (not note-id) ad-do-it
      (applescript "tell application \"Notes\" to \
delete (first note whose id is #{note-id})")
      (Notes-kill-org-subtree))))

(defvar Notes-org-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap save-buffer] 'Notes-update-from-org)
    m))

;;;###autoload
(defun Notes (&optional buffer)
  "Pull all notes into a org-mode buffer BUFFER."
  (interactive (ignore (message "Pulling notes ...") (sit-for 0.1)))
  (switch-to-buffer (or buffer "*Notes*"))
  (erase-buffer)
  (insert "#+TITLE: Notes\n\n")
  (org-mode)
  (add-hook 'org-ctrl-c-ctrl-c-hook #'Notes-update-from-org nil t)
  (push (cons t Notes-org-map) minor-mode-overriding-map-alist)
  (dolist (a (Notes-notes))
    (insert "* " (caar a) "\n")
    (org-set-property "account-id" (cadr (car a)))
    (dolist (f (cdr a))
      (insert (make-string (1+ (org-level-increment)) ?*) " " (caar f) "\n")
      (org-set-property "folder-id" (cadr (car f)))
      (dolist (n (cdr f))
        (Notes-insert-note n))))
  (goto-char (point-min))
  (outline-next-visible-heading 1)
  (and (org-goto-first-child) (org-goto-first-child))
  (org-content (1+ (* 2 (org-level-increment))))
  (set-buffer-modified-p nil)
  (setq-local revert-buffer-function
              (lambda (_ignore-auto noconfirm)
                (when (or noconfirm
                          (yes-or-no-p "Pull from Notes.app? "))
                  (Notes buffer)))))

;;;###autoload
(defun Notes-new-note (name &optional body)
  "Make a new note with NAME and BODY in `Notes-default-folder'.
If region is active it is used for BODY."
  (interactive (list (read-string "Title: ")
                     (if (use-region-p)
                         (concat "#+BEGIN_EXAMPLE\n"
                                 (buffer-substring-no-properties
                                  (region-beginning) (region-end))
                                 "\n#+END_EXAMPLE")
                       (read-string "Body: "))))
  (when (or (not name) (equal name ""))
    (error "Title required"))
  (let ((body (unless (equal body "") body)))
    (Notes-update (Notes-normalise-org
                   (list :name name
                         :body (with-temp-buffer
                                 (org-mode)
                                 (insert "* " name "\n")
                                 (when body
                                   (indent-rigidly (point)
                                                   (progn (insert body "\n")
                                                          (point))
                                                   2))
                                 (Notes-export-org-subtree)))))))

(defun Notes-view-body ()
  "View the html source of the note body."
  (interactive)
  (let ((note-id (or (org-entry-get-with-inheritance "note-id")
                     (error "No existing note at point")))
        (print-escape-newlines nil))
    (with-output-to-temp-buffer "*note-html*"
      (princ (read (applescript "\
tell application \"Notes\" to get body of note id #{note-id}"))))))

(provide 'Notes)
;;; Notes.el ends here
