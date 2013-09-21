;;; Reminders.el --- Reminders.app                   -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 1.0
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

;; Interact with the Reminders.app seamlessly in Emacs.

;;; Code:

(eval-when-compile
  (require 'applescript)
  (require 'cl))

(require 'org)
(require 'org-element)

(defun Reminders-accounts ()
  (let ((a (split-string (read (applescript
                                "set AppleScript's text item delimiters to {\"---\"}
tell application \"Reminders\"
  set XX to {}
  repeat with A in accounts
    set {name:x1, id:x2} to properties of A
    copy {x1, x2} as text to end of XX
  end repeat
  set AppleScript's text item delimiters to {\"###\"}
  return XX as text
end tell"))
                         "###")))
    (mapcar (lambda (x) (split-string x "---")) a)))

(defun Reminders-lists (account)
  (let ((l (split-string (read (applescript "\
set AppleScript's text item delimiters to {\"---\"}
tell application \"Reminders\"
  set XX to {}
  repeat with L in lists of account #{account}
    set {name:x1, id:x2} to L
    copy {x1, x2} as text to end of XX
  end repeat
  set AppleScript's text item delimiters to {\"###\"}
  return XX as text
end tell"))
                         "###")))
    (mapcar (lambda (x) (split-string x "---")) l)))

(defconst Reminders-property-keys
  '(:name :reminder-id :body :completed :completion-date :creation-date
          :due-date :modification-date :remind-me-date :priority))

(defconst Reminders-ut-handler "\
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

(defconst Reminders-dateFromUT-handler "\
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

(defconst Reminders-reminderProps-handler "\
on reminderProps(r)
  tell application \"Reminders\"
    set {name:x1, id:x2, body:x3, completed:x4, completion date:x5, ¬
          creation date:x6, due date:x7, modification date:x8, remind me date:x9,¬
          priority:x10} to (properties of r)
    return ({x1,x2,x3,x4,my ut(x5),my ut(x6),my ut(x7),my ut(x8),my ut(x9), x10} as text)
  end tell
end reminderProps")

(defun Reminders-to-plist (r &optional sep)
  (let ((sep (or sep "---")))
    (loop for k in Reminders-property-keys
          for v in (split-string r sep)
          collect k collect (if (string-match "date\\'" (symbol-name k))
                                (string-to-number v)
                              v))))

;;; (Reminders-reminders-1 "iCloud" "Reminders")
(defun Reminders-reminders-1 (account list &optional qs)
  "Return all reminders in LIST of ACCOUNT."
  (let ((rs (split-string (read (applescript Reminders-ut-handler
                                             Reminders-reminderProps-handler "\
tell application \"Reminders\"
  set myReminders to {}
  set myRemindersRef to a reference to myReminders
  set AppleScript's text item delimiters to {\"---\"}
  set nil to missing value
  repeat with r in (every reminder of list #{list} in account #{account})
    set temp to my reminderProps(r)
    if (#{qs} is missing value) or #{qs} is in temp then
      copy temp to end of myRemindersRef
    end if
  end repeat
  set AppleScript's text item delimiters to {\"###\"}
  return myRemindersRef as text
end tell"))
                          "###" t)))
    (mapcar #'Reminders-to-plist rs)))

(defun Reminders-reminders (&optional qs)
  "Return all reminders as a tree."
  (loop for a in (Reminders-accounts)
        collect (cons a (loop for l in (Reminders-lists (car a))
                              collect (cons l (Reminders-reminders-1
                                               (car a) (car l) qs))))))

(defun Reminders-normalise (r)
  (loop for x in r
        collect (cond ((not (stringp x)) x)
                      ((equal x "missing value") nil)
                      ((equal x "true") t)
                      ((equal x "false") nil)
                      (t x))))

;;; FIXME: too slow
(defun Reminders-update (data)
  (destructuring-bind (&key reminder-id name body priority due-date completion-date
                            remind-me-date container modification-date
                            &allow-other-keys)
      data
    (let ((priority (or priority 0)))
      (read (applescript Reminders-ut-handler
                         Reminders-dateFromUT-handler
                         Reminders-reminderProps-handler
                         "set nil to missing value
tell application \"Reminders\"
  if #{container} is not missing value then
    set l to first list whose id is #{container}
  else
    set l to default list
  end if

  if (class of l) is not list then
    error \"Cannot not find a container\"
  end if

  if #{reminder-id} is missing value then
    set r to make new reminder in l
  else
    set r to first reminder whose id is #{reminder-id}

    -- Mind the round-off error by org mode
    if #{modification-date} is not missing value and \
          (modification date of r) - my dateFromUT(#{modification-date}) > 59
      error \"Reminder is modified outside org\"
    end if
  end if

  set name of r to #{name}
  set priority of r to #{priority}

  if #{body} is missing value or #{body} is \"\" then
    delete body of r
  else
    set body of r to #{body}
  end if

  -- XXX: due date and remind me date are the same as of 2013-09-16
  if #{due-date} is missing value and #{remind-me-date} is missing value then
    -- NB: delete seems slow
    if due date of r is not missing value then
      delete due date of r
    else if remind me date of r is not missing value
      delete remind me date of r
    end if
  else if #{due-date} is not missing value
    set due date of r to my dateFromUT(#{due-date})
  else
    set remind me date of r to my dateFromUT(#{remind-me-date})
  end if

  if #{completion-date} is missing value then
    if completion date of r is not missing value then
      delete completion date of r
    end if
    set completed of r to false
  else
    set completion date of r to my dateFromUT(#{completion-date})
    set completed of r to true
  end if
  set AppleScript's text item delimiters to {\"---\"}
  return my reminderProps(r)
end tell")))))

(defun Reminders-normalise-org (p)
  (loop for (k v) on p by #'cddr
        collect k
        collect (pcase k
                  (:priority (pcase v
                               (?A 1)
                               (?B 5)
                               (?C 9)
                               (t 0)))
                  ((pred (lambda (x)
                           (and v (string-match-p "-date\\'" (symbol-name x)))))
                   (float-time (apply #'encode-time (org-parse-time-string v))))
                  (t v))))

(defun Reminders-from-org-data (data)
  (let ((r (make-symbol "reminder")))
    (org-element-map data '(headline planning node-property)
      (lambda (x)
        (pcase (car x)
          (`headline
           (put r :name (org-element-property :raw-value x))
           (put r :priority (org-element-property :priority x)))
          (`planning
           (put r :due-date
                (org-element-property :raw-value
                                      (org-element-property :deadline x)))
           (put r :remind-me-date
                (org-element-property :raw-value
                                      (org-element-property :scheduled x)))
           (put r :completion-date
                (org-element-property :raw-value
                                      (org-element-property :closed x))))
          (`node-property
           (put r (intern (concat ":" (org-element-property :key x)))
                (org-element-property :value x))))))
    (symbol-plist r)))

(defun Reminders-insert-reminder (r)
  (destructuring-bind (&key name reminder-id body completed completion-date
                            creation-date due-date modification-date
                            remind-me-date priority)
      (Reminders-normalise r)
    (insert (make-string (1+ (* 2 (org-level-increment))) ?*) " "
            (if completed "DONE " "")
            (pcase (string-to-number priority)
              (0 "")
              (1 "[#A] ")
              (5 "[#B] ")
              (9 "[#C] ")
              (t ""))
            name "\n")
    (when body
      (indent-rigidly (point)
                      (progn (insert body) (point))
                      (+ 2 (* 2 (org-level-increment))))
      (or (bolp) (insert "\n")))
    (save-excursion
      (forward-line -1)
      (org-set-property "reminder-id" reminder-id)
      (when (> due-date 0)
        (org-add-planning-info
         'deadline (Reminders-seconds-to-org due-date)))
      (when (and (> remind-me-date 0) (/= remind-me-date due-date))
        (org-add-planning-info
         'scheduled (Reminders-seconds-to-org remind-me-date)))
      (when (> completion-date 0)
        ;; NOTE: seconds are lost
        (org-add-planning-info
         'closed (Reminders-seconds-to-org completion-date)))
      (org-set-property "creation-date"
                        (Reminders-seconds-to-org creation-date))
      (org-set-property "modification-date"
                        (Reminders-seconds-to-org modification-date)))))

(defun Reminders-kill-org-subtree ()
  (org-back-to-heading t)
  (kill-region (point) (progn (org-end-of-subtree t t) (point))))

(defun Reminders-seconds-to-org (s)
  (format-time-string (cdr org-time-stamp-formats) (seconds-to-time s)))

(defun Reminders-update-from-org ()
  (when (= 3 (org-reduced-level (org-current-level)))
    (pcase-let* ((ws (window-start))
                 (pt (point))
                 (list-id (org-entry-get-with-inheritance "list-id"))
                 (elems (Reminders-from-org-data
                         (save-restriction
                           (widen)
                           (org-narrow-to-subtree)
                           (org-element-parse-buffer))))
                 (body (org-export-as 'ascii t nil t))
                 (r (Reminders-update
                     (Reminders-normalise-org
                      (plist-put (plist-put elems :body body) :container list-id)))))
      (Reminders-kill-org-subtree)
      (Reminders-insert-reminder (Reminders-to-plist r))
      (set-window-start nil ws)
      (goto-char pt))
    (message "Current reminder updated")
    'synced))

(defun Reminders-sort (rs)
  "Sort reminders in date reverse order with completed at the end."
  (sort rs (lambda (r1 r2)
             (let ((r1 (Reminders-normalise r1))
                   (r2 (Reminders-normalise r2)))
               (cond
                ((and (plist-get r1 :completed)
                      (not (plist-get r2 :completed)))
                 nil)
                ((and (not (plist-get r1 :completed))
                      (plist-get r2 :completed))
                 t)
                (t (> (plist-get r1 :creation-date)
                      (plist-get r2 :creation-date))))))))

;;; NOTE: with the introduction of nadvice.el in emacs 24.4 it might
;;; be alright to use advice after all.
(defadvice org-kill-line (around delete-reminder activate)
  (let ((reminder-id (when (and (bolp)
                                (org-at-heading-p)
                                (= 3 (org-reduced-level (org-current-level)))
                                (org-entry-get (point) "reminder-id")
                                (yes-or-no-p "Delete this reminder? "))
                       (org-entry-get (point) "reminder-id"))))
    (if (not reminder-id) ad-do-it
      (applescript "tell application \"Reminders\" to delete \
 (first reminder whose id is #{reminder-id})")
      (kill-region (point) (progn (org-end-of-subtree t t) (point))))))

;;;###autoload
(defun Reminders-new-reminder (name body &rest props)
  (interactive (list (read-string "Name: ") (read-string "Body: ")))
  (check-type name string)
  (when (equal name "")
    (error "Name is empty"))
  (prog1 (if (consp props)
             (Reminders-update (list* :name name :body body props))
           ;; Optimise for a common case.
           (applescript "\
tell application \"Reminders\"
  set nil to missing value
  if #{body} is missing value then
    set props to {name:#{name}}
  else
    set props to {name:#{name},body:#{body}}
  end
  make new reminder in default list with properties props
end tell"))
    (when (called-interactively-p 'interactive)
      (message "done"))))

;;;###autoload
(defalias 'Reminders 'Reminders-to-org)

;;;###autoload
(defun Reminders-to-org (&optional qs buffer)
  "Import all reminders from Reminders.app to an org buffer.
Mapping between reminder and org
    remind me date  => scheduled
    due date        => deadline
    completion date => closed

Note: seconds may be rounded off due to limits of org."
  (interactive
   (prog1 (list (read-string "Query string: "))
     (ignore (message "Pulling reminders ...") (sit-for 0))))
  (let* ((qs (if (equal qs "") nil qs))
         (buffer (or buffer "*Reminders*"))
         ;; Oddly in applescript "" is in "whatever" is false
         (reminders (Reminders-reminders qs)))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert "#+TITLE: Reminders.app\n#+TODO: TODO | DONE\n#+STARTUP: logdone\n\n")
      (org-mode)
      (add-hook 'org-ctrl-c-ctrl-c-hook #'Reminders-update-from-org nil t)
      (dolist (a reminders)
        (insert "* " (car (car a)) "\n")
        (org-set-property "account-id" (cadr (car a)))
        (dolist (l (cdr a))
          (insert (make-string (1+ (org-level-increment)) ?*) " " (car (car l)) "\n")
          (org-set-property "list-id" (cadr (car l)))
          (when (and qs (not (consp (cdr l))))
            (kill-buffer (current-buffer))
            (error "No reminders matching `%s' found" qs))
          (dolist (r (Reminders-sort (cdr l)))
            (Reminders-insert-reminder r))))
      (goto-char (point-min))
      (org-content (1+ (* 2 (org-level-increment))))
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto noconfirm)
                    (when (or noconfirm
                              (yes-or-no-p "Pull from Reminders.app? "))
                      (Reminders-to-org qs buffer))))
      (switch-to-buffer (current-buffer)))))

(provide 'Reminders)
;;; Reminders.el ends here
