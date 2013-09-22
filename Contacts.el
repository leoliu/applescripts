;;; Contacts.el --- Contacts.app                     -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.5.0
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

;; Allow BBDB to push and pull from Contacts.app.

;;; Code:

(eval-when-compile (require 'applescript))

(eval-and-compile
  (or (fboundp 'user-error)
      (defalias 'user-error 'error)))

(defun Contacts-groups ()
  "List of contact groups."
  (split-string (read (applescript "\
tell application \"Contacts\"
  set gs to {}
  repeat with g in groups
    copy name of g to end of gs
  end repeat
  set AppleScript's text item delimiters to {\"----\"}
  return gs as text
end tell"))
                "----"))

(defcustom Contacts-query-limit 30
  "Maximum number of entries returned by `Contacts-query'."
  :type 'integer
  :group 'applescript)

;;;###autoload
(defun Contacts-query (string &optional group)
  "Get a list of vCards containing STRING.
The list length is limited by `Contacts-query-limit'.
Allow the user to choose a GROUP to query when called with prefix."
  (interactive
   (prog1
       (list (read-string "Query string: ")
             (and current-prefix-arg
                  (completing-read "Which group: " (Contacts-groups) nil t)))
     (ignore (message "Pulling contacts from Contacts...") (sit-for 0.1))))

  (let ((result (split-string (read (applescript "\
tell application \"Contacts\"
  set thePeople to {}
  set nil to missing value
  set G to #{group}
  set Q to #{string}
  ignoring case
    if G is missing value then
      set thePeople to people whose vcard contains Q
    else
      set thePeople to people in group G whose vcard contains Q
    end if
  end ignoring

  if (count thePeople) > #{Contacts-query-limit} then
    set thePeople to items 1 thru #{Contacts-query-limit} of thePeople
  end if

  set theVcards to {}
  -- using a Ref is more efficient
  set theVcardsRef to a reference to theVcards
  repeat with p in thePeople
    copy the vcard of p to the end of theVcardsRef
  end repeat
  set AppleScript's text item delimiters to {\"----\"}
  return theVcardsRef as text
end tell")) "----" t)))
    (when (called-interactively-p 'interactive)
      (if (null result)
          (message "No contacts matching %S" string)
        (help-setup-xref (list #'Contacts-query string group)
                         (called-interactively-p 'interactive))
        (with-help-window (help-buffer)
          (princ (mapconcat 'identity result "\n")))))
    result))

(provide 'Contacts)
;;; Contacts.el ends here
