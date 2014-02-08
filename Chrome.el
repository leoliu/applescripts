;;; Chrome.el --- Chrome.app                         -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Keywords: tools, processes

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

;; Control Chrome from Emacs.

;;; Code:

(eval-when-compile
  (require 'applescript)
  (require 'cl))

(defcustom Chrome-application-name "Google Chrome Canary"
  "Chrome application name."
  :type 'string
  :group 'applescript)

(defun Chrome-read-tab ()
  (let ((tabs (split-string (read (applescript "
tell application #{Chrome-application-name}
  set allTabs to {}
  repeat with w in windows
    repeat with x in tabs of w
      copy URL of x as text to end of allTabs
    end repeat
  end repeat
  set AppleScript's text item delimiters to {\"----\"}
  return allTabs as text
end tell"))
                            "----")))
    (completing-read "Chrome tab: " tabs nil t)))

;;;###autoload
(defun Chrome-reload (&optional tab)
  (interactive (and current-prefix-arg (list (Chrome-read-tab))))
  (applescript "tell application #{Chrome-application-name}
set nil to missing value
  if #{tab} is missing value then
    set x to active tab of front window
  else
    set x to first tab of some window whose URL is #{tab}
  end if
  tell x to reload
end tell"))

;;;###autoload
(defun Chrome-browse-url-helper (url prefix &optional background)
  (let ((url (substring-no-properties url))
        (prefix (substring-no-properties prefix))
        (background (if background "true" "false")))
    (applescript "tell application #{Chrome-application-name}
  try
    set x to (first tab of front window whose URL starts with #{prefix})
  on error
    tell front window
      set x to make tab
    end tell
  end try
  if x is loading then
    tell x to stop
  end if
  set URL of x to #{url}
  if #{background} is \"false\" then
    tell x to activate
  end if
end tell")))

(provide 'Chrome)
;;; Chrome.el ends here
