2;; -*- lexical-binding: t -*-
;;; annals.el --- EMACS task based session manager and developer notebook 

;; Copyright (c) Matthew O. Smith <matt@m0smith.com>
;;
;; Author: 
;;     Matthew Smith <matt@m0smith.com>
;; URL: http://www.github.com/m0smith/annals
;; Version: 0.0.1
;; Package-Requires: (())
;; Keywords: task, notebook, jira, mylyn

;;; License:

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA.
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; See http://www.github.com/m0smith/annals/README.md
;;


;;; Customization:

(defgroup annals nil
  "EMACS task based session manager and developer notebook"
  :prefix "annals-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/m0smith/annals/"))

(defcustom annals-active-directory (expand-file-name "~/annals")
  "Directory where all the tasks live"
  :group 'annals
  :package-version '(annals . "1.0")
  :type 'directory)

(defcustom annals-jira-server nil
  "The URL for the JIRA server or nil if it is not used"
  :group 'annals
  :package-version '(annals . "1.0")
  :type 'string)

;;; Code:

(defun annals-file-name-default (_task-id)
  "The name of the task note file."
 "annals.org")


(defun annals-json-call (url)
  "SERVICE is a known service to the malabat server 

   ARGS-PLIST is a list of '(key val key val ...). If pm is not
  in the list, is is pulled from buffer.  Skip entries with a nil key or value

  ARRAY-TYPE is for the JSON reader and can be 'list or 'vector.  Default to vector.

  OBJECT-TYPE is for the JSON reader and can be `alist', `plist',
  or `hash-table'.  Default to `alist'.

  READTABLE is the JSON readtable, default to `json-reatable'."

  (setq url-request-method "GET"
	url-request-extra-headers nil
	url-request-data nil)
  
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char url-http-end-of-headers)
    (setq json-array-type  'vector
	  json-object-type 'alist)
    
    (let* ((readtable nil)
	   (json-readtable-old (when readtable 
				(let ((r json-readtable))
				  (setq json-readtable readtable) 
				  r)))
	  (rtnval (json-read)))
      (kill-buffer (current-buffer))
      (when readtable (setq json-readtable json-readtable-old))
      rtnval)))

(defun annals-jira-rest-url (issue-id)
  (format "%s/rest/api/latest/issue/%s" annals-jira-server issue-id))

(defun annals-jira-browse-url (issue-id)
  (format "%s/browse/%s" annals-jira-server issue-id))

(defun annals-jira (issue-id)
  (let ((url (annals-jira-rest-url issue-id)))
    (unless (url-get-authentication url nil 'any t)
      (url-basic-auth (url-generic-parse-url url) t))
    (annals-json-call url)))

(defun annals-jira-summary (issue-or-id)
  " ISSUE-OR-ID can either be a string Jira ID like \"ABC-123\" or the result of calling `annals-jira'"
  (let ((jira-issue (if (stringp issue-or-id) (annals-jira issue-or-id) issue-or-id)))
    (cdr (assoc 'summary (cdr (assoc 'fields jira-issue))))))

(defun annals-jira-self (issue-or-id)
  " ISSUE-OR-ID can either be a string Jira ID like \"ABC-123\" or the result of calling `annals-jira'"
  (let ((jira-issue (if (stringp issue-or-id) (annals-jira issue-or-id) issue-or-id)))
    (cdr (assoc 'self jira-issue))))


(defun annals-create-file (task-id file-name)
  "Create the note file for the task.  Pull information from Jira if TASK-ID is a Jira issue-id."
  (let* ((jira-issue (annals-jira task-id))
	 (jira-summary (annals-jira-summary jira-issue))
	 (title (if jira-summary 
		    (format "* [[%s][%s]] %s\n\n" (annals-jira-browse-url task-id) task-id jira-summary) 
		  (format "* %s" task-id))))
    (write-region title "" file-name)))

;;;###autoload
(defun annals-task (task-id)
  "Start a new task is TASK-ID as the `desktop-dirname'.  This closes
open buffers and saves the active desktop.  

It also creates and opens a note file file to keep notes in. If
TASK-ID is a Jira task \"ABC-123\" and `annals-jira-server' is
not null, then the note file will default to a link to the issue and its summary.

If the currently active task is selected, simply call `annals-checkpoint'.
"
  (interactive "sAnnal Task Id:")
  (annals-checkpoint)
  (let* ((full-dir (expand-file-name task-id annals-active-directory))
	 (annal-file (expand-file-name (annals-file-name-default task-id) full-dir)))
    (unless (string= desktop-dirname full-dir)
      (unless (file-directory-p full-dir)
	(make-directory full-dir t))
      (desktop-read full-dir)
      (unless (file-regular-p annal-file)
	(annals-create-file task-id annal-file))
      (find-file-other-window annal-file))))


(defun annals-checkpoint ()
  "Save the current state and keep it active"
  (interactive)
  (desktop-save-in-desktop-dir))

(provide 'annals)

;;; annals ends here
