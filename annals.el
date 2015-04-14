;; -*- lexical-binding: t -*-
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
;; m0smith-malabar-mode-199

;;; Customization:

(defgroup annals nil
  "EMACS task based session manager and developer notebook"
  :prefix "annals-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/m0smith/annals/"))

(defcustom annals-active-directory (expand-file-name "~/annals")
  "Directory where all the active tasks live"
  :group 'annals
  :package-version '(annals . "1.0")
  :type 'directory)

(defcustom annals-archive-directory (expand-file-name "~/annals/.archive")
  "Directory where all the archived tasks live"
  :group 'annals
  :package-version '(annals . "1.0")
  :type 'directory)

(defcustom annals-jira-server nil
  "The URL for the JIRA server or nil if it is not used"
  :group 'annals
  :package-version '(annals . "1.0")
  :type '(choice (const  :tag "Disabled" nil)
		 (string :tag "URL")))

(defcustom annals-github-api-server "https://api.github.com"
  "The URL for the Github API server or nil if it is not used"
  :group 'annals
  :package-version '(annals . "1.0")
  :type '(choice (const  :tag "Disabled" nil)
		 (string :tag "URL")))

(defcustom annals-github-browser-server "https://github.com"
  "The URL for the Github browser or nil if it is not used"
  :group 'annals
  :package-version '(annals . "1.0")
  :type '(choice (const  :tag "Disabled" nil)
		 (string :tag "URL")))


(defcustom annals-github-task-separator "-"
  "Separator used in task-id to separator repo, project, and
  issue id.  For example, if the annals-github-task-separator
  were set to \"_\" then the task id for the first issue of the
  annals project would be \"m0smith.annals.1\"."
  :group 'annals
  :package-version '(annals . "1.0")
  :type 'string)


(defvar annals-active-task-id nil
  "The currently active task id")

;;; Code:

(defun annals-file-name-default (_task-id)
  "The name of the task note file."
 "annals.org")


(defun annals-json-call (url)
  "Call a URL expecting JSON back.  Return the JSON formatted as vectors and alists for the arrays and maps.

URL is the REST URL to call."

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

(defun annals-task-summary (task-id task-dir)
  "Given a task dir, extract the summary from the first line of the annals.org"
  (let ((file-name (expand-file-name (annals-file-name-default task-id) task-dir)))
    (when (file-readable-p file-name)
      (with-temp-buffer
	(insert-file-contents (expand-file-name (annals-file-name-default task-id) task-dir) nil 0 150 t)
	(let ((start (progn (beginning-of-line) (point)))
	      (end (progn (end-of-line) (point))))
	  (goto-char start)
	  (search-forward "]]" end t)
	  (concat task-id " " (buffer-substring (point) end)))))))


(defun annals-list-tasks ()
  "Return an alist of task summary to task id.  See `annals-task-summary' for how the summary is created"
  (let ((task-ids (directory-files annals-active-directory nil "^[^.]"))
	(task-dirs (directory-files annals-active-directory t "^[^.]")))
    (mapcar (lambda (i) (cons  (annals-task-summary (car i) (cdr i)) (car i))) (-zip task-ids task-dirs))))


;;; 
;;; JIRA
;;

(defun annals-jira-rest-url (issue-id)
  (when (and annals-jira-server (string-match "[A-Z]+-[0-9]+"))
    (format "%s/rest/api/latest/issue/%s" annals-jira-server issue-id)))

(defun annals-jira-browse-url (issue-id)
  (when (and annals-jira-server (string-match "[A-Z]+-[0-9]+"))
      (format "%s/browse/%s" annals-jira-server issue-id)))

(defun annals-jira (issue-id)
  (-when-let ((url (annals-jira-rest-url issue-id)))
    (unless (url-get-authentication url nil 'any t)
      (url-basic-auth (url-generic-parse-url url) t))
    (annals-json-call url)))

(defun annals-jira-attribute (issue-or-id &rest keys)
  " ISSUE-OR-ID can either be a string Jira ID like \"ABC-123\" or the result of calling `annals-jira'"
  (when annals-jira-server
    (let ((jira-issue (if (stringp issue-or-id) (annals-jira issue-or-id) issue-or-id)))
      (-reduce-from (lambda (map key) (cdr (assoc key map))) jira-issue keys))))
 
(defun annals-jira-summary (issue-or-id)
  " ISSUE-OR-ID can either be a string Jira ID like \"ABC-123\" or the result of calling `annals-jira'"
  (annals-jira-attribute issue-or-id 'fields 'summary))

(defun annals-jira-self (issue-or-id)
  " ISSUE-OR-ID can either be a string Jira ID like \"ABC-123\" or the result of calling `annals-jira'"
  (annals-jira-attribute issue-or-id 'self))


(defun annals-jira-create-file (task-id file-name)
  "Create the note file for the task.  Pull information from Jira if TASK-ID is a Jira issue-id.  Return FILE-NAME if there is a Jira issue or nil."
  (let* ((jira-issue (annals-jira task-id))
	 (jira-summary (annals-jira-summary jira-issue))
	 (title (when jira-summary 
		    (format "* [[%s][%s]] %s\n\n" (annals-jira-browse-url task-id) task-id jira-summary) )))
		  
    (when title 
      (write-region title "" file-name)
      file-name)))


;;;
;;; Github
;;;

(defun annals-github-format ( pattern url issue-id)
  (let ((r-pat (format "^\\([a-zA-Z0-9]+\\)%s\\([-0-9a-zA-Z]+\\)%s\\([0-9]+\\)$" 
		       annals-github-task-separator
		       annals-github-task-separator)))
    (when (string-match r-pat issue-id)
      (format pattern url (match-string 1 issue-id) 
	      (match-string 2 issue-id) 
	      (match-string 3 issue-id))))
  
(defun annals-github-rest-url (issue-id)
"Looks like https://api.github.com/repos/m0smith/malabar-mode/issues/134"
  (when annals-github-api-server
    (annals-github-format "%s/repos/%s/%s/issues/%s" annals-github-api-server issue-id)))

(defun annals-github-browse-url (issue-id)
"Looks like https://github.com/m0smith/malabar-mode/issues/134"
  (when annals-github-browser-server
      (annals-github-format "%s/%s/%s/issues/%s" annals-github-browser-server issue-id)))

(defun annals-github (issue-id)
  (-when-let (url (annals-github-rest-url issue-id))
    (unless (url-get-authentication url nil 'any t)
      (url-basic-auth (url-generic-parse-url url) t))
    (annals-json-call url)))

(defun annals-github-create-file (task-id file-name)
  "Create the note file for the task.  Pull information from Jira if TASK-ID is a Jira issue-id.  Return FILE-NAME if there is a Jira issue or nil."
  (let* ((github-issue (annals-github task-id))
	 (github-summary (annals-jira-attribute github-issue 'title))
	 (title (when github-summary 
		    (format "* [[%s][%s]] %s\n\n" (annals-github-browse-url task-id) task-id github-summary) )))
		  
    (when title 
      (write-region title "" file-name)
      file-name)))

(defun annals-default-create-file (task-id file-name)
    (when task-id 
      (write-region (format "* %s\n\n" task-id)  "" file-name)
      file-name))

(defun annals-read-task-id ()
  "Prompts users for a task from the available tasks or allows the
user to enter a new task id"
  (let* ((tasks (annals-list-tasks))
	 ;(def (car (rassoc annals-active-task-id tasks)))
	 (prompt (format "Annals Task Id (default: %s): " annals-active-task-id))
	 (key (completing-read prompt  tasks nil 'confirm))
	 (val (cdr (assoc key tasks))))
    (or val (if (= 0 (length key)) annals-active-task-id key))))
    

;;;###autoload
(defun annals-task (task-id)
  "Start a new task is TASK-ID as the `desktop-dirname'.  This closes
open buffers and saves the active desktop.  

It also creates and opens a note file file to keep notes in. If
TASK-ID is a Jira task \"ABC-123\" and `annals-jira-server' is
not null, then the note file will default to a link to the issue and its summary.

If the currently active task is selected, simply call `annals-checkpoint'.
"
  (interactive (list (annals-read-task-id)))
  (annals-checkpoint)
  
  (let* ((full-dir (expand-file-name task-id annals-active-directory))
	 (annal-file (expand-file-name (annals-file-name-default task-id) full-dir)))
    (unless (string= (file-name-as-directory desktop-dirname) 
		     (file-name-as-directory full-dir))
      (desktop-kill)
      (unless (file-directory-p full-dir)
	(make-directory full-dir t))
      (desktop-read full-dir)
      (setq annals-active-task-id task-id)
      (unless (file-regular-p annal-file)
	(or
	 (annals-jira-create-file task-id annal-file)
	 (annals-github-create-file task-id annal-file)
	 (annals-default-create-file task-id annal-file)))
      (find-file-other-window annal-file))))


;;;###autoload
(defun annals-archive (task-id)
  "Archive task is TASK-ID.  This closes
open buffers and saves the active desktop.  

It also moves the task to the archive dir `annals-archive-directory'.
"
  (interactive (list (annals-read-task-id)))
  (annals-checkpoint)
  (make-directory (expand-file-name annals-archive-directory) t)
  (rename-file (expand-file-name task-id annals-active-directory)
	       (expand-file-name task-id annals-archive-directory)))

(defun annals-checkpoint ()
  "Save the current state and keep it active"
  (interactive)
  (desktop-save-in-desktop-dir))

(provide 'annals)

;;; annals ends here
