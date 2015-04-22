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

(defvar annals-session-stamp nil
  "The session name.  Just set each time a task is started.")

(defvar-local annals-buffer-name nil
  "A buffer local variable to control inclusion of non-file
  buffers in an annal.  If this is non-nil, it will mark a buffer
  for saving to this name when `annals-checkpoint' is called.")

(defvar annals-buffer-name-counter 1)

(eval-after-load "desktop"
  '(progn
     (add-to-list 'desktop-globals-to-save 'annals-buffer-name-counter)))

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
  (when (and annals-jira-server (string-match "[A-Z]+-[0-9]+" issue-id))
    (format "%s/rest/api/latest/issue/%s" annals-jira-server issue-id)))

(defun annals-jira-browse-url (issue-id)
  (when (and annals-jira-server (string-match "[A-Z]+-[0-9]+" issue-id))
      (format "%s/browse/%s" annals-jira-server issue-id)))

(defun annals-jira (issue-id)
  (setq 
	url-http-extra-headers nil
	)

  (-when-let (url (annals-jira-rest-url issue-id))    
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
	      (match-string 3 issue-id)))))
  
(defun annals-github-rest-url (issue-id)
"Looks like https://api.github.com/repos/m0smith/malabar-mode/issues/134"
  (when annals-github-api-server
    (annals-github-format "%s/repos/%s/%s/issues/%s" annals-github-api-server issue-id)))

(defun annals-github (issue-id)
  "Pull the JSON info for the github issue."
  (-when-let (url (annals-github-rest-url issue-id))
    (unless (url-get-authentication url nil 'any t)
      (url-basic-auth (url-generic-parse-url url) t))
    (annals-json-call url)))

(defun annals-github-create-file (task-id file-name)
  "Create the note file for the task.  Pull information from Jira if TASK-ID is a Jira issue-id.  Return FILE-NAME if there is a Jira issue or nil."
  (let* ((github-issue (annals-github task-id))
	 (github-summary (annals-jira-attribute github-issue 'title))
	 (github-url (annals-jira-attribute github-issue 'html_url))
	 (title (when github-summary 
		    (format "* [[%s][%s]] %s\n\n" github-url task-id github-summary) )))
		  
    (when title 
      (write-region title "" file-name)
      file-name)))

;;;
;;; dired minor mode
;;;

(defun annals-dired-task () 
 "In Dired, make the thing at point the active task, if it is a task."
 (interactive)
 (let* ((full-name (dired-file-name-at-point))
	(desktop-full-name (desktop-full-file-name full-name)))
   (if (file-readable-p desktop-full-name)
       (annals-task (file-name-nondirectory (directory-file-name full-name)))
     (message "Not an annuls dir"))))

(defun annals-dired-archive () 
 "In Dired, archive  the thing at point, if it is a task."
 (interactive)
 (let* ((full-name (dired-file-name-at-point))
	(desktop-full-name (desktop-full-file-name full-name)))
   (if (file-readable-p desktop-full-name)
       (annals-archive (file-name-nondirectory (directory-file-name full-name)))
     (message "Not an annuls dir"))))


(define-minor-mode annals-dired-mode
  "Toggle Annals-Dired mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Annals-Dired mode is enabled, the control delete key
gobbles all preceding whitespace except the last.
See the command \\[annals-dired-task]."
 ;; The initial value.
 nil
 ;; The indicator for the mode line.
 ""
 ;; The minor mode bindings.
 '(
   ((kbd "|a") . annals-dired-task)
   ((kbd "|z") . annals-dired-archive)
)
 :group 'annals)


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





(defun annals-task-directory (task-id)
  "Return the directory associated with a task.  Should be equivilent to `desktop-dirname', but not necessarily equal to the  `annals-active-task-id' when TASK-ID is the active task"
  (let* ((full-dir (expand-file-name task-id annals-active-directory)))
    full-dir))

(defun annals-write-non-file-buffer (task-id buffer)
  (with-current-buffer buffer
    (when (and annals-buffer-name annals-session-stamp)
      (let* ((file-name (format "%s-%s" annals-buffer-name annals-session-stamp))
	    (full-name (expand-file-name file-name (annals-task-directory task-id))))
	(write-region (format "%s -*- mode: %s ; -*- %s\n" 
			      (or comment-start "") 
			      mode-name 
			      (or comment-end ""))
		      nil full-name)
	(write-region nil nil full-name t)))))
	   

(defun annals-write-non-file-buffers ()
  (when annals-active-task-id
    (mapcar (lambda (b) (annals-write-non-file-buffer annals-active-task-id b)) (buffer-list))))


(defun annals-buffer-name-counter-next () 
  (let ((rtnval annals-buffer-name-counter))
    (setq annals-buffer-name-counter (+ 1 annals-buffer-name-counter))
;    (add-dir-local-variable nil "annals-buffer-name-counter
    rtnval))

;;;###autoload
(defun annals-buffer-name-create (&optional buffer)
  "Add the buffer local variable `annals-buffer-name' to BUFFER, default to 
the current buffer.

Called interactively applies to the current buffer.

Called in a buffer that already has `annals-buffer-name' set,
will give it a new name.  This allows the user to keep the old
version and start building a new version.

Use this function in a hook to add the `annals-buffer-name' to
a buffer.  Only appropriate for non-file buffers.  File buffers
are already handled.

Example:  

     (add-hook 'sql-login-hook 'annals-buffer-name-create)
"
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq annals-buffer-name
	  (format "%s.%d"
		  (replace-regexp-in-string "[*]" "" 
					    (replace-regexp-in-string "[: ]" "-" (buffer-name)))
		  (annals-buffer-name-counter-next)))))





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
	 (desktop-save-mode t)
	 (annal-file (expand-file-name (annals-file-name-default task-id) full-dir)))
    (unless (and (boundp 'desktop-dirname) desktop-dirname full-dir
		 (string= (file-name-as-directory desktop-dirname) 
			  (file-name-as-directory full-dir)))
      (annals-suspend)
      (unless (file-directory-p full-dir)
	(make-directory full-dir t))
      (ignore-errors
	(desktop-read full-dir))
      (setq annals-active-task-id task-id
	    annals-session-stamp (format-time-string "%Y-%m-%d"))
      (unless (file-regular-p annal-file)
	(or
	 (annals-jira-create-file task-id annal-file)
	 (annals-github-create-file task-id annal-file)
	 (annals-default-create-file task-id annal-file)))
      (find-file-other-window annal-file)
      (run-hooks 'annals-task-hook)
      (find-file-other-window full-dir))))

;;;###autoload
(defun annals-archive (task-id)
  "Archive task is TASK-ID.  If the current task is the one being
archived, closes open buffers and saves the active desktop before .

It also moves the task to the archive dir `annals-archive-directory'.
"
  (interactive (list (annals-read-task-id)))

  (if (and (stringp task-id) (string= task-id annals-active-task-id))
      (annals-suspend)
    (annals-checkpoint))

  (make-directory (expand-file-name annals-archive-directory) t)
  (rename-file (expand-file-name task-id annals-active-directory)
	       (expand-file-name task-id annals-archive-directory)))

(defun annals-checkpoint ()
  "Save the current state and keep it active"
  (interactive)
  (if (and (boundp 'desktop-dirname) desktop-dirname)
      (progn
	(desktop-save-in-desktop-dir)
	(annals-write-non-file-buffers))
    (message "annals is not active")))

(defun annals-suspend ()
  "Save the active desktop and turn off the annals feature."
  (interactive)
  (when (and (boundp 'desktop-dirname) desktop-dirname)
    (desktop-save desktop-dirname t)
    (setq annals-active-task-id nil
	  annals-session-stamp nil
	  annals-buffer-name-counter 1
	  desktop-dirname nil)))


(add-hook 'desktop-no-desktop-file-hook 
	  (lambda ()
	    (setq annals-buffer-name-counter 1)))

(provide 'annals)

;;; annals ends here
