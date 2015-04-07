2;; -*- lexical-binding: t -*-
;;; annals.el --- EMACS task based session manager and developer notebook

;; Copyright (c) Matthew O. Smith <matt@m0smith.com>
;;
;; Author: 
;;     Matthew Smith <matt@m0smith.com>
;; URL: http://www.github.com/m0smith/annals
;; Version: 0.0.1
;; Package-Requires: (())
;; Keywords: task, notebook

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


;;; Code:

(defun annals-task (dir)
  "Start a new task is DIR as the `desktop-dirname'.  This closes
open buffers and saves the active desktop."
  (interactive "GAnnal Dir:")
  (let ((full-dir (expand-file-name dir)))
    (unless (file-directory-p full-dir)
      (make-directory full-dir t))
    (desktop-read full-dir)))

(provide 'annals)

;;; annals ends here
