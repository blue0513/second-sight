;;; second-sight --- quickly view file-contents with posframe

;; Copyright (C) 2019- blue0513

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: blue0513
;; URL: https://github.com/blue0513/second-sight
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (posframe "0.4.3"))

;;; Commentary:

;; Edit your init.el
;;
;; (require 'second-sight)
;;

;;; Code:

(require 'posframe)

(defvar second-sight-buffer "*second-sight-buffer*")
(defvar second-sight-buffer-showing nil)
(defvar second-sight-buffer-min-width 20)
(defvar second-sight-buffer-color "black")

(defun second-sight--get-file-content (full-path)
  "Read contents from FULL-PATH."
  (with-temp-buffer
    (insert-file-contents full-path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun second-sight--generate-frame (string)
  "Create posframe with STRING by using `posframe-show'."
  (posframe-show
   second-sight-buffer
   :string (or string "No file content found")
   :background-color second-sight-buffer-color
   :min-width second-sight-buffer-min-width
   :position (point)))

(defun second-sight--delete-frame ()
  "Delete posframe."
  (setq second-sight-buffer-showing nil)
  (posframe-delete second-sight-buffer))

(defun second-sight--show-frame (content)
  "Show posframe with CONTENT."
  (setq second-sight-buffer-showing t)
  (second-sight--generate-frame content))

(defun second-sight--show-file (full-path)
  "Show posframe with file-contents in FULL-PATH if possible."
  (if (and (null second-sight-buffer-showing)
	   (file-exists-p full-path)
	   (posframe-workable-p))
      (let* ((content (second-sight--get-file-content full-path)))
	(second-sight--show-frame content))))

(defun second-sight--show-file-safely (full-path)
  "Show posframe with file-contents in FULL-PATH after validations."
  (if second-sight-buffer-showing
      (second-sight--delete-frame)
    (if (and full-path
	     (file-exists-p full-path)
	     (file-exists-p (expand-file-name full-path)))
	(let* ((target-path (expand-file-name full-path)))
	  (second-sight--show-file target-path))
      (message (format "No valid path found: %s" full-path)))))

(defun second-sight-file (full-path)
  "Basic function to show second-sight's posframe with file in FULL-PATH."
  (second-sight--show-file-safely full-path))

(defun second-sight-delete-frame ()
  "Delete second-sight's posframe."
  (interactive)
  (second-sight--delete-frame))

(defun second-sight-at-point ()
  "Second-sight for `thing-at-point'."
  (interactive)
  (let* ((full-path (thing-at-point 'symbol)))
    (second-sight-file full-path)))

(defun second-sight-dired ()
  "Second-sight for `dired'."
  (interactive)
  (let* ((full-path (dired-get-filename)))
    (second-sight-file full-path)))

(defun second-sight-counsel ()
  "Second-sight for `counsel-find-file', `counsel-recentf', `counsel-git'.
And for `dumb-jump_with_ivy'."
  (interactive)
  (let* ((raw-string (ivy-state-current ivy-last))
	 (filename (replace-regexp-in-string "\\:[0-9]\\:.*" "" raw-string))
	 (full-path (if (file-exists-p (expand-file-name filename))
			(expand-file-name filename)
		      (expand-file-name
		       (concat "~" (thing-at-point 'symbol) filename)))))
    (second-sight-file full-path)))

(defun second-sight ()
  "Quickly view the file-content without visiting buffer.
This command will call the proper function according to the situation."
  (interactive)
  (cond
   ((eq major-mode 'dired-mode)
    (second-sight-dired))
   ((file-exists-p (format "%s" (thing-at-point 'symbol)))
    (second-sight-at-point))
   ((and (bound-and-true-p ivy-mode) (active-minibuffer-window))
    (second-sight-counsel))
   (second-sight-buffer-showing
    (second-sight-delete-frame))
   (t (message "second-sight: No valid command found"))))

;; * provide

(provide 'second-sight)

;;; second-sight.el ends here
