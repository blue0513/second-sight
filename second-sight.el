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

(defun second-sight--get-file-content (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun second-sight--generate-frame (string)
  (posframe-show
   second-sight-buffer
   :string (or string "No file content found")
   :background-color second-sight-buffer-color
   :min-width second-sight-buffer-min-width
   :position (point)))

(defun second-sight--delete-frame ()
  (setq second-sight-buffer-showing nil)
  (posframe-delete-frame second-sight-buffer))

(defun second-sight--show-frame (content)
  (setq second-sight-buffer-showing t)
  (second-sight--generate-frame content))

(defun second-sight--show-file (filename)
  (if (and (null second-sight-buffer-showing)
	   (file-exists-p filename)
	   (posframe-workable-p))
      (let* ((content (second-sight--get-file-content filename)))
	(second-sight--show-frame content))
    (progn
      (second-sight--delete-frame))))

(defun second-sight--show-file-safely (filename)
  (if second-sight-buffer-showing
      (second-sight--delete-frame)
    (if (and filename
	     (file-exists-p filename)
	     (file-exists-p (expand-file-name filename)))
	(let* ((full-filepath (expand-file-name filename)))
	  (second-sight--show-file full-filepath))
      (message "No valid filename found"))))

(defun second-sight-file (filename)
  (second-sight--show-file-safely filename))

(defun second-sight-delete-frame ()
  (interactive)
  (second-sight--delete-frame))

;; For thing-at-point
(defun second-sight-at-point ()
  (interactive)
  (let* ((filename (thing-at-point 'symbol)))
    (second-sight-file filename)))

;; For dired
(defun second-sight-dired ()
  (interactive)
  (let* ((filename (dired-get-filename)))
    (second-sight-file filename)))

;; For counsel-find-file, counsel-recentf, counsel-git, dumb-jump with ivy
(defun second-sight-counsel ()
  (interactive)
  (let* ((raw-string (ivy-state-current ivy-last))
	 (filename (replace-regexp-in-string "\\:[0-9]\\:.*" "" raw-string)))
    (second-sight-file filename)))

;; * provide

(provide 'second-sight)

;;; second-sight.el ends here
