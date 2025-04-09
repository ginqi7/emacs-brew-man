;;; brew-man.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defcustom brew-man-ruby-command (executable-find "ruby")
  "The ruby command path.")

(defvar brew-man--ruby-file
  (file-name-concat (file-name-directory (or load-file-name (buffer-file-name)))
                    "brew_man.rb"))

(defun brew-man-start ()
  "Start websocket bridge real-time-translation."
  (interactive)
  (websocket-bridge-app-start "brew-man"
                              "ruby"
                              brew-man--ruby-file))


(defun brew-man-restart ()
  "Restart websocket bridge real-time-translation and show process."
  (interactive)
  (websocket-bridge-app-exit "brew-man")
  (brew-man-start)
  (websocket-bridge-app-open-buffer "brew-man"))

(defun brew-man-tap-list ()
  (interactive)
  (websocket-bridge-call "brew-man" "tap-list" #'brew-man-show-tap-list))

(defun brew-man-formula-list ())

(defun brew-man-casks-list ())


(defun brew-man-show-test (&rest args)
  (print args))

(defun brew-man-show-tap-list (&rest tap-list)
  (let ((data (mapcar (lambda (tap) (list tap)) tap-list)))
    (brew-man--tabulated-list-mode
     "*brew-man-tap-list*"
     [("Name" 30 t)
      ("Formula" 8 t)
      ("Casks" 8 t)
      ("Update" 20 t)
      ("" 8 t)]
     0
     data)
    (mapc #'brew-man-tap-info tap-list)))

(defun brew-man-tap-info (tap-name)
  (websocket-bridge-call "brew-man" "tap-info" tap-name #'brew-man-update-tap-entry))

(defun brew-man--update-tabulated-list-entry (new-entry)
  "Update the entry with NAME to have NEW-VALUE."
  (cl-loop for item in (append (nth 1 new-entry) nil)
           for index from 0
           do
           (tabulated-list-set-col index item t)))

(defun brew-man--locate-tabulated-list-entry (entry)
  (let* ((key (car entry))
         (index
          (cl-position-if
           (lambda (item) (equal (car item) key))
           tabulated-list-entries)))
    (goto-line (1+ index))))


(defun brew-man-update-tap-entry (tap-info)
  (with-current-buffer (get-buffer-create "*brew-man-tap-list*")
    (let ((entry (brew-man--list-to-entry tabulated-list-format :name tap-info)))
      (save-excursion
        (brew-man--locate-tabulated-list-entry entry)
        (brew-man--update-tabulated-list-entry entry)))))

(defun brew-man--plist-p (lst)
  "Return t if LIST is a property list, nil otherwise."
  (and (listp lst)                  ; Check if it is a list
       (evenp (length lst))        ; Check if its length is even
       (cl-loop for i from 0 below (/ (length lst) 2)
                always (symbolp (nth (* 2 i) lst))))) ; Check keys are symbols)

(defun brew-man--list-to-entry (header key lst)
  (print (brew-man--plist-p lst))
  (if (brew-man--plist-p lst)
      (brew-man--plist-to-entry header key lst)
    (brew-man--normal-list-to-entry header key lst)))

(defun brew-man--plist-to-entry (header key plst)
  (let* ((header-list (append header nil))
         (header-names (mapcar #'downcase (mapcar #'car header-list)))
         (header-keys (mapcar
                       (lambda (name) (intern (format ":%s" name)))
                       header-names))
         (entry-values (mapcar
                        (lambda (header-key) (or (plist-get plst header-key) ""))
                        header-keys)))
    (list (plist-get plst key) (vconcat entry-values))))

(defun brew-man--normal-list-to-entry (header key lst)
  (let* ((index-seq (number-sequence 0 (1- (length header))))
         (entry-values (mapcar (lambda (n) (or (nth n lst) "")) index-seq)))
    (list (nth key lst) (vconcat entry-values))))

(defun brew-man--tabulated-list-mode (buffer header key data)
  (let ((buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (setq tabulated-list-format header)
      (setq tabulated-list-entries
            (mapcar
             (lambda (row)
               (brew-man--normal-list-to-entry header key row))
             data))
      (tabulated-list-mode)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (pop-to-buffer (current-buffer)))))

(provide 'brew-man)
;;; brew-man.el ends here
