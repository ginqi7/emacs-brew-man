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

(require 'websocket-bridge)
(require 'tabulated-list)

(defcustom brew-man-ruby-command (executable-find "ruby")
  "The ruby command path.")

(defvar brew-man--ruby-file
  (file-name-concat (file-name-directory (or load-file-name (buffer-file-name)))
                    "brew_man.rb"))

(defvar brew-man--list-buffer-name "*brew-man-list*")

(defvar brew-man--tap-list-buffer-name "*brew-man-tap-list*")


(defun brew-man-start ()
  "Start Brew manager."
  (interactive)
  (websocket-bridge-app-start "brew-man" "ruby" brew-man--ruby-file)
  (sit-for 0.2)
  (brew-man-refresh))


(defun brew-man-restart ()
  "Restart Brew manager."
  (interactive)
  (websocket-bridge-app-exit "brew-man")
  (brew-man-start)
  (websocket-bridge-app-open-buffer "brew-man"))

(defun brew-man-cask-list ()
  "List casks."
  (interactive)
  (websocket-bridge-call "brew-man" "cask-list" #'brew-man-show-cask-list))

(defun brew-man-refresh ()
  "Refresh data."
  (interactive)
  (websocket-bridge-call "brew-man" "refresh"))


(defun brew-man-tap-list ()
  "List taps."
  (interactive)
  (websocket-bridge-call "brew-man" "tap-list" #'brew-man-show-tap-list))

(defun brew-man-list ()
  "List taps."
  (interactive)
  (websocket-bridge-call "brew-man" "list" #'brew-man-show-list))

(defun brew-man-show-list (data)
  (brew-man--tabulated-list-mode
   brew-man--list-buffer-name
   [("Name" 20 t)
    ("Type" 10 t)
    ("Tap" 20 t)
    ("Homepage" 30 t)
    ("Version" 10 t)
    ("InstalledTime" 20 t)
    ("Desc" 30 t)
    ("" 8 t)]
   :name
   data))

(defun brew-man-show-tap-list (data)
  (brew-man--tabulated-list-mode
   brew-man--tap-list-buffer-name
   [("Name" 30 t)
    ("Formulae" 15 t)
    ("Casks" 15 t)
    ("Update" 20 t)
    ("" 8 t)]
   :name
   data))


(defun brew-man-tap-info (tap-name)
  (websocket-bridge-call "brew-man" "tap-info" tap-name #'brew-man-update-tap-entry))

(defun brew-man-cask-info (tap-name)
  (websocket-bridge-call "brew-man" "cask-info" tap-name #'brew-man-update-cask-entry))

(defun brew-man-formula-info (formula-name)
  (websocket-bridge-call "brew-man" "formula-info" formula-name #'brew-man-update-formula-entry))


(defun brew-man--update-tabulated-list-entry (new-entry)
  "Update the entry with NAME to have NEW-VALUE."
  (cl-loop for item in (append (nth 1 new-entry) nil)
           for index from 0
           do
           (tabulated-list-set-col index item t)
           (tabulated-list-revert)))

(defun brew-man--locate-tabulated-list-entry (entry)
  (let* ((key (car entry))
         (index
          (cl-position-if
           (lambda (item) (equal (car item) key))
           tabulated-list-entries)))
    (goto-line (1+ index))))

(defun brew-man-update-entry (buffer-name info)
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((entry (brew-man--list-to-entry tabulated-list-format :name info)))
      (save-excursion
        (brew-man--locate-tabulated-list-entry entry)
        (brew-man--update-tabulated-list-entry entry)))))

(defun brew-man-update-formula-entry (info)
  (brew-man-update-entry "*brew-man-formula-list*" info))

(defun brew-man-update-cask-entry (info)
  (brew-man-update-entry "*brew-man-cask-list*" info))

(defun brew-man-update-tap-entry (info)
  (brew-man-update-entry brew-man--tap-list-buffer-name info))

(defun brew-man--plist-p (lst)
  "Return t if LIST is a property list, nil otherwise."
  (and (listp lst)                  ; Check if it is a list
       (evenp (length lst))        ; Check if its length is even
       (cl-loop for i from 0 below (/ (length lst) 2)
                always (symbolp (nth (* 2 i) lst))))) ; Check keys are symbols)

(defun brew-man--list-to-entry (header key lst)
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
               (brew-man--list-to-entry header key row))
             data))
      (brew-man-mode)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (pop-to-buffer (current-buffer)))))

(defun brew-man-click()
  (interactive)
  (when (string= (buffer-name) brew-man--list-buffer-name)
    (brew-man-tap-list-keys))
  (when (string= (buffer-name) brew-man--tap-list-buffer-name)
    (brew-man-tap-list-keys)))

(transient-define-prefix brew-man-tap-list-keys ()
  ["Brew Man Tap List Keys"
   ("a" "Add" brew-man-tap-add)
   ("d" "Delete" brew-man-tap-delete)])

(defun brew-man-tap-add ()
  (interactive)
  (when-let ((tap-name (read-string "Input tap Name: ")))
    (brew-man-send-command (format "brew tap %s" tap-name) #'message)))

(defun brew-man-tap-delete ()
  (interactive)
  (brew-man-send-command (format "brew untap %s" (tabulated-list-get-id)) #'message))

(transient-define-prefix brew-man-list-keys ()
  ["Brew Man List Keys"])

(defun brew-man-send-command (cmd &optional callback)
  (websocket-bridge-call "brew-man" "run-command" cmd callback))

(define-derived-mode brew-man-mode tabulated-list-mode "brew-man"
  "Major mode for Brew Mananger."
  (keymap-set brew-man-mode-map "RET" 'brew-man-click)
  (keymap-set brew-man-mode-map "j" 'next-line)
  (keymap-set brew-man-mode-map "k" 'previous-line)
  (keymap-set brew-man-mode-map "l" 'tabulated-list-next-column)
  (keymap-set brew-man-mode-map "h" 'tabulated-list-previous-column))

(provide 'brew-man)
;;; brew-man.el ends here
