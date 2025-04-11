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
(require 'transient)

(defcustom brew-man-ruby-command (executable-find "ruby")
  "The ruby command path.")

(defvar brew-man--ruby-file
  (file-name-concat (file-name-directory (or load-file-name (buffer-file-name)))
                    "brew_man.rb"))

(defvar brew-man--list-buffer-name "*brew-man-list*")

(defvar brew-man--list-last-line 0)

(defvar brew-man--tap-list-buffer-name "*brew-man-tap-list*")

(defvar brew-man--tap-list-last-line 0)

(defvar brew-man--info-buffer-name "*brew-man-info*")



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

(defun brew-man-refresh ()
  "Refresh data."
  (interactive)
  (websocket-bridge-call "brew-man" "refresh"))

(defun brew-man-tap-list (&optional refresh-p)
  "List taps."
  (interactive)
  (websocket-bridge-call "brew-man" "tap-list" #'brew-man-show-tap-list refresh-p))

(defun brew-man-list (&optional refresh-p)
  "List taps."
  (interactive)
  (websocket-bridge-call "brew-man" "list" #'brew-man-show-list refresh-p))

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
   data)
  (goto-line brew-man--list-last-line))

(defun brew-man-show-tap-list (data)
  (brew-man--tabulated-list-mode
   brew-man--tap-list-buffer-name
   [("Name" 30 t)
    ("Formulae" 15 t)
    ("Casks" 15 t)
    ("Update" 20 t)
    ("" 8 t)]
   :name
   data)
  (goto-line brew-man--tap-list-last-line))



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
    (brew-man-list-keys))
  (when (string= (buffer-name) brew-man--tap-list-buffer-name)
    (brew-man-tap-list-keys)))

(transient-define-prefix brew-man-tap-list-keys ()
  ["Brew Man Tap List Keys"
   ("a" "Add" brew-man-tap-add)
   ("d" "Delete" brew-man-tap-delete)
   ("i" "Install in tap" brew-man-install-in-tap)
   ("r" "Refresh" (lambda () (interactive) (brew-man-tap-list t)))])

(defun brew-man-install-in-tap ()
  (interactive)
  (let ((tap (tabulated-list-get-id)))
    (websocket-bridge-call "brew-man" "select-in-tap" tap #'brew-man-install)))

(defun brew-man-install (elements)
  (let* ((selected-split (split-string (completing-read "Select element: " elements) " "))
         (name (car selected-split))
         (type (cadr selected-split))
         (cmd (format "brew install --%s %s" type name)))
    (message (format "Command [%s] Running." cmd))
    (brew-man-send-command cmd #'message)))


(defun brew-man-tap-add ()
  (interactive)
  (when-let* ((tap-name (read-string "Input tap Name: "))
              (cmd (format "brew tap %s" tap-name)))
    (message (format "Command [%s] Running." cmd))
    (setq brew-man--tap-list-last-line (array-current-line))
    (brew-man-send-command cmd #'brew-man-tap-list)))

(defun brew-man-tap-delete ()
  (interactive)
  (let ((cmd (format "brew untap %s" (tabulated-list-get-id))))
    (message (format "Command [%s] Running." cmd))
    (setq brew-man--tap-list-last-line (array-current-line))
    (brew-man-send-command cmd #'brew-man-tap-list)))


(defun brew-man-add ()
  (interactive)
  (when-let* ((type (downcase (completing-read "Select a type: " '("Cask" "Formula"))))
              (name (read-string "Input tap Name: "))
              (cmd (format "brew install --%s %s" type name)))
    (message (format "Command [%s] Running." cmd))
    (setq brew-man--list-last-line (array-current-line))
    (brew-man-send-command cmd #'brew-man-list)))

(defun brew-man-delete ()
  (interactive)
  (let ((cmd (format "brew uninstall %s" (tabulated-list-get-id))))
    (message (format "Command [%s] Running." cmd))
    (setq brew-man--list-last-line (array-current-line))
    (brew-man-send-command cmd #'brew-man-list)))

(transient-define-prefix brew-man-list-keys ()
  ["Brew Man List Keys"
   ("a" "Add" brew-man-add)
   ("d" "Delete" brew-man-delete)
   ("r" "Refresh" (lambda () (interactive) (brew-man-list t)))])

(transient-define-prefix brew-man-keys ()
  ["Brew Man Keys"
   ("t" "Tap" brew-man-tap-list)
   ("l" "List" brew-man-list)
   ("s" "Select" brew-man-select)
   ("q" "Query" brew-man-query)])

(defun brew-man-select ()
  (interactive)
  (websocket-bridge-call "brew-man" "select-in-tap" nil #'brew-man-select-info))

(defun brew-man-show-info (info)
  (with-current-buffer (get-buffer-create brew-man--info-buffer-name)
    (erase-buffer)
    (insert info)
    (pop-to-buffer (current-buffer))))

(defun brew-man-select-info (elements)
  (let* ((selected-split (split-string (completing-read "Select element: " elements) " "))
         (name (car selected-split))
         (type (cadr selected-split))
         (cmd (format "brew info --%s %s" type name)))
    (message (format "Command [%s] Running." cmd))
    (brew-man-send-command cmd #'brew-man-show-info)))

(defun brew-man-query (&optional keyword)
  (interactive)
  (unless keyword
    (setq keyword (read-string "Input a search keyword: ")))
  (let ((cmd (format "brew search %s --desc --eval-all" keyword)))
    (brew-man-send-command cmd #'brew-man-show-info)))


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
