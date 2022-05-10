;;; comessage.el --- Providing message coexistence   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Taiki Sugawara

;; Author: Taiki Sugawara <buzz.taiki@gmail.com>
;; Keywords: convenience

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

;; `comessage-mode' is a global minor mode that provides message coexistence.
;; It will resolve multiple packages competing for message output (e.g. flymake and eldoc).

;; Example:
;; To separate flymake and eldoc messages, you can do as the following configuration:
;;
;; 	(comessage-define-advice my/comessage-group-flymake 'flymake)
;; 	(comessage-define-advice my/comessage-group-eldoc 'eldoc)
;; 	
;; 	(advice-add 'flymake-goto-next-error :around #'my/comessage-group-flymake)
;; 	(advice-add 'eldoc-message :around #'my/comessage-group-eldoc)
;; 	(advice-add 'eldoc-minibuffer-message :around #'my/comessage-group-eldoc)


;;; Code:

(defgroup comessage nil
  "Providing message coexistence."
  :group 'convenience
  :group 'minibuffer)

(defun comessage--message-advice (message-fn format-string &rest args)
  "An around advice of `message'.

MESSAGE-FN would be `message' recieving FORMAT-STRING and ARGS."
  (if (or (null format-string) (string-empty-p format-string))
      (funcall message-fn nil)
    (let ((incoming (apply #'format-message format-string args))
          (current (current-message)))
      (let ((inhibit-message t))
        (funcall message-fn "%s" incoming))
      (let ((message-log-max nil))
        (funcall message-fn "%s" (comessage--compose-message current incoming))))))

(defun comessage--compose-message (current incoming)
  "Compose message by CURRENT and INCOMING."
  (setq incoming (comessage--apply-group incoming (or (comessage--group incoming) 'comessage--default-group)))
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (insert (string-trim (or current "")))
      (unless (get-text-property (point-min) 'comessage-group)
        (put-text-property (point-min) (point-max) 'comessage-group 'comessage--default-group))
      (goto-char (point-min))

      (while (not (or (eobp) (eq (get-text-property (point) 'comessage-group)
                                 (comessage--group incoming))))
        (goto-char (or (next-single-property-change (point) 'comessage-group)
                       (point-max))))
      (when (and (eobp) (> (buffer-size) 0))
        (insert "\n"))
      (delete-region (point) (or (next-single-property-change (point) 'comessage-group) (point-max)))
      (insert incoming)
      (buffer-string))))

(defun comessage--apply-group (str group)
  "Apply GROUP to string STR as a message group."
  (and str (propertize str 'comessage-group group)))

(defun comessage--group (str)
  "Return the message group of STR."
  (and str (get-text-property 0 'comessage-group str)))

(define-minor-mode comessage-mode
  "A global minor mode that provides message coexistence."
  :global t
  (if comessage-mode
      (advice-add 'message :around #'comessage--message-advice)
    (advice-remove 'message #'comessage--message-advice)))

(defun comessage (group format-string &rest args)
  "As same as (message FORMAT-STRING &rest ARGS).
Additionaly GROUP as a group of this message."
  (apply #'message (comessage--apply-group format-string group) args))

(defun comessage-with-group (group fn)
  "Call FN with GROUP as a message group."
  (let ((wrapper (lambda (fn format-string &rest args)
                   (apply fn (comessage--apply-group format-string group) args))))
    (unwind-protect
        (progn
          (add-function :around (symbol-function #'message) wrapper)
          (funcall fn))
      (remove-function (symbol-function #'message) wrapper))))

(defmacro comessage-define-advice (name group)
  "Define an around advice NAME with GROUP as a message group."
  `(defun ,name (fn &rest args)
     (comessage-with-group ,group (lambda () (apply fn args)))))

(provide 'comessage)
;;; comessage.el ends here
