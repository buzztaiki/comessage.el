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

;;; TODO:
;; - Categorize message texts by text-property `comessage-category'.
;; - Overwrite message without accumulating them if it is in the same category.
;; - Do the same thing for uncategorized messages.
;; - Add helper function or macro for adding category to message text to customize existing functions.

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
  (setq incoming (propertize incoming 'comessage-group (or (get-text-property 0 'comessage-group incoming)
                                                           'comessage--default-group)))
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (insert (string-trim (or current "")))
      (unless (get-text-property (point-min) 'comessage-group)
        (put-text-property (point-min) (point-max) 'comessage-group 'comessage--default-group))
      (goto-char (point-min))

      (while (not (or (eobp) (eq (get-text-property (point) 'comessage-group)
                                 (get-text-property 0 'comessage-group incoming))))
        (goto-char (or (next-single-property-change (point) 'comessage-group)
                       (point-max))))
      (when (and (eobp) (> (buffer-size) 0))
        (insert "\n"))
      (delete-region (point) (or (next-single-property-change (point) 'comessage-group) (point-max)))
      (insert incoming)
      (buffer-string))))

(define-minor-mode comessage-mode
  "A global minor mode that provides message coexistence."
  :global t
  (if comessage-mode
      (advice-add 'message :around #'comessage--message-advice)
    (advice-remove 'message #'comessage--message-advice)))


(provide 'comessage)
;;; comessage.el ends here
