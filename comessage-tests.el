;;; comessage-tests.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Taiki Sugawara

;; Author: Taiki Sugawara <buzz.taiki@gmail.com>

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

(require 'comessage)

(ert-deftest test-comessage--compose-message/when-current-is-empty-or-blank ()
  (should (equal (comessage--compose-message nil "moo") "moo"))
  (should (equal (comessage--compose-message "" "moo") "moo"))
  (should (equal (comessage--compose-message "  " "moo") "moo")))

(ert-deftest test-comessage--compose-message/when-add-same-group ()
  (should (equal (comessage--compose-message "woo" "moo") "moo"))
  (should (equal (comessage--compose-message (comessage--apply-group "woo" 'noise)
                                             (comessage--apply-group "moo" 'noise))
                 "moo")))

(ert-deftest test-comessage--compose-message/when-add-another-group ()
  (should (equal (comessage--compose-message (comessage--apply-group "moo" 'noise)
                                             (comessage--apply-group "cow" 'animal))
                 "moo\ncow")))

(ert-deftest test-comessage--compose-message/complex ()
  (let ((got (seq-reduce #'comessage--compose-message
                         (list (comessage--apply-group "moo" 'noise)
                               (comessage--apply-group "cow" 'animal)
                               "message"
                               (comessage--apply-group "vim" 'editor)
                               (comessage--apply-group "cat" 'animal)
                               (comessage--apply-group "emacs" 'editor)
                               "next message")
                         nil))
        (wont (mapconcat #'identity
                         (list "moo" "cat" "next message" "emacs")
                         "\n")))
    (should (equal got wont))))


(ert-deftest test-comessage-with-group/if-enabled ()
  (message nil)
  (with-temp-buffer
    (comessage-mode 1)
    (comessage-with-group 'noise
      (lambda ()
        (message "moo")
        (should (equal (current-message) "moo"))
        (message "woo")
        (should (equal (current-message) "woo"))))
    (message "cow")
    (should (equal (current-message) "woo\ncow"))))

(ert-deftest test-comessage-with-group/if-disabled ()
  (message nil)
  (with-temp-buffer
    (comessage-mode -1)
    (comessage-with-group 'noise
      (lambda ()
        (message "moo")
        (should (equal (current-message) "moo"))
        (message "woo")
        (should (equal (current-message) "woo"))))
    (message "cow")
    (should (equal (current-message) "cow"))))

(provide 'comessage-tests)
;;; comessage-tests.el ends here
