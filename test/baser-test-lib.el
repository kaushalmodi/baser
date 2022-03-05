;;; baser-test-lib.el --- Test library for baser.el                   -*- lexical-binding: t; -*-

;; Authors: Kaushal Modi <kaushal.modi@gmail.com>

;; This file is not part of GNU Emacs.

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

;;; Code:

(defun baser-test-conversion-in-buffer (fn content ref)
  "Test if FN function converts buffer content CONTENT to REF."
  (with-temp-buffer
    (insert content)
    (push-mark (point-min) :nomsg)
    (goto-char (point-max))
    (setq-local transient-mark-mode t)
    (setq-local mark-active t)
    (let ((inhibit-message t)) ;Silence messages like "Made 3 hex -> dec conversions"
      (call-interactively fn))
    (should (string= ref
                     (buffer-substring-no-properties (point-min) (point-max))))))


(provide 'baser-test-lib)
