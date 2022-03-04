;; -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Test conversions between hexadecimal and binary.

;;; Code:

;;;; Hexadecimal -> Binary
(ert-deftest test-hex-to-bin ()
  (let ((inp '(   "a"  "0xb"  "'hc" "4'hd" "8'he"))
        (ref '("1010" "1011" "1100" "1101" "1110"))
        out)
    (dolist (hex inp)
      (push (baser-hex-to-bin hex) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-bin-invalid-hex-inp ()
  (let ((inp '("32'1234_abcd" "a&b" "'habcdefghi")))
    (dolist (hex inp)
      (should-error (baser-hex-to-bin hex)))))


(provide 'thex_bin)
