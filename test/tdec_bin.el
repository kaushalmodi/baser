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

;; Test conversions between decimal and binary.

;;; Code:

;;;; Decimal -> Binary
(ert-deftest test-pos-to-bin ()
  (let ((inp '(                   0                 12345 "4'd7" "3'd3"    "8'd100"))
        (ref '("0000_0000_0000_0000" "0011_0000_0011_1001" "0111" "0011" "0110_0100"))
        out)
    (dolist (dec inp)
      (push (baser-dec-to-bin dec) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-neg-to-bin ()
  (let ((inp '(                  -1  "-4'd2"     "-8'd3"))
        (ref '("1111_1111_1111_1111"  "1110" "1111_1101"))
        out)
    (dolist (dec inp)
      (push (baser-dec-to-bin dec) out))
    (should (equal ref (nreverse out)))))


;;;; Binary -> Decimal
(ert-deftest test-bin-to-dec ()
  (let ((inp '("1111" "4'b1111" "'b101" "0b1110" "2'b11" "3'b11"))
        (ref '(   15        -1       5       14      -1       3))
        out)
    (dolist (bin inp)
      (push (baser-bin-to-dec bin) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-bin-to-dec-inp-too-large-8-bits ()
  (let ((inp '("2'b111")))
    (dolist (bin inp)
      (should-error (baser-bin-to-dec bin)
                    :type 'baser-number-too-large))))


(provide 'tdec_bin)
