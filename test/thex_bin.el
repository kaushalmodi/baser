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
  (let ((inp '(   "a"  "0xb"  "'hc" "4'hd" "8'he"  "5'hfa"   "6'hff"))
        (ref '("1010" "1011" "1100" "1101" "1110" "1_1010" "11_1111"))
        out)
    (dolist (hex inp)
      (push (baser-hex-to-bin hex) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-bin-large-number ()
  (let ((inp '(                                                           "68'he_abcd_ef12_3456_7890"))
        (ref '("1110_1010_1011_1100_1101_1110_1111_0001_0010_0011_0100_0101_0110_0111_1000_1001_0000"))
        out)
    (dolist (hex inp)
      (push (baser-hex-to-bin hex) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-bin-invalid-hex-inp ()
  (let ((inp '("32'1234_abcd" "a&b" "'habcdefghi")))
    (dolist (hex inp)
      (should-error (baser-hex-to-bin hex)
                    :type 'user-error))))


;;;; Binary -> Hexadecimal
(ert-deftest test-bin-to-hex ()
  (let ((inp '("1010" "4'b1011" "8'b1100" "12'b11_01" "4'b1111" "0b1010_0101"))
        (ref '("000a"       "b"      "0c"      "00d"        "f"        "00a5"))
        out)
    (dolist (bin inp)
      (push (baser-bin-to-hex bin) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-bin-to-hex-inp-too-large-8-bits ()
  (let ((inp '("1'b10")))
    (dolist (bin inp)
      (should-error (baser-bin-to-hex bin)
                    :type 'baser-number-too-large))))


(provide 'thex_bin)
