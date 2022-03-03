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

;; Test conversions between decimal and hexadecimal.

;;; Code:

;;;; Decimal -> Hexadecimal
(ert-deftest test-pos-to-hex-default-min-bytes ()
  (let ((inp '(      0       10      100     1023     1024     4095     4096    65535      65536))
        (ref '("0x0000" "0x000a" "0x0064" "0x03ff" "0x0400" "0x0fff" "0x1000" "0xffff" "0x010000"))
        out)
    (dolist (dec inp)
      (push (basejump-dec-to-hex dec) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-pos-to-hex-min-bytes-1 ()
  (let ((min-bytes 1)
        (inp '(    0     10    100    127    255     1023     1024     4095     4096))
        (ref '("0x00" "0x0a" "0x64" "0x7f" "0xff" "0x03ff" "0x0400" "0x0fff" "0x1000"))
        out)
    (dolist (dec inp)
      (push (basejump-dec-to-hex dec min-bytes) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-neg-to-hex-default-min-bytes ()
  (let ((inp '(     -1       -2    -1023    -1024))
        (ref '("0xffff" "0xfffe" "0xfc01" "0xfc00"))
        out)
    (dolist (dec inp)
      (push (basejump-dec-to-hex dec) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-neg-to-hex-min-bytes-1 ()
  (let ((min-bytes 1)
        (inp '(   -1     -2    -1023    -1024))
        (ref '("0xff" "0xfe" "0xfc01" "0xfc00"))
        out)
    (dolist (dec inp)
      (push (basejump-dec-to-hex dec min-bytes) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-pos-to-hex-inp-not-int ()
  (let ((inp '("a" 1.1 'x)))
    (dolist (i inp)
      (should-error (basejump-dec-to-hex i)))))

;;;; Hexadecimal -> Decimal
(ert-deftest test-hex-to-pos-1-byte ()
  (let ((num-bytes 1)
        (inp '("0x0f" "f" "0f" "0x00" "0x01"))
        (ref '(   15  15   15      0      1))
        out)
    (dolist (hex inp)
      (push (basejump-hex-to-dec hex num-bytes) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-pos-2-bytes ()
  (let ((num-bytes 2)
        (inp '("0x0f" "0xff" "f" "ff" "0x000F" "0x00" "0x01" "0x0FFF" "0x7fFf"))
        (ref '(   15    255  15  255       15      0      1     4095    32767))
        out)
    (dolist (hex inp)
      (push (basejump-hex-to-dec hex num-bytes) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-neg-1-byte ()
  (let ((num-bytes 1)
        (inp '("0xff" "0xfe"))
        (ref '(   -1     -2))
        out)
    (dolist (hex inp)
      (push (basejump-hex-to-dec hex num-bytes) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-neg-2-bytes ()
  (let ((num-bytes 2)
        (inp '("0xffff" "0xfffe" "0x8000" "0x8001"))
        (ref '(     -1       -2   -32768   -32767))
        out)
    (dolist (hex inp)
      (push (basejump-hex-to-dec hex num-bytes) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-dec-inp-not-string ()
  (let ((inp '(1 1.1 'x)))
    (dolist (i inp)
      (should-error (basejump-hex-to-dec i nil)))))

(ert-deftest test-hex-to-dec-inp-too-large ()
  (let ((num-bytes 1)
        (inp '(128 1000)))
    (dolist (i inp)
      (should-error (basejump-hex-to-dec i num-bytes)))))


(provide 'tdec_hex)
