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
(ert-deftest test-hex-to-pos-8-bits ()
  (let ((num-bits 8)
        (inp '("0x0f" "f" "0f" "0x00" "0x01"))
        (ref '(   15  15   15      0      1))
        out)
    (dolist (hex inp)
      (push (basejump-hex-to-dec hex num-bits) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-pos-16-bits ()
  (let ((num-bits 16)
        (inp '("0x0f" "0xff" "f" "ff" "0x000F" "0x00" "0x01" "0x0FFF" "0x7fFf"))
        (ref '(   15    255  15  255       15      0      1     4095    32767))
        out)
    (dolist (hex inp)
      (push (basejump-hex-to-dec hex num-bits) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-neg-8-bits ()
  (let ((num-bits 8)
        (inp '("0xff" "ff" "0xfe" "FD" "8'hfc" "'hfb"))
        (ref '(   -1   -1     -2   -3      -4    251))
        out)
    (dolist (hex inp)
      (push (basejump-hex-to-dec hex num-bits) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-neg-16-bits ()
  (let ((num-bits 16)
        (inp '("0xffff" "0xfffe" "0x8000" "0x8001"))
        (ref '(     -1       -2   -32768   -32767))
        out)
    (dolist (hex inp)
      (push (basejump-hex-to-dec hex num-bits) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-dec-auto-num-bits ()
  (let ((inp '("8'hff" "16'hff" "'h7fff" "'hffff" "'hffffffff"))
        (ref '(    -1      255    32767    65535           -1))
        out)
    (dolist (hex inp)
      (push (basejump-hex-to-dec hex) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-dec-3-bits ()
  (let ((inp '("3'h2" "3'h1" "3'h0" "3'h7"))
        (ref '(    2      1      0     -1))
        out)
    (dolist (hex inp)
      (push (basejump-hex-to-dec hex) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-dec-7-bits ()
  (let ((num-bits 7)
        (inp '("1f" "3f" "7f"))
        (ref '( 31   63   -1))
        out)
    (dolist (hex inp)
      (push (basejump-hex-to-dec hex num-bits) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-dec-underscores ()
  (let ((inp '("16'hab_cd" "'hffff_fffe" "24'h21_12_83"))
        (ref '(    -21555            -2        2167427))
        out)
    (dolist (hex inp)
      (push (basejump-hex-to-dec hex) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-dec-inp-not-string ()
  (let ((inp '(1 1.1 'x)))
    (dolist (i inp)
      (should-error (basejump-hex-to-dec i nil)))))

(ert-deftest test-hex-to-dec-inp-too-large-3-bits ()
  (let ((num-bits 3)
        (inp '("0x8")))
    (dolist (hex inp)
      (should-error (basejump-hex-to-dec hex num-bits)))))

(ert-deftest test-hex-to-dec-inp-too-large-8-bits ()
  (let ((num-bits 8)
        (inp '("0x100")))
    (dolist (hex inp)
      (should-error (basejump-hex-to-dec hex num-bits)))))

(ert-deftest test-hex-to-dec-invalid-hex-inp ()
  (let ((inp '("32'1234_abcd" "a&b" "'habcdefghi")))
    (dolist (hex inp)
      (should-error (basejump-hex-to-dec hex)))))


(provide 'tdec_hex)
