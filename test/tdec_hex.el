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

(require 'baser-test-lib)


;;;; Decimal -> Hexadecimal
(ert-deftest test-pos-dec-to-hex ()
  (let ((inp '(    0     10    100   1023   1024   4095   4096  32767))
        (ref '("0000" "000a" "0064" "03ff" "0400" "0fff" "1000" "7fff"))
        out)
    (dolist (dec inp)
      (push (baser-dec-to-hex dec) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-neg-dec-to-hex ()
  (let ((inp '(   -1     -2  -1023  -1024))
        (ref '("ffff" "fffe" "fc01" "fc00"))
        out)
    (dolist (dec inp)
      (push (baser-dec-to-hex dec) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-dec-str-to-hex ()
  (let ((inp '("32'd65535" "-8'd1"))
        (ref '( "0000ffff"    "ff"))
        out)
    (dolist (dec inp)
      (push (baser-dec-to-hex dec) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-dec-to-hex-region-conversion ()
  (let ((content "123 -123 -4'd1 16'd1000 -8'd1 32'd65535 -1 -2 -1023 -1024")
        (ref "007b ff85 f 03e8 ff 0000ffff ffff fffe fc01 fc00"))
    (baser-test-conversion-in-buffer #'baser-dec-to-hex content ref)))

(ert-deftest test-pos-dec-to-hex-inp-not-int ()
  (let ((inp '("a" 1.1 'x)))
    (dolist (i inp)
      (should-error (baser-dec-to-hex i)))))

(ert-deftest test-dec-to-hex-inp-too-large ()
  (let ((inp '(65535 "3'd4" "-2'd3")))
    (dolist (dec inp)
      (should-error (baser-dec-to-hex dec)
                    :type 'baser-number-too-large))))


;;;; Hexadecimal -> Decimal
(ert-deftest test-hex-to-pos-8-bits ()
  (let ((num-bits 8)
        (inp '("0x0f" "f" "0f" "0x00" "0x01"))
        (ref '(   15  15   15      0      1))
        out)
    (dolist (hex inp)
      (push (baser-hex-to-dec hex num-bits) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-pos-16-bits ()
  (let ((num-bits 16)
        (inp '("0x0f" "0xff" "f" "ff" "0x000F" "0x00" "0x01" "0x0FFF" "0x7fFf"))
        (ref '(   15    255  15  255       15      0      1     4095    32767))
        out)
    (dolist (hex inp)
      (push (baser-hex-to-dec hex num-bits) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-neg-8-bits ()
  (let ((num-bits 8)
        (inp '("0xff" "ff" "0xfe" "FD" "8'hfc" "'hfb"))
        (ref '(   -1   -1     -2   -3      -4    251))
        out)
    (dolist (hex inp)
      (push (baser-hex-to-dec hex num-bits) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-neg-16-bits ()
  (let ((num-bits 16)
        (inp '("0xffff" "0xfffe" "0x8000" "0x8001"))
        (ref '(     -1       -2   -32768   -32767))
        out)
    (dolist (hex inp)
      (push (baser-hex-to-dec hex num-bits) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-dec-auto-num-bits ()
  (let ((inp '("8'hff" "16'hff" "'h7fff" "'hffff" "'hffffffff"))
        (ref '(    -1      255    32767    65535           -1))
        out)
    (dolist (hex inp)
      (push (baser-hex-to-dec hex) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-dec-3-bits ()
  (let ((inp '("3'h2" "3'h1" "3'h0" "3'h7"))
        (ref '(    2      1      0     -1))
        out)
    (dolist (hex inp)
      (push (baser-hex-to-dec hex) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-dec-7-bits ()
  (let ((num-bits 7)
        (inp '("1f" "3f" "7f"))
        (ref '( 31   63   -1))
        out)
    (dolist (hex inp)
      (push (baser-hex-to-dec hex num-bits) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-dec-underscores ()
  (let ((inp '("16'hab_cd" "'hffff_fffe" "24'h21_12_83"))
        (ref '(    -21555            -2        2167427))
        out)
    (dolist (hex inp)
      (push (baser-hex-to-dec hex) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-hex-to-dec-region-conversion ()
  (let ((content "16'hab_cd 'hffff_fffe 24'h21_12_83")
        (ref "-21555 -2 2167427"))
    (baser-test-conversion-in-buffer #'baser-hex-to-dec content ref)))

(ert-deftest test-hex-to-dec-inp-not-string ()
  (let ((inp '(1 1.1 'x)))
    (dolist (i inp)
      (should-error (baser-hex-to-dec i nil)))))

(ert-deftest test-hex-to-dec-inp-too-large-3-bits ()
  (let ((num-bits 3)
        (inp '("0x8")))
    (dolist (hex inp)
      (should-error (baser-hex-to-dec hex num-bits)
                    :type 'baser-number-too-large))))

(ert-deftest test-hex-to-dec-inp-too-large-8-bits ()
  (let ((num-bits 8)
        (inp '("0x100")))
    (dolist (hex inp)
      (should-error (baser-hex-to-dec hex num-bits)
                    :type 'baser-number-too-large))))

(ert-deftest test-hex-to-dec-invalid-hex-inp ()
  (let ((inp '("32'1234_abcd" "a&b" "'habcdefghi")))
    (dolist (hex inp)
      (should-error (baser-hex-to-dec hex)
                    :type 'user-error))))


(provide 'tdec_hex)
