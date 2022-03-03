;;; basejump.el --- Convert integers among base 2, 10, 16           -*- lexical-binding: t -*-

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

;; basejump.el provides functions to convert signed and unsigned
;; integers among decimal, binary and hexadecimal formats.

;;; Code:

(require 'subr-x)                       ;For `string-remove-prefix'


;;; Variables



;;; Functions

;;;; Decimal <-> Hexadecimal
(defun basejump-dec-to-hex--core (dec &optional min-bytes)
  "Convert signed decimal number DEC to hex.

DEC is a positive or negative integer.

Optional argument MIN-BYTES is used to set the minimum number of
bytes in the output hex string."
  (unless (integerp dec)
    (error (format "Input %S is not an integer" dec)))
  (let* ((bytes (ceiling (/ (/ (log (1+ (abs dec))) (log 2)) 8))) ;minimum number of required bytes
         (bytes (max (or min-bytes 2) bytes))
         (hex-fmt (format "0x%%0%0dx" (* 2 bytes))))
    (when (< dec 0)
      (let ((max (expt 2 (* 8 bytes))))
        (setq dec (- max (- dec)))))
    (format hex-fmt dec)))

(defun basejump-dec-to-hex (dec &optional min-bytes beg end)
  "Convert signed decimal number DEC to hex.

DEC is a positive or negative integer.

If a region is selected, convert all integers in the selected
region in the buffer to hex.  BEG and END are auto-set to the
beginning and end of the selected region.

Else, prompt the user to enter the integer in the minibuffer. The
hex output is printed in the echo area.

When called non-interactively, return the hex string.

Optional argument MIN-BYTES is used to set the minimum number of
bytes in the output hex string."
  (interactive
   (if (use-region-p)
       (list nil nil (region-beginning) (region-end))
     (list (string-to-number
            (read-string "Enter an integer in decimal: ")))))
  (cond
   ((and (interactive-p) beg end) ;Fn called interactively after selecting a region
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (while (re-search-forward "\\-?[0-9]+\\b" nil :noerror)
          (let ((hex (basejump-dec-to-hex--core
                      (string-to-number (match-string-no-properties 0)))))
            (replace-match hex))))))
   ((and (interactive-p) dec) ;Fn called interactively without selecting a region
    (message "dec %d -> %s" dec (basejump-dec-to-hex--core dec)))
   (dec                                 ;Fn called non-interactively
    (basejump-dec-to-hex--core dec min-bytes))
   (t                          ;not interactive, no region, dec is nil
    (error "Unsupported scenario"))))

(defun basejump-hex-to-dec--core (hex &optional num-bits)
  "Convert HEX string to a signed decimal number.

Optional argument NUM-BITS is used to determine the sign of the
decimal number and if the hex string can be represented by those
many bits.  If this argument is not specified, it defaults to
16."
  (let ((hex hex)
        (num-bits num-bits))
    ;; (message "dbg input hex : %S" hex)
    ;; (message "dbg input num-bits : %S" num-bits)
    (unless (stringp hex)
      (error (format "Input %S is not an string" hex)))
    ;; Override the `hex' and `num-bits' values with the parsed
    ;; versions.
    (save-match-data
      (string-match "\\(?:\\(?:\\(?1:[0-9]*\\)'h\\)\\|0x\\)?\\(?2:[0-9a-fA-F]+\\)" hex)
      ;; (message "dbg match 1 : %S" (match-string-no-properties 1 hex))
      ;; (message "dbg match 2 : %S" (match-string-no-properties 2 hex))
      (when (match-string-no-properties 1 hex)
        (setq num-bits (string-to-number (match-string-no-properties 1 hex))))
      (setq hex (match-string-no-properties 2 hex)))
    (setq num-bits (or num-bits 16)) ;Default value of `num-bits' if not set or parsed from `hex' string
    (let* ((unsigned-max-pos (1- (expt 2 num-bits)))
           (signed-max-pos (lsh unsigned-max-pos -1))
           (unsigned-val (string-to-number hex 16)))
      (when (> unsigned-val unsigned-max-pos)
        (error (format "%s cannot be represented as a %0d bit hex value" hex num-bits)))
      (let* ((negativep (> unsigned-val signed-max-pos))
             (dec (if negativep
                      (- unsigned-val (1+ unsigned-max-pos))
                    unsigned-val)))
        ;; (message "dbg unsigned-max-pos : %S" unsigned-max-pos)
        ;; (message "dbg signed-max-pos : %S" signed-max-pos)
        ;; (message "dbg unsigned-val : %S" unsigned-val)
        ;; (message "dbg negativep : %S" negativep)
        ;; (message "dbg dec : %S" dec)
        dec))))

(defun basejump-hex-to-dec (hex &optional num-bits beg end)
  "Convert HEX string to a signed decimal number.

The input is represented by NUM-BITS number of bits.

If a region is selected, convert all hex strings in the selected
region in the buffer to decimal.  BEG and END are auto-set to the
beginning and end of the selected region.

Else, prompt the user to enter a hex number in the
minibuffer. The decimal output is printed in the echo area.

When called non-interactively, return the hex string."
  (interactive
   (if (use-region-p)
       (list nil nil (region-beginning) (region-end))
     (list (read-string "Enter a hex number: "))))
  (when (null num-bits)
    (setq num-bits 16))
  (cond
   ((and (interactive-p) beg end) ;Fn called interactively after selecting a region
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (while (re-search-forward "\\(\\([0-9]*'h\\)\\|0x\\)\\([0-9a-fA-F]+\\)" nil :noerror)
          (let ((dec (basejump-hex-to-dec--core (match-string-no-properties 0))))
            (replace-match (number-to-string dec)))))))
   ((and (interactive-p) hex) ;Fn called interactively without selecting a region
    (message "%s"
             (format "hex %d'h%s -> %s"
                     num-bits hex (basejump-hex-to-dec--core hex num-bits))))
   (hex                                 ;Fn called non-interactively
    (basejump-hex-to-dec--core hex num-bits))
   (t                        ;not interactive, no region, hex is nil
    (error "Unsupported scenario"))))


(provide 'basejump)

;;; basejump.el ends here
