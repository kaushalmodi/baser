;;; baser.el --- Convert integers among base 2, 10, 16           -*- lexical-binding: t -*-

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

;; baser.el provides functions to convert signed and unsigned
;; integers among decimal, binary and hexadecimal formats.

;;; Code:

(require 'subr-x)                       ;For `string-remove-prefix'


;;; Variables

(defcustom baser-default-num-bits 16
  "Number of bits to be assumed if not parsed from the string."
  :group 'baser
  :type 'integer)

(define-error 'baser-number-too-large "Number too large to fit in the number of bits")
(define-error 'baser-unreachable "Reached an unreachable scenario")



;;; Functions

;;;; Decimal <-> Hexadecimal
(defun baser--parse-dec (dec)
  "Parse the input DEC string.

Return a cons (NUM-BITS . DEC-VAL) where NUM-BITS is the number
of bits parsed from DEC, and DEC-VAL is the decimal value.

If the input DEC string has the \"\\='d\"
notation (e.g. \"\\='d1234\"), but without the number of bits, set
NUM-BITS to 32.

If a decimal string cannot be parsed, return nil."
  (let (num-bits dec-val)
    (save-match-data
      (when (string-match "\\`\\(?3:-?\\)\\(?:\\(?:\\(?1:[0-9]*\\)'d\\)\\|0d\\)?\\(?2:[0-9_]+\\)\\'" dec)
        (let ((num-bits-str (match-string-no-properties 1 dec))
              (minus (match-string-no-properties 3 dec)))
          (setq dec-val (string-to-number
                         (concat
                          minus
                          (replace-regexp-in-string
                           "_" ""
                           (match-string-no-properties 2 dec)))))
          ;; (message "dbg num-bits-str : %S" num-bits-str)
          ;; (message "dbg minus : %S" minus)
          (when (stringp num-bits-str)
            (if (string= "" num-bits-str)
                (setq num-bits 32)
              (setq num-bits (string-to-number num-bits-str)))))))
    ;; (message "dbg num-bits : %S" num-bits)
    ;; (message "dbg dec-val : %S" dec-val)
    `(,num-bits . ,dec-val)))

(defun baser--dec-to-hex-core (dec-str)
  "Convert decimal number DEC-STR in string format to hex.

Returns a cons (NUM-BITS . HEX-STR) where NUM-BITS is the number
of bits, and HEX-STR is the converted hexadecimal string."
  (unless (stringp dec-str)
    (error (format "Input %S is not an string" dec-str)))
  (let* ((parsed-dec (baser--parse-dec dec-str))
         (num-bits (car parsed-dec))
         (dec (cdr parsed-dec))
         (bytes (max
                 2 ;minimum number of required bytes
                 (ceiling (/ (/ (log (1+ (abs dec))) (log 2)) 8))))
         (hex-fmt (format "%%0%0dx" (* 2 bytes)))
         hex-str)
    ;; Default value of `num-bits' if not parsed from `dec-str'.
    (setq num-bits (or num-bits baser-default-num-bits))
    (let* ((unsigned-max-pos (1- (expt 2 num-bits)))
           (signed-max-pos (lsh unsigned-max-pos -1))
           (signed-max-neg (- (1+ signed-max-pos)))
           (overflowp (or (and (>= dec 0) (> dec signed-max-pos))
                          (and (< dec 0) (< dec signed-max-neg)))))
      (when overflowp
        (signal 'baser-number-too-large
                (format "%d cannot be represented using %0d bits" dec num-bits))))
    (when (< dec 0)
      (let ((max (expt 2 (* 8 bytes))))
        (setq dec (- max (- dec)))))
    (setq hex-str (format hex-fmt dec))
    `(,num-bits . ,hex-str)))

(defun baser-dec-to-hex (dec &optional beg end)
  "Convert a decimal number DEC to hex.

DEC can a be number or a string representing a decimal number.

If a region is selected, convert all integers in the selected
region in the buffer to hex.  BEG and END are auto-set to the
beginning and end of the selected region.

Else, prompt the user to enter the number in the minibuffer.  The
hex output is printed in the echo area.

When called non-interactively, return the hex string."
  (interactive
   (if (use-region-p)
       (list nil nil (region-beginning) (region-end))
     (list (read-string "Enter a decimal number: "))))
  (let* ((dec-str (if (stringp dec)
                      dec
                    (number-to-string dec)))
         (num-bits-hex (baser--dec-to-hex-core dec-str))
         num-bits hex)
	(setq num-bits (car num-bits-hex))
    (setq hex (cdr num-bits-hex))
    (cond
     ((and (interactive-p) beg end) ;Fn called interactively after selecting a region
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char beg)
          (while (re-search-forward "\\-?\\(\\([0-9]*'d\\)\\|0x\\)*\\([0-9_]+\\)\\b" nil :noerror)
            (let ((hex (cdr (baser--dec-to-hex-core (match-string-no-properties 0)))))
              (replace-match hex))))))
     ((and (interactive-p) dec) ;Fn called interactively without selecting a region
      (message "%s" (format "dec %s -> %s (%d bit hexadecimal)" dec-str hex num-bits)))
     (dec                               ;Fn called non-interactively
      hex)
     (t                          ;not interactive, no region, dec is nil
      (signal 'baser-unreachable "Unsupported scenario")))))

(defun baser--parse-hex (hex)
  "Parse the input HEX string.

Return a cons (NUM-BITS . HEX-STR) where NUM-BITS is the number
of bits parsed from HEX, and HEX-STR is just the hex string
portion without any prefixes or underscores.

If the HEX string has the \"\\='h\" notation (e.g. \"\\='habcd\"),
but without the number of bits, set NUM-BITS to 32.

If a hexadecimal string cannot be parsed, return nil."
  (let (num-bits hex-str)
    (save-match-data
      (when (string-match "\\`\\(?:\\(?:\\(?1:[0-9]*\\)'h\\)\\|0x\\)?\\(?2:[0-9a-fA-F_]+\\)\\'" hex)
        (let ((num-bits-str (match-string-no-properties 1 hex)))
          (setq hex-str (downcase
                         (replace-regexp-in-string
                          "_" ""
                          (match-string-no-properties 2 hex))))
          ;; (message "dbg match 1 : %S" num-bits-str)
          ;; (message "dbg match 2 : %S" hex-str)
          (when (stringp num-bits-str)
            (if (string= "" num-bits-str)
                (setq num-bits 32)
              (setq num-bits (string-to-number num-bits-str)))))))
    ;; (message "dbg num-bits : %S" num-bits)
    ;; (message "dbg hex-str : %S" hex-str)
    `(,num-bits . ,hex-str)))

(defun baser--hex-to-dec-core (inp-hex &optional num-bits)
  "Convert INP-HEX string to a signed decimal number.

Optional argument NUM-BITS is used to determine the sign of the
decimal number and if the hex string can be represented by those
many bits.  If this argument is not specified, it defaults to
`baser-default-num-bits'.

Returns a cons (NUM-BITS . DEC-VALUE) where NUM-BITS is the
number of bits, and DEC-VALUE is the converted decimal number."
  (unless (stringp inp-hex)
    (error (format "Input %S is not an string" inp-hex)))
  (let* ((parsed-hex (baser--parse-hex inp-hex))
         (num-bits (or (car parsed-hex) num-bits))
         (hex (cdr parsed-hex)))
    (when (null hex)
      (error "%s" (format "Input %s is not a valid hex string" inp-hex)))
    ;; Default value of `num-bits' if not set or parsed from `hex'
    ;; string.
    (setq num-bits (or num-bits baser-default-num-bits))
    (let* ((unsigned-max-pos (1- (expt 2 num-bits)))
           (signed-max-pos (lsh unsigned-max-pos -1))
           (unsigned-val (string-to-number hex 16)))
      (when (> unsigned-val unsigned-max-pos)
        (signal 'baser-number-too-large
                (format "%s cannot be represented as a %0d bit hex value" hex num-bits)))
      (let* ((negativep (> unsigned-val signed-max-pos))
             (dec (if negativep
                      (- unsigned-val (1+ unsigned-max-pos))
                    unsigned-val)))
        ;; (message "dbg unsigned-max-pos : %S" unsigned-max-pos)
        ;; (message "dbg signed-max-pos : %S" signed-max-pos)
        ;; (message "dbg unsigned-val : %S" unsigned-val)
        ;; (message "dbg negativep : %S" negativep)
        ;; (message "dbg dec : %S" dec)
        `(,num-bits . ,dec)))))

(defun baser-hex-to-dec (hex &optional num-bits beg end)
  "Convert HEX string to a signed decimal number.

The input is represented by NUM-BITS number of bits.

If a region is selected, convert all hex strings in the selected
region in the buffer to decimal.  BEG and END are auto-set to the
beginning and end of the selected region.

Else, prompt the user to enter a hex number in the
minibuffer.  The decimal output is printed in the echo area.

When called non-interactively, returns the decimal value."
  (interactive
   (if (use-region-p)
       (list nil nil (region-beginning) (region-end))
     (list (read-string "Enter a hex number: "))))
  (cond
   ((and (interactive-p) beg end) ;Fn called interactively after selecting a region
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (while (re-search-forward "\\(\\([0-9]*'h\\)\\|0x\\)\\([0-9a-fA-F_]+\\)" nil :noerror)
          (let ((dec (cdr (baser--hex-to-dec-core (match-string-no-properties 0)))))
            (replace-match (number-to-string dec)))))))
   ((and (interactive-p) hex) ;Fn called interactively without selecting a region
    (let* ((num-bits-dec (baser--hex-to-dec-core hex))
           (num-bits (car num-bits-dec))
           (dec-val (cdr num-bits-dec)))
      (message "%s" (format "hex %s -> %s (%d bit decimal)" hex dec-val num-bits))))
   (hex                                 ;Fn called non-interactively
    (cdr (baser--hex-to-dec-core hex num-bits)))
   (t                        ;not interactive, no region, hex is nil
    (signal 'baser-unreachable "Unsupported scenario"))))

;;;; Hexadecimal <-> Binary
(defun baser--hex-to-bin-core (inp-hex)
  "Convert hex number string INP-HEX to binary.

Returns a cons (NUM-BITS . BIN-STR) where NUM-BITS is the number
of bits, and BIN-STR is the binary representation."
  (let* ((parsed-hex (baser--parse-hex inp-hex))
         (num-bits (car parsed-hex))
         (hex (cdr parsed-hex))
         (bin ""))
    (when (null hex)
      (error "%s" (format "Input %s is not a valid hex string" inp-hex)))
    (dolist (h (split-string hex ""))
      (when (not (string= "" h))
        (setq bin (concat bin
                          (cond
                           ((string= "0" h) "0000")
                           ((string= "1" h) "0001")
                           ((string= "2" h) "0010")
                           ((string= "3" h) "0011")
                           ((string= "4" h) "0100")
                           ((string= "5" h) "0101")
                           ((string= "6" h) "0110")
                           ((string= "7" h) "0111")
                           ((string= "8" h) "1000")
                           ((string= "9" h) "1001")
                           ((string= "a" h) "1010")
                           ((string= "b" h) "1011")
                           ((string= "c" h) "1100")
                           ((string= "d" h) "1101")
                           ((string= "e" h) "1110")
                           ((string= "f" h) "1111")
                           (t
                            (signal 'baser-unreachable "Unsupported scenario")))
                          "_"))))
    (setq bin (string-remove-suffix "_" bin))
    `(,num-bits . ,bin)))

(defun baser-hex-to-bin (hex &optional beg end)
  "Convert HEX string to binary.

If a region is selected, convert all hex strings in the selected
region in the buffer to binary.  BEG and END are auto-set to the
beginning and end of the selected region.

Else, prompt the user to enter a hex number in the
minibuffer.  The binary output is printed in the echo area.

When called non-interactively, return the binary string."
  (interactive
   (if (use-region-p)
       (list nil nil (region-beginning) (region-end))
     (list (read-string "Enter a hex number: "))))
  (cond
   ((and (interactive-p) beg end) ;Fn called interactively after selecting a region
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (while (re-search-forward "\\(\\([0-9]*'h\\)\\|0x\\)\\([0-9a-fA-F_]+\\)" nil :noerror)
          (let ((bin (cdr (baser--hex-to-bin-core (match-string-no-properties 0)))))
            (replace-match bin))))))
   ((and (interactive-p) hex) ;Fn called interactively without selecting a region
    (let* ((num-bits-bin (baser--hex-to-bin-core hex))
           (num-bits (car num-bits-bin))
           (bin-str (cdr num-bits-bin)))
      (if (integerp num-bits)
          (message "%s" (format "hex %s -> %s (%d bit binary)" hex bin-str num-bits))
        (message "%s" (format "hex %s -> %s (binary)" hex bin-str)))))
   (hex                                 ;Fn called non-interactively
    (cdr (baser--hex-to-bin-core hex)))
   (t                        ;not interactive, no region, hex is nil
    (signal 'baser-unreachable "Unsupported scenario"))))


(provide 'baser)

;;; baser.el ends here
