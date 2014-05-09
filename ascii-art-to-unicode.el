;;; ascii-art-to-unicode.el --- a small artist adjunct -*- lexical-binding: t -*-

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Thien-Thi Nguyen <ttn@gnu.org>
;; Version: 1.6

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The command `aa2u' converts simple ASCII art line drawings in
;; the {active,accessible} region of the current buffer to Unicode.
;;
;; Example use case:
;; - M-x artist-mode RET
;; - C-c C-a r               ; artist-select-op-rectangle
;; - (draw two rectangles)
;;
;;   +---------------+
;;   |               |
;;   |       +-------+--+
;;   |       |       |  |
;;   |       |       |  |
;;   |       |       |  |
;;   +-------+-------+  |
;;           |          |
;;           |          |
;;           |          |
;;           +----------+
;;
;; - C-c C-c                 ; artist-mode-off (optional)
;; - C-x n n                 ; narrow-to-region
;; - M-x aa2u RET
;;
;;   ┌───────────────┐
;;   │               │
;;   │       ┌───────┼──┐
;;   │       │       │  │
;;   │       │       │  │
;;   │       │       │  │
;;   └───────┼───────┘  │
;;           │          │
;;           │          │
;;           │          │
;;           └──────────┘
;;
;; Much easier on the eyes now!
;;
;;
;; See Also
;; - HACKING: <http://git.sv.gnu.org/cgit/emacs/elpa.git/tree/packages/ascii-art-to-unicode/HACKING>

;;; Code:

(require 'cl-lib)
(require 'pcase)

;;;---------------------------------------------------------------------------
;;; support

(defun aa2u-ucs-bd-uniform-name (weight &rest components)
  "Return a string naming UCS char w/ WEIGHT and COMPONENTS.
The string begins with \"BOX DRAWINGS\"; followed by WEIGHT,
a symbol from the set:

  HEAVY
  LIGHT

followed by COMPONENTS, a list of one or two symbols from the set:

  VERTICAL
  HORIZONTAL
  DOWN
  UP
  RIGHT
  LEFT

If of length two, the first element in COMPONENTS should be
the \"Y-axis\" (VERTICAL, DOWN, UP).  In that case, the returned
string includes \"AND\" between the elements of COMPONENTS.

Lastly, all words are separated by space (U+20)."
  (format "BOX DRAWINGS %s %s"
          weight
          (mapconcat 'symbol-name components
                     " AND ")))

(defun aa2u-1c (stringifier &rest components)
  "Apply STRINGIFIER to COMPONENTS; return the UCS char w/ this name.
The char is a string (of length one), with two properties:

  aa2u-stringifier
  aa2u-components

Their values are STRINGIFIER and COMPONENTS, respectively."
  (let ((s (string (cdr (assoc-string (apply stringifier components)
                                      (ucs-names))))))
    (propertize s
                'aa2u-stringifier stringifier
                'aa2u-components components)))

(defun aa2u-phase-1 ()
  (goto-char (point-min))
  (let ((vert (aa2u-1c 'aa2u-ucs-bd-uniform-name 'LIGHT 'VERTICAL)))
    (while (search-forward "|" nil t)
      (replace-match vert t t)))
  (goto-char (point-min))
  (let ((horz (aa2u-1c 'aa2u-ucs-bd-uniform-name 'LIGHT 'HORIZONTAL)))
    (while (search-forward "-" nil t)
      (replace-match horz t t))))

(defun aa2u-replacement (pos)
  (let ((cc (- pos (line-beginning-position))))
    (cl-labels
        ((ok (name pos)
             (when (or
                    ;; Infer LIGHTness between "snug" ‘?+’es.
                    ;;              |
                    ;;  +-----------++--+   +
                    ;;  | somewhere ++--+---+-+----+
                    ;;  +-+---------+ nowhere |+--+
                    ;;    +         +---------++
                    ;;              |      +---|
                    (eq ?+ (char-after pos))
                    ;; Require properly directional neighborliness.
                    (memq (case name
                            ((n s) 'VERTICAL)
                            ((w e) 'HORIZONTAL))
                          (get-text-property pos 'aa2u-components)))
               name))
         (v (name dir) (let ((bol (line-beginning-position dir))
                             (eol (line-end-position dir)))
                         (when (< cc (- eol bol))
                           (ok name (+ bol cc)))))
         (h (name dir) (let ((bol (line-beginning-position))
                             (eol (line-end-position))
                             (pos (+ pos dir)))
                         (unless (or (> bol pos)
                                     (<= eol pos))
                           (ok name pos))))
         (light (&rest components) (apply 'aa2u-1c
                                          'aa2u-ucs-bd-uniform-name
                                          'LIGHT components)))
      (let* ((n (v 'n 0))
             (s (v 's 2))
             (w (h 'w -1))
             (e (h 'e  1)))
        (pcase (delq nil (list n s w e))
          (`(n s w e) (light 'VERTICAL 'HORIZONTAL))
          (`(s e)     (light 'DOWN 'RIGHT))
          (`(s w)     (light 'DOWN 'LEFT))
          (`(n e)     (light 'UP 'RIGHT))
          (`(n w)     (light 'UP 'LEFT))
          (`(n s e)   (light 'VERTICAL 'RIGHT))
          (`(n s w)   (light 'VERTICAL 'LEFT))
          (`(n w e)   (light 'UP 'HORIZONTAL))
          (`(s w e)   (light 'DOWN 'HORIZONTAL))
          (`(n)       (light 'UP))
          (`(s)       (light 'DOWN))
          (`(w)       (light 'LEFT))
          (`(e)       (light 'RIGHT))
          (_          nil))))))

(defun aa2u-phase-2 ()
  (goto-char (point-min))
  (let (changes)
    ;; (phase 2.1 -- what WOULD change)
    ;; This is for the benefit of ‘aa2u-replacement ok’, which
    ;; otherwise (monolithic phase 2) would need to convert the
    ;; "properly directional neighborliness" impl from a simple
    ;; ‘memq’ to an ‘intersction’.
    (while (search-forward "+" nil t)
      (let ((p (point)))
        (push (cons p (or (aa2u-replacement (1- p))
                          "?"))
              changes)))
    ;; (phase 2.2 -- apply changes)
    (dolist (ch changes)
      (goto-char (car ch))
      (delete-char -1)
      (insert (cdr ch)))))

(defun aa2u-phase-3 ()
  (remove-text-properties (point-min) (point-max)
                          (list 'aa2u-stringifier nil
                                'aa2u-components nil)))

;;;---------------------------------------------------------------------------
;;; command

;;;###autoload
(defun aa2u (beg end &optional interactive)
  "Convert simple ASCII art line drawings to Unicode.
Specifically, perform the following replacements:

  - (hyphen)          BOX DRAWINGS LIGHT HORIZONTAL
  | (vertical bar)    BOX DRAWINGS LIGHT VERTICAL
  + (plus)            (one of)
                      BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
                      BOX DRAWINGS LIGHT DOWN AND RIGHT
                      BOX DRAWINGS LIGHT DOWN AND LEFT
                      BOX DRAWINGS LIGHT UP AND RIGHT
                      BOX DRAWINGS LIGHT UP AND LEFT
                      BOX DRAWINGS LIGHT VERTICAL AND RIGHT
                      BOX DRAWINGS LIGHT VERTICAL AND LEFT
                      BOX DRAWINGS LIGHT UP AND HORIZONTAL
                      BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
                      BOX DRAWINGS LIGHT UP
                      BOX DRAWINGS LIGHT DOWN
                      BOX DRAWINGS LIGHT LEFT
                      BOX DRAWINGS LIGHT RIGHT
                      QUESTION MARK

More precisely, hyphen and vertical bar are substituted unconditionally,
first, and plus is substituted with a character depending on its north,
south, east and west neighbors.

This command operates on either the active region,
or the accessible portion otherwise."
  (interactive "r\np")
  ;; This weirdness, along w/ the undocumented "p" in the ‘interactive’
  ;; form, is to allow ‘M-x aa2u’ (interactive invocation) w/ no region
  ;; selected to default to the accessible portion (as documented), which
  ;; was the norm in ascii-art-to-unicode.el prior to 1.5.  A bugfix,
  ;; essentially.  This is ugly, unfortunately -- is there a better way?!
  (when (and interactive (not (region-active-p)))
    (setq beg (point-min)
          end (point-max)))
  (save-excursion
    (save-restriction
      (widen)
      (narrow-to-region beg end)
      (aa2u-phase-1)
      (aa2u-phase-2)
      (aa2u-phase-3))))

;;;---------------------------------------------------------------------------
;;; that's it

(provide 'ascii-art-to-unicode)

;;; ascii-art-to-unicode.el ends here
