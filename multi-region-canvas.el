;;; multi-region-canvas.el --- multi-region in PDFs read with pdf-canvas  -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; pdf-canvas holds several selections itself, on any number of pages
;; (drag, then C-drag; M-drag for a picture area).  Here they are only
;; passed on: to the commands of multi-region, and to other packages
;; through `multi-region-selections', in the same form as the
;; selections of a pdf-tools buffer.
;;
;; Part of multi-region; load `multi-region', not this file.

;;; Code:

(require 'seq)

(declare-function pdf-canvas-selections "ext:pdf-canvas")
(declare-function pdf-canvas-selection-count "ext:pdf-canvas")
(declare-function pdf-canvas-add-selection "ext:pdf-canvas")
(declare-function pdf-canvas-remove-selection "ext:pdf-canvas")
(declare-function pdf-canvas-clear-selections "ext:pdf-canvas")

(defun multi-region--canvas-p ()
  "Return non-nil if the current buffer is a pdf-canvas buffer."
  (derived-mode-p 'pdf-canvas-mode))

(defun multi-region--canvas-selections ()
  "Return the selections of a pdf-canvas buffer, page by page.
Each is a plist (:page PAGE :edges EDGES :text TEXT), with :figure t
and an empty TEXT for a picture area, as in a pdf-tools buffer.  The
keys pdf-canvas adds, such as :ends, are kept."
  (mapcar (lambda (s)
            (if (plist-get s :figure)
                (plist-put (copy-sequence s) :text "")
              s))
          (pdf-canvas-selections)))

(defun multi-region--canvas-texts ()
  "Return the selected strings of a pdf-canvas buffer, page by page.
Picture areas are left out."
  (mapcar (lambda (s) (plist-get s :text))
          (seq-remove (lambda (s) (plist-get s :figure))
                      (multi-region--canvas-selections))))

(defun multi-region--canvas-bring-back (selections)
  "Select again SELECTIONS, as `pdf-canvas-selections' returned them.
Those still selected are not added twice."
  (let ((have (mapcar (lambda (s) (list (plist-get s :page)
                                        (plist-get s :mode)
                                        (plist-get s :ends)))
                      (pdf-canvas-selections))))
    (dolist (s selections)
      (unless (member (list (plist-get s :page) (plist-get s :mode)
                            (plist-get s :ends))
                      have)
        (pdf-canvas-add-selection s)))))

(defun multi-region--canvas-remove-last ()
  "Deselect the selection made last in a pdf-canvas buffer."
  (let ((n (pdf-canvas-selection-count)))
    (when (= n 0)
      (user-error "No selections"))
    ;; pdf-canvas numbers its selections in the order they were made.
    (pdf-canvas-remove-selection (1- n))))

(provide 'multi-region-canvas)
;;; multi-region-canvas.el ends here
