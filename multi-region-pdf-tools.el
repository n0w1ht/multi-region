;;; multi-region-pdf-tools.el --- What multi-region needs from pdf-tools  -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Every part of multi-region that relies on how pdf-tools works inside,
;; rather than on what it documents, is in this file.  The rest of the
;; PDF support (multi-region-pdf.el) goes through the functions here,
;; and otherwise uses only documented pdf-tools functions and hooks.
;;
;; What this file takes for granted:
;;
;;  1. `pdf-view-active-region' holds the region on display, either as
;;     (PAGE . EDGES), as pdf-tools 1.3 and later keep it, or as a plain
;;     list of EDGES, as older ones do.  With no region yet, which form
;;     is in use is guessed from the variable's doc string.
;;
;;  2. `pdf-view-display-region' takes (REGION RECTANGLE-P
;;     SELECTION-STYLE) and draws the page by calling
;;     `pdf-info-renderpage-text-regions' with PAGE and WIDTH first,
;;     when RECTANGLE-P is nil.  Replacing that function for the call
;;     lets this package say what is highlighted while pdf-tools makes
;;     and shows the image.
;;
;;  3. `pdf-info-renderpage' takes (PAGE WIDTH FILE-OR-BUFFER &rest
;;     COMMANDS) and understands :foreground, :background, :alpha,
;;     :selection-style, :highlight-text and :highlight-region.
;;
;;  4. The mouse commands `pdf-view-mouse-set-region',
;;     `pdf-view-mouse-extend-region' and
;;     `pdf-view-mouse-set-region-rectangle' draw through
;;     `pdf-view-display-region' and deselect through
;;     `pdf-view-deactivate-region', calling both by name, so that they
;;     can be replaced while the command runs.
;;
;;  5. `pdf-view--have-rectangle-region', a private variable, makes
;;     `pdf-view-mouse-extend-region' draw a rectangle when non-nil.
;;
;;  6. The faces `pdf-view-region' and `pdf-view-rectangle' give the
;;     colors, through `pdf-util-face-colors'.
;;
;;  7. pdf-tools keeps the page on display and its image in
;;     `image-mode-window-get' under `page' and `image'.
;;
;;  8. pdf-tools activates the mark while a region is selected, so that
;;     C-g deselects it.
;;
;; multi-region-pdf-tools-test.el loads the pdf-tools that is installed
;; and checks each of these.  Run it after updating pdf-tools: a
;; failure names the assumption that no longer holds.

;;; Code:

(require 'cl-lib)

(defvar pdf-view-active-region)
(defvar pdf-view-selection-style)
(defvar pdf-view--have-rectangle-region)
(declare-function pdf-view-display-region "ext:pdf-view")
(declare-function pdf-view-deactivate-region "ext:pdf-view")
(declare-function pdf-view-redisplay "ext:pdf-view")
(declare-function pdf-info-renderpage "ext:pdf-info")
(declare-function pdf-util-face-colors "ext:pdf-util")
(declare-function pdf-util-scale-pixel-to-relative "ext:pdf-util")
(declare-function image-mode-window-get "image-mode")

(defconst multi-region--pdf-tools-api
  '((functions pdf-view-mode pdf-view-mouse-set-region pdf-view-mouse-extend-region
               pdf-view-mouse-set-region-rectangle pdf-view-deactivate-region
               pdf-view-display-region pdf-view-redisplay
               pdf-info-gettext pdf-info-renderpage
               pdf-info-renderpage-text-regions
               pdf-util-face-colors pdf-util-scale-pixel-to-relative)
    (variables pdf-view-active-region pdf-view--have-rectangle-region
               pdf-view-selection-style pdf-view-dark-minor-mode
               pdf-view-before-change-page-hook
               pdf-view-after-change-page-hook)
    (faces pdf-view-region pdf-view-rectangle))
  "The pdf-tools functions, variables and faces multi-region uses.
multi-region-pdf-tools-test.el checks that each exists, and that no
other pdf-tools name is used.")

;;;; The region, in either form (1)

(defvar-local multi-region--pdf-paged nil
  "Non-nil once a region of the form (PAGE . EDGES) was seen here.")

(defun multi-region--pdf-page ()
  "Return the PDF page shown in the selected window."
  (image-mode-window-get 'page))

(defun multi-region--pdf-image (window)
  "Return the image of the page shown in WINDOW."
  (image-mode-window-get 'image window))

(defun multi-region--pdf-paged-p ()
  "Return non-nil if `pdf-view-active-region' has the form (PAGE . EDGES).
Older pdf-tools keep a plain list of edges there, newer ones put the
page number in front.  Both are supported."
  (cond ((consp (bound-and-true-p pdf-view-active-region))
         (setq multi-region--pdf-paged (numberp (car pdf-view-active-region))))
        (multi-region--pdf-paged)
        (t (let ((doc (get 'pdf-view-active-region 'variable-documentation)))
             (and (stringp doc) (string-match-p "\\bpage\\b" doc) t)))))

(defun multi-region--pdf-edges ()
  "Return the list of edges selected on the PDF page on display."
  (let ((region (bound-and-true-p pdf-view-active-region)))
    (if (numberp (car-safe region))
        (cdr region)
      region)))

(defun multi-region--pdf-region-page ()
  "Return the page the active PDF region names, or nil.
Only the form (PAGE . EDGES) names one."
  (let ((region (bound-and-true-p pdf-view-active-region)))
    (and (numberp (car-safe region)) (car region))))

(defun multi-region--pdf-live-page ()
  "Return the page of the active PDF region.
That is the page in front of the region in current pdf-tools, or else
the page on display."
  (or (multi-region--pdf-region-page) (multi-region--pdf-page)))

(defun multi-region--pdf-set (page edges)
  "Make EDGES, a list of edges on PAGE, the active PDF region."
  (setq pdf-view-active-region
        (and edges (if (multi-region--pdf-paged-p) (cons page edges) edges))))

(defun multi-region--pdf-activate-mark ()
  "Activate the mark, as pdf-tools does for a region it selects (8)."
  (let (mark-ring)
    (push-mark-command nil t))
  (setq deactivate-mark nil))

;;;; Drawing (2, 3, 6)

(defun multi-region--pdf-commands (command edges)
  "Return the drawing COMMAND for each of EDGES, as `pdf-info-renderpage' wants."
  (apply #'append (mapcar (lambda (e) (list command e)) edges)))

(defun multi-region--pdf-draw-edges (page edges rects &optional display)
  "Draw PAGE with the text regions EDGES and the picture areas RECTS.
Text is highlighted as pdf-tools does, and picture areas as pdf-tools
highlights a rectangle.  DISPLAY is the function that shows the
image, `pdf-view-display-region' by default."
  (if (not (or edges rects))
      (pdf-view-redisplay)
    (let* ((dark (bound-and-true-p pdf-view-dark-minor-mode))
           (box (bound-and-true-p pdf-view--have-rectangle-region))
           (style (bound-and-true-p pdf-view-selection-style))
           (text (pdf-util-face-colors
                  (if box 'pdf-view-rectangle 'pdf-view-region) dark))
           (area (pdf-util-face-colors 'pdf-view-rectangle dark))
           (commands
            (append
             (cond
              ((null edges) nil)
              (box `(:background ,(car text) :foreground ,(cdr text) :alpha 0.35
                     ,@(multi-region--pdf-commands :highlight-region edges)))
              (t `(:selection-style ,style
                   :foreground ,(car text) :background ,(cdr text)
                   ,@(multi-region--pdf-commands :highlight-text edges))))
             (and rects
                  `(:background ,(car area) :foreground ,(cdr area) :alpha 0.35
                    ,@(multi-region--pdf-commands :highlight-region rects))))))
      ;; Let pdf-tools make and show the image, which differs between
      ;; versions; only what is highlighted on it is ours.
      (cl-letf (((symbol-function 'pdf-info-renderpage-text-regions)
                 (lambda (pg width &rest _)
                   (apply #'pdf-info-renderpage pg width nil commands))))
        (funcall (or display #'pdf-view-display-region)
                 (if (multi-region--pdf-paged-p)
                     (cons page edges)
                   (or edges (list nil)))
                 nil style)))))

;;;; Mouse commands (4, 5)

;; While the mouse is dragged, pdf-tools draws the page anew at every
;; step, which is slow on big pages.  A step is skipped when the mouse
;; has already moved on, and the caller draws the last one after the
;; drag.

(defun multi-region--pdf-call-mouse (command event draw &optional quiet)
  "Run pdf-tools' mouse COMMAND with EVENT, letting DRAW show the page.
Each time COMMAND would draw the page with its region, DRAW is called
instead with the page, the list of edges on it, and the function
that draws, for `multi-region--pdf-draw-edges'.  With QUIET, COMMAND
deselecting the regions on the page does not draw it.  The region
is made as text, never as a rectangle.  Return non-nil if a drawing
was skipped."
  (let ((display (symbol-function 'pdf-view-display-region))
        (deactivate (symbol-function 'pdf-view-deactivate-region))
        skipped)
    (cl-letf (((symbol-function 'pdf-view-display-region)
               (lambda (&optional region &rest _)
                 (if (input-pending-p)
                     (setq skipped t)
                   (let ((paged (numberp (car-safe region))))
                     (funcall draw
                              (if paged (car region) (multi-region--pdf-page))
                              (if paged (cdr region) region)
                              display)))))
              ((symbol-function 'pdf-view-deactivate-region)
               (if quiet
                   (lambda () (setq pdf-view-active-region nil))
                 deactivate)))
      (setq pdf-view--have-rectangle-region nil)
      (funcall command event)
      (setq pdf-view--have-rectangle-region nil))
    skipped))

(defun multi-region--pdf-event-edges (event)
  "Return the point on the PDF page of mouse EVENT, as relative edges.
The result is (X Y X Y), or nil if EVENT is not on the page."
  (let ((posn (event-start event)))
    (when (posn-image posn)
      (let ((xy (posn-object-x-y posn)))
        (pdf-util-scale-pixel-to-relative
         (list (car xy) (cdr xy) (car xy) (cdr xy)))))))

(provide 'multi-region-pdf-tools)
;;; multi-region-pdf-tools.el ends here
