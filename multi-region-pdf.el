;;; multi-region-pdf.el --- multi-region in PDFs read with pdf-tools  -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A pdf-tools page is an image, so there is no point or mark and
;; selecting is mouse only.  pdf-tools can hold several regions on the
;; page on display (drag, then C-drag); this file remembers them per
;; page, keeps picture areas (M-drag) apart from text, and copies across
;; pages.
;;
;; What depends on pdf-tools internals is in multi-region-pdf-tools.el.
;;
;; Part of multi-region; load `multi-region', not this file.

;;; Code:

(require 'seq)
(require 'multi-region-pdf-tools)

(declare-function pdf-view-mouse-extend-region "ext:pdf-view")
(declare-function pdf-view-mouse-set-region "ext:pdf-view")
(declare-function pdf-view-mouse-set-region-rectangle "ext:pdf-view")
(declare-function pdf-view-deactivate-region "ext:pdf-view")
(declare-function pdf-view-redisplay "ext:pdf-view")
(declare-function pdf-info-gettext "ext:pdf-info")
(declare-function multi-region--reset "multi-region")

(defvar-local multi-region--pdf-regions nil
  "PDF regions remembered per page, as an alist of (PAGE . REGIONS).
The entry for the page on display may be stale; the live value for
that page is `multi-region--pdf-edges'.")

(defvar-local multi-region--pdf-timer nil
  "Timer that redraws restored PDF regions.")

(defvar-local multi-region--pdf-shown nil
  "The window and page image last drawn by `multi-region--pdf-show'.")

(defvar-local multi-region--pdf-rects nil
  "Picture areas selected in the PDF, most recent first.
Each is (PAGE . EDGES), EDGES being (LEFT TOP RIGHT BOTTOM) in
relative coordinates.  They are kept apart from the active region,
which holds text.")

(defvar-local multi-region--pdf-last-rect nil
  "The entry of `multi-region--pdf-rects' added last.
It is nil once text was selected after it.")

(defun multi-region--pdf-p ()
  "Return non-nil if the current buffer is a pdf-tools buffer."
  (derived-mode-p 'pdf-view-mode))

;;;; Regions per page

(defun multi-region--pdf-stash (page edges)
  "Remember EDGES as the regions of PAGE, which is not the live one."
  (setq multi-region--pdf-regions
        (assq-delete-all page multi-region--pdf-regions))
  (when edges
    (push (cons page (copy-sequence edges)) multi-region--pdf-regions)))

(defun multi-region--pdf-merge (pg edges page old)
  "Return EDGES, on page PG, with OLD put back in front.
OLD are edges on PAGE and are put back only if PG is PAGE."
  (if (and old (eql pg page))
      (append old (seq-remove (lambda (e) (member e old)) edges))
    edges))

(defun multi-region--pdf-rects-on (page)
  "Return the picture areas selected on PAGE, in the order they were added."
  (let (rects)
    (dolist (rect multi-region--pdf-rects rects)
      (when (eql (car rect) page)
        (push (cdr rect) rects)))))

(defun multi-region--pdf-rect-at (page edges)
  "Return the entry of the picture area on PAGE around the point EDGES."
  (seq-find (lambda (rect)
              (and (eql (car rect) page)
                   (<= (nth 0 (cdr rect)) (nth 0 edges) (nth 2 (cdr rect)))
                   (<= (nth 1 (cdr rect)) (nth 1 edges) (nth 3 (cdr rect)))))
            multi-region--pdf-rects))

;;;; Mouse

(defun multi-region--pdf-track (command event page old &optional quiet rect)
  "Call pdf-tools' mouse COMMAND with EVENT, drawing only what is needed.
OLD are edges on PAGE to keep showing and put back afterwards, as
`multi-region--pdf-merge' does.  With QUIET, the regions on the page
are not drawn away when COMMAND deselects them first.  With RECT,
COMMAND makes a picture area: it is drawn as one, next to OLD, and
not merged with them; the caller sorts that out."
  (let ((skipped
         (multi-region--pdf-call-mouse
          command event
          (lambda (pg edges display)
            (if rect
                (multi-region--pdf-draw-edges
                 pg (and (eql pg page) old)
                 (append (multi-region--pdf-rects-on pg) (list (car edges)))
                 display)
              (multi-region--pdf-draw-edges
               pg (multi-region--pdf-merge pg edges page old)
               (multi-region--pdf-rects-on pg) display)))
          quiet)))
    (unless rect
      (when (and old page (eql (multi-region--pdf-region-page) page))
        (multi-region--pdf-set page (multi-region--pdf-merge
                                     page (multi-region--pdf-edges) page old)))
      (when (and skipped (multi-region--pdf-edges))
        (multi-region--pdf-draw)))))

(defun multi-region--pdf-mouse-drag (event)
  "Add the region dragged over with EVENT to the PDF page.
pdf-tools 1.3 and later start a new region list on every drag, even
the C-drag that is meant to add to it, so the regions already there
are put back: in what is drawn during the drag, and in the region
afterwards."
  (let* ((page (multi-region--pdf-region-page))
         (old (and page (copy-sequence (multi-region--pdf-edges)))))
    (multi-region--pdf-track #'pdf-view-mouse-extend-region event page old)
    (when (multi-region--pdf-edges)
      (setq multi-region--pdf-last-rect nil))
    ;; Dragged on another page, as pdf-view-roll-minor-mode allows.
    (let ((now (multi-region--pdf-region-page)))
      (when (and old now (not (eql now page)))
        (multi-region--pdf-stash page old)))
    (force-mode-line-update)))

(defun multi-region-pdf-mouse-set-region (event)
  "Start a new PDF region with the mouse, keeping selections on a click.
EVENT is the button press.  A drag replaces the regions on the page,
as `pdf-view-mouse-set-region' does.  A click that drags over
nothing leaves them selected, as a click does in a text buffer."
  (interactive "@e")
  (let ((page (multi-region--pdf-live-page))
        (old (copy-sequence (multi-region--pdf-edges))))
    ;; Deselect quietly: a click must not make the page blink.
    (multi-region--pdf-track #'pdf-view-mouse-set-region event nil nil old)
    (cond
     ((multi-region--pdf-edges)
      (setq multi-region--pdf-last-rect nil))
     ((null old))
     (t
      ;; Only clicked.  The regions are still drawn; select them again.
      (multi-region--pdf-set page old)
      (multi-region--pdf-activate-mark)))
    (force-mode-line-update)))

(defun multi-region-pdf-mouse-rectangle (event)
  "Select a picture area of a PDF page by dragging a rectangle.
EVENT is the button press.  The area is added to the selections,
next to the text selected on the page: `multi-region-selections'
reports it with :figure t, so that it can be saved as a picture.
A click without a drag deselects the picture area under it."
  (interactive "@e")
  (let* ((page (multi-region--pdf-live-page))
         (old (copy-sequence (multi-region--pdf-edges)))
         (click (multi-region--pdf-event-edges event)))
    (multi-region--pdf-track #'pdf-view-mouse-set-region-rectangle
                             event page old t t)
    (let ((pg (multi-region--pdf-live-page))
          (rect (car (last (multi-region--pdf-edges)))))
      ;; Give back the text pdf-tools deselected.
      (multi-region--pdf-set page old)
      (cond
       ((and rect (< (nth 0 rect) (nth 2 rect)) (< (nth 1 rect) (nth 3 rect)))
        (push (cons pg rect) multi-region--pdf-rects)
        (setq multi-region--pdf-last-rect (car multi-region--pdf-rects)))
       (click
        (let ((hit (multi-region--pdf-rect-at pg click)))
          (when hit
            (setq multi-region--pdf-rects (delq hit multi-region--pdf-rects))))))
      (if (multi-region--pdf-edges)
          (multi-region--pdf-activate-mark)
        (deactivate-mark))
      (multi-region--pdf-draw-edges pg (and (eql pg page) old)
                                    (multi-region--pdf-rects-on pg))
      (force-mode-line-update))))

;;;; Turning pages

(defun multi-region--pdf-save ()
  "Remember the regions selected on the page that is being left.
This goes on `pdf-view-before-change-page-hook'."
  (let ((page (multi-region--pdf-page)))
    (when page
      (multi-region--pdf-paged-p)       ; Learn the format while we can.
      (multi-region--pdf-stash page (multi-region--pdf-edges)))))

(defun multi-region--pdf-draw ()
  "Draw the active PDF region the way pdf-tools drew it when selecting.
Without arguments `pdf-view-display-region' highlights glyph by glyph,
which can show less than was selected and will be copied.  The
picture areas on the page are drawn too."
  (let ((page (multi-region--pdf-live-page)))
    (multi-region--pdf-draw-edges page (multi-region--pdf-edges)
                                  (multi-region--pdf-rects-on page))))

(defun multi-region--pdf-show (buffer window)
  "Draw the active PDF regions of BUFFER in WINDOW.
Do nothing if WINDOW already shows them."
  (when (and (buffer-live-p buffer)
             (window-live-p window)
             (eq (window-buffer window) buffer))
    (with-selected-window window
      (when (and (or (multi-region--pdf-edges)
                     (multi-region--pdf-rects-on (multi-region--pdf-page)))
                 (not (equal multi-region--pdf-shown
                             (cons window (multi-region--pdf-image window)))))
        (multi-region--pdf-draw)
        (setq multi-region--pdf-shown
              (cons window (multi-region--pdf-image window)))))))

(defun multi-region--pdf-show-now ()
  "Draw restored PDF regions once the page-turning command is done.
This runs on `post-command-hook', before the screen is updated, so the
page appears with its highlights instead of blinking without them."
  (remove-hook 'post-command-hook #'multi-region--pdf-show-now t)
  (multi-region--pdf-show (current-buffer) (car multi-region--pdf-shown)))

(defun multi-region--pdf-restore ()
  "Reselect the regions remembered for the page just arrived at.
This goes on `pdf-view-after-change-page-hook'."
  (let* ((page (multi-region--pdf-page))
         (regions (cdr (assq page multi-region--pdf-regions))))
    (when (and (or regions (multi-region--pdf-rects-on page))
               (null (multi-region--pdf-edges)))
      (when regions
        (multi-region--pdf-set page (copy-sequence regions))
        ;; As pdf-tools does for its own selections, so that `C-g'
        ;; deselects.
        (multi-region--pdf-activate-mark))
      (multi-region--pdf-redraw-later))))

(defun multi-region--pdf-redraw-later ()
  "Draw the selections of the page on display once the command is done.
Page-turning commands draw the plain page once more after
`pdf-view-after-change-page-hook' has run, and so does pdf-tools when
the mark is deactivated."
  ;; Some draw it later still, when Emacs is idle; the timer covers
  ;; them, and does nothing if the highlight is still there.
  (setq multi-region--pdf-shown (list (selected-window)))
  (add-hook 'post-command-hook #'multi-region--pdf-show-now nil t)
  (when (timerp multi-region--pdf-timer)
    (cancel-timer multi-region--pdf-timer))
  (setq multi-region--pdf-timer
        (run-with-idle-timer 0.1 nil #'multi-region--pdf-show
                             (current-buffer) (selected-window))))

(defun multi-region--pdf-after-deactivate ()
  "Draw the picture areas again after pdf-tools drew the page without them.
This goes on `deactivate-mark-hook', after pdf-tools' own function."
  (when (multi-region--pdf-rects-on (multi-region--pdf-page))
    (multi-region--pdf-redraw-later)))

(defun multi-region--pdf-setup ()
  "Start keeping PDF regions across pages in the current buffer."
  (add-hook 'pdf-view-before-change-page-hook #'multi-region--pdf-save nil t)
  (add-hook 'pdf-view-after-change-page-hook #'multi-region--pdf-restore nil t)
  (add-hook 'deactivate-mark-hook #'multi-region--pdf-after-deactivate t t))

(defun multi-region--pdf-teardown ()
  "Undo `multi-region--pdf-setup'."
  (remove-hook 'pdf-view-before-change-page-hook #'multi-region--pdf-save t)
  (remove-hook 'pdf-view-after-change-page-hook #'multi-region--pdf-restore t)
  (remove-hook 'deactivate-mark-hook #'multi-region--pdf-after-deactivate t)
  (remove-hook 'post-command-hook #'multi-region--pdf-show-now t)
  (when (timerp multi-region--pdf-timer)
    (cancel-timer multi-region--pdf-timer))
  (setq multi-region--pdf-timer nil))

;;;; Reading the selections

(defun multi-region--pdf-page-selections (page edges rects style)
  "Return the selections on PAGE: text EDGES and picture areas RECTS.
Text comes in the order it was selected, and each picture area
before the first text below its top.  STYLE is the selection style."
  (let ((rects (sort (copy-sequence rects)
                     (lambda (a b) (< (nth 1 a) (nth 1 b)))))
        selections)
    (dolist (e edges)
      (while (and rects (< (nth 1 (car rects)) (nth 1 e)))
        (push (pop rects) selections))
      (push e selections))
    (mapcar (lambda (e)
              (if (memq e edges)
                  (list :page page :edges e
                        :text (pdf-info-gettext page e style))
                ;; pdf-tools reads text from one corner to the other,
                ;; not inside a box, so a picture area has none.
                (list :page page :edges e :text "" :figure t)))
            (append (nreverse selections) rects))))

(defun multi-region--pdf-selections ()
  "Return the selections of a PDF, page by page.
Within a page they come in the order they were selected, picture
areas placed by their height on the page.  Each is a plist (:page
PAGE :edges EDGES :text TEXT); a picture area has :figure t and an
empty TEXT."
  (let* ((edges (multi-region--pdf-edges))
         ;; The region knows its page in current pdf-tools.
         (page (and (or edges multi-region--pdf-regions multi-region--pdf-rects)
                    (multi-region--pdf-live-page)))
         (pages (and page
                     (cons (cons page edges)
                           (seq-remove (lambda (entry) (eql (car entry) page))
                                       multi-region--pdf-regions))))
         (style (bound-and-true-p pdf-view-selection-style)))
    (dolist (rect multi-region--pdf-rects)
      (unless (assq (car rect) pages)
        (push (list (car rect)) pages)))
    (apply #'append
           (mapcar (lambda (entry)
                     (multi-region--pdf-page-selections
                      (car entry) (cdr entry)
                      (multi-region--pdf-rects-on (car entry)) style))
                   (sort pages (lambda (a b) (< (car a) (car b))))))))

(defun multi-region--pdf-texts ()
  "Return the selected strings of a PDF, page by page.
Picture areas are left out."
  (mapcar (lambda (s) (plist-get s :text))
          (seq-remove (lambda (s) (plist-get s :figure))
                      (multi-region--pdf-selections))))

(defun multi-region--pdf-preview-texts (snapshot length)
  "Return the strings of the first text regions in SNAPSHOT, page by page.
SNAPSHOT is from `multi-region--pdf-snapshot'.  Each string is read
from the PDF, so reading stops once they hold LENGTH characters."
  (let ((style (bound-and-true-p pdf-view-selection-style))
        (n 0)
        texts)
    (catch 'done
      (dolist (entry (sort (copy-sequence (plist-get snapshot :regions))
                           (lambda (a b) (< (car a) (car b)))))
        (dolist (e (cdr entry))
          (let ((text (pdf-info-gettext (car entry) e style)))
            (push text texts)
            (setq n (+ n (length text)))
            (when (>= n length)
              (throw 'done nil))))))
    (nreverse texts)))

(defun multi-region--pdf-count ()
  "Return the number of selections in a PDF, on all pages."
  (let ((page (multi-region--pdf-live-page))
        (n (+ (length (multi-region--pdf-edges))
              (length multi-region--pdf-rects))))
    (dolist (entry multi-region--pdf-regions n)
      (unless (eq (car entry) page)
        (setq n (+ n (length (cdr entry))))))))

;;;; Deselecting and bringing back

(defun multi-region--pdf-snapshot ()
  "Return the selections of a PDF as (:regions REGIONS :rects RECTS).
REGIONS is an alist of (PAGE . EDGES) that includes the page on
display, and RECTS the picture areas."
  (let ((page (multi-region--pdf-live-page))
        (edges (multi-region--pdf-edges)))
    (list :regions (append (and edges (list (cons page (copy-sequence edges))))
                           (seq-remove (lambda (entry) (eql (car entry) page))
                                       (copy-tree multi-region--pdf-regions)))
          :rects (copy-tree multi-region--pdf-rects))))

(defun multi-region--pdf-bring-back (snapshot)
  "Select again what SNAPSHOT, from `multi-region--pdf-snapshot', holds."
  (let ((page (multi-region--pdf-live-page)))
    (dolist (entry (plist-get snapshot :regions))
      (let* ((live (if (eql (car entry) page)
                       (multi-region--pdf-edges)
                     (cdr (assq (car entry) multi-region--pdf-regions))))
             (edges (append live (seq-remove (lambda (e) (member e live))
                                             (cdr entry)))))
        (if (eql (car entry) page)
            (multi-region--pdf-set page edges)
          (multi-region--pdf-stash (car entry) edges))))
    (dolist (rect (reverse (plist-get snapshot :rects)))
      (unless (member rect multi-region--pdf-rects)
        (push rect multi-region--pdf-rects)))
    (when (multi-region--pdf-edges)
      (multi-region--pdf-activate-mark))
    (when (or (multi-region--pdf-edges)
              (multi-region--pdf-rects-on page))
      (multi-region--pdf-draw))))

(defun multi-region--pdf-remove-last ()
  "Deselect the text region or picture area added last on the page."
  (let* ((page (multi-region--pdf-live-page))
         (edges (multi-region--pdf-edges))
         (last (and (memq multi-region--pdf-last-rect multi-region--pdf-rects)
                    (eql (car multi-region--pdf-last-rect) page)
                    multi-region--pdf-last-rect))
         (rect (or last
                   (and (null edges)
                        (seq-find (lambda (r) (eql (car r) page))
                                  multi-region--pdf-rects)))))
    (cond
     (rect
      (setq multi-region--pdf-rects (delq rect multi-region--pdf-rects)
            multi-region--pdf-last-rect nil)
      (multi-region--pdf-draw)
      (force-mode-line-update))
     ((null edges)
      (user-error "No selection on this page"))
     ((or (cdr edges) (multi-region--pdf-rects-on page))
      (multi-region--pdf-set page (butlast edges))
      (unless (multi-region--pdf-edges)
        (deactivate-mark))
      (multi-region--pdf-draw)
      (force-mode-line-update))
     (t
      (pdf-view-deactivate-region)))))

(defun multi-region--pdf-clear ()
  "Deselect everything in a PDF, and draw the page without selections."
  (let ((had-rects multi-region--pdf-rects))
    (multi-region--reset)
    (cond ((multi-region--pdf-edges)
           (pdf-view-deactivate-region))
          (had-rects (pdf-view-redisplay t)))))

(defun multi-region--pdf-reset ()
  "Forget the regions of other pages and all picture areas."
  (setq multi-region--pdf-regions nil
        multi-region--pdf-rects nil
        multi-region--pdf-last-rect nil))

(provide 'multi-region-pdf)
;;; multi-region-pdf.el ends here
