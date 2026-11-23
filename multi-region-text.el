;;; multi-region-text.el --- multi-region in text buffers  -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Selections in buffers of text: text, Org, Markdown, code, eww, and
;; the chapter of an EPUB on display.  Each selection is an overlay.
;; Pictures shown in the buffer are selected along with the text.
;;
;; Part of multi-region; load `multi-region', not this file.

;;; Code:

(require 'seq)

(defvar multi-region-mode)
(defvar nov-documents-index)
(declare-function multi-region--nov-p "multi-region-epub")
(declare-function multi-region-mouse-drag "multi-region")

;;;; Selections

(defvar-local multi-region--overlays nil
  "Selection overlays of this buffer, most recently added first.")

(defun multi-region--live ()
  "Return this buffer's selection overlays, dropping dead ones."
  (setq multi-region--overlays
        (seq-filter (lambda (ov) (eq (overlay-buffer ov) (current-buffer)))
                    multi-region--overlays)))

(defun multi-region--sorted ()
  "Return this buffer's selection overlays in buffer order."
  (sort (copy-sequence (multi-region--live))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun multi-region--at (pos)
  "Return the selection overlay that covers POS, or nil."
  (seq-find (lambda (ov) (<= (overlay-start ov) pos (overlay-end ov)))
            (multi-region--live)))

(defun multi-region--add (beg end)
  "Select the text between BEG and END.
Selections that overlap or touch that text are merged into the new
one.  Return the new overlay, or nil if the text is empty."
  (when (< beg end)
    ;; A picture is selected whole or not at all.
    (let ((first (multi-region--image-bounds beg))
          (last (multi-region--image-bounds (1- end))))
      (when first (setq beg (min beg (car first))))
      (when last (setq end (max end (cdr last)))))
    (dolist (old (multi-region--live))
      (let ((s (overlay-start old))
            (e (overlay-end old)))
        (when (and (<= s end) (>= e beg))
          (setq beg (min beg s)
                end (max end e))
          (delete-overlay old))))
    ;; Front-advance, so that typing just before a selection does not
    ;; grow it.
    (let ((ov (make-overlay beg end nil t nil)))
      (overlay-put ov 'multi-region t)
      (overlay-put ov 'face 'multi-region-face)
      (overlay-put ov 'evaporate t)
      ;; An EPUB chapter is rendered anew in the same buffer; see
      ;; `multi-region--nov-save'.
      (when (multi-region--nov-p)
        (overlay-put ov 'multi-region-chapter nov-documents-index))
      (push ov multi-region--overlays)
      (multi-region--decorate)
      (force-mode-line-update)
      ov)))

(defun multi-region--delete (ov)
  "Deselect the selection overlay OV."
  (delete-overlay ov)
  (multi-region--decorate)
  (force-mode-line-update))

(defun multi-region--text-texts ()
  "Return the selected strings of a text buffer, in buffer order."
  (mapcar (lambda (ov)
            (filter-buffer-substring (overlay-start ov) (overlay-end ov)))
          (multi-region--sorted)))

(defun multi-region--text-selections ()
  "Return the selections of a text buffer as plists, in buffer order."
  (mapcar (lambda (ov)
            (let ((beg (overlay-start ov))
                  (end (overlay-end ov)))
              (multi-region--with-images
               (list :beg beg :end end
                     :text (buffer-substring-no-properties beg end))
               (multi-region--images beg end))))
          (multi-region--sorted)))

(defun multi-region--with-images (selection images)
  "Return SELECTION, a plist, with :images IMAGES added if there are any."
  (if images
      (append selection (list :images images))
    selection))

(defun multi-region--text-count ()
  "Return the number of selections in a text buffer."
  (length (multi-region--live)))

(defun multi-region--text-snapshot ()
  "Return the selections of a text buffer as (BEG . END) marker pairs."
  (mapcar (lambda (ov)
            (cons (copy-marker (overlay-start ov))
                  (copy-marker (overlay-end ov))))
          (multi-region--sorted)))

(defun multi-region--text-bring-back (pairs)
  "Select again the (BEG . END) marker PAIRS of a text snapshot."
  (dolist (pair pairs)
    (when (eq (marker-buffer (car pair)) (current-buffer))
      (multi-region--add (car pair) (cdr pair)))))

(defun multi-region--text-remove-last ()
  "Deselect the selection added last in a text buffer."
  (let ((ov (car (multi-region--live))))
    (unless ov
      (user-error "No selections"))
    (multi-region--delete ov)))

(defun multi-region--text-reset ()
  "Deselect everything in a text buffer, remembering nothing."
  (mapc #'delete-overlay (multi-region--live))
  (setq multi-region--overlays nil)
  (multi-region--decorate))

;;;; Pictures

;; A picture in a buffer is text with an image as its `display'
;; property, or under an overlay that has one: eww, EPUBs and Org and
;; Markdown inline images all work that way.  Selecting text around a
;; picture selects the picture too, and `multi-region-selections'
;; reports it.

(defcustom multi-region-image-margin 4
  "Width in pixels of the band drawn around selected pictures.
The band has the background color of `multi-region-face'.  With 0,
selected pictures look the same as unselected ones."
  :type 'integer
  :group 'multi-region)

(defconst multi-region--image-extensions
  "\\.\\(?:png\\|jpe?g\\|gif\\|svg\\|webp\\|bmp\\|tiff?\\|xpm\\)"
  "Regexp matching the extension of a picture file name.")

(defconst multi-region--org-image-link
  (concat "\\[\\[\\(?:file:\\)?\\([^]\n]+" multi-region--image-extensions
          "\\)\\]\\(?:\\[[^]\n]*\\]\\)?\\]")
  "Regexp matching an Org link to a picture file; group 1 is the file.")

(defconst multi-region--markdown-image-link
  (concat "!\\[[^]\n]*\\](<?\\([^)>\n\t ]+" multi-region--image-extensions
          "\\)>?\\(?:[ \t]+\"[^\"\n]*\"\\)?)")
  "Regexp matching a Markdown picture; group 1 is the file.")

(defvar-local multi-region--decor nil
  "Overlays that draw a band around selected pictures.")

(defun multi-region--image-in (display)
  "Return the image in the `display' property value DISPLAY, or nil."
  (cond ((eq (car-safe display) 'image) display)
        ((vectorp display) (seq-some #'multi-region--image-in display))
        ((consp (car-safe display)) (seq-some #'multi-region--image-in display))))

(defun multi-region--display-at (pos)
  "Return the `display' property in effect at POS.
The overlays that mark selected pictures are left out."
  (let ((ov (seq-find (lambda (ov)
                        (and (overlay-get ov 'display)
                             (not (overlay-get ov 'multi-region-decor))))
                      (overlays-at pos t))))
    (if ov
        (overlay-get ov 'display)
      (get-text-property pos 'display))))

(defun multi-region--image-at (pos)
  "Return the image displayed at POS, or nil."
  (and (<= (point-min) pos) (< pos (point-max))
       (multi-region--image-in (multi-region--display-at pos))))

(defun multi-region--image-bounds (pos)
  "Return (BEG . END) of the picture displayed at POS, or nil.
A big picture in eww is cut into slices, one per character and
rows of them on separate lines, all showing the same image; BEG and
END span all of them."
  (let ((image (or (multi-region--image-at pos)
                   ;; Between two rows of slices.
                   (and (eq (char-after pos) ?\n)
                        (let ((image (multi-region--image-at (1- pos))))
                          (and image
                               (eq image (multi-region--image-at (1+ pos)))
                               image))))))
    (when image
      (let ((beg pos) (end (1+ pos)))
        (while (or (eq (multi-region--image-at (1- beg)) image)
                   (and (eq (char-before beg) ?\n)
                        (eq (multi-region--image-at (- beg 2)) image)))
          (setq beg (1- beg)))
        (while (or (eq (multi-region--image-at end) image)
                   (and (eq (char-after end) ?\n)
                        (eq (multi-region--image-at (1+ end)) image)))
          (setq end (1+ end)))
        (cons beg end)))))

(defun multi-region--image-source (image)
  "Return where IMAGE comes from, as (:file FILE) or (:data DATA :type TYPE)."
  (let ((file (plist-get (cdr image) :file))
        (data (plist-get (cdr image) :data)))
    (cond
     (file (list :file (expand-file-name file)))
     (data (let ((type (plist-get (cdr image) :type)))
             (list :data data
                   :type (if (memq type '(nil imagemagick))
                             (and (stringp data) (image-type-from-data data))
                           type)))))))

(defun multi-region--image-links (beg end)
  "Return the picture links between BEG and END that show no picture.
Those are links in Org and Markdown buffers whose inline image is
not displayed, to files that exist."
  (let ((regexp (cond ((derived-mode-p 'org-mode) multi-region--org-image-link)
                      ((derived-mode-p 'markdown-mode)
                       multi-region--markdown-image-link)))
        (case-fold-search t)
        links)
    (when regexp
      (save-excursion
        (save-match-data
          (goto-char beg)
          (while (re-search-forward regexp end t)
            (let ((file (expand-file-name (match-string-no-properties 1))))
              (when (and (not (multi-region--image-at (match-beginning 0)))
                         (file-exists-p file))
                (push (list :beg (match-beginning 0) :end (match-end 0)
                            :file file)
                      links)))))))
    (nreverse links)))

(defun multi-region--images (beg end)
  "Return the pictures between BEG and END, in buffer order.
Each is a plist (:beg BEG :end END :file FILE), or (:beg BEG :end END
:data DATA :type TYPE) for a picture that is not in a file."
  (let ((pos beg) seen images)
    (while (< pos end)
      (let ((image (multi-region--image-at pos)))
        (when (and image (not (memq image seen)))
          (push image seen)
          (let ((bounds (multi-region--image-bounds pos))
                (source (multi-region--image-source image)))
            (when source
              (push (append (list :beg (car bounds) :end (cdr bounds)) source)
                    images)))))
      (setq pos (next-single-char-property-change pos 'display nil end)))
    (sort (append (nreverse images) (multi-region--image-links beg end))
          (lambda (a b) (< (plist-get a :beg) (plist-get b :beg))))))

(defun multi-region--subst-image (display old new)
  "Return DISPLAY with the image OLD in it replaced by NEW."
  (cond ((eq display old) new)
        ((vectorp display)
         (vconcat (mapcar (lambda (d) (multi-region--subst-image d old new))
                          display)))
        ((consp (car-safe display))
         (mapcar (lambda (d) (multi-region--subst-image d old new)) display))
        (t display)))

(defun multi-region--decorate ()
  "Draw a band around every selected picture, and none around others."
  (mapc #'delete-overlay multi-region--decor)
  (setq multi-region--decor nil)
  (when (and (> multi-region-image-margin 0) (display-images-p))
    (let (marked)
      (dolist (ov (multi-region--live))
        (let ((pos (overlay-start ov))
              (end (overlay-end ov)))
          (while (< pos end)
            (let* ((next (next-single-char-property-change pos 'display nil end))
                   (display (multi-region--display-at pos))
                   (image (multi-region--image-in display)))
              (when image
                (let ((new (or (cdr (assq image marked))
                               (let ((new (cons 'image
                                                (plist-put
                                                 (copy-sequence (cdr image))
                                                 :margin
                                                 multi-region-image-margin))))
                                 (push (cons image new) marked)
                                 new)))
                      (decor (make-overlay pos next nil)))
                  (overlay-put decor 'multi-region-decor t)
                  ;; The margin takes this face's background.
                  (overlay-put decor 'face 'multi-region-face)
                  (overlay-put decor 'priority 100)
                  (overlay-put decor 'evaporate t)
                  (overlay-put decor 'display
                               (multi-region--subst-image display image new))
                  (push decor multi-region--decor)))
              (setq pos next))))))))

;;;; Mouse

(defun multi-region--mouse-range (a b mode)
  "Return the list (BEG END) that a drag from A to B selects.
MODE is 0 to select by character, 1 by word and 2 by line."
  (condition-case nil
      (mouse-start-end a b mode)
    (error (list (min a b) (max a b)))))

(defun multi-region--track-drag (window origin mode)
  "Follow a mouse drag in WINDOW that began at position ORIGIN.
Select the text dragged over; MODE is as in
`multi-region--mouse-range'.  A click that covers no text deselects
the selection under it instead, or selects the picture under it.
Return nil if the click did none of that."
  (let* ((range (multi-region--mouse-range origin origin mode))
         (ov (make-overlay (car range) (nth 1 range)))
         (edges (window-edges window))
         (top (nth 1 edges))
         (bottom (1- (nth 3 edges)))
         event)
    (overlay-put ov 'face 'multi-region-face)
    (unwind-protect
        (progn
          (track-mouse
            (while (progn
                     (setq event (read-event))
                     (or (mouse-movement-p event)
                         (memq (car-safe event)
                               '(switch-frame select-window))))
              (when (mouse-movement-p event)
                (let* ((end (event-end event))
                       (pos (posn-point end)))
                  (if (and (eq (posn-window end) window)
                           (null (posn-area end))
                           (integerp pos))
                      (let ((r (multi-region--mouse-range origin pos mode)))
                        (move-overlay ov (car r) (nth 1 r)))
                    ;; The pointer left the text: scroll, as an ordinary
                    ;; drag does.
                    (let ((row (cdr (cdr (mouse-position)))))
                      (cond
                       ((null row))
                       ((< row top)
                        (mouse-scroll-subr window (- row top) ov origin))
                       ((>= row bottom)
                        (mouse-scroll-subr window (1+ (- row bottom))
                                           ov origin)))))))))
          ;; The loop ended on the button release.
          (let ((beg (overlay-start ov))
                (end (overlay-end ov)))
            (if (< beg end)
                (progn (multi-region--add beg end) t)
              (let ((old (multi-region--at origin))
                    (image (multi-region--image-bounds origin)))
                (cond
                 (old (multi-region--delete old) t)
                 ;; A click on a picture selects it.
                 (image (multi-region--add (car image) (cdr image)) t))))))
      (delete-overlay ov))))

(defun multi-region--text-binding (event)
  "Return what EVENT is bound to in the text it was made on, if anything.
That is a binding made by a `keymap' text property or the major mode,
such as eww's C-click on a link, which opens it in a new window."
  (let* ((key (vector (event-basic-type event)))
         (key (vector (event-convert-list
                       (append (event-modifiers event) (list (aref key 0))))))
         (command (let ((multi-region-mode nil))
                    (key-binding key t nil (event-start event)))))
    (and command
         (not (eq command (global-key-binding key)))
         (not (eq command 'multi-region-mouse-drag))
         command)))

(defun multi-region--text-mouse-drag (event)
  "Do `multi-region-mouse-drag' for EVENT in a text buffer."
  (let* ((posn (event-start event))
         (window (posn-window posn))
         (origin (posn-point posn)))
    (when (and (windowp window)
               (null (posn-area posn))
               (integerp origin))
      (unless (multi-region--track-drag window origin
                                        (1- (event-click-count event)))
        ;; A click that selected nothing does what it does without
        ;; this mode, such as opening a link in eww or an EPUB.
        (let ((command (multi-region--text-binding event)))
          (when command
            (funcall command event)))))))

(provide 'multi-region-text)
;;; multi-region-text.el ends here
