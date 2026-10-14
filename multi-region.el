;;; multi-region.el --- Select several separate regions at once  -*- lexical-binding: t; -*-

;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, mouse

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Select several disjoint pieces of text at the same time, then copy,
;; kill, replace or run a command on all of them.  In "I love ClaudeAI
;; very much" you can have "love" and "much" selected, and nothing else.
;;
;; Setup:
;;
;;   (require 'multi-region)
;;   (global-multi-region-mode 1)     ; or `multi-region-mode' per buffer
;;
;; Mouse:
;;
;;   C-drag           add the dragged text to the selections
;;   C-double-click   add the word under the pointer
;;   C-click          deselect the selection under the pointer, or
;;                    select the picture under it
;;   M-drag           in a PDF, select a picture area (M-click deselects)
;;
;; A region made the normal way (plain drag, or C-SPC and motion) is kept
;; as a selection as soon as you start a C-drag, so "drag, then C-drag,
;; then C-drag" works as it does in a word processor.
;;
;; Keyboard (prefix C-c m):
;;
;;   C-c m SPC   select the active region; without one, select the
;;               picture or word at point, or deselect the selection at
;;               point
;;   C-c m w     copy all selections        (M-w does this too)
;;   C-c m k     kill all selections        (C-w does this too)
;;   C-c m r     replace every selection with a string
;;   C-c m x     run a region command on every selection
;;   C-c m n/p   jump to the next/previous selection
;;   C-c m u     deselect the most recent selection
;;   C-c m c     deselect everything
;;   C-c m /     bring back what was last deselected all at once
;;
;; M-w and C-w only change behavior while selections exist.  Copied
;; selections are joined with `multi-region-separator'.
;;
;; Works in any text buffer (text, Org, Markdown, code, ...) and in
;; read-only ones such as eww.  On a link in eww or an EPUB, C-drag
;; selects; a C-click that selects nothing still opens the link in a
;; new window, as it does without this mode.  Two viewers get extra
;; support:
;;
;; - nov.el (EPUB): selections are remembered per chapter and put back
;;   when you return, and copying collects them from all chapters.
;;
;; - pdf-tools: a PDF page is an image, so there is no point or mark and
;;   selecting is mouse only.  pdf-tools can already hold several
;;   regions on one page (drag, then C-drag); this package gives them
;;   the same commands as above (copy, deselect last, deselect all),
;;   remembers them per page and copies across pages.  Kill, replace,
;;   run-command and next/previous do not apply to PDFs.  Both current
;;   pdf-tools, which keep the region as (PAGE . EDGES), and older ones,
;;   which keep a plain list of edges, are supported.  pdf-tools 1.3
;;   forgets the regions already on a page when C-drag adds one; with
;;   this mode on, C-drag keeps them.  A click without a drag, with or
;;   without Control, keeps the selections too; a plain drag starts
;;   over, as it does in pdf-tools.
;;
;; In PDF and EPUB buffers C-g deselects everything, on all pages or
;; chapters.
;;
;; Pictures are selected along with text.  In eww, EPUBs and Org or
;; Markdown buffers showing inline images, a picture is selected whole
;; when a selection covers it, and gets a band in the selection color.
;; In a PDF, M-drag marks a picture area, such as a figure, which is
;; kept apart from the text and drawn as pdf-tools draws rectangles.
;; Copying takes the text only; `multi-region-selections' reports both.
;;
;; Other packages can read the selections with `multi-region-selections'.
;;
;; To use other keys:
;;
;;   (define-key multi-region-mode-map (kbd "C-c m") nil)
;;   (define-key multi-region-mode-map (kbd "C-c s") multi-region-command-map)
;;   (define-key multi-region-mode-map [C-down-mouse-1] nil)
;;   (define-key multi-region-mode-map [M-S-down-mouse-1] #'multi-region-mouse-drag)

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'thingatpt)

(defvar multi-region-mode)

;; Optional integrations.  Neither library is required.
(defvar nov-documents-index)
(defvar pdf-view-active-region)
(defvar pdf-view-selection-style)
(defvar pdf-view--have-rectangle-region)
(declare-function pdf-view-mouse-extend-region "ext:pdf-view")
(declare-function pdf-view-mouse-set-region "ext:pdf-view")
(declare-function pdf-view-deactivate-region "ext:pdf-view")
(declare-function pdf-view-display-region "ext:pdf-view")
(declare-function pdf-info-gettext "ext:pdf-info")
(declare-function pdf-info-renderpage "ext:pdf-info")
(declare-function pdf-util-face-colors "ext:pdf-util")
(declare-function pdf-util-scale-pixel-to-relative "ext:pdf-util")
(declare-function pdf-view-image-size "ext:pdf-view")
(declare-function pdf-view-redisplay "ext:pdf-view")
(declare-function pdf-view-mouse-set-region-rectangle "ext:pdf-view")
(declare-function image-mode-window-get "image-mode")

;;;; Options

(defgroup multi-region nil
  "Select several separate regions of text at once."
  :group 'convenience
  :prefix "multi-region-")

(defface multi-region-face
  '((t :inherit region))
  "Face for text selected with `multi-region-mode'.")

(defcustom multi-region-separator "\n"
  "String put between selections when they are copied or killed."
  :type 'string)

(defcustom multi-region-thing 'word
  "What `multi-region-toggle' selects when the region is not active.
This can be any symbol that `bounds-of-thing-at-point' accepts."
  :type '(choice (const word) (const symbol) (const sexp)
                 (const sentence) (const line) symbol))

(defcustom multi-region-keep-after-copy nil
  "Non-nil means selections stay selected after they are copied."
  :type 'boolean)

;;;; State

(defvar-local multi-region--overlays nil
  "Selection overlays of this buffer, most recently added first.")

(defvar-local multi-region--nov-stash nil
  "Selections in EPUB chapters other than the one rendered now.
An alist of (CHAPTER . ENTRIES), each entry being (BEG END TEXT
IMAGES), IMAGES as `multi-region--images' returns them.")

(defvar-local multi-region--pdf-regions nil
  "PDF regions remembered per page, as an alist of (PAGE . REGIONS).
The entry for the page on display may be stale; the live value for
that page is `pdf-view-active-region'.")

(defvar-local multi-region--pdf-timer nil
  "Timer that redraws restored PDF regions.")

(defvar-local multi-region--pdf-shown nil
  "The window and page image last drawn by `multi-region--pdf-show'.")

(defvar-local multi-region--pdf-paged nil
  "Non-nil once a region of the form (PAGE . EDGES) was seen here.")

(defvar-local multi-region--pdf-rects nil
  "Picture areas selected in the PDF, most recent first.
Each is (PAGE . EDGES), EDGES being (LEFT TOP RIGHT BOTTOM) in
relative coordinates.  They are kept apart from
`pdf-view-active-region', which holds text.")

(defvar-local multi-region--pdf-last-rect nil
  "The entry of `multi-region--pdf-rects' added last.
It is nil once text was selected after it.")

(defun multi-region--pdf-p ()
  "Return non-nil if the current buffer is a pdf-tools buffer."
  (derived-mode-p 'pdf-view-mode))

(defun multi-region--nov-p ()
  "Return non-nil if the current buffer is a nov.el buffer."
  (derived-mode-p 'nov-mode))

;;;; Selections in text buffers

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

(defun multi-region--absorb-region ()
  "Turn the active region, if there is one, into a selection."
  (when (and (not (multi-region--pdf-p)) (use-region-p))
    ;; `region-bounds' also covers rectangles, one piece per line.
    (let ((bounds (region-bounds)))
      (deactivate-mark)
      (dolist (b bounds)
        (multi-region--add (car b) (cdr b))))))

(defun multi-region--text-texts ()
  "Return the selected strings of a text buffer, in reading order."
  (let ((live (mapcar (lambda (ov)
                        (filter-buffer-substring (overlay-start ov)
                                                 (overlay-end ov)))
                      (multi-region--sorted))))
    (if (and (multi-region--nov-p) multi-region--nov-stash)
        (let ((chapters
               (cons (cons nov-documents-index live)
                     (mapcar (lambda (chapter)
                               (cons (car chapter)
                                     (mapcar (lambda (entry) (nth 2 entry))
                                             (cdr chapter))))
                             multi-region--nov-stash))))
          (apply #'append
                 (mapcar #'cdr
                         (sort chapters
                               (lambda (a b) (< (car a) (car b)))))))
      live)))

;;;; Pictures in text buffers

;; A picture in a buffer is text with an image as its `display'
;; property, or under an overlay that has one: eww, EPUBs and Org and
;; Markdown inline images all work that way.  Selecting text around a
;; picture selects the picture too, and `multi-region-selections'
;; reports it.

(defcustom multi-region-image-margin 4
  "Width in pixels of the band drawn around selected pictures.
The band has the background color of `multi-region-face'.  With 0,
selected pictures look the same as unselected ones."
  :type 'integer)

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

;;;; nov.el: keep selections across chapters

(defun multi-region--nov-save ()
  "Move the selections of the rendered chapter into the stash."
  (dolist (ov (reverse (multi-region--sorted)))
    (let ((chapter (overlay-get ov 'multi-region-chapter))
          (beg (overlay-start ov))
          (end (overlay-end ov)))
      (when chapter
        (push (list beg end (buffer-substring beg end)
                    (multi-region--images beg end))
              (alist-get chapter multi-region--nov-stash)))
      (delete-overlay ov)))
  (setq multi-region--overlays nil)
  (multi-region--decorate))

(defun multi-region--flexible-regexp (text)
  "Return a regexp matching TEXT with any whitespace between its words."
  (mapconcat #'regexp-quote
             (split-string text "[ \t\n\r]+" t)
             "[ \t\n\r]+"))

(defun multi-region--nov-restore ()
  "Reselect the stashed selections of the chapter just rendered.
The chapter may have been filled to a different width since, so a
selection whose text is no longer at its old position is searched
for instead."
  (let ((entries (alist-get nov-documents-index multi-region--nov-stash))
        (case-fold-search nil))
    (when entries
      (setq multi-region--nov-stash
            (assq-delete-all nov-documents-index multi-region--nov-stash))
      (dolist (entry entries)
        (let ((beg (nth 0 entry))
              (end (nth 1 entry))
              (text (nth 2 entry)))
          (if (and (<= end (point-max))
                   (string= (buffer-substring-no-properties beg end) text))
              (multi-region--add beg end)
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward (multi-region--flexible-regexp text)
                                       nil t)
                (multi-region--add (match-beginning 0) (match-end 0))))))))))

(defun multi-region--nov-around-render (orig-fun &rest args)
  "Carry selections over a chapter rendering done by ORIG-FUN.
ARGS are passed on unchanged.  This is advice for
`nov-render-document'."
  (if (not multi-region-mode)
      (apply orig-fun args)
    (multi-region--nov-save)
    (prog1 (apply orig-fun args)
      (multi-region--nov-restore))))

;;;; pdf-tools: several regions per page, kept across pages

(defun multi-region--pdf-page ()
  "Return the PDF page shown in the selected window."
  (image-mode-window-get 'page))

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
    (if (and (consp region) (numberp (car region)))
        (cdr region)
      region)))

(defun multi-region--pdf-live-page ()
  "Return the page of the active PDF region.
That is the page in front of the region in current pdf-tools, or else
the page on display."
  (let ((region (bound-and-true-p pdf-view-active-region)))
    (if (numberp (car-safe region))
        (car region)
      (multi-region--pdf-page))))

(defun multi-region--pdf-stash (page edges)
  "Remember EDGES as the regions of PAGE, which is not the live one."
  (setq multi-region--pdf-regions
        (assq-delete-all page multi-region--pdf-regions))
  (when edges
    (push (cons page (copy-sequence edges)) multi-region--pdf-regions)))

(defun multi-region--pdf-merge (region page old)
  "Return REGION with the edges OLD put back in front.
REGION has the form of `pdf-view-active-region'.  OLD are put back
only if REGION is (PAGE . EDGES) for the same PAGE; a plain list of
edges, as older pdf-tools keep, already holds them."
  (if (and old (numberp (car-safe region)) (eql (car region) page))
      (cons page (append old (seq-remove (lambda (e) (member e old))
                                         (cdr region))))
    region))

;; While the mouse is dragged, pdf-tools draws the page anew at every
;; step, which is slow on big pages.  A step is skipped when the mouse
;; has already moved on, and the last one is drawn after the drag.

(defun multi-region--pdf-rects-on (page)
  "Return the picture areas selected on PAGE, in the order they were added."
  (let (rects)
    (dolist (rect multi-region--pdf-rects rects)
      (when (eql (car rect) page)
        (push (cdr rect) rects)))))

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

(defun multi-region--pdf-track (command event page old &optional quiet rect)
  "Call pdf-tools' mouse COMMAND with EVENT, drawing only what is needed.
OLD are edges on PAGE to keep showing and put back afterwards, as
`multi-region--pdf-merge' does.  With QUIET, the regions on the page
are not drawn away when COMMAND deselects them first.  With RECT,
COMMAND makes a picture area: it is drawn as one, next to OLD, and
not merged with them; the caller sorts that out."
  (let ((display (symbol-function 'pdf-view-display-region))
        (deactivate (symbol-function 'pdf-view-deactivate-region))
        skipped)
    (cl-letf (((symbol-function 'pdf-view-display-region)
               (lambda (&optional region &rest _)
                 (if (input-pending-p)
                     (setq skipped t)
                   (let* ((region (if rect region
                                    (multi-region--pdf-merge region page old)))
                          (paged (numberp (car-safe region)))
                          (pg (if paged (car region) (multi-region--pdf-page)))
                          (edges (if paged (cdr region) region)))
                     (if rect
                         (multi-region--pdf-draw-edges
                          pg (and (eql pg page) old)
                          (append (multi-region--pdf-rects-on pg)
                                  (list (car edges)))
                          display)
                       (multi-region--pdf-draw-edges
                        pg edges (multi-region--pdf-rects-on pg) display))))))
              ((symbol-function 'pdf-view-deactivate-region)
               (if quiet
                   (lambda () (setq pdf-view-active-region nil))
                 deactivate)))
      ;; Text is highlighted as text; picture areas are kept apart.
      (setq pdf-view--have-rectangle-region nil)
      (funcall command event)
      (setq pdf-view--have-rectangle-region nil))
    (unless rect
      (when (and old (eql (car-safe pdf-view-active-region) page))
        (setq pdf-view-active-region
              (multi-region--pdf-merge pdf-view-active-region page old)))
      (when (and skipped (multi-region--pdf-edges))
        (multi-region--pdf-draw)))))

(defun multi-region--pdf-mouse-drag (event)
  "Add the region dragged over with EVENT to the PDF page.
pdf-tools 1.3 and later start a new region list on every drag, even
the C-drag that is meant to add to it, so the regions already there
are put back: in what is drawn during the drag, and in the region
afterwards."
  (let* ((page (and (multi-region--pdf-paged-p)
                    (car-safe pdf-view-active-region)))
         (old (and page (copy-sequence (multi-region--pdf-edges)))))
    (multi-region--pdf-track #'pdf-view-mouse-extend-region event page old)
    (when (multi-region--pdf-edges)
      (setq multi-region--pdf-last-rect nil))
    (let ((region (bound-and-true-p pdf-view-active-region)))
      ;; Dragged on another page, as pdf-view-roll-minor-mode allows.
      (when (and old (numberp (car-safe region)) (not (eql (car region) page)))
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
      (let (mark-ring)
        (push-mark-command nil t))
      (setq deactivate-mark nil)))
    (force-mode-line-update)))

(defun multi-region--pdf-event-edges (event)
  "Return the point on the PDF page of mouse EVENT, as relative edges.
The result is (X Y X Y), or nil if EVENT is not on the page."
  (let ((posn (event-start event)))
    (when (posn-image posn)
      (let ((xy (posn-object-x-y posn)))
        (pdf-util-scale-pixel-to-relative
         (list (car xy) (cdr xy) (car xy) (cdr xy)))))))

(defun multi-region--pdf-rect-at (page edges)
  "Return the entry of the picture area on PAGE around the point EDGES."
  (seq-find (lambda (rect)
              (and (eql (car rect) page)
                   (<= (nth 0 (cdr rect)) (nth 0 edges) (nth 2 (cdr rect)))
                   (<= (nth 1 (cdr rect)) (nth 1 edges) (nth 3 (cdr rect)))))
            multi-region--pdf-rects))

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
    (let* ((region (bound-and-true-p pdf-view-active-region))
           (paged (numberp (car-safe region)))
           (pg (if paged (car region) (multi-region--pdf-page)))
           (rect (car (last (if paged (cdr region) region)))))
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
          (progn
            (let (mark-ring)
              (push-mark-command nil t))
            (setq deactivate-mark nil))
        (deactivate-mark))
      (multi-region--pdf-draw-edges pg (and (eql pg page) old)
                                    (multi-region--pdf-rects-on pg))
      (force-mode-line-update))))

(defun multi-region-keyboard-quit ()
  "Deselect everything, on all PDF pages or EPUB chapters, then quit.
\\[keyboard-quit] runs this in PDF and EPUB buffers."
  (interactive)
  (multi-region-clear)
  (keyboard-quit))

(define-obsolete-function-alias 'multi-region-pdf-keyboard-quit
  #'multi-region-keyboard-quit "0.2.2")

(defun multi-region--pdf-set (page edges)
  "Make EDGES, a list of edges on PAGE, the active PDF region."
  (setq pdf-view-active-region
        (and edges (if (multi-region--pdf-paged-p) (cons page edges) edges))))

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
      (when (and (or pdf-view-active-region
                     (multi-region--pdf-rects-on (multi-region--pdf-page)))
                 (not (equal multi-region--pdf-shown
                             (cons window (image-mode-window-get 'image window)))))
        (multi-region--pdf-draw)
        (setq multi-region--pdf-shown
              (cons window (image-mode-window-get 'image window)))))))

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
               (null pdf-view-active-region))
      (when regions
        (multi-region--pdf-set page (copy-sequence regions))
        ;; As pdf-tools does for its own selections, activate the mark so
        ;; that `C-g' deselects.
        (let (mark-ring)
          (push-mark-command nil t))
        (setq deactivate-mark nil))
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

;;;; Either kind of buffer

(defun multi-region--texts ()
  "Return all selected strings of the current buffer."
  (if (multi-region--pdf-p)
      (multi-region--pdf-texts)
    (multi-region--text-texts)))

(defun multi-region--count ()
  "Return the number of selections in the current buffer."
  (if (multi-region--pdf-p)
      (let ((page (multi-region--pdf-live-page))
            (n (+ (length (multi-region--pdf-edges))
                  (length multi-region--pdf-rects))))
        (dolist (entry multi-region--pdf-regions n)
          (unless (eq (car entry) page)
            (setq n (+ n (length (cdr entry)))))))
    (let ((n (length (multi-region--live))))
      (dolist (chapter multi-region--nov-stash n)
        (setq n (+ n (length (cdr chapter))))))))

(defun multi-region--any-p ()
  "Return non-nil if the current buffer has selections."
  (> (multi-region--count) 0))

(defun multi-region--assert-text ()
  "Signal an error in buffers that have no text to move point over."
  (when (multi-region--pdf-p)
    (user-error "Not available in PDF buffers; select with the mouse")))

(defun multi-region--reset ()
  "Drop all selection state this package keeps in the current buffer."
  (mapc #'delete-overlay (multi-region--live))
  (setq multi-region--overlays nil
        multi-region--nov-stash nil
        multi-region--pdf-regions nil
        multi-region--pdf-rects nil
        multi-region--pdf-last-rect nil)
  (multi-region--decorate)
  (force-mode-line-update))

(defun multi-region--call-original (command)
  "Run what COMMAND would do if `multi-region-mode' were off."
  (let ((original (let ((multi-region-mode nil))
                    (or (command-remapping command) command))))
    (setq this-command original)
    (call-interactively original)))

;;;; For other packages

(defun multi-region--with-images (selection images)
  "Return SELECTION, a plist, with :images IMAGES added if there are any."
  (if images
      (append selection (list :images images))
    selection))

(defun multi-region--text-selections ()
  "Return the selections of a text buffer as plists, in reading order."
  (let ((nov (multi-region--nov-p))
        (live (mapcar (lambda (ov)
                        (let ((beg (overlay-start ov))
                              (end (overlay-end ov)))
                          (multi-region--with-images
                           (list :beg beg :end end
                                 :text (buffer-substring-no-properties beg end))
                           (multi-region--images beg end))))
                      (multi-region--sorted))))
    (if (not nov)
        live
      (let ((all (mapcar (lambda (s) (append s (list :chapter nov-documents-index)))
                         live)))
        (dolist (chapter multi-region--nov-stash)
          (dolist (entry (cdr chapter))
            (push (multi-region--with-images
                   (list :beg (nth 0 entry) :end (nth 1 entry)
                         :text (substring-no-properties (nth 2 entry))
                         :chapter (car chapter))
                   (nth 3 entry))
                  all)))
        (sort all (lambda (a b)
                    (let ((ca (plist-get a :chapter)) (cb (plist-get b :chapter)))
                      (if (/= ca cb) (< ca cb)
                        (< (plist-get a :beg) (plist-get b :beg))))))))))

(defun multi-region-selections (&optional absorb)
  "Return the selections of the current buffer, in reading order.
With ABSORB non-nil, an active region first becomes a selection.

In a text buffer each selection is a plist (:beg BEG :end END :text
TEXT).  In a nov.el buffer it also has :chapter, the chapter index; for
a selection in a chapter that is not displayed, BEG and END are where
it was when it was selected.  A selection that holds pictures also has
:images, a list of (:beg BEG :end END :file FILE), or (:beg BEG :end
END :data DATA :type TYPE) for a picture that is not in a file.

In a PDF each selection is a plist (:page PAGE :edges EDGES :text
TEXT), EDGES being one region as a list (LEFT TOP RIGHT BOTTOM) of
relative coordinates.  A picture area selected with M-drag also has
:figure t, and empty TEXT.

This is the function for other packages to read the selections; it
does not change them.  Use `multi-region-clear' to deselect."
  (when absorb (multi-region--absorb-region))
  (if (multi-region--pdf-p)
      (multi-region--pdf-selections)
    (multi-region--text-selections)))

;;;; Commands

(defun multi-region-toggle ()
  "Select or deselect text at point.
With an active region, select it and deactivate the mark.  Without
one, deselect the selection point is in; if point is not in one,
select the picture at point, or else the `multi-region-thing' at
point, a word by default."
  (interactive)
  (multi-region--assert-text)
  (let (old)
    (cond
     ((use-region-p)
      (multi-region--absorb-region))
     ((setq old (multi-region--at (point)))
      (multi-region--delete old))
     (t
      (let ((bounds (or (multi-region--image-bounds (point))
                        (bounds-of-thing-at-point multi-region-thing))))
        (unless bounds
          (user-error "Nothing to select here"))
        (multi-region--add (car bounds) (cdr bounds)))))))

(defvar-local multi-region--cleared nil
  "What `multi-region-clear' last deselected, for `multi-region-restore'.
In a text buffer a list of (BEG . END) marker pairs, with the stashed
EPUB chapters as (:nov STASH); in a PDF (:pdf REGIONS :rects RECTS),
REGIONS being an alist of (PAGE . EDGES) that includes the page on
display and RECTS the picture areas.")

(defun multi-region--snapshot ()
  "Return the selections of this buffer in the form of `multi-region--cleared'."
  (cond
   ((multi-region--pdf-p)
    (let ((page (multi-region--pdf-live-page))
          (edges (multi-region--pdf-edges)))
      (list :pdf (append (and edges (list (cons page (copy-sequence edges))))
                         (seq-remove (lambda (entry) (eql (car entry) page))
                                     (copy-tree multi-region--pdf-regions)))
            :rects (copy-tree multi-region--pdf-rects))))
   (t
    (let ((nov (multi-region--nov-p)))
      (if nov
          ;; Chapters are rendered anew, so keep text, not markers.
          (let ((stash (copy-tree multi-region--nov-stash)))
            (dolist (ov (reverse (multi-region--sorted)))
              (let ((beg (overlay-start ov)) (end (overlay-end ov)))
                (push (list beg end (buffer-substring-no-properties beg end)
                            (multi-region--images beg end))
                      (alist-get nov-documents-index stash))))
            (list :nov stash))
        (mapcar (lambda (ov)
                  (cons (copy-marker (overlay-start ov))
                        (copy-marker (overlay-end ov))))
                (multi-region--sorted)))))))

(defun multi-region-clear ()
  "Deselect everything in the current buffer.
\\[multi-region-restore] brings it back."
  (interactive)
  (let ((rects multi-region--pdf-rects))
    (when (multi-region--any-p)
      (setq multi-region--cleared (multi-region--snapshot)))
    (multi-region--reset)
    (when (multi-region--pdf-p)
      (cond ((bound-and-true-p pdf-view-active-region)
             (pdf-view-deactivate-region))
            (rects (pdf-view-redisplay t))))))

(defun multi-region-restore ()
  "Select again what was deselected all at once, last time.
That is everything that \\[multi-region-clear], \\[keyboard-quit] in a
PDF or EPUB, or copying cleared.  Selections made since are kept."
  (interactive)
  (let ((saved multi-region--cleared)
        (before (multi-region--count)))
    (unless saved
      (user-error "Nothing to bring back"))
    (cond
     ((eq (car saved) :pdf)
      (unless (multi-region--pdf-p)
        (user-error "Nothing to bring back"))
      (let ((page (multi-region--pdf-live-page)))
        (dolist (entry (cadr saved))
          (let* ((live (if (eql (car entry) page)
                           (multi-region--pdf-edges)
                         (cdr (assq (car entry) multi-region--pdf-regions))))
                 (edges (append live (seq-remove (lambda (e) (member e live))
                                                 (cdr entry)))))
            (if (eql (car entry) page)
                (multi-region--pdf-set page edges)
              (multi-region--pdf-stash (car entry) edges))))
        (dolist (rect (reverse (plist-get saved :rects)))
          (unless (member rect multi-region--pdf-rects)
            (push rect multi-region--pdf-rects)))
        (when (multi-region--pdf-edges)
          (let (mark-ring)
            (push-mark-command nil t))
          (setq deactivate-mark nil))
        (when (or (multi-region--pdf-edges)
                  (multi-region--pdf-rects-on page))
          (multi-region--pdf-draw))))
     ((eq (car saved) :nov)
      (unless (multi-region--nov-p)
        (user-error "Nothing to bring back"))
      (dolist (chapter (cadr saved))
        (setf (alist-get (car chapter) multi-region--nov-stash)
              (append (alist-get (car chapter) multi-region--nov-stash)
                      (cdr chapter))))
      ;; Selects the entries of the chapter on display.
      (multi-region--nov-restore))
     (t
      (dolist (pair saved)
        (when (eq (marker-buffer (car pair)) (current-buffer))
          (multi-region--add (car pair) (cdr pair))))))
    (setq multi-region--cleared nil)
    (force-mode-line-update)
    (message "Brought back %d selection%s"
             (- (multi-region--count) before)
             (if (= (- (multi-region--count) before) 1) "" "s"))))

(defun multi-region-remove-last ()
  "Deselect the most recently added selection.
In a PDF this is the last text region or picture area added on the
page on display."
  (interactive)
  (if (multi-region--pdf-p)
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
          (pdf-view-deactivate-region))))
    (let ((ov (car (multi-region--live))))
      (unless ov
        (user-error "No selections"))
      (multi-region--delete ov))))

(defun multi-region-copy (&optional separator)
  "Copy all selections to the kill ring as one string.
The selections are joined with `multi-region-separator', or with
SEPARATOR, which is read from the minibuffer when a prefix argument
is given.  An active region counts as one more selection.

In EPUB and PDF buffers this includes selections in other chapters
and on other pages.  Afterwards everything is deselected unless
`multi-region-keep-after-copy' is non-nil."
  (interactive
   (list (and current-prefix-arg (read-string "Separator: "))))
  (multi-region--absorb-region)
  (let ((texts (multi-region--texts)))
    (unless texts
      (user-error "No selections"))
    (kill-new (mapconcat #'identity texts
                         (or separator multi-region-separator)))
    (unless multi-region-keep-after-copy
      (multi-region-clear))
    (message "Copied %d selection%s"
             (length texts) (if (cdr texts) "s" ""))))

(defun multi-region-kill ()
  "Kill all selections: copy them as one string, then delete them.
An active region counts as one more selection."
  (interactive)
  (multi-region--assert-text)
  (multi-region--absorb-region)
  (let ((ovs (multi-region--sorted)))
    (unless ovs
      (user-error "No selections"))
    (kill-new (mapconcat #'identity (multi-region--texts)
                         multi-region-separator))
    ;; As with `kill-region', read-only text is copied before the error.
    (barf-if-buffer-read-only)
    (dolist (ov (reverse ovs))
      (delete-region (overlay-start ov) (overlay-end ov)))
    (multi-region--reset)))

(defun multi-region-replace (string)
  "Replace the text of every selection with STRING."
  (interactive
   (progn
     (multi-region--assert-text)
     (barf-if-buffer-read-only)
     (list (read-string "Replace selections with: "))))
  (multi-region--absorb-region)
  (let ((ovs (multi-region--sorted)))
    (unless ovs
      (user-error "No selections"))
    (save-excursion
      (dolist (ov (reverse ovs))
        (let ((beg (overlay-start ov))
              (end (overlay-end ov)))
          (goto-char beg)
          (delete-region beg end)
          (insert string))))
    (multi-region--reset)))

(defun multi-region-do (command)
  "Run COMMAND once on every selection, as if it were the region.
COMMAND is called interactively with point at the start and the
mark at the end of each selection, last selection first.  For
example `upcase-region', `comment-region' or `fill-region'."
  (interactive (list (read-command "Command to run on each selection: ")))
  (multi-region--assert-text)
  (multi-region--absorb-region)
  (let ((ovs (reverse (multi-region--sorted))))
    (unless ovs
      (user-error "No selections"))
    (save-mark-and-excursion
      (dolist (ov ovs)
        (when (overlay-buffer ov)
          (goto-char (overlay-start ov))
          (set-mark (overlay-end ov))
          (call-interactively command))))))

(defun multi-region-next ()
  "Move point to the next selection."
  (interactive)
  (multi-region--assert-text)
  (let ((ov (seq-find (lambda (o) (> (overlay-start o) (point)))
                      (multi-region--sorted))))
    (unless ov
      (user-error "No selection after point"))
    (goto-char (overlay-start ov))))

(defun multi-region-previous ()
  "Move point to the previous selection."
  (interactive)
  (multi-region--assert-text)
  (let ((ov (seq-find (lambda (o) (< (overlay-start o) (point)))
                      (reverse (multi-region--sorted)))))
    (unless ov
      (user-error "No selection before point"))
    (goto-char (overlay-start ov))))

(defun multi-region-kill-ring-save ()
  "Copy all selections; without any, do what \\[kill-ring-save] does."
  (interactive)
  (if (multi-region--any-p)
      (call-interactively #'multi-region-copy)
    (multi-region--call-original 'kill-ring-save)))

(defun multi-region-kill-region ()
  "Kill all selections; without any, do what \\[kill-region] does.
PDF selections cannot be deleted, so there they are copied."
  (interactive)
  (cond
   ((not (multi-region--any-p))
    (multi-region--call-original 'kill-region))
   ((multi-region--pdf-p)
    (call-interactively #'multi-region-copy))
   (t
    (call-interactively #'multi-region-kill))))

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

(defun multi-region-mouse-drag (event)
  "Add the text dragged over with the mouse to the selections.
EVENT is the button press that starts the drag.  A double click
selects a word and a triple click a line.  A single click without
dragging deselects the selection under the pointer.  If the region
is active when the drag starts, it is kept as a selection too.

In a PDF buffer this adds a region to the page, as
`pdf-view-mouse-extend-region' is meant to do."
  (interactive "@e")
  (if (multi-region--pdf-p)
      (multi-region--pdf-mouse-drag event)
    (multi-region--absorb-region)
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
              (funcall command event))))))))

;;;; Modes

(defvar multi-region-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'multi-region-toggle)
    (define-key map "w" #'multi-region-copy)
    (define-key map "k" #'multi-region-kill)
    (define-key map "r" #'multi-region-replace)
    (define-key map "x" #'multi-region-do)
    (define-key map "n" #'multi-region-next)
    (define-key map "p" #'multi-region-previous)
    (define-key map "u" #'multi-region-remove-last)
    (define-key map "c" #'multi-region-clear)
    (define-key map "/" #'multi-region-restore)
    map)
  "Keymap of `multi-region-mode' commands, bound to a prefix key.")

(defvar multi-region-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [C-down-mouse-1] #'multi-region-mouse-drag)
    ;; Links in eww and EPUBs bind C-down-mouse-1 themselves.
    (define-key map [remap shr-mouse-browse-url-new-window]
      #'multi-region-mouse-drag)
    ;; In PDFs only: a plain click there would deselect everything.
    (define-key map [down-mouse-1]
      `(menu-item "" multi-region-pdf-mouse-set-region
                  :filter ,(lambda (cmd) (and (multi-region--pdf-p) cmd))))
    ;; In PDFs only: M-drag selects a picture area.
    (define-key map [M-down-mouse-1]
      `(menu-item "" multi-region-pdf-mouse-rectangle
                  :filter ,(lambda (cmd) (and (multi-region--pdf-p) cmd))))
    ;; In PDFs and EPUBs only: C-g deselects on all pages or chapters.
    (define-key map [remap keyboard-quit]
      `(menu-item "" multi-region-keyboard-quit
                  :filter ,(lambda (cmd)
                             (and (or (multi-region--pdf-p) (multi-region--nov-p))
                                  cmd))))
    (define-key map (kbd "C-c m") multi-region-command-map)
    (define-key map [remap kill-ring-save] #'multi-region-kill-ring-save)
    (define-key map [remap kill-region] #'multi-region-kill-region)
    map)
  "Keymap of `multi-region-mode'.")

(defun multi-region--lighter ()
  "Return the mode line text of `multi-region-mode'."
  (let ((n (or (ignore-errors (multi-region--count)) 0)))
    (if (> n 0)
        (format " MR[%d]" n)
      " MR")))

;;;###autoload
(define-minor-mode multi-region-mode
  "Select several separate regions of text at the same time.

Drag with the Control key held to add a selection, or use
\\[multi-region-toggle] on the active region or the word at point.
\\[multi-region-copy] copies all selections and \\[multi-region-clear]
deselects them.

\\{multi-region-mode-map}"
  :lighter (:eval (multi-region--lighter))
  (cond
   (multi-region-mode
    (add-hook 'change-major-mode-hook #'multi-region--reset nil t)
    (cond
     ((multi-region--pdf-p)
      (add-hook 'pdf-view-before-change-page-hook
                #'multi-region--pdf-save nil t)
      (add-hook 'pdf-view-after-change-page-hook
                #'multi-region--pdf-restore nil t)
      (add-hook 'deactivate-mark-hook
                #'multi-region--pdf-after-deactivate t t))
     ((multi-region--nov-p)
      ;; Does nothing in buffers where the mode is off.
      (advice-add 'nov-render-document :around
                  #'multi-region--nov-around-render))))
   (t
    (remove-hook 'change-major-mode-hook #'multi-region--reset t)
    (remove-hook 'pdf-view-before-change-page-hook
                 #'multi-region--pdf-save t)
    (remove-hook 'pdf-view-after-change-page-hook
                 #'multi-region--pdf-restore t)
    (remove-hook 'deactivate-mark-hook
                 #'multi-region--pdf-after-deactivate t)
    (multi-region--reset))))

(defun multi-region--turn-on ()
  "Turn on `multi-region-mode', except in the minibuffer."
  (unless (minibufferp)
    (multi-region-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-multi-region-mode
  multi-region-mode multi-region--turn-on
  :group 'multi-region)

(provide 'multi-region)
;;; multi-region.el ends here
