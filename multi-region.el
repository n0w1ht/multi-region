;;; multi-region.el --- Select several separate regions at once  -*- lexical-binding: t; -*-

;; Version: 0.2.0
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
;;   C-click          deselect the selection under the pointer
;;
;; A region made the normal way (plain drag, or C-SPC and motion) is kept
;; as a selection as soon as you start a C-drag, so "drag, then C-drag,
;; then C-drag" works as it does in a word processor.
;;
;; Keyboard (prefix C-c m):
;;
;;   C-c m SPC   select the active region; without one, select the word
;;               at point, or deselect the selection at point
;;   C-c m w     copy all selections        (M-w does this too)
;;   C-c m k     kill all selections        (C-w does this too)
;;   C-c m r     replace every selection with a string
;;   C-c m x     run a region command on every selection
;;   C-c m n/p   jump to the next/previous selection
;;   C-c m u     deselect the most recent selection
;;   C-c m c     deselect everything
;;
;; M-w and C-w only change behavior while selections exist.  Copied
;; selections are joined with `multi-region-separator'.
;;
;; Works in any text buffer (text, Org, Markdown, code, ...) and in
;; read-only ones.  Two viewers get extra support:
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
;;   which keep a plain list of edges, are supported.
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

(require 'seq)
(require 'thingatpt)

(defvar multi-region-mode)

;; Optional integrations.  Neither library is required.
(defvar nov-documents-index)
(defvar pdf-view-active-region)
(defvar pdf-view-selection-style)
(declare-function pdf-view-mouse-extend-region "ext:pdf-view")
(declare-function pdf-view-deactivate-region "ext:pdf-view")
(declare-function pdf-view-display-region "ext:pdf-view")
(declare-function pdf-info-gettext "ext:pdf-info")
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
An alist of (CHAPTER . ENTRIES), each entry being (BEG END TEXT).")

(defvar-local multi-region--pdf-regions nil
  "PDF regions remembered per page, as an alist of (PAGE . REGIONS).
The entry for the page on display may be stale; the live value for
that page is `pdf-view-active-region'.")

(defvar-local multi-region--pdf-timer nil
  "Timer that redraws restored PDF regions.")

(defvar-local multi-region--pdf-paged nil
  "Non-nil once a region of the form (PAGE . EDGES) was seen here.")

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
      (force-mode-line-update)
      ov)))

(defun multi-region--delete (ov)
  "Deselect the selection overlay OV."
  (delete-overlay ov)
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

;;;; nov.el: keep selections across chapters

(defun multi-region--nov-save ()
  "Move the selections of the rendered chapter into the stash."
  (dolist (ov (reverse (multi-region--sorted)))
    (let ((chapter (overlay-get ov 'multi-region-chapter))
          (beg (overlay-start ov))
          (end (overlay-end ov)))
      (when chapter
        (push (list beg end (buffer-substring beg end))
              (alist-get chapter multi-region--nov-stash)))
      (delete-overlay ov)))
  (setq multi-region--overlays nil))

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
      (setq multi-region--pdf-regions
            (assq-delete-all page multi-region--pdf-regions))
      (when (multi-region--pdf-edges)
        (push (cons page (copy-sequence (multi-region--pdf-edges)))
              multi-region--pdf-regions)))))

(defun multi-region--pdf-show (buffer window)
  "Draw the active PDF regions of BUFFER in WINDOW."
  (when (and (buffer-live-p buffer)
             (window-live-p window)
             (eq (window-buffer window) buffer))
    (with-selected-window window
      (when pdf-view-active-region
        (pdf-view-display-region)))))

(defun multi-region--pdf-restore ()
  "Reselect the regions remembered for the page just arrived at.
This goes on `pdf-view-after-change-page-hook'."
  (let* ((page (multi-region--pdf-page))
         (regions (cdr (assq page multi-region--pdf-regions))))
    (when (and regions (null pdf-view-active-region))
      (multi-region--pdf-set page (copy-sequence regions))
      ;; As pdf-tools does for its own selections, activate the mark so
      ;; that `C-g' deselects.
      (let (mark-ring)
        (push-mark-command nil t))
      (setq deactivate-mark nil)
      ;; Page-turning commands redraw the plain page once more after
      ;; this hook has run, so draw the highlight when Emacs is idle.
      (when (timerp multi-region--pdf-timer)
        (cancel-timer multi-region--pdf-timer))
      (setq multi-region--pdf-timer
            (run-with-idle-timer 0.1 nil #'multi-region--pdf-show
                                 (current-buffer) (selected-window))))))

(defun multi-region--pdf-selections ()
  "Return the selections of a PDF, page by page.
Within a page they come in the order they were selected.  Each is a
plist (:page PAGE :edges EDGES :text TEXT)."
  (let* ((edges (multi-region--pdf-edges))
         ;; The region knows its page in current pdf-tools.
         (page (and (or edges multi-region--pdf-regions)
                    (if (numberp (car-safe (bound-and-true-p pdf-view-active-region)))
                        (car pdf-view-active-region)
                      (multi-region--pdf-page))))
         (pages (and page
                     (cons (cons page edges)
                           (seq-remove (lambda (entry) (eq (car entry) page))
                                       multi-region--pdf-regions))))
         (style (bound-and-true-p pdf-view-selection-style))
         selections)
    (dolist (entry (sort pages (lambda (a b) (< (car a) (car b)))))
      (dolist (edges (cdr entry))
        (push (list :page (car entry) :edges edges
                    :text (pdf-info-gettext (car entry) edges style))
              selections)))
    (nreverse selections)))

(defun multi-region--pdf-texts ()
  "Return the selected strings of a PDF, page by page."
  (mapcar (lambda (s) (plist-get s :text)) (multi-region--pdf-selections)))

;;;; Either kind of buffer

(defun multi-region--texts ()
  "Return all selected strings of the current buffer."
  (if (multi-region--pdf-p)
      (multi-region--pdf-texts)
    (multi-region--text-texts)))

(defun multi-region--count ()
  "Return the number of selections in the current buffer."
  (if (multi-region--pdf-p)
      (let ((page (multi-region--pdf-page))
            (n (length (multi-region--pdf-edges))))
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
        multi-region--pdf-regions nil)
  (force-mode-line-update))

(defun multi-region--call-original (command)
  "Run what COMMAND would do if `multi-region-mode' were off."
  (let ((original (let ((multi-region-mode nil))
                    (or (command-remapping command) command))))
    (setq this-command original)
    (call-interactively original)))

;;;; For other packages

(defun multi-region--text-selections ()
  "Return the selections of a text buffer as plists, in reading order."
  (let ((nov (multi-region--nov-p))
        (live (mapcar (lambda (ov)
                        (let ((beg (overlay-start ov))
                              (end (overlay-end ov)))
                          (list :beg beg :end end
                                :text (buffer-substring-no-properties beg end))))
                      (multi-region--sorted))))
    (if (not nov)
        live
      (let ((all (mapcar (lambda (s) (append s (list :chapter nov-documents-index)))
                         live)))
        (dolist (chapter multi-region--nov-stash)
          (dolist (entry (cdr chapter))
            (push (list :beg (nth 0 entry) :end (nth 1 entry)
                        :text (substring-no-properties (nth 2 entry))
                        :chapter (car chapter))
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
it was when it was selected.  In a PDF each selection is a plist
\(:page PAGE :edges EDGES :text TEXT), EDGES being one region as a
list (LEFT TOP RIGHT BOTTOM) of relative coordinates.

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
select the `multi-region-thing' at point, a word by default."
  (interactive)
  (multi-region--assert-text)
  (let (old)
    (cond
     ((use-region-p)
      (multi-region--absorb-region))
     ((setq old (multi-region--at (point)))
      (multi-region--delete old))
     (t
      (let ((bounds (bounds-of-thing-at-point multi-region-thing)))
        (unless bounds
          (user-error "Nothing to select here"))
        (multi-region--add (car bounds) (cdr bounds)))))))

(defun multi-region-clear ()
  "Deselect everything in the current buffer."
  (interactive)
  (multi-region--reset)
  (when (and (multi-region--pdf-p) (bound-and-true-p pdf-view-active-region))
    (pdf-view-deactivate-region)))

(defun multi-region-remove-last ()
  "Deselect the most recently added selection.
In a PDF this is the last region added on the page on display."
  (interactive)
  (if (multi-region--pdf-p)
      (let ((edges (multi-region--pdf-edges)))
        (cond
         ((null edges)
          (user-error "No selection on this page"))
         ((cdr edges)
          (multi-region--pdf-set (multi-region--pdf-page) (butlast edges))
          (pdf-view-display-region)
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
the selection under it instead."
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
                (multi-region--add beg end)
              (let ((old (multi-region--at origin)))
                (when old
                  (multi-region--delete old))))))
      (delete-overlay ov))))

(defun multi-region-mouse-drag (event)
  "Add the text dragged over with the mouse to the selections.
EVENT is the button press that starts the drag.  A double click
selects a word and a triple click a line.  A single click without
dragging deselects the selection under the pointer.  If the region
is active when the drag starts, it is kept as a selection too.

In a PDF buffer this adds a region to the page, as
`pdf-view-mouse-extend-region' does."
  (interactive "@e")
  (if (multi-region--pdf-p)
      (pdf-view-mouse-extend-region event)
    (multi-region--absorb-region)
    (let* ((posn (event-start event))
           (window (posn-window posn))
           (origin (posn-point posn)))
      (when (and (windowp window)
                 (null (posn-area posn))
                 (integerp origin))
        (multi-region--track-drag window origin
                                  (1- (event-click-count event)))))))

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
    map)
  "Keymap of `multi-region-mode' commands, bound to a prefix key.")

(defvar multi-region-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [C-down-mouse-1] #'multi-region-mouse-drag)
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
                #'multi-region--pdf-restore nil t))
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
