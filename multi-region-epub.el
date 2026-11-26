;;; multi-region-epub.el --- multi-region in EPUBs read with nov.el  -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; nov.el shows one chapter of an EPUB at a time, rendering it anew in
;; the same buffer.  The chapter on display has its selections as in
;; any text buffer (see multi-region-text.el); the selections of the
;; other chapters are kept here, as text, and put back when their
;; chapter is shown again.  Copying collects them from all chapters.
;;
;; Part of multi-region; load `multi-region', not this file.

;;; Code:

(require 'seq)
(require 'multi-region-text)

(defvar multi-region-mode)
(defvar nov-documents-index)

(defvar-local multi-region--nov-stash nil
  "Selections in EPUB chapters other than the one rendered now.
An alist of (CHAPTER . ENTRIES), each entry being (BEG END TEXT
IMAGES), IMAGES as `multi-region--images' returns them.")

(defun multi-region--nov-p ()
  "Return non-nil if the current buffer is a nov.el buffer."
  (derived-mode-p 'nov-mode))

;;;; Keeping selections across chapters

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

(defun multi-region--nearest-match (regexp pos)
  "Return (BEG . END) of the match of REGEXP that starts nearest POS.
Matches that start in a selection are passed over, so that two
selections of the same words do not land on the same place.  Return
nil if there is no such match."
  (let (best)
    (unless (equal regexp "")
      (save-excursion
        (goto-char (point-min))
        (while (and (re-search-forward regexp nil t)
                    ;; Further matches are only further away.
                    (not (and best
                              (> (match-beginning 0) pos)
                              (>= (- (match-beginning 0) pos)
                                  (abs (- (car best) pos))))))
          (let ((beg (match-beginning 0)))
            (unless (seq-some (lambda (ov) (overlay-get ov 'multi-region))
                              (overlays-at beg))
              (when (or (null best)
                        (< (abs (- beg pos)) (abs (- (car best) pos))))
                (setq best (cons beg (match-end 0)))))))))
    best))

(defun multi-region--nov-restore ()
  "Reselect the stashed selections of the chapter just rendered.
The chapter may have been filled to a different width since, so a
selection whose text is no longer at its old position is searched
for instead, nearest that position."
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
            (let ((match (multi-region--nearest-match
                          (multi-region--flexible-regexp text) beg)))
              (when match
                (multi-region--add (car match) (cdr match))))))))))

(defun multi-region--nov-around-render (orig-fun &rest args)
  "Carry selections over a chapter rendering done by ORIG-FUN.
ARGS are passed on unchanged.  This is advice for
`nov-render-document'."
  (if (not multi-region-mode)
      (apply orig-fun args)
    (multi-region--nov-save)
    (prog1 (apply orig-fun args)
      (multi-region--nov-restore))))

(defun multi-region--nov-setup ()
  "Start keeping selections across chapters."
  ;; Does nothing in buffers where the mode is off.
  (advice-add 'nov-render-document :around
              #'multi-region--nov-around-render))

(defun multi-region--nov-teardown ()
  "Stop keeping selections across chapters, once no EPUB buffer needs it.
The advice is shared by all buffers; it goes when the mode is off in
every nov.el buffer."
  (unless (seq-some (lambda (buffer)
                      (and (not (eq buffer (current-buffer)))
                           (buffer-local-value 'multi-region-mode buffer)
                           (with-current-buffer buffer
                             (multi-region--nov-p))))
                    (buffer-list))
    (advice-remove 'nov-render-document #'multi-region--nov-around-render)))

;;;; Reading the selections of all chapters

(defun multi-region--nov-texts ()
  "Return the selected strings of all chapters, in reading order."
  (let ((chapters
         (cons (cons nov-documents-index (multi-region--text-texts))
               (mapcar (lambda (chapter)
                         (cons (car chapter)
                               (mapcar (lambda (entry) (nth 2 entry))
                                       (cdr chapter))))
                       multi-region--nov-stash))))
    (apply #'append
           (mapcar #'cdr
                   (sort chapters (lambda (a b) (< (car a) (car b))))))))

(defun multi-region--nov-selections ()
  "Return the selections of all chapters as plists, in reading order.
Each has :chapter, the chapter index, besides what a selection in a
text buffer has."
  (let ((all (mapcar (lambda (s) (append s (list :chapter nov-documents-index)))
                     (multi-region--text-selections))))
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
                    (< (plist-get a :beg) (plist-get b :beg))))))))

(defun multi-region--nov-count ()
  "Return the number of selections in all chapters."
  (let ((n (multi-region--text-count)))
    (dolist (chapter multi-region--nov-stash n)
      (setq n (+ n (length (cdr chapter)))))))

;;;; Deselecting and bringing back

(defun multi-region--nov-snapshot ()
  "Return the selections of all chapters as a stash.
Chapters are rendered anew, so it keeps text, not markers."
  (let ((stash (copy-tree multi-region--nov-stash)))
    (dolist (ov (reverse (multi-region--sorted)))
      (let ((beg (overlay-start ov)) (end (overlay-end ov)))
        (push (list beg end (buffer-substring-no-properties beg end)
                    (multi-region--images beg end))
              (alist-get nov-documents-index stash))))
    stash))

(defun multi-region--nov-bring-back (stash)
  "Select again the selections in STASH, a snapshot of all chapters."
  (dolist (chapter stash)
    (setf (alist-get (car chapter) multi-region--nov-stash)
          (append (alist-get (car chapter) multi-region--nov-stash)
                  (cdr chapter))))
  ;; Selects the entries of the chapter on display.
  (multi-region--nov-restore))

(defun multi-region--nov-reset ()
  "Forget the selections of the chapters not on display."
  (setq multi-region--nov-stash nil))

(provide 'multi-region-epub)
;;; multi-region-epub.el ends here
