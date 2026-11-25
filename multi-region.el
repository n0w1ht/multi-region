;;; multi-region.el --- Select several separate regions at once  -*- lexical-binding: t; -*-

;; Version: 0.6.0
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
;; Keyboard (prefix C-c M, which `multi-region-prefix-key' changes):
;;
;;   C-c M SPC   select the active region; without one, select the
;;               picture or word at point, or deselect the selection at
;;               point
;;   C-c M w     copy all selections        (M-w does this too)
;;   C-c M k     kill all selections        (C-w does this too)
;;   C-c M r     replace every selection with a string
;;   C-c M x     run a region command on every selection
;;   C-c M n/p   jump to the next/previous selection
;;   C-c M u     deselect the most recent selection
;;   C-c M c     deselect everything
;;   C-c M /     bring back what was last deselected all at once;
;;               again for the set before, C-u to pick from a list
;;   C-c M h     open the manual (also C-h R multi-region)
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
;; - pdf-canvas: that viewer holds several selections itself, on any
;;   number of pages (drag, then C-drag; M-drag for a picture area).
;;   This package gives them the commands above and reports them to
;;   other packages through `multi-region-selections', in the same form
;;   as the selections of a pdf-tools buffer.
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
;;   (setopt multi-region-prefix-key "C-c s")  ; or setq before loading
;;   (define-key multi-region-mode-map [C-down-mouse-1] nil)
;;   (define-key multi-region-mode-map [M-S-down-mouse-1] #'multi-region-mouse-drag)

;;
;; The code is split by viewer:
;;
;;   multi-region.el            commands, keys and the mode
;;   multi-region-text.el       text buffers, and pictures in them
;;   multi-region-epub.el       nov.el: selections across chapters
;;   multi-region-pdf.el        pdf-tools: selections across pages
;;   multi-region-pdf-tools.el  everything that relies on pdf-tools internals
;;   multi-region-canvas.el     pdf-canvas

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'thingatpt)
(require 'multi-region-text)
(require 'multi-region-epub)
(require 'multi-region-pdf)
(require 'multi-region-canvas)

(defvar multi-region-mode)
(declare-function pdf-canvas-selections "ext:pdf-canvas")
(declare-function pdf-canvas-selection-count "ext:pdf-canvas")
(declare-function pdf-canvas-clear-selections "ext:pdf-canvas")
(declare-function pdf-canvas-mouse-add "ext:pdf-canvas")
(declare-function pdf-canvas-quit "ext:pdf-canvas")

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

(defcustom multi-region-restore-history 5
  "How many deselected sets of selections each buffer remembers.
\\[multi-region-restore] brings back the newest; pressed again right
away, the one before.  Each clear, copy, kill or replace that
deselects something adds one."
  :type 'natnum)

;;;; The manual

(defconst multi-region--doc-directory
  (expand-file-name "doc" (file-name-directory (or load-file-name buffer-file-name "")))
  "The folder of multi-region's Info manual.")

(declare-function info-initialize "info")
(defvar Info-directory-list)

(defun multi-region--add-manual ()
  "Let Info find multi-region's manual, so that C-h R multi-region shows it."
  (when (file-exists-p (expand-file-name "multi-region.info"
                                         multi-region--doc-directory))
    (info-initialize)
    (add-to-list 'Info-directory-list multi-region--doc-directory t)))

(with-eval-after-load 'info (multi-region--add-manual))

(defun multi-region-manual ()
  "Show multi-region's manual."
  (interactive)
  (require 'info)
  (multi-region--add-manual)
  (info "multi-region"))

;;;; Any kind of buffer

(defun multi-region--viewer ()
  "Return what shows the current buffer: `canvas', `pdf', `epub' or `text'."
  (cond ((multi-region--canvas-p) 'canvas)
        ((multi-region--pdf-p) 'pdf)
        ((multi-region--nov-p) 'epub)
        (t 'text)))

(defun multi-region--paged-p ()
  "Return non-nil if the current buffer shows a PDF, in either viewer.
Such a buffer has no text to move point over."
  (memq (multi-region--viewer) '(canvas pdf)))

(defun multi-region--join-hyphens (text)
  "TEXT with the words PDFs hyphenate with a ¬ joined again.
\"inflam¬ mation\" becomes \"inflammation\", also over a line break."
  (replace-regexp-in-string "\\([^ \t\n\r]\\)¬[ \t\r\n]*" "\\1" text))

(defun multi-region--texts ()
  "Return all selected strings of the current buffer."
  (mapcar #'multi-region--join-hyphens
          (pcase (multi-region--viewer)
            ('canvas (multi-region--canvas-texts))
            ('pdf (multi-region--pdf-texts))
            ('epub (multi-region--nov-texts))
            (_ (multi-region--text-texts)))))

(defun multi-region--count ()
  "Return the number of selections in the current buffer."
  (pcase (multi-region--viewer)
    ('canvas (pdf-canvas-selection-count))
    ('pdf (multi-region--pdf-count))
    ('epub (multi-region--nov-count))
    (_ (multi-region--text-count))))

(defun multi-region--any-p ()
  "Return non-nil if the current buffer has selections."
  (> (multi-region--count) 0))

(defun multi-region--assert-text ()
  "Signal an error in buffers that have no text to move point over."
  (when (multi-region--paged-p)
    (user-error "Not available in PDF buffers; select with the mouse")))

(defun multi-region--absorb-region ()
  "Turn the active region, if there is one, into a selection."
  (when (and (not (multi-region--paged-p)) (use-region-p))
    ;; `region-bounds' also covers rectangles, one piece per line.
    (let ((bounds (region-bounds)))
      (deactivate-mark)
      (dolist (b bounds)
        (multi-region--add (car b) (cdr b))))))

(defun multi-region--reset ()
  "Drop all selection state this package keeps in the current buffer."
  (multi-region--text-reset)
  (multi-region--nov-reset)
  (multi-region--pdf-reset)
  (force-mode-line-update))

(defun multi-region--call-original (command)
  "Run what COMMAND would do if `multi-region-mode' were off."
  (let ((original (let ((multi-region-mode nil))
                    (or (command-remapping command) command))))
    (setq this-command original)
    (call-interactively original)))

;;;; For other packages

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
:figure t, and empty TEXT.  In a pdf-canvas buffer EDGES is the box
around the selected text, and the plist has further keys, see
`pdf-canvas-selections'.

In TEXT, a word hyphenated with a ¬, as PDFs often have, is joined
again (\"inflam¬ mation\" is \"inflammation\").

This is the function for other packages to read the selections; it
does not change them.  Use `multi-region-clear' to deselect."
  (when absorb (multi-region--absorb-region))
  (mapcar (lambda (s)
            (plist-put s :text (multi-region--join-hyphens (plist-get s :text))))
          (pcase (multi-region--viewer)
            ('canvas (multi-region--canvas-selections))
            ('pdf (multi-region--pdf-selections))
            ('epub (multi-region--nov-selections))
            (_ (multi-region--text-selections)))))

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

(defvar-local multi-region--history nil
  "What was deselected all at once, newest first, for `multi-region-restore'.
Each entry is a plist:

:viewer      what showed the buffer, as `multi-region--viewer' says
:selections  the selections, in the form that viewer keeps them
:count       how many there were
:reason      what deselected them, such as \"copy\" or \"C-g\"
:preview     the start of their text, to tell entries apart
:time        when, as `float-time' returns it")

(defvar-local multi-region--restore-chain nil
  "Where a run of \\[multi-region-restore] presses stands.
A plist (:base BASE :current ENTRY :index N): BASE is the entry of
what was selected before the run started, or nil; ENTRY is the
history entry on display, taken out of `multi-region--history'; N is
where it was there.")

(defun multi-region--snapshot ()
  "Return the selections of this buffer as (VIEWER . SELECTIONS)."
  (let ((viewer (multi-region--viewer)))
    (cons viewer
          (pcase viewer
            ('canvas (pdf-canvas-selections))
            ('pdf (multi-region--pdf-snapshot))
            ('epub (multi-region--nov-snapshot))
            (_ (multi-region--text-snapshot))))))

(defun multi-region--preview (texts)
  "Return a short line showing the start of TEXTS, a list of strings."
  (truncate-string-to-width
   (replace-regexp-in-string "[ \t\n\r]+" " "
                             (mapconcat #'identity texts " | "))
   60 nil nil "…"))

(defun multi-region--remember (snapshot reason &optional texts)
  "Add SNAPSHOT, from `multi-region--snapshot', to the history.
REASON says what deselected it.  TEXTS are its strings, if known."
  (let ((texts (or texts (ignore-errors (multi-region--texts)))))
    (push (list :viewer (car snapshot)
                :selections (cdr snapshot)
                :count (multi-region--count)
                :reason reason
                :preview (multi-region--preview texts)
                :time (float-time))
          multi-region--history))
  (setq multi-region--restore-chain nil)
  (let ((keep (max 1 multi-region-restore-history)))
    (when (> (length multi-region--history) keep)
      (setcdr (nthcdr (1- keep) multi-region--history) nil))))

(defun multi-region--clear-now ()
  "Deselect everything in the current buffer, remembering nothing."
  (pcase (multi-region--viewer)
    ('canvas (multi-region--reset)
             (pdf-canvas-clear-selections))
    ('pdf (multi-region--pdf-clear))
    (_ (multi-region--reset))))

(defun multi-region--clear (reason)
  "Deselect everything, remembering it under REASON if anything was selected."
  (when (multi-region--any-p)
    (multi-region--remember (multi-region--snapshot) reason))
  (multi-region--clear-now))

(defun multi-region-clear ()
  "Deselect everything in the current buffer.
\\[multi-region-restore] brings it back."
  (interactive)
  (multi-region--clear "clear"))

(defun multi-region--bring-back (viewer selections)
  "Select again SELECTIONS, kept the way VIEWER keeps them."
  (pcase viewer
    ('canvas (multi-region--canvas-bring-back selections))
    ('pdf (multi-region--pdf-bring-back selections))
    ('epub (multi-region--nov-bring-back selections))
    (_ (multi-region--text-bring-back selections))))

(defun multi-region--ago (time)
  "Return how long ago TIME was, in words."
  (let ((s (- (float-time) time)))
    (cond ((< s 60) "just now")
          ((< s 3600) (format "%d min ago" (/ s 60)))
          ((< s 86400) (format "%d h ago" (/ s 3600)))
          (t (format "%d days ago" (/ s 86400))))))

(defun multi-region--describe (entry)
  "Return a line describing the history ENTRY."
  (let ((n (plist-get entry :count)))
    (format "%d selection%s, %s, %s: %s"
            n (if (= n 1) "" "s")
            (plist-get entry :reason)
            (multi-region--ago (plist-get entry :time))
            (plist-get entry :preview))))

(defun multi-region--choose-entry (entries)
  "Ask for one of the history ENTRIES and return its index."
  (let* ((i 0)
         (choices (mapcar (lambda (e)
                            (setq i (1+ i))
                            (cons (format "%d. %s" i (multi-region--describe e))
                                  (1- i)))
                          entries))
         (choice (completing-read "Bring back: "
                                  (lambda (string pred action)
                                    (if (eq action 'metadata)
                                        '(metadata (display-sort-function . identity))
                                      (complete-with-action
                                       action choices string pred)))
                                  nil t)))
    (cdr (assoc choice choices))))

(defun multi-region-restore (&optional choose)
  "Select again what was last deselected all at once.
That is everything that \\[multi-region-clear], \\[keyboard-quit] in a
PDF or EPUB, or copying cleared.  After \\[multi-region-replace], it
selects the replacements; after \\[multi-region-kill], once \\[undo]
has put the text back, that text.  Selections made since are kept.

Pressed again right away (with `repeat-mode' on, / is enough), it brings
back the set deselected before that one instead, and so on, as far as
`multi-region-restore-history' goes.  With a prefix argument CHOOSE,
pick the set from a list."
  (interactive "P")
  (let* ((viewer (multi-region--viewer))
         (chain (and (eq last-command 'multi-region-restore)
                     (not choose)
                     multi-region--restore-chain))
         (history multi-region--history)
         (index (cond (choose
                       (unless history (user-error "Nothing to bring back"))
                       (multi-region--choose-entry history))
                      (chain (plist-get chain :index))
                      (t 0)))
         (entry (nth index history))
         base)
    (unless (and entry (eq (plist-get entry :viewer) viewer))
      (user-error (if chain "No older selections to bring back"
                    "Nothing to bring back")))
    (setq multi-region--history (delq entry multi-region--history))
    (if chain
        (progn
          ;; Put back the set brought back last, where it was, and
          ;; show this one instead, next to what was selected before.
          (setq base (plist-get chain :base))
          (let ((current (plist-get chain :current)))
            (if (= index 0)
                (push current multi-region--history)
              (let ((cell (nthcdr (1- index) multi-region--history)))
                (setcdr cell (cons current (cdr cell))))))
          (multi-region--clear-now)
          (when base
            (multi-region--bring-back viewer (cdr base))))
      (when (multi-region--any-p)
        (setq base (multi-region--snapshot))))
    (let ((before (multi-region--count)))
      (multi-region--bring-back viewer (plist-get entry :selections))
      ;; The next press takes the entry after this one.  It is at
      ;; INDEX now that this one is out, or one further on after
      ;; the entry shown before was put back in front of it.
      (let ((next (if chain (1+ index) index)))
        (setq multi-region--restore-chain
              (list :base base :current entry :index next)))
      (force-mode-line-update)
      (let ((n (- (multi-region--count) before))
            (older (nth (plist-get multi-region--restore-chain :index)
                        multi-region--history)))
        (message "Brought back %d selection%s (%s)%s"
                 n (if (= n 1) "" "s")
                 (plist-get entry :reason)
                 (if older "; again for older ones" ""))))))

(defvar multi-region-restore-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "/" #'multi-region-restore)
    map)
  "Keys that repeat `multi-region-restore' under `repeat-mode'.")
(put 'multi-region-restore 'repeat-map 'multi-region-restore-repeat-map)

(defun multi-region-remove-last ()
  "Deselect the most recently added selection.
In a pdf-tools buffer this is the last text region or picture area
added on the page on display."
  (interactive)
  (pcase (multi-region--viewer)
    ('canvas (multi-region--canvas-remove-last))
    ('pdf (multi-region--pdf-remove-last))
    (_ (multi-region--text-remove-last))))

(defun multi-region-keyboard-quit ()
  "Deselect everything, on all PDF pages or EPUB chapters, then quit.
\\[keyboard-quit] runs this in PDF and EPUB buffers."
  (interactive)
  (multi-region--clear "C-g")
  (if (multi-region--canvas-p)
      ;; Also removes the search highlights, then quits.
      (pdf-canvas-quit)
    (keyboard-quit)))

(define-obsolete-function-alias 'multi-region-pdf-keyboard-quit
  #'multi-region-keyboard-quit "0.2.2")

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
      (multi-region--clear "copy"))
    (message "Copied %d selection%s"
             (length texts) (if (cdr texts) "s" ""))))

(defun multi-region-kill ()
  "Kill all selections: copy them as one string, then delete them.
An active region counts as one more selection.  After \\[undo] puts
the text back, \\[multi-region-restore] selects it again."
  (interactive)
  (multi-region--assert-text)
  (multi-region--absorb-region)
  (let ((ovs (multi-region--sorted)))
    (unless ovs
      (user-error "No selections"))
    (let ((texts (multi-region--texts)))
      (kill-new (mapconcat #'identity texts multi-region-separator))
      ;; As with `kill-region', read-only text is copied before the error.
      (barf-if-buffer-read-only)
      (multi-region--remember (cons 'text (multi-region--text-snapshot t))
                              "kill" texts))
    (dolist (ov (reverse ovs))
      (delete-region (overlay-start ov) (overlay-end ov)))
    (multi-region--reset)))

(defun multi-region-replace (string)
  "Replace the text of every selection with STRING.
Afterwards \\[multi-region-restore] selects the replacements; after
\\[undo], the text they replaced."
  (interactive
   (progn
     (multi-region--assert-text)
     (barf-if-buffer-read-only)
     (list (read-string "Replace selections with: "))))
  (multi-region--absorb-region)
  (let ((ovs (multi-region--sorted)))
    (unless ovs
      (user-error "No selections"))
    (multi-region--remember (cons 'text (multi-region--text-snapshot t))
                            "replace")
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
   ((multi-region--paged-p)
    (call-interactively #'multi-region-copy))
   (t
    (call-interactively #'multi-region-kill))))

;;;; Mouse

(defun multi-region-mouse-drag (event)
  "Add the text dragged over with the mouse to the selections.
EVENT is the button press that starts the drag.  A double click
selects a word and a triple click a line.  A single click without
dragging deselects the selection under the pointer.  If the region
is active when the drag starts, it is kept as a selection too.

In a PDF buffer this adds a region to the page, as
`pdf-view-mouse-extend-region' is meant to do."
  (interactive "@e")
  (pcase (multi-region--viewer)
    ;; pdf-canvas adds selections itself; a click deselects there too.
    ('canvas (pdf-canvas-mouse-add event))
    ('pdf (multi-region--pdf-mouse-drag event))
    (_ (multi-region--absorb-region)
       (multi-region--text-mouse-drag event))))

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
    (define-key map "h" #'multi-region-manual)
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
    ;; pdf-canvas binds C-g to its own command, which deselects without
    ;; remembering what was selected.
    (define-key map [remap pdf-canvas-quit]
      `(menu-item "" multi-region-keyboard-quit
                  :filter ,(lambda (cmd) (and (multi-region--canvas-p) cmd))))
    (define-key map [remap kill-ring-save] #'multi-region-kill-ring-save)
    (define-key map [remap kill-region] #'multi-region-kill-region)
    map)
  "Keymap of `multi-region-mode'.")

(defun multi-region--set-prefix-key (symbol key)
  "Set SYMBOL to KEY and make KEY the prefix of `multi-region-command-map'."
  (let ((old (and (boundp symbol) (symbol-value symbol))))
    (when old
      (define-key multi-region-mode-map (kbd old) nil)))
  (set-default symbol key)
  (when key
    (define-key multi-region-mode-map (kbd key) multi-region-command-map)))

(defcustom multi-region-prefix-key "C-c M"
  "Key that leads to the commands of `multi-region-mode'.
A key as `kbd' reads it, such as \"C-c M\" or \"C-c s\", or nil
for none.  The commands are in `multi-region-command-map'.

Set it with Customize or `setopt', or with `setq' before
multi-region is loaded; a `setq' later has no effect."
  :type '(choice (string :tag "Key") (const :tag "None" nil))
  :set #'multi-region--set-prefix-key
  :group 'multi-region)

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
    (pcase (multi-region--viewer)
      ('pdf (multi-region--pdf-setup))
      ('epub (multi-region--nov-setup))))
   (t
    (remove-hook 'change-major-mode-hook #'multi-region--reset t)
    (multi-region--pdf-teardown)
    (multi-region--nov-teardown)
    (multi-region--reset))))

(defcustom multi-region-ignored-modes
  '(dired-mode magit-section-mode ibuffer-mode
    vterm-mode eat-mode term-mode eshell-mode)
  "Major modes in which `global-multi-region-mode' leaves the mode off.
Modes derived from them count too.  In these buffers C-drag and
\\[kill-ring-save] keep doing what the major mode makes them do.
`multi-region-mode' can still be turned on by hand there."
  :type '(repeat symbol)
  :group 'multi-region)

(defun multi-region--turn-on ()
  "Turn on `multi-region-mode', except where it does not belong.
That is the minibuffer and buffers in `multi-region-ignored-modes'."
  (unless (or (minibufferp)
              (apply #'derived-mode-p multi-region-ignored-modes))
    (multi-region-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-multi-region-mode
  multi-region-mode multi-region--turn-on
  :group 'multi-region)

(provide 'multi-region)
;;; multi-region.el ends here
