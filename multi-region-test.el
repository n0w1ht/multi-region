;;; multi-region-test.el --- Tests for multi-region -*- lexical-binding: t; -*-

;; Run with:
;;   emacs -Q --batch -L . -l multi-region-test.el -f ert-run-tests-batch-and-exit
;;
;; pdf-tools and nov.el are not needed: their parts are imitated.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'multi-region)

;;;; Text buffers

(ert-deftest multi-region-test-selections-in-text ()
  (with-temp-buffer
    (insert "I love ClaudeAI very much")
    (multi-region-mode 1)
    (multi-region--add 22 26)            ; "much"
    (multi-region--add 3 7)              ; "love"
    (should (equal (multi-region-selections)
                   '((:beg 3 :end 7 :text "love") (:beg 22 :end 26 :text "much"))))
    ;; With ABSORB, the active region becomes a selection too.
    (let ((transient-mark-mode t))
      (goto-char 8) (set-mark 16) (activate-mark)
      (should (equal (mapcar (lambda (s) (plist-get s :text))
                             (multi-region-selections t))
                     '("love" "ClaudeAI" "much")))
      (should-not (use-region-p)))
    ;; Reading the selections does not change them.
    (should (= (multi-region--count) 3))
    (multi-region-clear)
    (should-not (multi-region-selections))))

(ert-deftest multi-region-test-selections-in-epub ()
  (with-temp-buffer
    (insert "Chapter two text with a selection.")
    (multi-region-mode 1)
    (defvar nov-documents-index)
    (let ((nov-documents-index 2))
      (cl-letf (((symbol-function 'multi-region--nov-p) (lambda () t)))
        (multi-region--add 25 34)
        (setq multi-region--nov-stash
              (list (cons 1 (list (list 5 9 (propertize "kept" 'face 'bold))))
                    (cons 3 (list (list 1 4 "end")))))
        (should (equal (multi-region-selections)
                       '((:beg 5 :end 9 :text "kept" :chapter 1)
                         (:beg 25 :end 34 :text "selection" :chapter 2)
                         (:beg 1 :end 4 :text "end" :chapter 3))))
        (should-not (text-properties-at 0 (plist-get (car (multi-region-selections))
                                                     :text)))))))

;;;; Bringing back cleared selections

(ert-deftest multi-region-test-restore-in-text ()
  (with-temp-buffer
    (insert "I love ClaudeAI very much")
    (multi-region-mode 1)
    (should-error (multi-region-restore) :type 'user-error)
    (multi-region--add 3 7)              ; "love"
    (multi-region--add 22 26)            ; "much"
    (multi-region-clear)
    (should (= (multi-region--count) 0))
    ;; Edits before a selection move it, as markers do.
    (goto-char (point-min))
    (insert ">> ")
    (multi-region--add 11 19)            ; "ClaudeAI", selected since
    (multi-region-restore)
    (should (equal (mapcar (lambda (s) (plist-get s :text)) (multi-region-selections))
                   '("love" "ClaudeAI" "much")))
    ;; Only once.
    (should-error (multi-region-restore) :type 'user-error)
    ;; Clearing with nothing selected keeps the last snapshot.
    (multi-region-clear)
    (multi-region-clear)
    (multi-region-restore)
    (should (= (multi-region--count) 3))
    ;; Copying clears too, and can be undone the same way.
    (let ((kill-ring nil))
      (multi-region-copy))
    (should (= (multi-region--count) 0))
    (multi-region-restore)
    (should (= (multi-region--count) 3))))

(ert-deftest multi-region-test-restore-in-epub ()
  (with-temp-buffer
    (insert "Chapter two text with a selection.")
    (multi-region-mode 1)
    (defvar nov-documents-index)
    (let ((nov-documents-index 2))
      (cl-letf (((symbol-function 'multi-region--nov-p) (lambda () t)))
        (multi-region--add 25 34)
        (setq multi-region--nov-stash (list (cons 1 (list (list 5 9 "kept")))))
        (multi-region-clear)
        (should (= (multi-region--count) 0))
        (multi-region-restore)
        (should (equal (multi-region-selections)
                       '((:beg 5 :end 9 :text "kept" :chapter 1)
                         (:beg 25 :end 34 :text "selection" :chapter 2))))))))

;;;; PDF buffers, with both forms of `pdf-view-active-region'

(defvar pdf-view-active-region nil)
(defvar multi-region-test--page 1)

(defmacro multi-region-test--with-pdf (paged &rest body)
  "Run BODY in an imitation pdf-tools buffer.
PAGED non-nil imitates current pdf-tools, where the region is
\(PAGE . EDGES); nil imitates older ones, where it is a list of edges."
  (declare (indent 1))
  `(with-temp-buffer
     (setq-local pdf-view-active-region nil)
     (let ((multi-region-test--page 1)
           (doc (if ,paged "The active region as a cons cell of page and list of edges."
                  "The active region as a list of edges.")))
       (cl-letf (((symbol-function 'multi-region--pdf-p) (lambda () t))
                 ((symbol-function 'image-mode-window-get)
                  (lambda (&rest _) multi-region-test--page))
                 ((symbol-function 'pdf-info-gettext)
                  (lambda (page edges &rest _) (format "p%d:%S" page (car edges))))
                 ((symbol-function 'pdf-view-display-region) #'ignore)
                 ((symbol-function 'pdf-view-redisplay) #'ignore)
                 ((symbol-function 'pdf-util-face-colors)
                  (lambda (&rest _) '("fg" . "bg")))
                 ((symbol-function 'pdf-view-deactivate-region)
                  (lambda () (setq pdf-view-active-region nil)))
                 ((get 'pdf-view-active-region 'variable-documentation) doc))
         (multi-region-mode 1)
         ,@body))))

(defun multi-region-test--select (paged page &rest edges)
  "Make EDGES the active region on PAGE, in the PAGED form or not."
  (setq pdf-view-active-region (if paged (cons page edges) edges)))

(defun multi-region-test--turn-to (page)
  "Imitate pdf-tools turning to PAGE."
  (multi-region--pdf-save)
  (setq pdf-view-active-region nil
        multi-region-test--page page)
  (multi-region--pdf-restore))

(dolist (paged '(t nil))
  (eval
   `(ert-deftest ,(intern (format "multi-region-test-pdf-%s"
                                  (if paged "current-pdf-tools" "older-pdf-tools")))
        ()
      (multi-region-test--with-pdf ,paged
        (multi-region-test--select ,paged 1 '(0.1 0.1 0.5 0.2) '(0.1 0.3 0.5 0.4))
        ;; Two selections, not three: the page number is not one.
        (should (= (multi-region--count) 2))
        (should (equal (multi-region--lighter) " MR[2]"))
        ;; Selections stay with their page when turning pages.
        (multi-region-test--turn-to 3)
        (should-not pdf-view-active-region)
        (multi-region-test--select ,paged 3 '(0.2 0.2 0.6 0.3))
        (should (= (multi-region--count) 3))
        (should (equal (multi-region-selections)
                       '((:page 1 :edges (0.1 0.1 0.5 0.2) :text "p1:0.1")
                         (:page 1 :edges (0.1 0.3 0.5 0.4) :text "p1:0.1")
                         (:page 3 :edges (0.2 0.2 0.6 0.3) :text "p3:0.2"))))
        ;; Coming back restores them in the form pdf-tools uses.
        (multi-region-test--turn-to 1)
        (should (equal pdf-view-active-region
                       ',(if paged '(1 (0.1 0.1 0.5 0.2) (0.1 0.3 0.5 0.4))
                           '((0.1 0.1 0.5 0.2) (0.1 0.3 0.5 0.4)))))
        ;; Copying works across pages.
        (let ((kill-ring nil) (multi-region-keep-after-copy t))
          (multi-region-copy " | ")
          (should (equal (car kill-ring) "p1:0.1 | p1:0.1 | p3:0.2")))
        ;; Removing the last selection keeps the form.
        (multi-region-remove-last)
        (should (equal pdf-view-active-region
                       ',(if paged '(1 (0.1 0.1 0.5 0.2)) '((0.1 0.1 0.5 0.2)))))
        (multi-region-remove-last)
        (should-not pdf-view-active-region)
        (should-error (multi-region-remove-last) :type 'user-error)))))

(defun multi-region-test--fake-extend (paged new)
  "Return an imitation of `pdf-view-mouse-extend-region' adding NEW.
PAGED non-nil imitates pdf-tools 1.3, which starts the region over
at every drag; nil imitates older ones, which add to it.  NEW nil
imitates a click without a drag."
  (lambda (_event)
    (if paged
        (setq pdf-view-active-region (list multi-region-test--page))
      (setq pdf-view-active-region pdf-view-active-region))
    (when new
      (pdf-view-display-region
       (if paged
           (cons multi-region-test--page (cons new (cdr pdf-view-active-region)))
         (cons new pdf-view-active-region)))
      (if paged
          (setcdr pdf-view-active-region
                  (append (cdr pdf-view-active-region) (list new)))
        (setq pdf-view-active-region
              (append pdf-view-active-region (list new)))))))

(dolist (paged '(t nil))
  (eval
   `(ert-deftest ,(intern (format "multi-region-test-pdf-c-drag-%s"
                                  (if paged "current-pdf-tools" "older-pdf-tools")))
        ()
      (multi-region-test--with-pdf ,paged
        (let (drawn)
          (cl-letf (((symbol-function 'pdf-view-display-region)
                     (lambda (&optional region &rest _) (push region drawn))))
            (multi-region-test--select ,paged 1 '(0.1 0.1 0.5 0.2))
            (cl-letf (((symbol-function 'pdf-view-mouse-extend-region)
                       (multi-region-test--fake-extend ,paged '(0.1 0.3 0.5 0.4))))
              (multi-region-mouse-drag nil))
            ;; The earlier region stays, while dragging and after.
            (should (equal (car drawn)
                           ',(if paged '(1 (0.1 0.1 0.5 0.2) (0.1 0.3 0.5 0.4))
                               '((0.1 0.3 0.5 0.4) (0.1 0.1 0.5 0.2)))))
            (should (= (multi-region--count) 2))
            ;; A click without a drag adds nothing and drops nothing.
            (cl-letf (((symbol-function 'pdf-view-mouse-extend-region)
                       (multi-region-test--fake-extend ,paged nil)))
              (multi-region-mouse-drag nil))
            (should (equal pdf-view-active-region
                           ',(if paged '(1 (0.1 0.1 0.5 0.2) (0.1 0.3 0.5 0.4))
                               '((0.1 0.1 0.5 0.2) (0.1 0.3 0.5 0.4)))))))))))

(ert-deftest multi-region-test-pdf-c-drag-other-page ()
  ;; With pdf-view-roll-minor-mode a drag can land on another visible
  ;; page; the regions of the first page are kept for it.
  (multi-region-test--with-pdf t
    (multi-region-test--select t 1 '(0.1 0.1 0.5 0.2))
    (cl-letf (((symbol-function 'pdf-view-mouse-extend-region)
               (lambda (_)
                 (setq pdf-view-active-region (list 2 '(0.2 0.2 0.6 0.3))))))
      (multi-region-mouse-drag nil))
    (should (= (multi-region--count) 2))
    (should (equal (mapcar (lambda (s) (plist-get s :page)) (multi-region-selections))
                   '(1 2)))))

(dolist (paged '(t nil))
  (eval
   `(ert-deftest ,(intern (format "multi-region-test-pdf-click-%s"
                                  (if paged "current-pdf-tools" "older-pdf-tools")))
        ()
      (multi-region-test--with-pdf ,paged
        (let ((transient-mark-mode t) drag)
          ;; Like `pdf-view-mouse-set-region': deselect, then add the
          ;; region dragged over, if any.
          (cl-letf (((symbol-function 'pdf-view-mouse-set-region)
                     (lambda (_event)
                       (setq pdf-view-active-region
                             (and ,paged (list multi-region-test--page)))
                       (when drag
                         (multi-region-test--select ,paged 1 drag)))))
            (multi-region-test--select ,paged 1 '(0.1 0.1 0.5 0.2) '(0.1 0.3 0.5 0.4))
            ;; A click keeps the selections.
            (multi-region-pdf-mouse-set-region nil)
            (should (= (multi-region--count) 2))
            (should (equal (multi-region--pdf-edges)
                           '((0.1 0.1 0.5 0.2) (0.1 0.3 0.5 0.4))))
            ;; A drag starts over.
            (setq drag '(0.2 0.5 0.6 0.6))
            (multi-region-pdf-mouse-set-region nil)
            (should (equal (multi-region--pdf-edges) '((0.2 0.5 0.6 0.6))))))))))

(ert-deftest multi-region-test-plain-click-binding ()
  ;; The plain-click command is only bound in PDF buffers.
  (with-temp-buffer
    (multi-region-mode 1)
    (should-not (eq (key-binding [down-mouse-1]) 'multi-region-pdf-mouse-set-region))
    (cl-letf (((symbol-function 'multi-region--pdf-p) (lambda () t)))
      (should (eq (key-binding [down-mouse-1]) 'multi-region-pdf-mouse-set-region))))
  ;; Likewise C-g, which deselects on all pages there.
  (with-temp-buffer
    (multi-region-mode 1)
    (should (eq (key-binding (kbd "C-g")) 'keyboard-quit))
    (cl-letf (((symbol-function 'multi-region--pdf-p) (lambda () t)))
      (should (eq (key-binding (kbd "C-g")) 'multi-region-keyboard-quit)))))

(ert-deftest multi-region-test-epub-quit-clears-all-chapters ()
  (with-temp-buffer
    (insert "Chapter two text with a selection.")
    (multi-region-mode 1)
    (defvar nov-documents-index)
    (let ((nov-documents-index 2))
      (cl-letf (((symbol-function 'multi-region--nov-p) (lambda () t)))
        (should (eq (key-binding (kbd "C-g")) 'multi-region-keyboard-quit))
        (multi-region--add 25 34)
        (setq multi-region--nov-stash (list (cons 1 (list (list 5 9 "kept")))))
        (should (= (multi-region--count) 2))
        (let (quit)
          (cl-letf (((symbol-function 'keyboard-quit) (lambda () (setq quit t))))
            (multi-region-keyboard-quit))
          (should quit))
        (should (= (multi-region--count) 0))))))

(ert-deftest multi-region-test-pdf-quit-clears-all-pages ()
  (multi-region-test--with-pdf t
    (multi-region-test--select t 1 '(0.1 0.1 0.5 0.2))
    (multi-region-test--turn-to 3)
    (multi-region-test--select t 3 '(0.2 0.2 0.6 0.3))
    (should (= (multi-region--count) 2))
    (let (quit)
      (cl-letf (((symbol-function 'keyboard-quit) (lambda () (setq quit t))))
        (multi-region-keyboard-quit))
      (should quit))
    (should (= (multi-region--count) 0))
    (multi-region-test--turn-to 1)
    (should-not pdf-view-active-region)))

(dolist (paged '(t nil))
  (eval
   `(ert-deftest ,(intern (format "multi-region-test-pdf-restore-%s"
                                  (if paged "current-pdf-tools" "older-pdf-tools")))
        ()
      (multi-region-test--with-pdf ,paged
        (let ((transient-mark-mode t))
          (multi-region-test--select ,paged 1 '(0.1 0.1 0.5 0.2))
          (multi-region-test--turn-to 3)
          (multi-region-test--select ,paged 3 '(0.2 0.2 0.6 0.3))
          (multi-region-clear)
          (should (= (multi-region--count) 0))
          ;; A selection made since on page 3 is kept.
          (multi-region-test--select ,paged 3 '(0.3 0.5 0.6 0.6))
          (multi-region-restore)
          (should (equal (mapcar (lambda (s) (list (plist-get s :page) (plist-get s :edges)))
                                 (multi-region-selections))
                         '((1 (0.1 0.1 0.5 0.2))
                           (3 (0.3 0.5 0.6 0.6))
                           (3 (0.2 0.2 0.6 0.3))))))))))

(ert-deftest multi-region-test-pdf-format-detection ()
  ;; With nothing selected yet, the form is read from pdf-tools' doc string.
  (multi-region-test--with-pdf t
    (should (multi-region--pdf-paged-p))
    (multi-region--pdf-set 4 '((0 0 1 1)))
    (should (equal pdf-view-active-region '(4 (0 0 1 1))))
    (multi-region--pdf-set 4 nil)
    (should-not pdf-view-active-region))
  (multi-region-test--with-pdf nil
    (should-not (multi-region--pdf-paged-p))
    (multi-region--pdf-set 4 '((0 0 1 1)))
    (should (equal pdf-view-active-region '((0 0 1 1))))))

;;;; Pictures

(defun multi-region-test--picture (file)
  "Return an image spec for FILE, as `create-image' would make it."
  (list 'image :type 'png :file file))

(ert-deftest multi-region-test-picture-in-selection ()
  (with-temp-buffer
    (insert "Before * after.")
    (let ((image (multi-region-test--picture "/tmp/fig.png")))
      (put-text-property 8 9 'display image))
    (multi-region-mode 1)
    (multi-region--add 1 16)
    (should (equal (multi-region-selections)
                   '((:beg 1 :end 16 :text "Before * after."
                      :images ((:beg 8 :end 9 :file "/tmp/fig.png"))))))
    ;; Text without a picture reports none.
    (multi-region-clear)
    (multi-region--add 1 7)
    (should (equal (multi-region-selections) '((:beg 1 :end 7 :text "Before"))))
    ;; Text that stops just before the picture leaves it out.
    (multi-region-clear)
    (multi-region--add 3 8)
    (should (equal (multi-region-selections) '((:beg 3 :end 8 :text "fore "))))))

(ert-deftest multi-region-test-sliced-picture ()
  ;; eww cuts big pictures into slices, one character each.
  (with-temp-buffer
    ;; Three rows of two slices.
    (insert "Text **\n**\n** text")
    (let ((image (list 'image :type 'png :data "PNGDATA")))
      (dolist (pos '(6 7 9 10 12 13))
        (put-text-property pos (1+ pos) 'display
                           (list (list 'slice 0 pos 1.0 10) image))))
    (multi-region-mode 1)
    ;; Selecting part of it selects all of it.
    (multi-region--add 8 10)
    (should (equal (multi-region-selections)
                   '((:beg 6 :end 14 :text "**\n**\n**"
                      :images ((:beg 6 :end 14 :data "PNGDATA" :type png))))))
    ;; C-c m SPC on a picture deselects it, and selects it again.
    (goto-char 9)
    (multi-region-toggle)
    (should (= (multi-region--count) 0))
    (multi-region-toggle)
    (should (equal (mapcar (lambda (s) (list (plist-get s :beg) (plist-get s :end)))
                           (multi-region-selections))
                   '((6 14))))))

(ert-deftest multi-region-test-picture-overlay ()
  ;; Org and Markdown show inline images with overlays.
  (with-temp-buffer
    (insert "See [[file:/tmp/a.png]] here")
    (let ((ov (make-overlay 5 24)))
      (overlay-put ov 'display (multi-region-test--picture "/tmp/a.png")))
    (multi-region-mode 1)
    (multi-region--add 1 29)
    (should (equal (plist-get (car (multi-region-selections)) :images)
                   '((:beg 5 :end 24 :file "/tmp/a.png"))))
    ;; The band drawn around a selected picture leaves its source alone.
    (multi-region-clear)
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
      (multi-region--add 1 29)
      (should (= (length multi-region--decor) 1))
      (let ((shown (overlay-get (car multi-region--decor) 'display)))
        (should (equal (plist-get (cdr shown) :margin) multi-region-image-margin))
        (should (equal (plist-get (cdr shown) :file) "/tmp/a.png")))
      (should (equal (plist-get (car (multi-region-selections)) :images)
                     '((:beg 5 :end 24 :file "/tmp/a.png"))))
      (multi-region-clear)
      (should-not multi-region--decor)
      (should-not (seq-filter (lambda (ov) (overlay-get ov 'multi-region-decor))
                              (overlays-in 1 29))))))

(ert-deftest multi-region-test-picture-links ()
  ;; Picture links whose image is not displayed count too, if the file
  ;; is there.
  (let* ((dir (make-temp-file "mr" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "fig.png" dir))
          (with-temp-buffer
            (insert "A [[file:fig.png]] and [[./gone.png]] and [[fig.png][caption]].")
            (setq major-mode 'org-mode)
            (multi-region-mode 1)
            (multi-region--add 1 (point-max))
            (should (equal (mapcar (lambda (i) (plist-get i :file))
                                   (plist-get (car (multi-region-selections)) :images))
                           (list (expand-file-name "fig.png" dir)
                                 (expand-file-name "fig.png" dir)))))
          (with-temp-buffer
            (insert "A ![alt](fig.png \"title\") b ![x](https://e.org/x.png)")
            (setq major-mode 'markdown-mode)
            (multi-region-mode 1)
            (multi-region--add 1 (point-max))
            (should (equal (plist-get (car (multi-region-selections)) :images)
                           (list (list :beg 3 :end 26
                                       :file (expand-file-name "fig.png" dir))))))
          ;; Elsewhere such text is just text.
          (with-temp-buffer
            (insert "A ![alt](fig.png)")
            (multi-region-mode 1)
            (multi-region--add 1 (point-max))
            (should-not (plist-get (car (multi-region-selections)) :images))))
      (delete-directory dir t))))

(ert-deftest multi-region-test-picture-in-epub-stash ()
  (with-temp-buffer
    (insert "Chapter * text.")
    (put-text-property 9 10 'display (multi-region-test--picture "/tmp/e.png"))
    (multi-region-mode 1)
    (defvar nov-documents-index)
    (let ((nov-documents-index 2))
      (cl-letf (((symbol-function 'multi-region--nov-p) (lambda () t)))
        (multi-region--add 1 16)
        ;; Leaving the chapter keeps the picture with the selection.
        (multi-region--nov-save)
        (setq nov-documents-index 3)
        (should (equal (multi-region-selections)
                       '((:beg 1 :end 16 :text "Chapter * text." :chapter 2
                          :images ((:beg 9 :end 10 :file "/tmp/e.png"))))))))))

(ert-deftest multi-region-test-click-selects-picture ()
  (with-temp-buffer
    (insert "Text * text")
    (put-text-property 6 7 'display (multi-region-test--picture "/tmp/c.png"))
    (multi-region-mode 1)
    ;; A click on a picture, with no drag: the button is released at once.
    (cl-letf (((symbol-function 'read-event) (lambda (&rest _) '(C-mouse-1)))
              ((symbol-function 'window-edges) (lambda (&rest _) '(0 0 80 25))))
      (should (multi-region--track-drag (selected-window) 6 0))
      (should (equal (mapcar (lambda (s) (list (plist-get s :beg) (plist-get s :end)))
                             (multi-region-selections))
                     '((6 7))))
      ;; A second click deselects it.
      (should (multi-region--track-drag (selected-window) 6 0))
      (should (= (multi-region--count) 0))
      ;; A click on plain text does neither.
      (should-not (multi-region--track-drag (selected-window) 2 0)))))

(defun multi-region-test--fake-rectangle (paged rect)
  "Return an imitation of `pdf-view-mouse-set-region-rectangle' making RECT.
It deselects first, as pdf-tools does.  RECT nil imitates a click."
  (lambda (_event)
    (pdf-view-deactivate-region)
    (setq pdf-view--have-rectangle-region t)
    (when paged
      (setq pdf-view-active-region (list multi-region-test--page)))
    (when rect
      (pdf-view-display-region
       (if paged (list multi-region-test--page rect) (list rect)) t)
      (if paged
          (setcdr pdf-view-active-region (list rect))
        (setq pdf-view-active-region (list rect))))))

(dolist (paged '(t nil))
  (eval
   `(ert-deftest ,(intern (format "multi-region-test-pdf-picture-%s"
                                  (if paged "current-pdf-tools" "older-pdf-tools")))
        ()
      (multi-region-test--with-pdf ,paged
        (defvar pdf-view--have-rectangle-region)
        (let ((transient-mark-mode t)
              (pdf-view--have-rectangle-region nil)
              drawn)
          (cl-letf (((symbol-function 'pdf-view-display-region)
                     (lambda (&optional region &rest _)
                       (push (funcall (symbol-function
                                       'pdf-info-renderpage-text-regions)
                                      (if ,paged (car region) multi-region-test--page)
                                      100)
                             drawn)))
                    ((symbol-function 'pdf-info-renderpage)
                     (lambda (_page _width _file &rest commands) commands))
                    ((symbol-function 'multi-region--pdf-event-edges)
                     (lambda (_) '(0.3 0.6 0.3 0.6))))
            (multi-region-test--select ,paged 1 '(0.1 0.1 0.5 0.2) '(0.1 0.8 0.5 0.9))
            (cl-letf (((symbol-function 'pdf-view-mouse-set-region-rectangle)
                       (multi-region-test--fake-rectangle ,paged '(0.2 0.5 0.7 0.7))))
              (multi-region-pdf-mouse-rectangle nil))
            ;; The text stays selected, and the area is drawn next to it.
            (should (equal (multi-region--pdf-edges)
                           '((0.1 0.1 0.5 0.2) (0.1 0.8 0.5 0.9))))
            (should-not pdf-view--have-rectangle-region)
            (should (equal (plist-get (car drawn) :highlight-text) '(0.1 0.1 0.5 0.2)))
            (should (equal (plist-get (car drawn) :highlight-region) '(0.2 0.5 0.7 0.7)))
            (should (= (multi-region--count) 3))
            ;; It comes between the text above and below it.
            (should (equal (multi-region-selections)
                           '((:page 1 :edges (0.1 0.1 0.5 0.2) :text "p1:0.1")
                             (:page 1 :edges (0.2 0.5 0.7 0.7) :text "" :figure t)
                             (:page 1 :edges (0.1 0.8 0.5 0.9) :text "p1:0.1"))))
            ;; Copying takes the text only.
            (let ((kill-ring nil) (multi-region-keep-after-copy t))
              (multi-region-copy " | ")
              (should (equal (car kill-ring) "p1:0.1 | p1:0.1")))
            ;; Picture areas stay with their page.
            (multi-region-test--turn-to 2)
            (should (= (multi-region--count) 3))
            (multi-region-test--turn-to 1)
            ;; Undo deselects the area added last, then text.
            (multi-region-remove-last)
            (should (= (multi-region--count) 2))
            (should-not multi-region--pdf-rects)
            ;; Clearing and bringing back includes picture areas.
            (cl-letf (((symbol-function 'pdf-view-mouse-set-region-rectangle)
                       (multi-region-test--fake-rectangle ,paged '(0.2 0.5 0.7 0.7))))
              (multi-region-pdf-mouse-rectangle nil))
            (multi-region-clear)
            (should (= (multi-region--count) 0))
            (multi-region-restore)
            (should (= (multi-region--count) 3))
            ;; A click inside a picture area deselects it.
            (cl-letf (((symbol-function 'pdf-view-mouse-set-region-rectangle)
                       (multi-region-test--fake-rectangle ,paged nil)))
              (multi-region-pdf-mouse-rectangle nil))
            (should (= (multi-region--count) 2))
            (should-not multi-region--pdf-rects)))))))

(ert-deftest multi-region-test-pdf-picture-only ()
  ;; A picture area with no text on the page.
  (multi-region-test--with-pdf t
    (defvar pdf-view--have-rectangle-region)
    (let ((pdf-view--have-rectangle-region nil))
      (cl-letf (((symbol-function 'pdf-info-renderpage) #'ignore)
                ((symbol-function 'pdf-view-mouse-set-region-rectangle)
                 (multi-region-test--fake-rectangle t '(0.2 0.5 0.7 0.7))))
        (multi-region-pdf-mouse-rectangle nil))
      (should-not pdf-view-active-region)
      (should (equal (multi-region-selections)
                     '((:page 1 :edges (0.2 0.5 0.7 0.7) :text "" :figure t))))
      (should (= (multi-region--count) 1))
      (should (equal (multi-region--lighter) " MR[1]"))
      (multi-region-remove-last)
      (should (= (multi-region--count) 0)))))

(ert-deftest multi-region-test-pdf-picture-binding ()
  (with-temp-buffer
    (multi-region-mode 1)
    (should-not (eq (key-binding [M-down-mouse-1]) 'multi-region-pdf-mouse-rectangle))
    (cl-letf (((symbol-function 'multi-region--pdf-p) (lambda () t)))
      (should (eq (key-binding [M-down-mouse-1]) 'multi-region-pdf-mouse-rectangle)))))

(provide 'multi-region-test)
;;; multi-region-test.el ends here
