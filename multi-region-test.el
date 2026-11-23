;;; multi-region-test.el --- Tests for multi-region in text buffers -*- lexical-binding: t; -*-

;; Run with:
;;   emacs -Q --batch -L . -l multi-region-test.el -f ert-run-tests-batch-and-exit
;;
;; Or run every test with `make test'.

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

(ert-deftest multi-region-test-not-sign-hyphen ()
  ;; "inflam¬ mation", as PDFs hyphenate, is copied and reported joined.
  (with-temp-buffer
    (insert "acute inflam¬ mation and liber¬\nation")
    (multi-region-mode 1)
    (multi-region--add 1 (point-max))
    (should (equal (plist-get (car (multi-region-selections)) :text)
                   "acute inflammation and liberation"))
    (should (equal (multi-region--texts) '("acute inflammation and liberation")))))

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
    ;; C-c M SPC on a picture deselects it, and selects it again.
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

(ert-deftest multi-region-test-restore-after-replace ()
  (with-temp-buffer
    (insert "I love ClaudeAI very much")
    (multi-region-mode 1)
    (multi-region--add 3 7)              ; "love"
    (multi-region--add 22 26)            ; "much"
    (multi-region-replace "like")
    (should (equal (buffer-string) "I like ClaudeAI very like"))
    (should (= (multi-region--count) 0))
    ;; The replacements are selected again.
    (multi-region-restore)
    (should (equal (mapcar (lambda (s) (plist-get s :text)) (multi-region-selections))
                   '("like" "like")))))

(ert-deftest multi-region-test-restore-after-kill-and-undo ()
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "I love ClaudeAI very much")
    (undo-boundary)
    (multi-region-mode 1)
    (multi-region--add 3 7)              ; "love"
    (multi-region--add 22 26)            ; "much"
    (let ((kill-ring nil))
      (multi-region-kill))
    (should (equal (buffer-string) "I  ClaudeAI very "))
    (undo-boundary)
    (let ((last-command nil))
      (undo))
    (should (equal (buffer-string) "I love ClaudeAI very much"))
    ;; The text undo put back is selected again.
    (multi-region-restore)
    (should (equal (mapcar (lambda (s) (plist-get s :text)) (multi-region-selections))
                   '("love" "much")))))

(ert-deftest multi-region-test-read-only-kill-keeps-snapshot ()
  ;; A kill that stops at read-only text deletes nothing, so what was
  ;; last cleared can still be brought back.
  (with-temp-buffer
    (insert "I love ClaudeAI very much")
    (multi-region-mode 1)
    (multi-region--add 3 7)              ; "love"
    (multi-region-clear)
    (multi-region--add 22 26)            ; "much"
    (setq buffer-read-only t)
    (let ((kill-ring nil))
      (should-error (multi-region-kill) :type 'buffer-read-only))
    (multi-region-restore)
    (should (equal (mapcar (lambda (s) (plist-get s :text)) (multi-region-selections))
                   '("love" "much")))))

(ert-deftest multi-region-test-prefix-key ()
  (unwind-protect
      (with-temp-buffer
        (multi-region-mode 1)
        (should (eq (key-binding (kbd "C-c M w")) 'multi-region-copy))
        (customize-set-variable 'multi-region-prefix-key "C-c s")
        (should (eq (key-binding (kbd "C-c s w")) 'multi-region-copy))
        (should-not (eq (key-binding (kbd "C-c M w")) 'multi-region-copy))
        (customize-set-variable 'multi-region-prefix-key nil)
        (should-not (eq (key-binding (kbd "C-c s w")) 'multi-region-copy)))
    (customize-set-variable 'multi-region-prefix-key "C-c M")))

(provide 'multi-region-test)
;;; multi-region-test.el ends here
