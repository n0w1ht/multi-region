;;; multi-region-epub-test.el --- Tests for multi-region in EPUBs -*- lexical-binding: t; -*-

;; Run with:
;;   emacs -Q --batch -L . -l multi-region-epub-test.el -f ert-run-tests-batch-and-exit
;;
;; Or run every test with `make test'.
;;
;; nov.el is not needed: its parts are imitated.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'multi-region)

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

(ert-deftest multi-region-test-picture-in-epub-stash ()
  (with-temp-buffer
    (insert "Chapter * text.")
    (put-text-property 9 10 'display (list 'image :type 'png :file "/tmp/e.png"))
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

(provide 'multi-region-epub-test)
;;; multi-region-epub-test.el ends here
