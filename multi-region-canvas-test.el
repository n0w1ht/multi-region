;;; multi-region-canvas-test.el --- Tests for multi-region in pdf-canvas buffers -*- lexical-binding: t; -*-

;; Run with:
;;   emacs -Q --batch -L . -l multi-region-canvas-test.el -f ert-run-tests-batch-and-exit
;;
;; Or run every test with `make test'.
;;
;; pdf-canvas is not needed: its parts are imitated.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'multi-region)

(defvar multi-region-test--canvas nil
  "The selections of the imitation pdf-canvas buffer, oldest first.")

(define-derived-mode pdf-canvas-mode special-mode "PDF-Canvas-Imitation")

(defmacro multi-region-test--with-canvas (&rest body)
  "Run BODY in an imitation pdf-canvas buffer."
  (declare (indent 0))
  `(with-temp-buffer
     (pdf-canvas-mode)
     (let ((multi-region-test--canvas nil))
       (cl-letf (((symbol-function 'pdf-canvas-selections)
                  (lambda ()
                    (let ((i -1))
                      (sort (mapcar (lambda (s)
                                      (append s (list :index (setq i (1+ i)))))
                                    multi-region-test--canvas)
                            (lambda (a b) (< (plist-get a :page)
                                             (plist-get b :page)))))))
                 ((symbol-function 'pdf-canvas-selection-count)
                  (lambda () (length multi-region-test--canvas)))
                 ((symbol-function 'pdf-canvas-add-selection)
                  (lambda (s)
                    (setq multi-region-test--canvas
                          (append multi-region-test--canvas
                                  (list (cl-loop for (k v) on s by #'cddr
                                                 unless (eq k :index)
                                                 append (list k v)))))))
                 ((symbol-function 'pdf-canvas-remove-selection)
                  (lambda (i)
                    (setq multi-region-test--canvas
                          (append (cl-subseq multi-region-test--canvas 0 i)
                                  (nthcdr (1+ i) multi-region-test--canvas)))))
                 ((symbol-function 'pdf-canvas-clear-selections)
                  (lambda () (setq multi-region-test--canvas nil)))
                 ((symbol-function 'pdf-canvas-quit) #'ignore))
         (multi-region-mode 1)
         ,@body))))

(ert-deftest multi-region-test-canvas-selections ()
  (multi-region-test--with-canvas
    (pdf-canvas-add-selection '(:page 3 :edges (0.1 0.5 0.4 0.6)
                                :text "inflam¬ mation" :mode 0
                                :ends (0.1 0.55 0.4 0.55)))
    (pdf-canvas-add-selection '(:page 2 :edges (0.2 0.2 0.8 0.5)
                                :text "Figure 1" :figure t :mode 3
                                :ends (0.2 0.2 0.8 0.5)))
    (should (= (multi-region--count) 2))
    (should (multi-region--any-p))
    (let ((sels (multi-region-selections)))
      ;; Page by page, a picture area with no text, hyphens joined.
      (should (equal (mapcar (lambda (s) (plist-get s :page)) sels) '(2 3)))
      (should (plist-get (car sels) :figure))
      (should (equal (plist-get (car sels) :text) ""))
      (should (equal (plist-get (cadr sels) :text) "inflammation"))
      (should (equal (plist-get (cadr sels) :edges) '(0.1 0.5 0.4 0.6))))
    ;; Copying takes the text only.
    (should (equal (multi-region--texts) '("inflammation")))
    ;; Reading them leaves pdf-canvas's own text alone.
    (should (equal (plist-get (car multi-region-test--canvas) :text)
                   "inflam¬ mation"))
    (should-error (multi-region-toggle) :type 'user-error)))

(ert-deftest multi-region-test-canvas-clear-restore-remove ()
  (multi-region-test--with-canvas
    (pdf-canvas-add-selection '(:page 1 :edges (0 0 1 1) :text "a" :mode 1
                                :ends (0.1 0.1 0.2 0.1)))
    (pdf-canvas-add-selection '(:page 4 :edges (0 0 1 1) :text "b" :mode 1
                                :ends (0.3 0.3 0.4 0.3)))
    (multi-region-clear)
    (should (= (multi-region--count) 0))
    ;; A selection made since is kept, and not doubled.
    (pdf-canvas-add-selection '(:page 4 :edges (0 0 1 1) :text "b" :mode 1
                                :ends (0.3 0.3 0.4 0.3)))
    (multi-region-restore)
    (should (equal (sort (multi-region--texts) #'string<) '("a" "b")))
    (multi-region-remove-last)
    (should (equal (multi-region--texts) '("b")))
    (multi-region-remove-last)
    (should-error (multi-region-remove-last) :type 'user-error)
    ;; C-g remembers what it deselects.
    (pdf-canvas-add-selection '(:page 1 :edges (0 0 1 1) :text "c" :mode 1
                                :ends (0.5 0.5 0.6 0.5)))
    (should (eq (key-binding [remap pdf-canvas-quit])
                'multi-region-keyboard-quit))
    (multi-region-keyboard-quit)
    (should (= (multi-region--count) 0))
    (multi-region-restore)
    (should (equal (multi-region--texts) '("c")))))

(ert-deftest multi-region-test-canvas-mouse ()
  (multi-region-test--with-canvas
    (let (seen)
      (cl-letf (((symbol-function 'pdf-canvas-mouse-add)
                 (lambda (event) (setq seen event))))
        ;; C-drag is handed to pdf-canvas; the PDF-only keys of
        ;; pdf-tools buffers are not taken.
        (should (eq (key-binding [C-down-mouse-1]) 'multi-region-mouse-drag))
        (should-not (eq (key-binding [down-mouse-1])
                        'multi-region-pdf-mouse-set-region))
        (should-not (eq (key-binding [M-down-mouse-1])
                        'multi-region-pdf-mouse-rectangle))
        (let ((event (list 'C-down-mouse-1 (posn-at-point))))
          (multi-region-mouse-drag event)
          (should (eq seen event)))))))

(provide 'multi-region-canvas-test)
;;; multi-region-canvas-test.el ends here
