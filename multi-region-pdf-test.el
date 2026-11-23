;;; multi-region-pdf-test.el --- Tests for multi-region in pdf-tools buffers -*- lexical-binding: t; -*-

;; Run with:
;;   emacs -Q --batch -L . -l multi-region-pdf-test.el -f ert-run-tests-batch-and-exit
;;
;; Or run every test with `make test'.
;;
;; pdf-tools is not needed: its parts are imitated, in both the
;; current and the older form.  multi-region-pdf-tools-test.el checks
;; the real pdf-tools against what is imitated here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'multi-region)

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

;;;; Picture areas

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

(provide 'multi-region-pdf-test)
;;; multi-region-pdf-test.el ends here
