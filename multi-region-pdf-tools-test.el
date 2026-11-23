;;; multi-region-pdf-tools-test.el --- Check the installed pdf-tools -*- lexical-binding: t; -*-

;; Run after updating pdf-tools:
;;   make test-pdf-tools
;; or
;;   emacs -Q --batch -L . -L /path/to/pdf-tools/lisp \
;;     -l multi-region-pdf-tools-test.el -f ert-run-tests-batch-and-exit
;;
;; Without -L, the pdf-tools installed with package.el is used.  The
;; tests are skipped when pdf-tools cannot be found.
;;
;; multi-region-pdf-tools.el lists, in its Commentary, what multi-region
;; takes for granted about how pdf-tools works inside.  Each test here
;; checks one of those, numbered as there, against the real pdf-tools,
;; mostly by reading its source.  A failing test names the assumption
;; that no longer holds, and so the code in multi-region-pdf-tools.el
;; to change.  The other PDF tests, in multi-region-pdf-test.el,
;; imitate pdf-tools and cannot notice such changes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'find-func)
(require 'multi-region)

(unless (require 'pdf-view nil t)
  (require 'package)
  (package-initialize)
  (require 'pdf-view nil t))

(defmacro multi-region-pdf-tools-test--assume (n &rest body)
  "Check assumption N of multi-region-pdf-tools.el with BODY."
  (declare (indent 1))
  `(progn
     (skip-unless (featurep 'pdf-view))
     (ert-info ((format "pdf-tools %s breaks assumption %d in multi-region-pdf-tools.el"
                        (multi-region-pdf-tools-test--version) ,n))
       ,@body)))

(defun multi-region-pdf-tools-test--version ()
  "Return the version of the pdf-tools that is loaded, as a string."
  (let ((dir (file-name-directory (or (locate-library "pdf-view") ""))))
    (if (string-match "pdf-tools-\\([0-9.]+\\)" dir)
        (match-string 1 dir)
      dir)))

(defun multi-region-pdf-tools-test--source (name)
  "Return the definition of the function or macro NAME, read from its source.
The test is skipped when the source cannot be found."
  (let ((form (ignore-errors
                (let ((place (find-function-noselect name t)))
                  (with-current-buffer (car place)
                    (save-excursion
                      (goto-char (cdr place))
                      (read (current-buffer))))))))
    (unless form
      (ert-skip (format "No source for `%s'" name)))
    form))

(defun multi-region-pdf-tools-test--arglist (name)
  "Return the argument list of the function NAME, as in its source."
  (nth 2 (multi-region-pdf-tools-test--source name)))

(defun multi-region-pdf-tools-test--has (form part)
  "Return non-nil if PART is FORM, or appears anywhere inside it."
  (cond ((equal form part) t)
        ((consp form)
         (or (multi-region-pdf-tools-test--has (car form) part)
             (multi-region-pdf-tools-test--has (cdr form) part)))
        ((vectorp form)
         (seq-some (lambda (f) (multi-region-pdf-tools-test--has f part)) form))))

(defun multi-region-pdf-tools-test--calls (caller callee)
  "Return non-nil if the source of CALLER mentions CALLEE."
  (multi-region-pdf-tools-test--has
   (multi-region-pdf-tools-test--source caller) callee))

(defun multi-region-pdf-tools-test--names-used (file)
  "Return the pdf-tools names in the code of FILE, comments and strings left out."
  (let (names)
    (with-temp-buffer
      (insert-file-contents (expand-file-name file (file-name-directory
                                                    (locate-library "multi-region"))))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (unless (eq (nth 1 form) 'multi-region--pdf-tools-api)
                (named-let walk ((f form))
                  (cond ((and (symbolp f)
                              (string-prefix-p "pdf-" (symbol-name f)))
                         (cl-pushnew f names))
                        ((consp f) (walk (car f)) (walk (cdr f))))))))
        (end-of-file nil)))
    names))

;;;; The list of what is used

(ert-deftest multi-region-pdf-tools-test-api-exists ()
  (skip-unless (featurep 'pdf-view))
  (require 'pdf-info)
  (require 'pdf-util)
  (let-alist (mapcar (lambda (kind) (cons (car kind) (cdr kind)))
                     multi-region--pdf-tools-api)
    (ert-info ((format "pdf-tools %s lacks something multi-region uses"
                       (multi-region-pdf-tools-test--version)))
      (should-not (seq-remove #'fboundp .functions))
      (should-not (seq-remove #'boundp .variables))
      (should-not (seq-remove #'facep .faces)))))

(ert-deftest multi-region-pdf-tools-test-api-is-complete ()
  ;; Every pdf-tools name the PDF code uses is in the list, so that the
  ;; test above covers it.
  (let ((listed (apply #'append (mapcar #'cdr multi-region--pdf-tools-api))))
    (dolist (file '("multi-region-pdf.el" "multi-region-pdf-tools.el"))
      (ert-info ((format "%s uses pdf-tools names missing from \
`multi-region--pdf-tools-api'" file))
        (should-not (seq-remove (lambda (name) (memq name listed))
                                (multi-region-pdf-tools-test--names-used file)))))))

;;;; The assumptions

(ert-deftest multi-region-pdf-tools-test-1-region-form ()
  (multi-region-pdf-tools-test--assume 1
    ;; pdf-tools starts the region as (PAGE) when selecting begins...
    (let ((paged (multi-region-pdf-tools-test--calls
                  'pdf-view-mouse-set-region
                  '(setq pdf-view-active-region (list page)))))
      ;; ...and the doc string guess agrees, with nothing selected yet.
      (with-temp-buffer
        (setq-local pdf-view-active-region nil)
        (should (eq (and (multi-region--pdf-paged-p) t) paged)))
      ;; With the plain form, the page would not be in the region.
      (unless paged
        (should (multi-region-pdf-tools-test--calls
                 'pdf-view-mouse-set-region 'pdf-view-active-region))))))

(ert-deftest multi-region-pdf-tools-test-2-display-region ()
  (multi-region-pdf-tools-test--assume 2
    (should (memq (cdr (func-arity 'pdf-view-display-region)) '(3 many)))
    (should (multi-region-pdf-tools-test--calls
             'pdf-view-display-region 'pdf-info-renderpage-text-regions))
    (should (equal (seq-take (multi-region-pdf-tools-test--arglist
                              'pdf-info-renderpage-text-regions)
                             2)
                   '(page width)))
    ;; Drawing through the real `pdf-view-display-region' highlights
    ;; what multi-region asks for.
    (let (sent)
      (with-temp-buffer
        (setq-local pdf-view-active-region nil)
        (cl-letf (((symbol-function 'pdf-view-image-size) (lambda (&rest _) '(100 . 100)))
                  ((symbol-function 'pdf-view-create-image) #'ignore)
                  ((symbol-function 'pdf-view-display-image) #'ignore)
                  ((symbol-function 'pdf-view-redisplay) #'ignore)
                  ((symbol-function 'pdf-util-face-colors)
                   (lambda (&rest _) '("#000000" . "#ffffff")))
                  ((symbol-function 'pdf-info-renderpage)
                   (lambda (page width _file &rest commands)
                     (setq sent (list page width commands)))))
          (multi-region--pdf-draw-edges 2 '((0.1 0.1 0.5 0.2)) '((0.2 0.5 0.7 0.7)))))
      (should (equal (car sent) 2))
      (should (equal (plist-get (nth 2 sent) :highlight-text) '(0.1 0.1 0.5 0.2)))
      (should (equal (plist-get (nth 2 sent) :highlight-region) '(0.2 0.5 0.7 0.7))))))

(ert-deftest multi-region-pdf-tools-test-3-renderpage ()
  (multi-region-pdf-tools-test--assume 3
    (should (equal (multi-region-pdf-tools-test--arglist 'pdf-info-renderpage)
                   '(page width &optional file-or-buffer &rest commands)))
    (dolist (keyword '(:foreground :background :alpha :selection-style
                       :highlight-text :highlight-region))
      (ert-info ((format "%s" keyword))
        (should (multi-region-pdf-tools-test--calls 'pdf-info-renderpage keyword))))))

(ert-deftest multi-region-pdf-tools-test-4-mouse-commands ()
  (multi-region-pdf-tools-test--assume 4
    (should (multi-region-pdf-tools-test--calls
             'pdf-view-mouse-set-region 'pdf-view-display-region))
    (should (multi-region-pdf-tools-test--calls
             'pdf-view-mouse-set-region 'pdf-view-deactivate-region))
    (dolist (command '(pdf-view-mouse-extend-region
                       pdf-view-mouse-set-region-rectangle))
      (ert-info ((format "%s" command))
        (should (or (multi-region-pdf-tools-test--calls
                     command 'pdf-view-mouse-set-region)
                    (multi-region-pdf-tools-test--calls
                     command 'pdf-view-display-region)))))))

(ert-deftest multi-region-pdf-tools-test-5-rectangle-variable ()
  (multi-region-pdf-tools-test--assume 5
    (should (boundp 'pdf-view--have-rectangle-region))
    (should (multi-region-pdf-tools-test--calls
             'pdf-view-mouse-extend-region 'pdf-view--have-rectangle-region))))

(ert-deftest multi-region-pdf-tools-test-6-faces ()
  (multi-region-pdf-tools-test--assume 6
    (should (facep 'pdf-view-region))
    (should (facep 'pdf-view-rectangle))
    (should (multi-region-pdf-tools-test--calls
             'pdf-view-display-region 'pdf-util-face-colors))))

(ert-deftest multi-region-pdf-tools-test-7-page-and-image ()
  (multi-region-pdf-tools-test--assume 7
    (should (equal (macroexpand '(pdf-view-current-page))
                   '(image-mode-window-get 'page nil)))
    (should (equal (macroexpand '(pdf-view-current-image w))
                   '(image-mode-window-get 'image w)))))

(ert-deftest multi-region-pdf-tools-test-8-mark ()
  (multi-region-pdf-tools-test--assume 8
    (should (multi-region-pdf-tools-test--calls
             'pdf-view-mouse-set-region 'pdf-view--push-mark))
    (should (multi-region-pdf-tools-test--calls
             'pdf-view--push-mark 'push-mark-command))))

(provide 'multi-region-pdf-tools-test)
;;; multi-region-pdf-tools-test.el ends here
