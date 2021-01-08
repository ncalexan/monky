;;; Sections

(require 'cl-lib)

(defvar-local monky-top-section nil)
(defvar monky-old-top-section nil)
(defvar monky-section-hidden-default nil)

;; A buffer in monky-mode is organized into hierarchical sections.
;; These sections are used for navigation and for hiding parts of the
;; buffer.
;;
;; Most sections also represent the objects that Monky works with,
;; such as files, diffs, hunks, commits, etc.  The 'type' of a section
;; identifies what kind of object it represents (if any), and the
;; parent and grand-parent, etc provide the context.

(cl-defstruct monky-section
  parent children beginning end type title hidden info)

(defun monky-set-section-info (info &optional section)
  (setf (monky-section-info (or section monky-top-section)) info))

(defun monky--new-section (title type)
  "Create a new section with title TITLE and type TYPE in current buffer.

If not `monky-top-section' exist, the new section will be the new top-section
otherwise, the new-section will be a child of the current top-section.

If TYPE is nil, the section won't be highlighted."
  (let* ((s (make-monky-section :parent monky-top-section
                                :title title
                                :type type
                                :hidden monky-section-hidden-default))
         (old (and monky-old-top-section
                   (monky-find-section (monky-section-path s)
                                       monky-old-top-section))))
    (if monky-top-section
        (push s (monky-section-children monky-top-section))
      (setq monky-top-section s))
    (if old
        (setf (monky-section-hidden s) (monky-section-hidden old)))
    s))

(defmacro monky-with-section (title type &rest body)
  "Create a new section of title TITLE and type TYPE and evaluate BODY there.

Sections create into BODY will be child of the new section.
BODY must leave point at the end of the created section.

If TYPE is nil, the section won't be highlighted."
  (declare (indent 2)
           (debug (symbolp symbolp body)))
  (let ((s (make-symbol "*section*")))
    `(let* ((,s (monky--new-section ,title ,type))
            (monky-top-section ,s))
       (setf (monky-section-beginning ,s) (point))
       ,@body
       (setf (monky-section-end ,s) (point))
       (setf (monky-section-children ,s)
             (nreverse (monky-section-children ,s)))
       ,s)))

(defmacro monky-create-buffer-sections (&rest body)
  "Empty current buffer of text and monky's section, and then evaluate BODY."
  (declare (indent 0)
           (debug (body)))
  `(let ((inhibit-read-only t))
     (erase-buffer)
     (let ((monky-old-top-section monky-top-section))
       (setq monky-top-section nil)
       ,@body
       (when (null monky-top-section)
         (monky-with-section 'top nil
           (insert "(empty)\n")))
       (monky-propertize-section monky-top-section)
       (monky-section-set-hidden monky-top-section
                                 (monky-section-hidden monky-top-section)))))

(defun monky-propertize-section (section)
  "Add text-property needed for SECTION."
  (put-text-property (monky-section-beginning section)
                     (monky-section-end section)
                     'monky-section section)
  (dolist (s (monky-section-children section))
    (monky-propertize-section s)))

(defun monky-find-section (path top)
  "Find the section at the path PATH in subsection of section TOP."
  (if (null path)
      top
    (let ((secs (monky-section-children top)))
      (while (and secs (not (equal (car path)
                                   (monky-section-title (car secs)))))
        (setq secs (cdr secs)))
      (and (car secs)
           (monky-find-section (cdr path) (car secs))))))

(defun monky-section-path (section)
  "Return the path of SECTION."
  (if (not (monky-section-parent section))
      '()
    (append (monky-section-path (monky-section-parent section))
            (list (monky-section-title section)))))

(defun monky-cancel-section (section)
  (delete-region (monky-section-beginning section)
                 (monky-section-end section))
  (let ((parent (monky-section-parent section)))
    (if parent
        (setf (monky-section-children parent)
              (delq section (monky-section-children parent)))
      (setq monky-top-section nil))))

(defun monky-current-section ()
  "Return the monky section at point."
  (monky-section-at (point)))

(defun monky-section-at (pos)
  "Return the monky section at position POS."
  (or (get-text-property pos 'monky-section)
      monky-top-section))

(defun monky-find-section-after (pos secs)
  "Find the first section that begins after POS in the list SECS."
  (while (and secs
              (not (> (monky-section-beginning (car secs)) pos)))
    (setq secs (cdr secs)))
  (car secs))

(defun monky-find-section-before (pos secs)
  "Find the last section that begins before POS in the list SECS."
  (let ((prev nil))
    (while (and secs
                (not (> (monky-section-beginning (car secs)) pos)))
      (setq prev (car secs))
      (setq secs (cdr secs)))
    prev))

(defun monky-next-section (section)
  "Return the section that is after SECTION."
  (let ((parent (monky-section-parent section)))
    (if parent
        (let ((next (cadr (memq section
                                (monky-section-children parent)))))
          (or next
              (monky-next-section parent))))))

;; TODO: To be replaced with `magit-section-movement-hook'.
;; (defvar-local monky-submode nil)

(defun monky-goto-next-section ()
  "Go to the next monky section."
  (interactive)
  (let* ((section (monky-current-section))
         (next (or (and (not (monky-section-hidden section))
                        (monky-section-children section)
                        (monky-find-section-after (point)
                                                  (monky-section-children
                                                   section)))
                   (monky-next-section section))))
    (cond
     ;; TODO: To be replaced with `magit-section-movement-hook' as well.
     ;; ((and next (eq (monky-section-type next) 'longer))
     ;;  (when monky-log-auto-more
     ;;    (monky-log-show-more-entries)
     ;;    (monky-goto-next-section)))
     (next
      (goto-char (monky-section-beginning next))
      ;; (if (memq monky-submode '(log blame))
      ;;     (monky-show-commit next))
      )
     (t (message "No next section")))))

(defun monky-prev-section (section)
  "Return the section that is before SECTION."
  (let ((parent (monky-section-parent section)))
    (if parent
        (let ((prev (cadr (memq section
                                (reverse (monky-section-children parent))))))
          (cond (prev
                 (while (and (not (monky-section-hidden prev))
                             (monky-section-children prev))
                   (setq prev (car (reverse (monky-section-children prev)))))
                 prev)
                (t
                 parent))))))


(defun monky-goto-previous-section ()
  "Goto the previous monky section."
  (interactive)
  (let ((section (monky-current-section)))
    (cond ((= (point) (monky-section-beginning section))
           (let ((prev (monky-prev-section (monky-current-section))))
             (if prev
                 (progn
                   ;; (if (memq monky-submode '(log blame))
                   ;;     (monky-show-commit prev))
                   (goto-char (monky-section-beginning prev)))
               (message "No previous section"))))
          (t
           (let ((prev (monky-find-section-before (point)
                                                  (monky-section-children
                                                   section))))
             ;; (if (memq monky-submode '(log blame))
             ;;     (monky-show-commit (or prev section)))
             (goto-char (monky-section-beginning (or prev section))))))))


(defun monky-section-context-type (section)
  (if (null section)
      '()
    (let ((c (or (monky-section-type section)
                 (if (symbolp (monky-section-title section))
                     (monky-section-title section)))))
      (if c
          (cons c (monky-section-context-type
                   (monky-section-parent section)))
        '()))))

(defmacro monky-section-case (opname &rest clauses)
  "Make different action depending of current section.

HEAD is (SECTION INFO &optional OPNAME),
  SECTION will be bind to the current section,
  INFO will be bind to the info's of the current section,
  OPNAME is a string that will be used to describe current action,

CLAUSES is a list of CLAUSE, each clause is (SECTION-TYPE &BODY)
where SECTION-TYPE describe section where BODY will be run.

This returns non-nil if some section matches.  If the
corresponding body return a non-nil value, it is returned,
otherwise it return t.

If no section matches, this returns nil if no OPNAME was given
and throws an error otherwise."

  (declare (indent 1)
           (debug (form &rest (sexp body))))
  (let ((section (make-symbol "*section*"))
        (type (make-symbol "*type*"))
        (context (make-symbol "*context*")))
    `(let* ((,section (monky-current-section))
            (,type (monky-section-type ,section))
            (,context (monky-section-context-type ,section)))
       (cond ,@(mapcar (lambda (clause)
                         (let ((prefix (car clause))
                               (body (cdr clause)))
                           `(,(if (eq prefix t)
                                  `t
                                `(monky-prefix-p ',(reverse prefix) ,context))
                             (or (progn ,@body)
                                 t))))
                       clauses)
             ,@(when opname
                 `(((not ,type)
                    (user-error "Nothing to %s here" ,opname))
                   (t
                    (error "Can't %s as %s"
                           ,opname
                           ,type))))))))

(defun monky-section-set-hidden (section hidden)
  "Hide SECTION if HIDDEN is not nil, show it otherwise."
  (setf (monky-section-hidden section) hidden)
  (let ((inhibit-read-only t)
        (beg (save-excursion
               (goto-char (monky-section-beginning section))
               (forward-line)
               (point)))
        (end (monky-section-end section)))
    (if (< beg end)
        (put-text-property beg end 'invisible hidden)))
  (if (not hidden)
      (dolist (c (monky-section-children section))
        (monky-section-set-hidden c (monky-section-hidden c)))))

(defun monky-toggle-section ()
  "Toggle hidden status of current section."
  (interactive)
  (let ((section (monky-current-section)))
    (when (monky-section-parent section)
      (goto-char (monky-section-beginning section))
      (monky-section-set-hidden section (not (monky-section-hidden section))))))

(defun monky-section-show-level-1-all ()
  "Collapse all the sections in the monky status buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((section (monky-current-section)))
	(monky-section-set-hidden section t))
      (forward-line 1))))

(defun monky-section-show-level-2-all ()
  "Show all the files changes, but not their contents."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((section (monky-current-section)))
	(if (memq (monky-section-type section) (list 'hunk 'diff))
	    (monky-section-set-hidden section t)
	  (monky-section-set-hidden section nil)))
      (forward-line 1))))

(defun monky-section-show-level-3-all ()
  "Expand all file contents and line numbers, but not the actual changes."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((section (monky-current-section)))
	(if (memq (monky-section-type section) (list 'hunk))
	    (monky-section-set-hidden section t)
	  (monky-section-set-hidden section nil)))
      (forward-line 1))))

(defun monky-section-show-level-4-all ()
  "Expand all sections."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((section (monky-current-section)))
	(monky-section-set-hidden section nil))
      (forward-line 1))))

(provide 'monky-section)
