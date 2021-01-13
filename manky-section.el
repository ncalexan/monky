;;; Sections

(require 'cl-lib)

(defvar-local manky-top-section nil)
(defvar manky-old-top-section nil)
(defvar manky-section-hidden-default nil)

;; A buffer in manky-mode is organized into hierarchical sections.
;; These sections are used for navigation and for hiding parts of the
;; buffer.
;;
;; Most sections also represent the objects that Manky works with,
;; such as files, diffs, hunks, commits, etc.  The 'type' of a section
;; identifies what kind of object it represents (if any), and the
;; parent and grand-parent, etc provide the context.

(cl-defstruct manky-section
  parent children beginning end type title hidden info)

(defun manky-set-section-info (info &optional section)
  (setf (manky-section-info (or section manky-top-section)) info))

(defun manky--new-section (title type)
  "Create a new section with title TITLE and type TYPE in current buffer.

If not `manky-top-section' exist, the new section will be the new top-section
otherwise, the new-section will be a child of the current top-section.

If TYPE is nil, the section won't be highlighted."
  (let* ((s (make-manky-section :parent manky-top-section
                                :title title
                                :type type
                                :hidden manky-section-hidden-default))
         (old (and manky-old-top-section
                   (manky-find-section (manky-section-path s)
                                       manky-old-top-section))))
    (if manky-top-section
        (push s (manky-section-children manky-top-section))
      (setq manky-top-section s))
    (if old
        (setf (manky-section-hidden s) (manky-section-hidden old)))
    s))

(defmacro manky-with-section (title type &rest body)
  "Create a new section of title TITLE and type TYPE and evaluate BODY there.

Sections create into BODY will be child of the new section.
BODY must leave point at the end of the created section.

If TYPE is nil, the section won't be highlighted."
  (declare (indent 2)
           (debug (symbolp symbolp body)))
  (let ((s (make-symbol "*section*")))
    `(let* ((,s (manky--new-section ,title ,type))
            (manky-top-section ,s))
       (setf (manky-section-beginning ,s) (point))
       ,@body
       (setf (manky-section-end ,s) (point))
       (setf (manky-section-children ,s)
             (nreverse (manky-section-children ,s)))
       ,s)))

(defmacro manky-create-buffer-sections (&rest body)
  "Empty current buffer of text and manky's section, and then evaluate BODY."
  (declare (indent 0)
           (debug (body)))
  `(let ((inhibit-read-only t))
     (erase-buffer)
     (let ((manky-old-top-section manky-top-section))
       (setq manky-top-section nil)
       ,@body
       (when (null manky-top-section)
         (manky-with-section 'top nil
           (insert "(empty)\n")))
       (manky-propertize-section manky-top-section)
       (manky-section-set-hidden manky-top-section
                                 (manky-section-hidden manky-top-section)))))

(defun manky-propertize-section (section)
  "Add text-property needed for SECTION."
  (put-text-property (manky-section-beginning section)
                     (manky-section-end section)
                     'manky-section section)
  (dolist (s (manky-section-children section))
    (manky-propertize-section s)))

(defun manky-find-section (path top)
  "Find the section at the path PATH in subsection of section TOP."
  (if (null path)
      top
    (let ((secs (manky-section-children top)))
      (while (and secs (not (equal (car path)
                                   (manky-section-title (car secs)))))
        (setq secs (cdr secs)))
      (and (car secs)
           (manky-find-section (cdr path) (car secs))))))

(defun manky-section-path (section)
  "Return the path of SECTION."
  (if (not (manky-section-parent section))
      '()
    (append (manky-section-path (manky-section-parent section))
            (list (manky-section-title section)))))

(defun manky-cancel-section (section)
  (delete-region (manky-section-beginning section)
                 (manky-section-end section))
  (let ((parent (manky-section-parent section)))
    (if parent
        (setf (manky-section-children parent)
              (delq section (manky-section-children parent)))
      (setq manky-top-section nil))))

(defun manky-current-section ()
  "Return the manky section at point."
  (manky-section-at (point)))

(defun manky-section-at (pos)
  "Return the manky section at position POS."
  (or (get-text-property pos 'manky-section)
      manky-top-section))

(defun manky-find-section-after (pos secs)
  "Find the first section that begins after POS in the list SECS."
  (while (and secs
              (not (> (manky-section-beginning (car secs)) pos)))
    (setq secs (cdr secs)))
  (car secs))

(defun manky-find-section-before (pos secs)
  "Find the last section that begins before POS in the list SECS."
  (let ((prev nil))
    (while (and secs
                (not (> (manky-section-beginning (car secs)) pos)))
      (setq prev (car secs))
      (setq secs (cdr secs)))
    prev))

(defun manky-next-section (section)
  "Return the section that is after SECTION."
  (let ((parent (manky-section-parent section)))
    (if parent
        (let ((next (cadr (memq section
                                (manky-section-children parent)))))
          (or next
              (manky-next-section parent))))))

;; TODO: To be replaced with `magit-section-movement-hook'.
;; (defvar-local manky-submode nil)

(defun manky-goto-next-section ()
  "Go to the next manky section."
  (interactive)
  (let* ((section (manky-current-section))
         (next (or (and (not (manky-section-hidden section))
                        (manky-section-children section)
                        (manky-find-section-after (point)
                                                  (manky-section-children
                                                   section)))
                   (manky-next-section section))))
    (cond
     ;; TODO: To be replaced with `magit-section-movement-hook' as well.
     ;; ((and next (eq (manky-section-type next) 'longer))
     ;;  (when manky-log-auto-more
     ;;    (manky-log-show-more-entries)
     ;;    (manky-goto-next-section)))
     (next
      (goto-char (manky-section-beginning next))
      ;; (if (memq manky-submode '(log blame))
      ;;     (manky-show-commit next))
      )
     (t (message "No next section")))))

(defun manky-prev-section (section)
  "Return the section that is before SECTION."
  (let ((parent (manky-section-parent section)))
    (if parent
        (let ((prev (cadr (memq section
                                (reverse (manky-section-children parent))))))
          (cond (prev
                 (while (and (not (manky-section-hidden prev))
                             (manky-section-children prev))
                   (setq prev (car (reverse (manky-section-children prev)))))
                 prev)
                (t
                 parent))))))


(defun manky-goto-previous-section ()
  "Goto the previous manky section."
  (interactive)
  (let ((section (manky-current-section)))
    (cond ((= (point) (manky-section-beginning section))
           (let ((prev (manky-prev-section (manky-current-section))))
             (if prev
                 (progn
                   ;; (if (memq manky-submode '(log blame))
                   ;;     (manky-show-commit prev))
                   (goto-char (manky-section-beginning prev)))
               (message "No previous section"))))
          (t
           (let ((prev (manky-find-section-before (point)
                                                  (manky-section-children
                                                   section))))
             ;; (if (memq manky-submode '(log blame))
             ;;     (manky-show-commit (or prev section)))
             (goto-char (manky-section-beginning (or prev section))))))))


(defun manky-section-context-type (section)
  (if (null section)
      '()
    (let ((c (or (manky-section-type section)
                 (if (symbolp (manky-section-title section))
                     (manky-section-title section)))))
      (if c
          (cons c (manky-section-context-type
                   (manky-section-parent section)))
        '()))))

(defmacro manky-section-case (opname &rest clauses)
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
    `(let* ((,section (manky-current-section))
            (,type (manky-section-type ,section))
            (,context (manky-section-context-type ,section)))
       (cond ,@(mapcar (lambda (clause)
                         (let ((prefix (car clause))
                               (body (cdr clause)))
                           `(,(if (eq prefix t)
                                  `t
                                `(manky-prefix-p ',(reverse prefix) ,context))
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

(defun manky-section-set-hidden (section hidden)
  "Hide SECTION if HIDDEN is not nil, show it otherwise."
  (setf (manky-section-hidden section) hidden)
  (let ((inhibit-read-only t)
        (beg (save-excursion
               (goto-char (manky-section-beginning section))
               (forward-line)
               (point)))
        (end (manky-section-end section)))
    (if (< beg end)
        (put-text-property beg end 'invisible hidden)))
  (if (not hidden)
      (dolist (c (manky-section-children section))
        (manky-section-set-hidden c (manky-section-hidden c)))))

(defun manky-toggle-section ()
  "Toggle hidden status of current section."
  (interactive)
  (let ((section (manky-current-section)))
    (when (manky-section-parent section)
      (goto-char (manky-section-beginning section))
      (manky-section-set-hidden section (not (manky-section-hidden section))))))

(defun manky-section-show-level-1-all ()
  "Collapse all the sections in the manky status buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((section (manky-current-section)))
	(manky-section-set-hidden section t))
      (forward-line 1))))

(defun manky-section-show-level-2-all ()
  "Show all the files changes, but not their contents."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((section (manky-current-section)))
	(if (memq (manky-section-type section) (list 'hunk 'diff))
	    (manky-section-set-hidden section t)
	  (manky-section-set-hidden section nil)))
      (forward-line 1))))

(defun manky-section-show-level-3-all ()
  "Expand all file contents and line numbers, but not the actual changes."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((section (manky-current-section)))
	(if (memq (manky-section-type section) (list 'hunk))
	    (manky-section-set-hidden section t)
	  (manky-section-set-hidden section nil)))
      (forward-line 1))))

(defun manky-section-show-level-4-all ()
  "Expand all sections."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((section (manky-current-section)))
	(manky-section-set-hidden section nil))
      (forward-line 1))))

(provide 'manky-section)
