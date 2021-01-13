(defvar manky--refresh-cache nil)

(defmacro manky--with-refresh-cache (key &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((k (cl-gensym)))
    `(if manky--refresh-cache
         (let ((,k ,key))
           (--if-let (assoc ,k (cdr manky--refresh-cache))
               (progn (cl-incf (caar manky--refresh-cache))
                      (cdr it))
             (cl-incf (cdar manky--refresh-cache))
             (let ((value ,(macroexp-progn body)))
               (push (cons ,k value)
                     (cdr manky--refresh-cache))
               value)))
       ,@body)))

(defun manky-toplevel ()
  (let ((root (manky-hg-string "root")))
    (if root
	    (concat root "/"))))

(defmacro manky-with-toplevel (&rest body)
  (declare (indent defun) (debug (body)))
  (let ((toplevel (cl-gensym "toplevel")))
    `(let ((,toplevel (manky-toplevel)))
       (if ,toplevel
           (let ((default-directory ,toplevel))
             ,@body)
         (manky--not-inside-repository-error)))))

(define-error 'manky-outside-hg-repo "Not inside Mercurial repository")
(define-error 'manky-hg-executable-not-found
  "Mercurial executable cannot be found")

(defun manky--not-inside-repository-error ()
  (if (executable-find manky-hg-executable)
      (signal 'manky-outside-hg-repo default-directory)
    (signal 'manky-git-executable-not-found manky-hg-executable)))

;;; _
(provide 'manky-hg)
;;; manky-hg.el ends here
