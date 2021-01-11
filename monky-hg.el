(defvar monky--refresh-cache nil)

(defmacro monky--with-refresh-cache (key &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((k (cl-gensym)))
    `(if monky--refresh-cache
         (let ((,k ,key))
           (--if-let (assoc ,k (cdr monky--refresh-cache))
               (progn (cl-incf (caar monky--refresh-cache))
                      (cdr it))
             (cl-incf (cdar monky--refresh-cache))
             (let ((value ,(macroexp-progn body)))
               (push (cons ,k value)
                     (cdr monky--refresh-cache))
               value)))
       ,@body)))

(defun monky-toplevel ()
  (let ((root (monky-hg-string "root")))
    (if root
	    (concat root "/"))))

(defmacro monky-with-toplevel (&rest body)
  (declare (indent defun) (debug (body)))
  (let ((toplevel (cl-gensym "toplevel")))
    `(let ((,toplevel (monky-toplevel)))
       (if ,toplevel
           (let ((default-directory ,toplevel))
             ,@body)
         (monky--not-inside-repository-error)))))

(define-error 'monky-outside-hg-repo "Not inside Mercurial repository")
(define-error 'monky-hg-executable-not-found
  "Mercurial executable cannot be found")

(defun monky--not-inside-repository-error ()
  (if (executable-find monky-hg-executable)
      (signal 'monky-outside-hg-repo default-directory)
    (signal 'monky-git-executable-not-found monky-hg-executable)))

;;; _
(provide 'monky-hg)
;;; monky-hg.el ends here
