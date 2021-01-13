(require 'f)

(defvar manky-support-path
  (f-dirname load-file-name))

(defvar manky-features-path
  (f-parent manky-support-path))

(defvar manky-root-path
  (f-parent manky-features-path))

(defvar manky-repo-path
  (f-expand "new" manky-root-path))

(add-to-list 'load-path manky-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'manky)
  (require 'espuds)
  (require 'ert))

(defvar ecukes-stderr)
(defvar ecukes-stdout)

(Fail
 (unless (s-blank? ecukes-stdout)
   (princ "==================== ECUKES OUTPUT ====================\n")
   (princ ecukes-stdout))
 (unless (s-blank? ecukes-stderr)
   (princ "==================== ECUKES ERROR ====================\n")
   (princ (ansi-red "%s" ecukes-stderr))))

(Before
 (setq ecukes-stderr "")
 (setq ecukes-stdout "")

 (let ((new-path manky-repo-path))
   (when (f-dir? new-path)
     (f-delete new-path 'force))))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
