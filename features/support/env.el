(require 'f)

(defvar monky-support-path
  (f-dirname load-file-name))

(defvar monky-features-path
  (f-parent monky-support-path))

(defvar monky-root-path
  (f-parent monky-features-path))

(defvar monky-repo-path
  (f-expand "new" monky-root-path))

(add-to-list 'load-path monky-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'monky)
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

 (let ((new-path monky-repo-path))
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
