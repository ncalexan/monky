;;; manky-queue.el --- Control Hg  -*- lexical-binding: t; -*-

;;; Queues

(require 'manky-section)

(defface manky-queue-patch
  '((t :weight bold :inherit (manky-header highlight)))
  "Face for patch name"
  :group 'manky-faces)

(defface manky-queue-active
  '((((class color) (background light))
     :box t
     :background "light green"
     :foreground "dark olive green")
    (((class color) (background dark))
     :box t
     :background "light green"
     :foreground "dark olive green"))
  "Face for active patch queue"
  :group 'manky-faces)

(defface manky-queue-positive-guard
  '((((class color) (background light))
     :box t
     :background "light green"
     :foreground "dark olive green")
    (((class color) (background dark))
     :box t
     :background "light green"
     :foreground "dark olive green"))
  "Face for queue postive guards"
  :group 'manky-faces)

(defface manky-queue-negative-guard
  '((((class color) (background light))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4")
    (((class color) (background dark))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4"))
  "Face for queue negative guards"
  :group 'manky-faces)

(defvar manky-queue-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "u") 'manky-qpop-item)
    (define-key map (kbd "U") 'manky-qpop-all)
    (define-key map (kbd "s") 'manky-qpush-item)
    (define-key map (kbd "S") 'manky-qpush-all)
    (define-key map (kbd "r") 'manky-qrefresh)
    (define-key map (kbd "R") 'manky-qrename-item)
    (define-key map (kbd "k") 'manky-qremove-item)
    (define-key map (kbd "N") 'manky-qnew)
    (define-key map (kbd "f") 'manky-qfinish-item)
    (define-key map (kbd "F") 'manky-qfinish-applied)
    (define-key map (kbd "d") 'manky-qfold-item)
    (define-key map (kbd "G") 'manky-qguard-item)
    (define-key map (kbd "o") 'manky-qreorder)
    (define-key map (kbd "A") 'manky-addremove-all)
    map))

;;; Queue mode
(define-minor-mode manky-queue-mode
  "Minor mode for hg Queue.

\\{manky-queue-mode-map}"
  :group manky
  :init-value ()
  :lighter ()
  :keymap manky-queue-mode-map)

(defvar manky-queue-buffer-name "*manky-queue*")

(defvar-local manky-patches-dir ".hg/patches/")

(defun manky-patch-series-file ()
  (concat manky-patches-dir "series"))

(defun manky-insert-patch (patch inserter &rest args)
  (let ((p (point))
        (manky-hide-diffs nil))
    (save-restriction
      (narrow-to-region p p)
      (apply inserter args)
      (goto-char (point-max))
      (if (not (eq (char-before) ?\n))
          (insert "\n"))
      (goto-char p)
      (while (and (not (eobp)) (not (looking-at "^diff")))
        (manky-delete-line t))
      (when (looking-at "^diff")
        (manky-wash-diffs))
      (goto-char (point-max)))))

(defun manky-insert-guards (patch)
  (let ((guards (cl-remove-if
                 (lambda (guard) (string= "unguarded" guard))
                 (split-string
                  (cadr (split-string
                         (manky-hg-string "qguard" patch
                                          "--config" "extensions.mq=")
                         ":"))))))
    (dolist (guard guards)
      (insert (propertize " " 'face 'manky-queue-patch)
              (propertize guard
                          'face
                          (if (manky-string-starts-with-p guard "+")
                              'manky-queue-positive-guard
                            'manky-queue-negative-guard))))
    (insert (propertize "\n" 'face 'manky-queue-patch))))

(defun manky-wash-queue-patch ()
  (manky-wash-queue-insert-patch #'insert-file-contents))

(defvar manky-queue-staged-all-files nil)
(defvar-local manky-queue-staged-files nil)
(defvar-local manky-queue-old-staged-files nil)

(defun manky-wash-queue-discarding ()
  (manky-wash-status-lines
   (lambda (status file)
     (let ((manky-section-hidden-default manky-hide-diffs))
       (if (or manky-queue-staged-all-files
               (member file manky-old-staged-files)
               (member file manky-queue-old-staged-files))
           (manky-queue-stage-file file)
         (manky-with-section file 'diff
           (manky-insert-diff file status "qdiff"))))))
  (setq manky-queue-staged-all-files nil))

(defun manky-wash-queue-insert-patch (inserter)
  (if (looking-at "^\\([^\n]+\\)$")
      (let ((patch (match-string 1)))
        (manky-delete-line)
        (let ((manky-section-hidden-default t))
          (manky-with-section patch 'patch
            (manky-set-section-info patch)
            (insert
             (propertize (format "\t%s" patch) 'face 'manky-queue-patch))
            (manky-insert-guards patch)
            (funcall #'manky-insert-patch
                     patch inserter (concat manky-patches-dir patch))
            (forward-line)))
        t)
    nil))

(defun manky-wash-queue-queue ()
  (if (looking-at "^\\([^()\n]+\\)\\(\\s-+([^)]*)\\)?$")
      (let ((queue (match-string 1)))
        (manky-delete-line)
        (when (match-beginning 2)
          (setq manky-patches-dir
                (if (string= queue "patches")
                    ".hg/patches/"
                  (concat ".hg/patches-" queue "/")))
          (put-text-property 0 (length queue) 'face 'manky-queue-active queue))
        (manky-with-section queue 'queue
          (manky-set-section-info queue)
          (insert "\t" queue)
          (forward-line))
        t)
    nil))

(defun manky-wash-queue-queues ()
    (if (looking-at "^patches (.*)\n?\\'")
        (progn
          (manky-delete-line t)
          nil)
      (manky-wash-sequence #'manky-wash-queue-queue)))

(defun manky-wash-queue-patches ()
  (manky-wash-sequence #'manky-wash-queue-patch))

;;; Queues
(defun manky-insert-queue-queues ()
  (manky-hg-section 'queues "Patch Queues:"
                    #'manky-wash-queue-queues
                    "qqueue" "--list" "extensions.mq="))

;;; Applied Patches
(defun manky-insert-queue-applied ()
  (manky-hg-section 'applied "Applied Patches:" #'manky-wash-queue-patches
                    "qapplied" "--config" "extensions.mq="))

;;; UnApplied Patches
(defun manky-insert-queue-unapplied ()
  (manky-hg-section 'unapplied "UnApplied Patches:" #'manky-wash-queue-patches
                    "qunapplied" "--config" "extensions.mq="))

;;; Series
(defun manky-insert-queue-series ()
  (manky-hg-section 'qseries "Series:" #'manky-wash-queue-patches
                    "qseries" "--config" "extensions.mq="))

;;; Qdiff
(defun manky-insert-queue-discarding ()
  (when (manky-qtip-p)
    (setq manky-queue-old-staged-files (cl-copy-list manky-queue-staged-files))
    (setq manky-queue-staged-files '())
    (let ((manky-hide-diffs t))
      (manky-hg-section 'discarding "Discarding (qdiff):"
                        #'manky-wash-queue-discarding
                        "log" "--style" manky-hg-style-files-status
                        "--rev" "qtip"))))

(defun manky-insert-queue-staged-changes ()
  (when (and (manky-qtip-p)
             (or manky-queue-staged-files manky-staged-files))
    (manky-with-section 'queue-staged nil
      (insert (propertize "Staged changes (qdiff):"
                          'face 'manky-section-title) "\n")
      (let ((manky-section-hidden-default t))
        (dolist (file (delete-dups (cl-copy-list (append manky-queue-staged-files
                                                         manky-staged-files))))
          (manky-with-section file 'diff
            (manky-insert-diff file nil "qdiff")))))
    (insert "\n")))

(defun manky-wash-active-guards ()
  (if (looking-at "^no active guards")
      (manky-delete-line t)
    (manky-wash-sequence
     (lambda ()
       (let ((guard (buffer-substring (point) (point-at-eol))))
         (manky-delete-line)
         (insert " " (propertize guard 'face 'manky-queue-positive-guard))
         (forward-line))))))


;;; Active guards
(defun manky-insert-active-guards ()
  (manky-hg-section 'active-guards "Active Guards:" #'manky-wash-active-guards
                    "qselect" "--config" "extensions.mq="))

;;; Queue Staged Changes

(defun manky-queue-stage-file (file)
  (push file manky-queue-staged-files))

(defun manky-queue-unstage-file (file)
  (setq manky-queue-staged-files (delete file manky-queue-staged-files)))

(defun manky-refresh-queue-buffer ()
  (manky-create-buffer-sections
    (manky-with-section 'queue nil
      (manky-insert-untracked-files)
      (manky-insert-missing-files)
      (manky-insert-changes)
      (manky-insert-staged-changes)
      (manky-insert-queue-discarding)
      (manky-insert-queue-staged-changes)
      (manky-insert-queue-queues)
      (manky-insert-active-guards)
      (manky-insert-queue-applied)
      (manky-insert-queue-unapplied)
      (manky-insert-queue-series))))

(defun manky-queue ()
  (interactive)
  (manky-with-process
    (let ((topdir (manky-get-root-dir)))
      (pop-to-buffer manky-queue-buffer-name)
      (manky-mode-init topdir 'queue #'manky-refresh-queue-buffer)
      (manky-queue-mode t))))

(defun manky-qqueue (queue)
  (manky-run-hg "qqueue"
                "--config" "extensions.mq="
                queue))

(defun manky-qpop (&optional patch)
  (interactive)
  (apply #'manky-run-hg
         "qpop"
         "--config" "extensions.mq="
         (if patch (list patch) '())))

(defun manky-qpush (&optional patch)
  (interactive)
  (apply #'manky-run-hg
         "qpush"
         "--config" "extensions.mq="
         (if patch (list patch) '())))

(defun manky-qpush-all ()
  (interactive)
  (manky-run-hg "qpush" "--all"
                "--config" "extensions.mq="))

(defun manky-qpop-all ()
  (interactive)
  (manky-run-hg "qpop" "--all"
                "--config" "extensions.mq="))

(defvar manky-log-edit-buffer-name "*manky-edit-log*"
  "Buffer name for composing commit messages.")

(defun manky-qrefresh ()
  (interactive)
  (if (not current-prefix-arg)
      (apply #'manky-run-hg "qrefresh"
             "--config" "extensions.mq="
             (append manky-staged-files manky-queue-staged-files))
    ;; get last commit message
    (with-current-buffer (get-buffer-create manky-log-edit-buffer-name)
      (manky-hg-insert
       (list "log" "--config" "extensions.mq="
             "--template" "{desc}" "-r" "-1")))
    (manky-pop-to-log-edit 'qrefresh)))

(defun manky-qremove (patch)
  (manky-run-hg "qremove" patch
                "--config" "extensions.mq="))

(defun manky-qnew (patch)
  (interactive (list (read-string "Patch Name : ")))
  (if (not current-prefix-arg)
      (manky-run-hg "qnew" patch
                    "--config" "extensions.mq=")
    (manky-pop-to-log-edit 'qnew patch)))

(defun manky-qinit ()
  (interactive)
  (manky-run-hg "qinit"
                "--config" "extensions.mq="))

(defun manky-qimport (node-1 &optional node-2)
  (manky-run-hg "qimport" "--rev"
                (if node-2 (concat node-1 ":" node-2) node-1)
                "--config" "extensions.mq="))

(defun manky-qrename (old-patch &optional new-patch)
  (let ((new-patch (or new-patch
                       (read-string "New Patch Name : "))))
    (manky-run-hg "qrename" old-patch new-patch
                  "--config" "extensions.mq=")))

(defun manky-qfold (patch)
  (manky-run-hg "qfold" patch
                "--config" "extensions.mq="))

(defun manky-qguard (patch)
  (let ((guards (manky-parse-args (read-string "Guards : "))))
    (apply #'manky-run-hg "qguard" patch
           "--config" "extensions.mq="
           "--" guards)))

(defun manky-qselect ()
  (interactive)
  (let ((guards (manky-parse-args (read-string "Guards : "))))
    (apply #'manky-run-hg "qselect"
           "--config" "extensions.mq="
           guards)))

(defun manky-qfinish (patch)
  (manky-run-hg "qfinish" patch
                "--config" "extensions.mq="))

(defun manky-qfinish-applied ()
  (interactive)
  (manky-run-hg "qfinish" "--applied"
                "--config" "extensions.mq="))

(defun manky-qreorder ()
  "Pop all patches and edit .hg/patches/series file to reorder them"
  (interactive)
  (let ((series (manky-patch-series-file)))
   (manky-qpop-all)
   (with-current-buffer (get-buffer-create manky-log-edit-buffer-name)
     (erase-buffer)
     (insert-file-contents series))
   (manky-pop-to-log-edit 'qreorder)))

(defun manky-queue-stage-all ()
  "Add all items in Changes to the staging area."
  (interactive)
  (manky-with-refresh
    (setq manky-queue-staged-all-files t)
    (manky-refresh-buffer)))

(defun manky-queue-unstage-all ()
  "Remove all items from the staging area"
  (interactive)
  (manky-with-refresh
    (setq manky-queue-staged-files '())
    (manky-refresh-buffer)))

(defun manky-qimport-item ()
  (interactive)
  (manky-section-action "qimport"
    ((log commits commit)
     (if (region-active-p)
	 (manky-qimport
	  (manky-section-info (manky-section-at (manky-next-sha1 (region-beginning))))
	  (manky-section-info (manky-section-at
			       (manky-previous-sha1 (- (region-end) 1)))))
       (manky-qimport (manky-section-info (manky-current-section)))))))

(defun manky-qpop-item ()
  (interactive)
  (manky-section-action "qpop"
    ((applied patch)
     (manky-qpop (manky-section-info (manky-current-section)))
     (manky-qpop))
    ((applied)
     (manky-qpop-all))
    ((staged diff)
     (manky-unstage-file (manky-section-title (manky-current-section)))
     (manky-queue-unstage-file (manky-section-title (manky-current-section)))
     (manky-refresh-buffer))
    ((staged)
     (manky-unstage-all)
     (manky-queue-unstage-all))
    ((queue-staged diff)
     (manky-unstage-file (manky-section-title (manky-current-section)))
     (manky-queue-unstage-file (manky-section-title (manky-current-section)))
     (manky-refresh-buffer))
    ((queue-staged)
     (manky-unstage-all)
     (manky-queue-unstage-all))))

(defun manky-qpush-item ()
  (interactive)
  (manky-section-action "qpush"
    ((unapplied patch)
     (manky-qpush (manky-section-info (manky-current-section))))
    ((unapplied)
     (manky-qpush-all))
    ((untracked file)
     (manky-run-hg "add" (manky-section-info (manky-current-section))))
    ((untracked)
     (manky-run-hg "add"))
    ((missing file)
     (manky-run-hg "remove" "--after" (manky-section-info (manky-current-section))))
    ((changes diff)
     (manky-stage-file (manky-section-title (manky-current-section)))
     (manky-queue-stage-file (manky-section-title (manky-current-section)))
     (manky-refresh-buffer))
    ((changes)
     (manky-stage-all)
     (manky-queue-stage-all))
    ((discarding diff)
     (manky-stage-file (manky-section-title (manky-current-section)))
     (manky-queue-stage-file (manky-section-title (manky-current-section)))
     (manky-refresh-buffer))
    ((discarding)
     (manky-stage-all)
     (manky-queue-stage-all))))

(defun manky-qremove-item ()
  (interactive)
  (manky-section-action "qremove"
    ((unapplied patch)
     (manky-qremove (manky-section-info (manky-current-section))))))

(defun manky-qrename-item ()
  (interactive)
  (manky-section-action "qrename"
    ((patch)
     (manky-qrename (manky-section-info (manky-current-section))))))

(defun manky-qfold-item ()
  (interactive)
  (manky-section-action "qfold"
    ((unapplied patch)
     (manky-qfold (manky-section-info (manky-current-section))))))

(defun manky-qguard-item ()
  (interactive)
  (manky-section-action "qguard"
    ((patch)
     (manky-qguard (manky-section-info (manky-current-section))))))

(defun manky-qfinish-item ()
  (interactive)
  (manky-section-action "qfinish"
    ((applied patch)
     (manky-qfinish (manky-section-info (manky-current-section))))))

(provide 'manky-queue)
