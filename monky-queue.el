;;; monky-queue.el --- Control Hg  -*- lexical-binding: t; -*-

;;; Queues

(require 'monky-section)

(defface monky-queue-patch
  '((t :weight bold :inherit (monky-header highlight)))
  "Face for patch name"
  :group 'monky-faces)

(defface monky-queue-active
  '((((class color) (background light))
     :box t
     :background "light green"
     :foreground "dark olive green")
    (((class color) (background dark))
     :box t
     :background "light green"
     :foreground "dark olive green"))
  "Face for active patch queue"
  :group 'monky-faces)

(defface monky-queue-positive-guard
  '((((class color) (background light))
     :box t
     :background "light green"
     :foreground "dark olive green")
    (((class color) (background dark))
     :box t
     :background "light green"
     :foreground "dark olive green"))
  "Face for queue postive guards"
  :group 'monky-faces)

(defface monky-queue-negative-guard
  '((((class color) (background light))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4")
    (((class color) (background dark))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4"))
  "Face for queue negative guards"
  :group 'monky-faces)

(defvar monky-queue-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "u") 'monky-qpop-item)
    (define-key map (kbd "U") 'monky-qpop-all)
    (define-key map (kbd "s") 'monky-qpush-item)
    (define-key map (kbd "S") 'monky-qpush-all)
    (define-key map (kbd "r") 'monky-qrefresh)
    (define-key map (kbd "R") 'monky-qrename-item)
    (define-key map (kbd "k") 'monky-qremove-item)
    (define-key map (kbd "N") 'monky-qnew)
    (define-key map (kbd "f") 'monky-qfinish-item)
    (define-key map (kbd "F") 'monky-qfinish-applied)
    (define-key map (kbd "d") 'monky-qfold-item)
    (define-key map (kbd "G") 'monky-qguard-item)
    (define-key map (kbd "o") 'monky-qreorder)
    (define-key map (kbd "A") 'monky-addremove-all)
    map))

;;; Queue mode
(define-minor-mode monky-queue-mode
  "Minor mode for hg Queue.

\\{monky-queue-mode-map}"
  :group monky
  :init-value ()
  :lighter ()
  :keymap monky-queue-mode-map)

(defun monky-queue-buffer-name (&optional dir)
  (format
   "monky-queue: %s"
   (file-name-nondirectory
    (directory-file-name (or dir default-directory)))))

(defvar-local monky-patches-dir ".hg/patches/")

(defun monky-patch-series-file ()
  (concat monky-patches-dir "series"))

(defun monky-insert-patch (patch inserter &rest args)
  (let ((p (point))
        (monky-hide-diffs nil))
    (save-restriction
      (narrow-to-region p p)
      (apply inserter args)
      (goto-char (point-max))
      (if (not (eq (char-before) ?\n))
          (insert "\n"))
      (goto-char p)
      (while (and (not (eobp)) (not (looking-at "^diff")))
        (monky-delete-line t))
      (when (looking-at "^diff")
        (monky-wash-diffs))
      (goto-char (point-max)))))

(defun monky-insert-guards (patch)
  (let ((guards (cl-remove-if
                 (lambda (guard) (string= "unguarded" guard))
                 (split-string
                  (cadr (split-string
                         (monky-hg-string "qguard" patch
                                          "--config" "extensions.mq=")
                         ":"))))))
    (dolist (guard guards)
      (insert (propertize " " 'face 'monky-queue-patch)
              (propertize guard
                          'face
                          (if (monky-string-starts-with-p guard "+")
                              'monky-queue-positive-guard
                            'monky-queue-negative-guard))))
    (insert (propertize "\n" 'face 'monky-queue-patch))))

(defun monky-wash-queue-patch ()
  (monky-wash-queue-insert-patch #'insert-file-contents))

(defvar monky-queue-staged-all-files nil)
(defvar-local monky-queue-staged-files nil)
(defvar-local monky-queue-old-staged-files nil)

(defun monky-wash-queue-discarding ()
  (monky-wash-status-lines
   (lambda (status file)
     (let ((monky-section-hidden-default monky-hide-diffs))
       (if (or monky-queue-staged-all-files
               (member file monky-old-staged-files)
               (member file monky-queue-old-staged-files))
           (monky-queue-stage-file file)
         (monky-with-section file 'diff
           (monky-insert-diff file status "qdiff"))))))
  (setq monky-queue-staged-all-files nil))

(defun monky-wash-queue-insert-patch (inserter)
  (if (looking-at "^\\([^\n]+\\)$")
      (let ((patch (match-string 1)))
        (monky-delete-line)
        (let ((monky-section-hidden-default t))
          (monky-with-section patch 'patch
            (monky-set-section-info patch)
            (insert
             (propertize (format "\t%s" patch) 'face 'monky-queue-patch))
            (monky-insert-guards patch)
            (funcall #'monky-insert-patch
                     patch inserter (concat monky-patches-dir patch))
            (forward-line)))
        t)
    nil))

(defun monky-wash-queue-queue ()
  (if (looking-at "^\\([^()\n]+\\)\\(\\s-+([^)]*)\\)?$")
      (let ((queue (match-string 1)))
        (monky-delete-line)
        (when (match-beginning 2)
          (setq monky-patches-dir
                (if (string= queue "patches")
                    ".hg/patches/"
                  (concat ".hg/patches-" queue "/")))
          (put-text-property 0 (length queue) 'face 'monky-queue-active queue))
        (monky-with-section queue 'queue
          (monky-set-section-info queue)
          (insert "\t" queue)
          (forward-line))
        t)
    nil))

(defun monky-wash-queue-queues ()
    (if (looking-at "^patches (.*)\n?\\'")
        (progn
          (monky-delete-line t)
          nil)
      (monky-wash-sequence #'monky-wash-queue-queue)))

(defun monky-wash-queue-patches ()
  (monky-wash-sequence #'monky-wash-queue-patch))

;;; Queues
(defun monky-insert-queue-queues ()
  (monky-hg-section 'queues "Patch Queues:"
                    #'monky-wash-queue-queues
                    "qqueue" "--list" "extensions.mq="))

;;; Applied Patches
(defun monky-insert-queue-applied ()
  (monky-hg-section 'applied "Applied Patches:" #'monky-wash-queue-patches
                    "qapplied" "--config" "extensions.mq="))

;;; UnApplied Patches
(defun monky-insert-queue-unapplied ()
  (monky-hg-section 'unapplied "UnApplied Patches:" #'monky-wash-queue-patches
                    "qunapplied" "--config" "extensions.mq="))

;;; Series
(defun monky-insert-queue-series ()
  (monky-hg-section 'qseries "Series:" #'monky-wash-queue-patches
                    "qseries" "--config" "extensions.mq="))

;;; Qdiff
(defun monky-insert-queue-discarding ()
  (when (monky-qtip-p)
    (setq monky-queue-old-staged-files (cl-copy-list monky-queue-staged-files))
    (setq monky-queue-staged-files '())
    (let ((monky-hide-diffs t))
      (monky-hg-section 'discarding "Discarding (qdiff):"
                        #'monky-wash-queue-discarding
                        "log" "--style" monky-hg-style-files-status
                        "--rev" "qtip"))))

(defun monky-insert-queue-staged-changes ()
  (when (and (monky-qtip-p)
             (or monky-queue-staged-files monky-staged-files))
    (monky-with-section 'queue-staged nil
      (insert (propertize "Staged changes (qdiff):"
                          'face 'monky-section-title) "\n")
      (let ((monky-section-hidden-default t))
        (dolist (file (delete-dups (cl-copy-list (append monky-queue-staged-files
                                                         monky-staged-files))))
          (monky-with-section file 'diff
            (monky-insert-diff file nil "qdiff")))))
    (insert "\n")))

(defun monky-wash-active-guards ()
  (if (looking-at "^no active guards")
      (monky-delete-line t)
    (monky-wash-sequence
     (lambda ()
       (let ((guard (buffer-substring (point) (point-at-eol))))
         (monky-delete-line)
         (insert " " (propertize guard 'face 'monky-queue-positive-guard))
         (forward-line))))))


;;; Active guards
(defun monky-insert-active-guards ()
  (monky-hg-section 'active-guards "Active Guards:" #'monky-wash-active-guards
                    "qselect" "--config" "extensions.mq="))

;;; Queue Staged Changes

(defun monky-queue-stage-file (file)
  (push file monky-queue-staged-files))

(defun monky-queue-unstage-file (file)
  (setq monky-queue-staged-files (delete file monky-queue-staged-files)))

(defun monky-refresh-queue-buffer ()
  (monky-create-buffer-sections
    (monky-with-section 'queue nil
      (monky-insert-untracked-files)
      (monky-insert-missing-files)
      (monky-insert-changes)
      (monky-insert-staged-changes)
      (monky-insert-queue-discarding)
      (monky-insert-queue-staged-changes)
      (monky-insert-queue-queues)
      (monky-insert-active-guards)
      (monky-insert-queue-applied)
      (monky-insert-queue-unapplied)
      (monky-insert-queue-series))))

(defun monky-queue ()
  (interactive)
  (monky-with-process
    (let ((topdir (monky-get-root-dir)))
      (pop-to-buffer (monky-queue-buffer-name))
      (monky-mode-init topdir 'queue #'monky-refresh-queue-buffer)
      (monky-queue-mode t))))

(defun monky-qqueue (queue)
  (monky-run-hg "qqueue"
                "--config" "extensions.mq="
                queue))

(defun monky-qpop (&optional patch)
  (interactive)
  (apply #'monky-run-hg
         "qpop"
         "--config" "extensions.mq="
         (if patch (list patch) '())))

(defun monky-qpush (&optional patch)
  (interactive)
  (apply #'monky-run-hg
         "qpush"
         "--config" "extensions.mq="
         (if patch (list patch) '())))

(defun monky-qpush-all ()
  (interactive)
  (monky-run-hg "qpush" "--all"
                "--config" "extensions.mq="))

(defun monky-qpop-all ()
  (interactive)
  (monky-run-hg "qpop" "--all"
                "--config" "extensions.mq="))

(defvar monky-log-edit-buffer-name "*monky-edit-log*"
  "Buffer name for composing commit messages.")

(defun monky-qrefresh ()
  (interactive)
  (if (not current-prefix-arg)
      (apply #'monky-run-hg "qrefresh"
             "--config" "extensions.mq="
             (append monky-staged-files monky-queue-staged-files))
    ;; get last commit message
    (with-current-buffer (get-buffer-create monky-log-edit-buffer-name)
      (monky-hg-insert
       (list "log" "--config" "extensions.mq="
             "--template" "{desc}" "-r" "-1")))
    (monky-pop-to-log-edit 'qrefresh)))

(defun monky-qremove (patch)
  (monky-run-hg "qremove" patch
                "--config" "extensions.mq="))

(defun monky-qnew (patch)
  (interactive (list (read-string "Patch Name : ")))
  (if (not current-prefix-arg)
      (monky-run-hg "qnew" patch
                    "--config" "extensions.mq=")
    (monky-pop-to-log-edit 'qnew patch)))

(defun monky-qinit ()
  (interactive)
  (monky-run-hg "qinit"
                "--config" "extensions.mq="))

(defun monky-qimport (node-1 &optional node-2)
  (monky-run-hg "qimport" "--rev"
                (if node-2 (concat node-1 ":" node-2) node-1)
                "--config" "extensions.mq="))

(defun monky-qrename (old-patch &optional new-patch)
  (let ((new-patch (or new-patch
                       (read-string "New Patch Name : "))))
    (monky-run-hg "qrename" old-patch new-patch
                  "--config" "extensions.mq=")))

(defun monky-qfold (patch)
  (monky-run-hg "qfold" patch
                "--config" "extensions.mq="))

(defun monky-qguard (patch)
  (let ((guards (monky-parse-args (read-string "Guards : "))))
    (apply #'monky-run-hg "qguard" patch
           "--config" "extensions.mq="
           "--" guards)))

(defun monky-qselect ()
  (interactive)
  (let ((guards (monky-parse-args (read-string "Guards : "))))
    (apply #'monky-run-hg "qselect"
           "--config" "extensions.mq="
           guards)))

(defun monky-qfinish (patch)
  (monky-run-hg "qfinish" patch
                "--config" "extensions.mq="))

(defun monky-qfinish-applied ()
  (interactive)
  (monky-run-hg "qfinish" "--applied"
                "--config" "extensions.mq="))

(defun monky-qreorder ()
  "Pop all patches and edit .hg/patches/series file to reorder them"
  (interactive)
  (let ((series (monky-patch-series-file)))
   (monky-qpop-all)
   (with-current-buffer (get-buffer-create monky-log-edit-buffer-name)
     (erase-buffer)
     (insert-file-contents series))
   (monky-pop-to-log-edit 'qreorder)))

(defun monky-queue-stage-all ()
  "Add all items in Changes to the staging area."
  (interactive)
  (monky-with-refresh
    (setq monky-queue-staged-all-files t)
    (monky-refresh-buffer)))

(defun monky-queue-unstage-all ()
  "Remove all items from the staging area"
  (interactive)
  (monky-with-refresh
    (setq monky-queue-staged-files '())
    (monky-refresh-buffer)))

(defun monky-qimport-item ()
  (interactive)
  (monky-section-action "qimport"
    ((log commits commit)
     (if (region-active-p)
	 (monky-qimport
	  (monky-section-info (monky-section-at (monky-next-sha1 (region-beginning))))
	  (monky-section-info (monky-section-at
			       (monky-previous-sha1 (- (region-end) 1)))))
       (monky-qimport (monky-section-info (monky-current-section)))))))

(defun monky-qpop-item ()
  (interactive)
  (monky-section-action "qpop"
    ((applied patch)
     (monky-qpop (monky-section-info (monky-current-section)))
     (monky-qpop))
    ((applied)
     (monky-qpop-all))
    ((staged diff)
     (monky-unstage-file (monky-section-title (monky-current-section)))
     (monky-queue-unstage-file (monky-section-title (monky-current-section)))
     (monky-refresh-buffer))
    ((staged)
     (monky-unstage-all)
     (monky-queue-unstage-all))
    ((queue-staged diff)
     (monky-unstage-file (monky-section-title (monky-current-section)))
     (monky-queue-unstage-file (monky-section-title (monky-current-section)))
     (monky-refresh-buffer))
    ((queue-staged)
     (monky-unstage-all)
     (monky-queue-unstage-all))))

(defun monky-qpush-item ()
  (interactive)
  (monky-section-action "qpush"
    ((unapplied patch)
     (monky-qpush (monky-section-info (monky-current-section))))
    ((unapplied)
     (monky-qpush-all))
    ((untracked file)
     (monky-run-hg "add" (monky-section-info (monky-current-section))))
    ((untracked)
     (monky-run-hg "add"))
    ((missing file)
     (monky-run-hg "remove" "--after" (monky-section-info (monky-current-section))))
    ((changes diff)
     (monky-stage-file (monky-section-title (monky-current-section)))
     (monky-queue-stage-file (monky-section-title (monky-current-section)))
     (monky-refresh-buffer))
    ((changes)
     (monky-stage-all)
     (monky-queue-stage-all))
    ((discarding diff)
     (monky-stage-file (monky-section-title (monky-current-section)))
     (monky-queue-stage-file (monky-section-title (monky-current-section)))
     (monky-refresh-buffer))
    ((discarding)
     (monky-stage-all)
     (monky-queue-stage-all))))

(defun monky-qremove-item ()
  (interactive)
  (monky-section-action "qremove"
    ((unapplied patch)
     (monky-qremove (monky-section-info (monky-current-section))))))

(defun monky-qrename-item ()
  (interactive)
  (monky-section-action "qrename"
    ((patch)
     (monky-qrename (monky-section-info (monky-current-section))))))

(defun monky-qfold-item ()
  (interactive)
  (monky-section-action "qfold"
    ((unapplied patch)
     (monky-qfold (monky-section-info (monky-current-section))))))

(defun monky-qguard-item ()
  (interactive)
  (monky-section-action "qguard"
    ((patch)
     (monky-qguard (monky-section-info (monky-current-section))))))

(defun monky-qfinish-item ()
  (interactive)
  (monky-section-action "qfinish"
    ((applied patch)
     (monky-qfinish (monky-section-info (monky-current-section))))))

(provide 'monky-queue)
