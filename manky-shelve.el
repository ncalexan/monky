;;; manky-shelve.el --- Control Hg  -*- lexical-binding: t; -*-

;;; Shelves

;; TODO: use "shelved" instead of "shelf" and "shelves".

(require 'manky-section)

(defun manky-list-shelved ()
  (manky-hg-lines "shelve" "--list"))

(defun manky-shelved-at-point ()
  (magit-section-value-if 'shelf))

(defun manky-read-shelved (prompt)
  (let ((shelved (manky-list-shelved)))
    (car (split-string
          (completing-read prompt shelved nil t nil nil
                           (manky-shelved-at-point)
                           (car shelved))))))

(defun manky-show-shelf (name)
  (let ((buffer (get-buffer-create "*manky-shelf*"))
        (inhibit-read-only t))
    (pop-to-buffer buffer)

    (erase-buffer)
    (manky-hg-section
     nil nil
     #'ignore
     "shelve" "-l" "-p" name)
    (goto-char (point-min))
    (when (re-search-forward "^diff " nil t)
      (goto-char (line-beginning-position))
      (manky-wash-diffs))
    (manky-mode)))

(defun manky-delete-shelf (name)
  (unless (zerop (manky-hg-exit-code "shelve" "--delete" name))
    (user-error "Could not drop shelved %s" name))
  (manky-refresh-buffer))

(defun manky-insert-shelves ()
  (when (member "shelve" (manky-extensions))
    (manky-hg-section 'shelves "Shelves:" #'manky-wash-shelves
                      "shelve" "--list")))

(defun manky-wash-shelves ()
  "Set shelf names on each line.
This is naive and assumes that shelf names never contain (."
  (while (re-search-forward
          (rx bol (group (+? not-newline))
              (+ space) "(")
          nil
          t)
    (progn
      (goto-char (line-beginning-position))
      (manky-with-section 'shelf nil
        (manky-set-section-info (match-string 1))
        (put-text-property
         (match-beginning 1)
         (match-end 1)
         'face
         'manky-commit-id)
        (goto-char (line-end-position))))))

;;;###autoload (autoload 'manky-shelve "manky-shelve" nil t)
(define-transient-command manky-shelve ()
  "Shelve uncommitted changes."
  ;; TODO: think through message, name, etc.
  ;; ["Arguments"
  ;;  ("-u" "Also save untracked files" ("-u" "--include-untracked"))
  ;;  ("-a" "Also save untracked and ignored files" ("-a" "--all"))]
  [["Shelve"
    ("z" "both"          manky-shelve-both)
    ("i" "index"         manky-shelve-index)
    ;; ("w" "worktree"      manky-shelve-worktree)
    ;; ("x" "keeping index" manky-shelve-keep-index)
    ]
   ["Snapshot"
    ("Z" "both"          manky-snapshot-both)
    ("I" "index"         manky-snapshot-index)
    ;; ("W" "worktree"      manky-snapshot-worktree)
    ;; ("r" "to wip ref"    manky-wip-commit)
    ]
   ;; ["Use"
   ;;  ("a" "Apply"         manky-shelve-apply)
   ;;  ("p" "Pop"           manky-shelve-pop)
   ;;  ("k" "Drop"          manky-shelve-drop)]
   ["Inspect"
    ("l" "List"          manky-shelve-list)
    ("v" "Show"          manky-shelve-show)]
   ;; ["Transform"
   ;;  ("b" "Branch"        manky-shelve-branch)
   ;;  ("B" "Branch here"   manky-shelve-branch-here)
   ;;  ("f" "Format patch"  manky-shelve-format-patch)]
   ])

(defun manky-shelve-arguments ()
  (transient-args 'manky-shelve))

(defun manky-hg-shelve (args)
  "Run hg shelve."
    (apply #'manky-run-hg-async "shelve" args))

(defun manky-shelve-both (args)
  (interactive (list
                (manky-shelve-arguments)))
  (manky-hg-shelve args))

(defun manky-shelve-index (args)
  (interactive (list
                (manky-shelve-arguments)))
  (when (not manky-staged-files)
    (if (y-or-n-p "Nothing staged. Stage and commit all changes? ")
        (manky-stage-all)
      (user-error "Nothing staged")))
  (manky-hg-shelve (append args (list "--") manky-staged-files nil)))

(defun manky-snapshot-both (args)
  (interactive (list
                (manky-shelve-arguments)))
  (manky-hg-shelve (append args (list "--keep"))))

(defun manky-snapshot-index (args)
  (interactive (list
                (manky-shelve-arguments)))
  (when (not manky-staged-files)
    (if (y-or-n-p "Nothing staged. Stage and commit all changes? ")
        (manky-stage-all)
      (user-error "Nothing staged")))
  (manky-hg-shelve (append args (list "--keep" "--") manky-staged-files nil)))

(defun manky-shelve-list (args)
  (interactive (list
                (manky-shelve-arguments)))
  (manky-hg-shelve args))

(defun manky-shelve-show (shelved args)
  (interactive (list
                (or (and (not current-prefix-arg)
                         (manky-shelved-at-point))
                    (manky-read-shelved "Show shelved"))
                (manky-shelve-arguments)))
  (manky-show-shelf shelved))

(provide 'manky-shelve)
