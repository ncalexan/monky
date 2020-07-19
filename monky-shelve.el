;;; monky-shelve.el --- Control Hg  -*- lexical-binding: t; -*-

;;; Shelves

;; TODO: use "shelved" instead of "shelf" and "shelves".

(defun monky-list-shelved ()
  (monky-hg-lines "shelve" "--list"))

(defun monky-shelved-at-point ()
  (magit-section-value-if 'shelf))

(defun monky-read-shelved (prompt)
  (let ((shelved (monky-list-shelved)))
    (car (split-string
          (completing-read prompt shelved nil t nil nil
                           (monky-shelved-at-point)
                           (car shelved))))))

(defun monky-show-shelf (name)
  (let ((buffer (get-buffer-create "*monky-shelf*"))
        (inhibit-read-only t))
    (pop-to-buffer buffer)

    (erase-buffer)
    (monky-hg-section
     nil nil
     #'ignore
     "shelve" "-l" "-p" name)
    (goto-char (point-min))
    (when (re-search-forward "^diff " nil t)
      (goto-char (line-beginning-position))
      (monky-wash-diffs))
    (monky-mode)))

(defun monky-delete-shelf (name)
  (unless (zerop (monky-hg-exit-code "shelve" "--delete" name))
    (user-error "Could not drop shelved %s" name))
  (monky-refresh-buffer))

(defun monky-insert-shelves ()
  (when (member "shelve" (monky-extensions))
    (monky-hg-section 'shelves "Shelves:" #'monky-wash-shelves
                      "shelve" "--list")))

(defun monky-wash-shelves ()
  "Set shelf names on each line.
This is naive and assumes that shelf names never contain (."
  (while (re-search-forward
          (rx bol (group (+? not-newline))
              (+ space) "(")
          nil
          t)
    (progn
      (goto-char (line-beginning-position))
      (monky-with-section 'shelf nil
        (monky-set-section-info (match-string 1))
        (put-text-property
         (match-beginning 1)
         (match-end 1)
         'face
         'monky-commit-id)
        (goto-char (line-end-position))))))

;;;###autoload (autoload 'monky-shelve "monky-shelve" nil t)
(define-transient-command monky-shelve ()
  "Shelve uncommitted changes."
  ;; TODO: think through message, name, etc.
  ;; ["Arguments"
  ;;  ("-u" "Also save untracked files" ("-u" "--include-untracked"))
  ;;  ("-a" "Also save untracked and ignored files" ("-a" "--all"))]
  [["Shelve"
    ("z" "both"          monky-shelve-both)
    ("i" "index"         monky-shelve-index)
    ;; ("w" "worktree"      monky-shelve-worktree)
    ;; ("x" "keeping index" monky-shelve-keep-index)
    ]
   ["Snapshot"
    ("Z" "both"          monky-snapshot-both)
    ("I" "index"         monky-snapshot-index)
    ;; ("W" "worktree"      monky-snapshot-worktree)
    ;; ("r" "to wip ref"    monky-wip-commit)
    ]
   ;; ["Use"
   ;;  ("a" "Apply"         monky-shelve-apply)
   ;;  ("p" "Pop"           monky-shelve-pop)
   ;;  ("k" "Drop"          monky-shelve-drop)]
   ["Inspect"
    ("l" "List"          monky-shelve-list)
    ("v" "Show"          monky-shelve-show)]
   ;; ["Transform"
   ;;  ("b" "Branch"        monky-shelve-branch)
   ;;  ("B" "Branch here"   monky-shelve-branch-here)
   ;;  ("f" "Format patch"  monky-shelve-format-patch)]
   ])

(defun monky-shelve-arguments ()
  (transient-args 'monky-shelve))

(defun monky-hg-shelve (args)
  "Run hg shelve."
    (apply #'monky-run-hg-async "shelve" args))

(defun monky-shelve-both (args)
  (interactive (list
                (monky-shelve-arguments)))
  (monky-hg-shelve args))

(defun monky-shelve-index (args)
  (interactive (list
                (monky-shelve-arguments)))
  (when (not monky-staged-files)
    (if (y-or-n-p "Nothing staged. Stage and commit all changes? ")
        (monky-stage-all)
      (user-error "Nothing staged")))
  (monky-hg-shelve (append args (list "--") monky-staged-files nil)))

(defun monky-snapshot-both (args)
  (interactive (list
                (monky-shelve-arguments)))
  (monky-hg-shelve (append args (list "--keep"))))

(defun monky-snapshot-index (args)
  (interactive (list
                (monky-shelve-arguments)))
  (when (not monky-staged-files)
    (if (y-or-n-p "Nothing staged. Stage and commit all changes? ")
        (monky-stage-all)
      (user-error "Nothing staged")))
  (monky-hg-shelve (append args (list "--keep" "--") monky-staged-files nil)))

(defun monky-shelve-list (args)
  (interactive (list
                (monky-shelve-arguments)))
  (monky-hg-shelve args))

(defun monky-shelve-show (shelved args)
  (interactive (list
                (or (and (not current-prefix-arg)
                         (monky-shelved-at-point))
                    (monky-read-shelved "Show shelved"))
                (monky-shelve-arguments)))
  (monky-show-shelf shelved))

(provide 'monky-shelve)
