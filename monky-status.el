;;; monky-status.el --- the grand overview  -*- lexical-binding: t -*-

;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'magit-section)
(require 'monky-mode)

;;; Options

(defgroup monky-status nil
  "Inspect and manipulate Mercurial repositories."
  :link '(info-link "(monky)Status Buffer")
  :group 'monky-modes)

(defcustom monky-status-mode-hook nil
  "Hook run after entering Monky-Status mode."
  :group 'monky-status
  :type 'hook)

(defcustom monky-status-headers-hook
  '(monky-insert-error-header
    monky-insert-parents-header
    ;; monky-insert-diff-filter-header
    ;; monky-insert-head-branch-header
    ;; monky-insert-upstream-branch-header
    ;; monky-insert-push-branch-header
    ;; monky-insert-tags-header
    )
  "Hook run to insert headers into the status buffer.

This hook is run by `monky-insert-status-headers', which in turn
has to be a member of `monky-status-sections-hook' to be used at
all."
  :package-version '(monky . "0.3")
  :group 'monky-status
  :type 'hook
  :options '(monky-insert-error-header
             monky-insert-parents-header
             ;; monky-insert-diff-filter-header
             ;; monky-insert-repo-header
             ;; monky-insert-remote-header
             ;; monky-insert-head-branch-header
             ;; monky-insert-upstream-branch-header
             ;; monky-insert-push-branch-header
             ;; monky-insert-tags-header
             ))

(defcustom monky-status-sections-hook
  '(monky-insert-status-headers
    monky-insert-changes
    ;; monky-insert-merge-log
    ;; monky-insert-rebase-sequence
    ;; monky-insert-am-sequence
    ;; monky-insert-sequencer-sequence
    ;; monky-insert-bisect-output
    ;; monky-insert-bisect-rest
    ;; monky-insert-bisect-log
    ;; monky-insert-untracked-files
    ;; monky-insert-unstaged-changes
    ;; monky-insert-staged-changes
    ;; monky-insert-stashes
    ;; monky-insert-unpushed-to-pushremote
    ;; monky-insert-unpushed-to-upstream-or-recent
    ;; monky-insert-unpulled-from-pushremote
    ;; monky-insert-unpulled-from-upstream
    )
  "Hook run to insert sections into a status buffer."
  :package-version '(monky . "0.3")
  :group 'monky-status
  :type 'hook)

;;;###autoload
(defun monky-status (&optional directory cache)
  "Show the status of the current Mercurial repository in a buffer.

If the current directory isn't located within Mercurial Git repository,
then prompt for an existing repository or an arbitrary directory,
depending on option `monky-repository-directories', and show the
status of the selected repository instead.

* If that option specifies any existing repositories, then offer
  those for completion and show the status buffer for the
  selected one.

* Otherwise read an arbitrary directory using regular file-name
  completion.  If the selected directory is the top-level of an
  existing working tree, then show the status buffer for that.

* Otherwise offer to initialize the selected directory as a new
  repository.  After creating the repository show its status
  buffer.

These fallback behaviors can also be forced using one or more
prefix arguments:

* With two prefix arguments (or more precisely a numeric prefix
  value of 16 or greater) read an arbitrary directory and act on
  it as described above.  The same could be accomplished using
  the command `monky-init'.

* With a single prefix argument read an existing repository, or
  if none can be found based on `monky-repository-directories',
  then fall back to the same behavior as with two prefix
  arguments."
  (interactive)
  ;;  (let ((monky--refresh-cache (list (cons 0 0))))
  ;;    (list (and (or current-prefix-arg (not (monky-toplevel)))
  ;;               (monky-read-repository
  ;;                (>= (prefix-numeric-value current-prefix-arg) 16)))
  ;;          monky--refresh-cache)))
  ;; (let ((magit--refresh-cache (or cache (list (cons 0 0)))))
  ;;   (if directory
  ;;       (let ((toplevel (magit-toplevel directory)))
  ;;         (setq directory (file-name-as-directory
  ;;                          (expand-file-name directory)))
  ;;         (if (and toplevel (file-equal-p directory toplevel))
  ;;             (magit-status-setup-buffer directory)
  ;;           (when (y-or-n-p
  ;;                  (if toplevel
  ;;                      (format "%s is a repository.  Create another in %s? "
  ;;                              toplevel directory)
  ;;                    (format "Create repository in %s? " directory)))
  ;;             ;; Creating a new repository invalidates cached values.
  ;;             (setq magit--refresh-cache nil)
  ;;             (magit-init directory))))
  (monky-status-setup-buffer default-directory)
  )
;; ))

(put 'monky-status 'interactive-only 'monky-status-setup-buffer)

;;;###autoload
(defalias 'monky 'monky-status
  "An alias for `monky-status' for better discoverability.

Instead of invoking this alias for `monky-status' using
\"M-x monky RET\", you should bind a key to `monky-status'
and read the info node `(monky)Getting Started', which
also contains other useful hints.")

;;; Mode

(defvar monky-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map monky-mode-map)
    ;; (define-key map "j" 'monky-status-jump)
    ;; (define-key map [remap dired-jump] 'monky-dired-jump)
    map)
  "Keymap for `monky-status-mode'.")

(define-derived-mode monky-status-mode monky-mode "Monky"
  "Mode for looking at Mercurial status.

This mode is documented in info node `(monky)Status Buffer'.

\\<monky-mode-map>\
Type \\[monky-refresh] to refresh the current buffer.
Type \\[monky-section-toggle] to expand or hide the section at point.
Type \\[monky-visit-thing] to visit the change or commit at point.

Type \\[monky-dispatch] to invoke major commands.

Staging and applying changes is documented in info node
`(monky)Staging and Unstaging' and info node `(monky)Applying'.

\\<monky-hunk-section-map>Type \
\\[monky-apply] to apply the change at point, \
\\[monky-stage] to stage,
\\[monky-unstage] to unstage, \
\\[monky-discard] to discard, or \
\\[monky-reverse] to reverse it.

\\<monky-status-mode-map>\
Type \\[monky-commit] to create a commit.

\\{monky-status-mode-map}"
  :group 'monky-status
  (hack-dir-local-variables-non-file-buffer)
  ;; (setq imenu-create-index-function
  ;;       'monky-imenu--status-create-index-function)
  )


;;;###autoload
(defun monky-status-setup-buffer (&optional directory)
  (unless directory
    (setq directory default-directory))
  ;; (magit--tramp-asserts directory)
  (let* ((default-directory directory)
         ;; (d (magit-diff--get-value 'magit-status-mode))
         ;; (l (magit-log--get-value  'magit-status-mode))
         ;; (file (and magit-status-goto-file-position
         ;;            (magit-file-relative-name)))
         ;; (line (and file (line-number-at-pos)))
         ;; (col  (and file (current-column)))
         (buf  (monky-setup-buffer #'monky-status-mode nil
                 ;; (magit-buffer-diff-args  (nth 0 d))
                 ;; (magit-buffer-diff-files (nth 1 d))
                 ;; (magit-buffer-log-args   (nth 0 l))
                 ;; (magit-buffer-log-files  (nth 1 l))
                 )))
    ;; (when file
    ;;   (with-current-buffer buf
    ;;     (let ((staged (magit-get-section '((staged) (status)))))
    ;;       (if (and staged
    ;;                (cadr (magit-diff--locate-hunk file line staged)))
    ;;           (magit-diff--goto-position file line col staged)
    ;;         (let ((unstaged (magit-get-section '((unstaged) (status)))))
    ;;           (unless (and unstaged
    ;;                        (magit-diff--goto-position file line col unstaged))
    ;;             (when staged
    ;;               (magit-diff--goto-position file line col staged))))))))
    buf))

(defun monky-status-refresh-buffer ()
  ;; (monky-git-exit-code "update-index" "--refresh")
  (magit-insert-section (status)
    (magit-run-section-hook 'monky-status-sections-hook)))

(defun monky-insert-status-headers ()
  "Insert header sections appropriate for `monky-status-mode' buffers.
The sections are inserted by running the functions on the hook
`monky-status-headers-hook'."
  (if t ;; (magit-rev-verify "HEAD")
      (magit-insert-headers 'monky-status-headers-hook)
    ;; (insert "In the beginning there was darkness\n\n")
    ))

(defun monky-insert-error-header ()
  "Insert the message about the Git error that just occurred.

This function is only aware of the last error that occur when Git
was run for side-effects.  If, for example, an error occurs while
generating a diff, then that error won't be inserted.  Refreshing
the status buffer causes this section to disappear again."
  ;; (when monky-this-error
  (magit-insert-section (error 'hg)
      (insert (propertize (format "%-10s" "HgError! ")
                          'font-lock-face 'magit-section-heading))
      (insert (propertize "test" ;; monky-this-error
                          'font-lock-face 'font-lock-warning-face))
      ;; (when-let ((key (car (where-is-internal 'monky-process-buffer))))
      ;;   (insert (format "  [Type `%s' for details]" (key-description key))))
      (insert ?\n))

  (magit-insert-section (error2 'hg)
    (insert (propertize (format "%-10s" "HgError2! ")
                        'font-lock-face 'magit-section-heading))
    (insert (propertize "test2" ;; monky-this-error
                        'font-lock-face 'font-lock-warning-face))
    ;; (when-let ((key (car (where-is-internal 'monky-process-buffer))))
    ;;   (insert (format "  [Type `%s' for details]" (key-description key))))
    (insert ?\n))
  ;; (setq monky-this-error nil))
  )

(defun monky-insert-section (section-title-and-type buffer-title washer cmd &rest args)
  "Run CMD and put its result in a new section.

SECTION-TITLE-AND-TYPE is either a string that is the title of the section
or (TITLE . TYPE) where TITLE is the title of the section and TYPE is its type.

If there is no type, or if type is nil, the section won't be highlighted.

BUFFER-TITLE is the inserted title of the section

WASHER is a function that will be run after CMD.
The buffer will be narrowed to the inserted text.
It should add sectioning as needed for monky interaction

CMD is either a string, an external command that will be run with
ARGS as arguments; or a function, that will be applied with ARGS
as arguments."
  (monky-with-process
    (let* ((body-beg nil)
           (section-title (if (consp section-title-and-type)
                              (car section-title-and-type)
                            section-title-and-type))
           (section-type (if (consp section-title-and-type)
                             (cdr section-title-and-type)
                           nil))
           (section (magit-insert-section ((eval section-title)) ;; TODO: value.
                      ;; section-title
                      (if buffer-title
                          (magit-insert-heading buffer-title))
                      (setq body-beg (point))
                      (if (functionp cmd)
                          (apply cmd args)
                        (apply 'monky-process-file cmd nil t nil args))
                      (if (not (eq (char-before) ?\n))
                          (insert "\n"))
                      (if washer
                          (save-restriction
                            (narrow-to-region body-beg (point))
                            (goto-char (point-min))
                            (funcall washer)
                            (goto-char (point-max))))
                      (when (= body-beg (point))
                        (magit-cancel section)))))
      section)))

(defun monky-hg-section (section-title-and-type buffer-title washer &rest args)
  (apply #'monky-insert-section
         section-title-and-type
         buffer-title
         washer
         monky-hg-executable
         (append monky-hg-standard-options args)))

(defun monky-wash-json-status ()
  ;; (let ((p (point))
  ;;       (j (json-read)))
  ;;   (delete-region p (point))
  (let* ((s (monky-hg-output
             (list "--config" "commands.status.verbose=1" "status" "-Tjson")))
         (j (json-read-from-string s)))
    (seq-let (untracked missing changes)
        (cl-loop for item across j
                 when (s-equals? (alist-get 'status item) "?")
                 collect item into untracked
                 when (s-equals? (alist-get 'status item) "!")
                 collect item into missing
                 when (or (and (not (alist-get 'status item))
                               (alist-get 'path item))
                          (-contains? '("M" "A" "R" "U") (alist-get 'status item)))
                 collect item into changes
                 finally return (list untracked missing changes))

      (when untracked
        (magit-insert-section (untracked)
          (magit-insert-heading "Untracked files:")
          (cl-loop for item in untracked
                   do
                   ;; (message "item %s" item)
                   (let ((file (alist-get 'path item)))
                     (magit-insert-section (file file)
                       (insert (propertize file 'font-lock-face 'monky-log-sha1) ?\n))) ;; TODO: faces for files and directories.
                   )))

      (when missing
        (magit-insert-section (missing)
          (magit-insert-heading "Missing files:")
          (cl-loop for item in missing
                   do
                   (let ((file (alist-get 'path item)))
                     (magit-insert-section (file file)
                       (insert (propertize file 'font-lock-face 'monky-log-sha1) ?\n))) ;; TODO: faces for files and directories.
                   )))

      (when changes
        (magit-insert-section (changes)
          (magit-insert-heading "Changes:")
          (cl-loop for item in changes
                   do
                   (let ((file (alist-get 'path item)))
                     (magit-insert-section (file file)
                       (insert
                        (format
                         "%s   %s\n"
                         (or (and (alist-get 'resolved item)   "resolved  ")
                             (and (alist-get 'unresolved item) "unresolved")
                             (alist-get 'status item))
                         file)))))))
      )))

    ;; (monky-with-section "Unfinished:" 'unfinished
    ;;   (cl-loop for item across j
    ;;            when (alist-get 'unfinished item)
    ;;            do
    ;;            ;; (message "item %s" item)
    ;;            (insert
    ;;             (format
    ;;              "%s   %s\n"
    ;;              (alist-get 'unfinished item)
    ;;              (alist-get 'unfinishedmsg item)))))
;; ))

;; (defun monky-refresh-status ()
  ;; (setq monky-parents nil
  ;;       monky-merged-files nil)
  ;; (monky-create-buffer-sections
  ;;   (monky-with-section 'status nil
  ;;     (monky-insert-parents)

(defun monky-wash-parent ()
  (let ((p (point)))
    (forward-line)
    (delete-region p (point)))
  )

(defun monky-insert-parents-header ()
  (monky-hg-section 'parent "Parent"
                    #'monky-wash-parent
                    "parent")

  (monky-hg-section 'parent "Parent"
                    #'monky-wash-parent
                    "parent")
)

(defun monky-insert-changes ()
  (monky-insert-parents-header)
  ;; (monky-wash-json-status)

  ;; (monky-hg-section 'changes "Changes"
  ;;                   #'monky-wash-json-status
  ;;                   "--config" "commands.status.verbose=1"
  ;;                   "status" "-Tjson")
)

;;; _
(provide 'monky-status)
;;; monky-status.el ends here
