;;; manky-status.el --- the grand overview  -*- lexical-binding: t -*-

;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'magit-section)
(require 'manky-mode)

;;; Options

(defgroup manky-status nil
  "Inspect and manipulate Mercurial repositories."
  :link '(info-link "(manky)Status Buffer")
  :group 'manky-modes)

(defcustom manky-status-mode-hook nil
  "Hook run after entering Manky-Status mode."
  :group 'manky-status
  :type 'hook)

(defcustom manky-status-headers-hook
  '(manky-insert-error-header
    manky-insert-parents-header
    ;; manky-insert-diff-filter-header
    ;; manky-insert-head-branch-header
    ;; manky-insert-upstream-branch-header
    ;; manky-insert-push-branch-header
    ;; manky-insert-tags-header
    )
  "Hook run to insert headers into the status buffer.

This hook is run by `manky-insert-status-headers', which in turn
has to be a member of `manky-status-sections-hook' to be used at
all."
  :package-version '(manky . "0.3")
  :group 'manky-status
  :type 'hook
  :options '(manky-insert-error-header
             manky-insert-parents-header
             ;; manky-insert-diff-filter-header
             ;; manky-insert-repo-header
             ;; manky-insert-remote-header
             ;; manky-insert-head-branch-header
             ;; manky-insert-upstream-branch-header
             ;; manky-insert-push-branch-header
             ;; manky-insert-tags-header
             ))

(defcustom manky-status-sections-hook
  '(manky-insert-status-headers
    manky-insert-changes
    ;; manky-insert-merge-log
    ;; manky-insert-rebase-sequence
    ;; manky-insert-am-sequence
    ;; manky-insert-sequencer-sequence
    ;; manky-insert-bisect-output
    ;; manky-insert-bisect-rest
    ;; manky-insert-bisect-log
    ;; manky-insert-untracked-files
    ;; manky-insert-unstaged-changes
    ;; manky-insert-staged-changes
    ;; manky-insert-stashes
    ;; manky-insert-unpushed-to-pushremote
    ;; manky-insert-unpushed-to-upstream-or-recent
    ;; manky-insert-unpulled-from-pushremote
    ;; manky-insert-unpulled-from-upstream
    )
  "Hook run to insert sections into a status buffer."
  :package-version '(manky . "0.3")
  :group 'manky-status
  :type 'hook)

;;;###autoload
(defun manky-status (&optional directory cache)
  "Show the status of the current Mercurial repository in a buffer.

If the current directory isn't located within Mercurial Git repository,
then prompt for an existing repository or an arbitrary directory,
depending on option `manky-repository-directories', and show the
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
  the command `manky-init'.

* With a single prefix argument read an existing repository, or
  if none can be found based on `manky-repository-directories',
  then fall back to the same behavior as with two prefix
  arguments."
  (interactive)
  ;;  (let ((manky--refresh-cache (list (cons 0 0))))
  ;;    (list (and (or current-prefix-arg (not (manky-toplevel)))
  ;;               (manky-read-repository
  ;;                (>= (prefix-numeric-value current-prefix-arg) 16)))
  ;;          manky--refresh-cache)))
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
  (manky-status-setup-buffer default-directory)
  )
;; ))

(put 'manky-status 'interactive-only 'manky-status-setup-buffer)

;;;###autoload
(defalias 'manky 'manky-status
  "An alias for `manky-status' for better discoverability.

Instead of invoking this alias for `manky-status' using
\"M-x manky RET\", you should bind a key to `manky-status'
and read the info node `(manky)Getting Started', which
also contains other useful hints.")

;;; Mode

(defvar manky-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map manky-mode-map)
    ;; (define-key map "j" 'manky-status-jump)
    ;; (define-key map [remap dired-jump] 'manky-dired-jump)
    map)
  "Keymap for `manky-status-mode'.")

(define-derived-mode manky-status-mode manky-mode "Manky"
  "Mode for looking at Mercurial status.

This mode is documented in info node `(manky)Status Buffer'.

\\<manky-mode-map>\
Type \\[manky-refresh] to refresh the current buffer.
Type \\[manky-section-toggle] to expand or hide the section at point.
Type \\[manky-visit-thing] to visit the change or commit at point.

Type \\[manky-dispatch] to invoke major commands.

Staging and applying changes is documented in info node
`(manky)Staging and Unstaging' and info node `(manky)Applying'.

\\<manky-hunk-section-map>Type \
\\[manky-apply] to apply the change at point, \
\\[manky-stage] to stage,
\\[manky-unstage] to unstage, \
\\[manky-discard] to discard, or \
\\[manky-reverse] to reverse it.

\\<manky-status-mode-map>\
Type \\[manky-commit] to create a commit.

\\{manky-status-mode-map}"
  :group 'manky-status
  (hack-dir-local-variables-non-file-buffer)
  ;; (setq imenu-create-index-function
  ;;       'manky-imenu--status-create-index-function)
  )


;;;###autoload
(defun manky-status-setup-buffer (&optional directory)
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
         (buf  (manky-setup-buffer #'manky-status-mode nil
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

(defun manky-status-refresh-buffer ()
  ;; (manky-git-exit-code "update-index" "--refresh")
  (magit-insert-section (status)
    (magit-run-section-hook 'manky-status-sections-hook)))

(defun manky-insert-status-headers ()
  "Insert header sections appropriate for `manky-status-mode' buffers.
The sections are inserted by running the functions on the hook
`manky-status-headers-hook'."
  (if t ;; (magit-rev-verify "HEAD")
      (magit-insert-headers 'manky-status-headers-hook)
    ;; (insert "In the beginning there was darkness\n\n")
    ))

(defun manky-insert-error-header ()
  "Insert the message about the Git error that just occurred.

This function is only aware of the last error that occur when Git
was run for side-effects.  If, for example, an error occurs while
generating a diff, then that error won't be inserted.  Refreshing
the status buffer causes this section to disappear again."
  ;; (when manky-this-error
  (magit-insert-section (error 'hg)
      (insert (propertize (format "%-10s" "HgError! ")
                          'font-lock-face 'magit-section-heading))
      (insert (propertize "test" ;; manky-this-error
                          'font-lock-face 'font-lock-warning-face))
      ;; (when-let ((key (car (where-is-internal 'manky-process-buffer))))
      ;;   (insert (format "  [Type `%s' for details]" (key-description key))))
      (insert ?\n))

  (magit-insert-section (error2 'hg)
    (insert (propertize (format "%-10s" "HgError2! ")
                        'font-lock-face 'magit-section-heading))
    (insert (propertize "test2" ;; manky-this-error
                        'font-lock-face 'font-lock-warning-face))
    ;; (when-let ((key (car (where-is-internal 'manky-process-buffer))))
    ;;   (insert (format "  [Type `%s' for details]" (key-description key))))
    (insert ?\n))
  ;; (setq manky-this-error nil))
  )

(defun manky-insert-section (section-title-and-type buffer-title washer cmd &rest args)
  "Run CMD and put its result in a new section.

SECTION-TITLE-AND-TYPE is either a string that is the title of the section
or (TITLE . TYPE) where TITLE is the title of the section and TYPE is its type.

If there is no type, or if type is nil, the section won't be highlighted.

BUFFER-TITLE is the inserted title of the section

WASHER is a function that will be run after CMD.
The buffer will be narrowed to the inserted text.
It should add sectioning as needed for manky interaction

CMD is either a string, an external command that will be run with
ARGS as arguments; or a function, that will be applied with ARGS
as arguments."
  (manky-with-process
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
                        (apply 'manky-process-file cmd nil t nil args))
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

(defun manky-hg-section (section-title-and-type buffer-title washer &rest args)
  (apply #'manky-insert-section
         section-title-and-type
         buffer-title
         washer
         manky-hg-executable
         (append manky-hg-standard-options args)))

(defun manky-wash-json-status ()
  ;; (let ((p (point))
  ;;       (j (json-read)))
  ;;   (delete-region p (point))
  (let* ((s (manky-hg-output
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
                       (insert (propertize file 'font-lock-face 'manky-log-sha1) ?\n))) ;; TODO: faces for files and directories.
                   )))

      (when missing
        (magit-insert-section (missing)
          (magit-insert-heading "Missing files:")
          (cl-loop for item in missing
                   do
                   (let ((file (alist-get 'path item)))
                     (magit-insert-section (file file)
                       (insert (propertize file 'font-lock-face 'manky-log-sha1) ?\n))) ;; TODO: faces for files and directories.
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

    ;; (manky-with-section "Unfinished:" 'unfinished
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

;; (defun manky-refresh-status ()
  ;; (setq manky-parents nil
  ;;       manky-merged-files nil)
  ;; (manky-create-buffer-sections
  ;;   (manky-with-section 'status nil
  ;;     (manky-insert-parents)

(defun manky-wash-parent ()
  (let ((p (point)))
    (forward-line)
    (delete-region p (point)))
  )

(defun manky-insert-parents-header ()
  (manky-hg-section 'parent "Parent"
                    #'manky-wash-parent
                    "parent")

  (manky-hg-section 'parent "Parent"
                    #'manky-wash-parent
                    "parent")
)

(defun manky-insert-changes ()
  (manky-insert-parents-header)
  ;; (manky-wash-json-status)

  ;; (manky-hg-section 'changes "Changes"
  ;;                   #'manky-wash-json-status
  ;;                   "--config" "commands.status.verbose=1"
  ;;                   "status" "-Tjson")
)

;;; _
(provide 'manky-status)
;;; manky-status.el ends here
