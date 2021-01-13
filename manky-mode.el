;;; manky-mode.el --- create and refresh Manky buffers  -*- lexical-binding: t -*-

;;; Commentary:

;; This library implements the abstract major-mode `manky-mode' from
;; which almost all other Manky major-modes derive.  The code in here
;; is mostly concerned with creating and refreshing Manky buffers.

;;; Code:

(require 'cl-lib)
(require 'dash)

(eval-when-compile
  (require 'subr-x))

(require 'transient)

(require 'magit-section)
(require 'manky-hg)

;; ;; For `magit-display-buffer-fullcolumn-most-v1' from `git-commit'
;; (defvar git-commit-mode)
;; ;; For `magit-refresh'
;; (defvar magit-post-commit-hook-commands)
;; (defvar magit-post-stage-hook-commands)
;; (defvar magit-post-unstage-hook-commands)
;; ;; For `magit-refresh' and `magit-refresh-all'
;; (declare-function magit-auto-revert-buffers "magit-autorevert" ())
;; ;; For `magit-refresh-buffer'
;; (declare-function magit-process-unset-mode-line-error-status "magit-process" ())
;; ;; For `magit-refresh-get-relative-position'
;; (declare-function magit-hunk-section-p "magit-diff" (obj))
;; ;; For `magit-mode-setup-internal'
;; (declare-function magit-status-goto-initial-section "magit-status" ())
;; ;; For `magit-mode' from `bookmark'
;; (defvar bookmark-make-record-function)

(require 'format-spec)
(require 'help-mode)

;;; Options

(defcustom manky-mode-hook
  '(;; manky-load-config-extensions
    )
  "Hook run when entering a mode derived from Manky mode."
  :package-version '(manky . "3.0.0")
  :group 'manky-modes
  :type 'hook
  :options '(;; manky-load-config-extensions
             bug-reference-mode))

(defcustom manky-setup-buffer-hook
  '(manky-maybe-save-repository-buffers
    manky-set-buffer-margin)
  "Hook run by `manky-setup-buffer'.

This is run right after displaying the buffer and right before
generating or updating its content.  `manky-mode-hook' and other,
more specific, `manky-mode-*-hook's on the other hand are run
right before displaying the buffer.  Usually one of these hooks
should be used instead of this one."
  :package-version '(manky . "2.3.0")
  :group 'manky-modes
  :type 'hook
  :options '(manky-maybe-save-repository-buffers
             manky-set-buffer-margin))

(defcustom manky-pre-refresh-hook '(manky-maybe-save-repository-buffers)
  "Hook run before refreshing in `manky-refresh'.

This hook, or `manky-post-refresh-hook', should be used
for functions that are not tied to a particular buffer.

To run a function with a particular buffer current, use
`manky-refresh-buffer-hook' and use `derived-mode-p'
inside your function."
  :package-version '(manky . "2.4.0")
  :group 'manky-refresh
  :type 'hook
  :options '(manky-maybe-save-repository-buffers))

(defcustom manky-post-refresh-hook nil
  "Hook run after refreshing in `manky-refresh'.

This hook, or `manky-pre-refresh-hook', should be used
for functions that are not tied to a particular buffer.

To run a function with a particular buffer current, use
`manky-refresh-buffer-hook' and use `derived-mode-p'
inside your function."
  :package-version '(manky . "2.4.0")
  :group 'manky-refresh
  :type 'hook)

(defcustom manky-display-buffer-function 'manky-display-buffer-traditional
  "The function used display a Manky buffer.

All Manky buffers (buffers whose major-modes derive from
`manky-mode') are displayed using `manky-display-buffer',
which in turn uses the function specified here."
  :package-version '(manky . "2.3.0")
  :group 'manky-buffers
  :type '(radio (function-item manky-display-buffer-traditional)
                ;; (function-item manky-display-buffer-same-window-except-diff-v1)
                ;; (function-item manky-display-buffer-fullframe-status-v1)
                ;; (function-item manky-display-buffer-fullframe-status-topleft-v1)
                ;; (function-item manky-display-buffer-fullcolumn-most-v1)
                (function-item display-buffer)
                (function :tag "Function")))

(defcustom manky-pre-display-buffer-hook '();; manky-save-window-configuration
  "Hook run by `manky-display-buffer' before displaying the buffer."
  :package-version '(manky . "2.3.0")
  :group 'manky-buffers
  :type 'hook
  :get 'manky-hook-custom-get
  :options '(manky-save-window-configuration))

(defcustom manky-post-display-buffer-hook '(manky-maybe-set-dedicated)
  "Hook run by `manky-display-buffer' after displaying the buffer."
  :package-version '(manky . "2.3.0")
  :group 'manky-buffers
  :type 'hook
  :get 'manky-hook-custom-get
  :options '(manky-maybe-set-dedicated))

(defcustom manky-generate-buffer-name-function
  'manky-generate-buffer-name-default-function
  "The function used to generate the name for a Manky buffer."
  :package-version '(manky . "2.3.0")
  :group 'manky-buffers
  :type '(radio (function-item manky-generate-buffer-name-default-function)
                (function :tag "Function")))

(defcustom manky-buffer-name-format "%x%M%v: %t%x"
  "The format string used to name Manky buffers.

The following %-sequences are supported:

`%m' The name of the major-mode, but with the `-mode' suffix
     removed.

`%M' Like \"%m\" but abbreviate `manky-status-mode' as `manky'.

`%v' The value the buffer is locked to, in parentheses, or an
     empty string if the buffer is not locked to a value.

`%V' Like \"%v\", but the string is prefixed with a space, unless
     it is an empty string.

`%t' The top-level directory of the working tree of the
     repository, or if `manky-uniquify-buffer-names' is non-nil
     an abbreviation of that.

`%x' If `manky-uniquify-buffer-names' is nil \"*\", otherwise the
     empty string.  Due to limitations of the `uniquify' package,
     buffer names must end with the path.

`%T' Obsolete, use \"%t%x\" instead.  Like \"%t\", but append an
     asterisk if and only if `manky-uniquify-buffer-names' is nil.

The value should always contain \"%m\" or \"%M\", \"%v\" or
\"%V\", and \"%t\" (or the obsolete \"%T\").

If `manky-uniquify-buffer-names' is non-nil, then the value must
end with \"%t\" or \"%t%x\" (or the obsolete \"%T\").  See issue
#2841.

This is used by `manky-generate-buffer-name-default-function'.
If another `manky-generate-buffer-name-function' is used, then
it may not respect this option, or on the contrary it may
support additional %-sequences."
  :package-version '(manky . "2.12.0")
  :group 'manky-buffers
  :type 'string)

(defcustom manky-uniquify-buffer-names t
  "Whether to uniquify the names of Manky buffers."
  :package-version '(manky . "2.3.0")
  :group 'manky-buffers
  :type 'boolean)

(defcustom manky-bury-buffer-function 'manky-restore-window-configuration
  "The function used to bury or kill the current Manky buffer."
  :package-version '(manky . "2.3.0")
  :group 'manky-buffers
  :type '(radio (function-item quit-window)
                (function-item manky-mode-quit-window)
                (function-item manky-restore-window-configuration)
                (function :tag "Function")))

(defcustom manky-prefix-use-buffer-arguments 'selected
  "Whether certain prefix commands reuse arguments active in relevant buffer.

This affects the transient prefix commands `manky-diff',
`manky-log' and `manky-show-refs'.

Valid values are:

`always': Always use the set of arguments that is currently
  active in the respective buffer, provided that buffer exists
  of course.
`selected': Use the set of arguments from the respective
  buffer, but only if it is displayed in a window of the current
  frame.  This is the default.
`current': Use the set of arguments from the respective buffer,
  but only if it is the current buffer.
`never': Never use the set of arguments from the respective
  buffer.

For more information see info node `(manky)Transient Arguments
and Buffer Arguments'."
  :package-version '(manky . "3.0.0")
  :group 'manky-buffers
  :group 'manky-commands
  :type '(choice
          (const :tag "always use args from buffer" always)
          (const :tag "use args from buffer if displayed in frame" selected)
          (const :tag "use args from buffer if it is current" current)
          (const :tag "never use args from buffer" never)))

(defcustom manky-direct-use-buffer-arguments 'selected
  "Whether certain commands reuse arguments active in relevant buffer.

This affects certain commands such as `manky-show-commit' that
are suffixes of the diff or log transient prefix commands, but
only if they are invoked directly, i.e. *not* as a suffix.

Valid values are:

`always': Always use the set of arguments that is currently
  active in the respective buffer, provided that buffer exists
  of course.
`selected': Use the set of arguments from the respective
  buffer, but only if it is displayed in a window of the current
  frame.  This is the default.
`current': Use the set of arguments from the respective buffer,
  but only if it is the current buffer.
`never': Never use the set of arguments from the respective
  buffer.

For more information see info node `(manky)Transient Arguments
and Buffer Arguments'."
  :package-version '(manky . "3.0.0")
  :group 'manky-buffers
  :group 'manky-commands
  :type '(choice
          (const :tag "always use args from buffer" always)
          (const :tag "use args from buffer if displayed in frame" selected)
          (const :tag "use args from buffer if it is current" current)
          (const :tag "never use args from buffer" never)))

(defcustom manky-region-highlight-hook '(manky-diff-update-hunk-region)
  "Functions used to highlight the region.

Each function is run with the current section as only argument
until one of them returns non-nil.  If all functions return nil,
then fall back to regular region highlighting."
  :package-version '(manky . "2.1.0")
  :group 'manky-refresh
  :type 'hook
  :options '(manky-diff-update-hunk-region))

(defcustom manky-create-buffer-hook nil
  "Normal hook run after creating a new `manky-mode' buffer."
  :package-version '(manky . "2.90.0")
  :group 'manky-refresh
  :type 'hook)

(defcustom manky-refresh-buffer-hook nil
  "Normal hook for `manky-refresh-buffer' to run after refreshing."
  :package-version '(manky . "2.1.0")
  :group 'manky-refresh
  :type 'hook)

(defcustom manky-refresh-status-buffer t
  "Whether the status buffer is refreshed after running git.

When this is non-nil, then the status buffer is automatically
refreshed after running git for side-effects, in addition to the
current Manky buffer, which is always refreshed automatically.

Only set this to nil after exhausting all other options to
improve performance."
  :package-version '(manky . "2.4.0")
  :group 'manky-refresh
  :group 'manky-status
  :type 'boolean)

(defcustom manky-refresh-verbose nil
  "Whether to revert Manky buffers verbosely."
  :package-version '(manky . "2.1.0")
  :group 'manky-refresh
  :type 'boolean)

(defcustom manky-save-repository-buffers t
  "Whether to save file-visiting buffers when appropriate.

If non-nil, then all modified file-visiting buffers belonging
to the current repository may be saved before running Manky
commands and before creating or refreshing Manky buffers.
If `dontask', then this is done without user intervention, for
any other non-nil value the user has to confirm each save.

The default is t to avoid surprises, but `dontask' is the
recommended value."
  :group 'manky-essentials
  :group 'manky-buffers
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask" t)
                 (const :tag "Save without asking" dontask)))

;;; Key Bindings

(defvar manky-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map [C-return]  'manky-visit-thing)
    (define-key map (kbd "C-m") 'manky-visit-thing)
    (define-key map (kbd "C-M-i") 'manky-dired-jump)
    (define-key map [M-tab]     'magit-section-cycle-diffs)
    (define-key map (kbd   "P") 'manky-push)
    (define-key map (kbd   "k") 'manky-delete-thing)
    (define-key map (kbd   "K") 'manky-file-untrack)
    (define-key map (kbd   "i") 'manky-gitignore)
    (define-key map (kbd   "I") 'manky-gitignore)
    (define-key map (kbd "SPC") 'manky-diff-show-or-scroll-up)
    (define-key map (kbd "S-SPC") 'manky-diff-show-or-scroll-down)
    (define-key map (kbd "DEL") 'manky-diff-show-or-scroll-down)
    (define-key map "+"         'manky-diff-more-context)
    (define-key map "-"         'manky-diff-less-context)
    (define-key map "0"         'manky-diff-default-context)
    (define-key map "$" 'manky-process-buffer)
    (define-key map "%" 'manky-worktree)
    (define-key map "a" 'manky-cherry-apply)
    (define-key map "A" 'manky-cherry-pick)
    (define-key map "b" 'manky-branch)
    (define-key map "B" 'manky-bisect)
    (define-key map "c" 'manky-commit)
    (define-key map "C" 'manky-clone)
    (define-key map "d" 'manky-diff)
    (define-key map "D" 'manky-diff-refresh)
    (define-key map "e" 'manky-ediff-dwim)
    (define-key map "E" 'manky-ediff)
    (define-key map "f" 'manky-fetch)
    (define-key map "F" 'manky-pull)
    (define-key map "g" 'manky-refresh)
    (define-key map "G" 'manky-refresh-all)
    (define-key map "h" 'manky-dispatch)
    (define-key map "?" 'manky-dispatch)
    (define-key map "l" 'manky-log)
    (define-key map "L" 'manky-log-refresh)
    (define-key map "m" 'manky-merge)
    (define-key map "M" 'manky-remote)
    (define-key map "o" 'manky-submodule)
    (define-key map "O" 'manky-subtree)
    (define-key map "q" 'manky-mode-bury-buffer)
    (define-key map "r" 'manky-rebase)
    (define-key map "R" 'manky-file-rename)
    (define-key map "s" 'manky-stage-file)
    (define-key map "S" 'manky-stage-modified)
    (define-key map "t" 'manky-tag)
    (define-key map "T" 'manky-notes)
    (define-key map "u" 'manky-unstage-file)
    (define-key map "U" 'manky-unstage-all)
    (define-key map "v" 'manky-revert-no-commit)
    (define-key map "V" 'manky-revert)
    (define-key map "w" 'manky-am)
    (define-key map "W" 'manky-patch)
    (define-key map "x" 'manky-reset-quickly)
    (define-key map "X" 'manky-reset)
    (define-key map "y" 'manky-show-refs)
    (define-key map "Y" 'manky-cherry)
    (define-key map "z" 'manky-stash)
    (define-key map "Z" 'manky-stash)
    (define-key map ":" 'manky-git-command)
    (define-key map "!" 'manky-run)
    (define-key map (kbd "C-c C-c") 'manky-dispatch)
    (define-key map (kbd "C-c C-e") 'manky-edit-thing)
    (define-key map (kbd "C-c C-o") 'manky-browse-thing)
    (define-key map (kbd "C-c C-w") 'manky-browse-thing)
    (define-key map (kbd "C-x a")   'manky-add-change-log-entry)
    (define-key map (kbd "C-x 4 a") 'manky-add-change-log-entry-other-window)
    (define-key map (kbd "C-w")     'manky-copy-section-value)
    (define-key map (kbd "M-w")     'manky-copy-buffer-revision)
    ;; (define-key map [remap previous-line]      'manky-previous-line)
    ;; (define-key map [remap next-line]          'manky-next-line)
    (define-key map [remap evil-previous-line] 'evil-previous-visual-line)
    (define-key map [remap evil-next-line]     'evil-next-visual-line)
    map)
  "Parent keymap for all keymaps of modes derived from `manky-mode'.")

(defun manky-delete-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which deletes the thing at point."
  (interactive)
  (user-error "There is no thing at point that could be deleted"))

(defun manky-visit-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which visits the thing at point."
  (interactive)
  (if (eq current-transient-command 'manky-dispatch)
      (call-interactively (key-binding (this-command-keys)))
    (user-error "There is no thing at point that could be visited")))

(defun manky-edit-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which lets you edit the thing at point, likely in another buffer."
  (interactive)
  (if (eq current-transient-command 'manky-dispatch)
      (call-interactively (key-binding (this-command-keys)))
    (user-error "There is no thing at point that could be edited")))

(defun manky-browse-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which visits the thing at point using `browse-url'."
  (interactive)
  (user-error "There is no thing at point that could be browsed"))

(defvar bug-reference-map)
(with-eval-after-load 'bug-reference
  (define-key bug-reference-map [remap manky-visit-thing]
    'bug-reference-push-button))

(easy-menu-define manky-mode-menu manky-mode-map
  "Manky menu"
  '("Manky"
    ["Refresh" manky-refresh t]
    ["Refresh all" manky-refresh-all t]
    "---"
    ["Stage" manky-stage t]
    ["Stage modified" manky-stage-modified t]
    ["Unstage" manky-unstage t]
    ["Reset index" manky-reset-index t]
    ["Commit" manky-commit t]
    ["Add log entry" manky-commit-add-log t]
    ["Tag" manky-tag-create t]
    "---"
    ["Diff working tree" manky-diff-working-tree t]
    ["Diff" manky-diff t]
    ("Log"
     ["Log" manky-log-other t]
     ["Reflog" manky-reflog-other t]
     ["Extended..." manky-log t])
    "---"
    ["Cherry pick" manky-cherry-pick t]
    ["Revert commit" manky-revert t]
    "---"
    ["Ignore globally" manky-gitignore-globally t]
    ["Ignore locally" manky-gitignore-locally t]
    ["Discard" manky-discard t]
    ["Reset head and index" manky-reset-mixed t]
    ["Stash" manky-stash-both t]
    ["Snapshot" manky-snapshot-both t]
    "---"
    ["Branch..." manky-checkout t]
    ["Merge" manky-merge t]
    ["Ediff resolve" manky-ediff-resolve t]
    ["Rebase..." manky-rebase t]
    "---"
    ["Push" manky-push t]
    ["Pull" manky-pull-branch t]
    ["Remote update" manky-fetch-all t]
    ("Submodule"
     ["Submodule update" manky-submodule-update t]
     ["Submodule update and init" manky-submodule-setup t]
     ["Submodule init" manky-submodule-init t]
     ["Submodule sync" manky-submodule-sync t])
    "---"
    ("Extensions")
    "---"
    ["Display Git output" manky-process-buffer t]
    ["Quit Manky" manky-mode-bury-buffer t]))

;;; Mode

;; (defun manky-load-config-extensions ()
;;   "Load Manky extensions that are defined at the Git config layer."
;;   (dolist (ext (manky-get-all "manky.extension"))
;;     (let ((sym (intern (format "manky-%s-mode" ext))))
;;       (when (fboundp sym)
;;         (funcall sym 1)))))

(define-derived-mode manky-mode magit-section-mode "Manky"
  "Parent major mode from which Manky major modes inherit.

Manky is documented in info node `(manky)'."
  :group 'manky
  (hack-dir-local-variables-non-file-buffer)
  ;; (setq mode-line-process (manky-repository-local-get 'mode-line-process))
  ;; (setq-local bookmark-make-record-function 'manky--make-bookmark)
  )

;;; Highlighting

;;; Local Variables

(defvar-local manky-buffer-arguments nil)
(defvar-local manky-buffer-diff-args nil)
(defvar-local manky-buffer-diff-files nil)
(defvar-local manky-buffer-diff-files-suspended nil)
(defvar-local manky-buffer-file-name nil)
(defvar-local manky-buffer-files nil)
(defvar-local manky-buffer-log-args nil)
(defvar-local manky-buffer-log-files nil)
(defvar-local manky-buffer-range nil)
(defvar-local manky-buffer-range-hashed nil)
(defvar-local manky-buffer-refname nil)
(defvar-local manky-buffer-revision nil)
(defvar-local manky-buffer-revision-hash nil)
(defvar-local manky-buffer-revisions nil)
(defvar-local manky-buffer-typearg nil)
(defvar-local manky-buffer-upstream nil)

;; These variables are also used in file-visiting buffers.
;; Because the user may change the major-mode, they have
;; to be permanent buffer-local.
(put 'manky-buffer-file-name 'permanent-local t)
(put 'manky-buffer-refname 'permanent-local t)
(put 'manky-buffer-revision 'permanent-local t)
(put 'manky-buffer-revision-hash 'permanent-local t)

;; `manky-status' re-enables mode function but its refresher
;; function does not reinstate this.
(put 'manky-buffer-diff-files-suspended 'permanent-local t)

(defvar-local manky-refresh-args nil
  "Obsolete.  Possibly the arguments used to refresh the current buffer.
Some third-party packages might still use this, but Manky does not.")
(put 'manky-refresh-args 'permanent-local t)
(make-obsolete-variable 'manky-refresh-args nil "Manky 3.0.0")

(defvar manky-buffer-lock-functions nil
  "Obsolete buffer-locking support for third-party modes.
Implement the generic function `manky-buffer-value' for
your mode instead of adding an entry to this variable.")
(make-obsolete-variable 'manky-buffer-lock-functions nil "Manky 3.0.0")

(cl-defgeneric manky-buffer-value ()
  (when-let ((fn (cdr (assq major-mode manky-buffer-lock-functions))))
    (funcall fn (with-no-warnings manky-refresh-args))))

(defvar-local manky-previous-section nil)
(put 'manky-previous-section 'permanent-local t)

;;; Setup Buffer

(defmacro manky-setup-buffer (mode &optional locked &rest bindings)
  (declare (indent 2))
  `(manky-setup-buffer-internal
    ,mode ,locked
    ,(cons 'list (mapcar (pcase-lambda (`(,var ,form))
                           `(list ',var ,form))
                         bindings))))

(defun manky-setup-buffer-internal (mode locked bindings)
  (let* ((value   (and locked
                       (with-temp-buffer
                         (pcase-dolist (`(,var ,val) bindings)
                           (set (make-local-variable var) val))
                         (let ((major-mode mode))
                           (manky-buffer-value)))))
         (buffer  (manky-get-mode-buffer mode value))
         (section (and buffer (manky-current-section)))
         (created (not buffer)))
    (unless buffer
      (setq buffer (manky-with-toplevel
                     (manky-generate-new-buffer mode value))))
    (with-current-buffer buffer
      (setq manky-previous-section section)
      (funcall mode)
      ;; (manky-xref-setup 'manky-setup-buffer-internal bindings)
      (pcase-dolist (`(,var ,val) bindings)
        (set (make-local-variable var) val))
      (when created
        ;; (manky-status-goto-initial-section)
        (run-hooks 'manky-create-buffer-hook)))
    (manky-display-buffer buffer)
    (with-current-buffer buffer
      (run-hooks 'manky-setup-buffer-hook)
      (manky-refresh-buffer))
    buffer))

(defun manky-mode-setup (mode &rest args)
  "Setup up a MODE buffer using ARGS to generate its content."
  (declare (obsolete manky-setup-buffer "Manky 3.0.0"))
  (with-no-warnings
    (manky-mode-setup-internal mode args)))

(defun manky-mode-setup-internal (mode args &optional locked)
  "Setup up a MODE buffer using ARGS to generate its content.
When optional LOCKED is non-nil, then create a buffer that is
locked to its value, which is derived from MODE and ARGS."
  (declare (obsolete manky-setup-buffer "Manky 3.0.0"))
  (let* ((value   (and locked
                       (with-temp-buffer
                         (with-no-warnings
                           (setq manky-refresh-args args))
                         (let ((major-mode mode))
                           (manky-buffer-value)))))
         (buffer  (manky-get-mode-buffer mode value))
         (section (and buffer (manky-current-section)))
         (created (not buffer)))
    (unless buffer
      (setq buffer (manky-with-toplevel
                     (manky-generate-new-buffer mode value))))
    (with-current-buffer buffer
      (setq manky-previous-section section)
      (with-no-warnings
        (setq manky-refresh-args args))
      (funcall mode)
      ;; (manky-xref-setup 'manky-mode-setup-internal args)
      (when created
        ;; (manky-status-goto-initial-section)
        (run-hooks 'manky-create-buffer-hook)))
    (manky-display-buffer buffer)
    (with-current-buffer buffer
      (run-hooks 'manky-mode-setup-hook)
      (manky-refresh-buffer))))

;;; Display Buffer

(defvar manky-display-buffer-noselect nil
  "If non-nil, then `manky-display-buffer' doesn't call `select-window'.")

(defun manky-display-buffer (buffer &optional display-function)
  "Display BUFFER in some window and maybe select it.

If optional DISPLAY-FUNCTION is non-nil, then use that to display
the buffer.  Otherwise use `manky-display-buffer-function', which
is the normal case.

Then, unless `manky-display-buffer-noselect' is non-nil, select
the window which was used to display the buffer.

Also run the hooks `manky-pre-display-buffer-hook'
and `manky-post-display-buffer-hook'."
  (with-current-buffer buffer
    (run-hooks 'manky-pre-display-buffer-hook))
  (let ((window (funcall (or display-function manky-display-buffer-function)
                         buffer)))
    (unless manky-display-buffer-noselect
      (let* ((old-frame (selected-frame))
             (new-frame (window-frame window)))
        (select-window window)
        (unless (eq old-frame new-frame)
          (select-frame-set-input-focus new-frame)))))
  (with-current-buffer buffer
    (run-hooks 'manky-post-display-buffer-hook)))

(defun manky-display-buffer-traditional (buffer)
  "Display BUFFER the way this has traditionally been done."
  (display-buffer
   buffer (if (and (derived-mode-p 'manky-mode)
                   (not (memq (with-current-buffer buffer major-mode)
                              '(manky-process-mode
                                manky-revision-mode
                                manky-diff-mode
                                manky-stash-mode
                                manky-status-mode))))
              '(display-buffer-same-window)
            nil))) ; display in another window

;; (defun manky-display-buffer-same-window-except-diff-v1 (buffer)
;;   "Display BUFFER in the selected window except for some modes.
;; If a buffer's `major-mode' derives from `manky-diff-mode' or
;; `manky-process-mode', display it in another window.  Display all
;; other buffers in the selected window."
;;   (display-buffer
;;    buffer (if (with-current-buffer buffer
;;                 (derived-mode-p 'manky-diff-mode 'manky-process-mode))
;;               nil  ; display in another window
;;             '(display-buffer-same-window))))

;; (defun manky--display-buffer-fullframe (buffer alist)
;;   (when-let ((window (or (display-buffer-reuse-window buffer alist)
;;                          (display-buffer-same-window buffer alist)
;;                          (display-buffer-pop-up-window buffer alist)
;;                          (display-buffer-use-some-window buffer alist))))
;;     (delete-other-windows window)
;;     window))

;; (defun manky-display-buffer-fullframe-status-v1 (buffer)
;;   "Display BUFFER, filling entire frame if BUFFER is a status buffer.
;; Otherwise, behave like `manky-display-buffer-traditional'."
;;   (if (eq (with-current-buffer buffer major-mode)
;;           'manky-status-mode)
;;       (display-buffer buffer '(manky--display-buffer-fullframe))
;;     (manky-display-buffer-traditional buffer)))

;; (defun manky--display-buffer-topleft (buffer alist)
;;   (or (display-buffer-reuse-window buffer alist)
;;       (when-let ((window2 (display-buffer-pop-up-window buffer alist)))
;;         (let ((window1 (get-buffer-window))
;;               (buffer1 (current-buffer))
;;               (buffer2 (window-buffer window2))
;;               (w2-quit-restore (window-parameter window2 'quit-restore)))
;;           (set-window-buffer window1 buffer2)
;;           (set-window-buffer window2 buffer1)
;;           (select-window window2)
;;           ;; Swap some window state that `manky-mode-quit-window' and
;;           ;; `quit-restore-window' inspect.
;;           (set-window-prev-buffers window2 (cdr (window-prev-buffers window1)))
;;           (set-window-prev-buffers window1 nil)
;;           (set-window-parameter window2 'manky-dedicated
;;                                 (window-parameter window1 'manky-dedicated))
;;           (set-window-parameter window1 'manky-dedicated t)
;;           (set-window-parameter window1 'quit-restore
;;                                 (list 'window 'window
;;                                       (nth 2 w2-quit-restore)
;;                                       (nth 3 w2-quit-restore)))
;;           (set-window-parameter window2 'quit-restore nil)
;;           window1))))

;; (defun manky-display-buffer-fullframe-status-topleft-v1 (buffer)
;;   "Display BUFFER, filling entire frame if BUFFER is a status buffer.
;; When BUFFER derives from `manky-diff-mode' or
;; `manky-process-mode', try to display BUFFER to the top or left of
;; the current buffer rather than to the bottom or right, as
;; `manky-display-buffer-fullframe-status-v1' would.  Whether the
;; split is made vertically or horizontally is determined by
;; `split-window-preferred-function'."
;;   (display-buffer
;;    buffer
;;    (cond ((eq (with-current-buffer buffer major-mode)
;;               'manky-status-mode)
;;           '(manky--display-buffer-fullframe))
;;          ((with-current-buffer buffer
;;             (derived-mode-p 'manky-diff-mode 'manky-process-mode))
;;           '(manky--display-buffer-topleft))
;;          (t
;;           '(display-buffer-same-window)))))

;; (defun manky--display-buffer-fullcolumn (buffer alist)
;;   (when-let ((window (or (display-buffer-reuse-window buffer alist)
;;                          (display-buffer-same-window buffer alist)
;;                          (display-buffer-below-selected buffer alist))))
;;     (delete-other-windows-vertically window)
;;     window))

;; (defun manky-display-buffer-fullcolumn-most-v1 (buffer)
;;   "Display BUFFER using the full column except in some cases.
;; For most cases where BUFFER's `major-mode' derives from
;; `manky-mode', display it in the selected window and grow that
;; window to the full height of the frame, deleting other windows in
;; that column as necessary.  However, display BUFFER in another
;; window if 1) BUFFER's mode derives from `manky-process-mode', or
;; 2) BUFFER's mode derives from `manky-diff-mode', provided that
;; the mode of the current buffer derives from `manky-log-mode' or
;; `manky-cherry-mode'."
;;   (display-buffer
;;    buffer
;;    (cond ((and (or git-commit-mode
;;                    (derived-mode-p 'manky-log-mode
;;                                    'manky-cherry-mode
;;                                    'manky-reflog-mode))
;;                (with-current-buffer buffer
;;                  (derived-mode-p 'manky-diff-mode)))
;;           nil)
;;          ((with-current-buffer buffer
;;             (derived-mode-p 'manky-process-mode))
;;           nil)
;;          (t
;;           '(manky--display-buffer-fullcolumn)))))

(defun manky-maybe-set-dedicated ()
  "Mark the selected window as dedicated if appropriate.

If a new window was created to display the buffer, then remember
that fact.  That information is used by `manky-mode-quit-window',
to determine whether the window should be deleted when its last
Manky buffer is buried."
  (let ((window (get-buffer-window (current-buffer))))
    (when (and (window-live-p window)
               (not (window-prev-buffers window)))
      (set-window-parameter window 'manky-dedicated t))))

;;; Get Buffer

(defvar-local manky--default-directory nil
  "Value of `default-directory' when buffer is generated.
This exists to prevent a let-bound `default-directory' from
tricking `manky-get-mode-buffer' or `manky-mode-get-buffers'
into thinking a buffer belongs to a repo that it doesn't.")
(put 'manky--default-directory 'permanent-local t)

(defun manky-mode-get-buffers ()
  (let ((topdir (manky-toplevel)))
    (--filter (with-current-buffer it
                (and (derived-mode-p 'manky-mode)
                     (equal manky--default-directory topdir)))
              (buffer-list))))

(defvar-local manky-buffer-locked-p nil)
(put 'manky-buffer-locked-p 'permanent-local t)

(defun manky-get-mode-buffer (mode &optional value frame)
  "Return buffer belonging to the current repository whose major-mode is MODE.

If no such buffer exists then return nil.  Multiple buffers with
the same major-mode may exist for a repository but only one can
exist that hasn't been looked to its value.  Return that buffer
\(or nil if there is no such buffer) unless VALUE is non-nil, in
which case return the buffer that has been looked to that value.

If FRAME nil or omitted, then consider all buffers.  Otherwise
  only consider buffers that are displayed in some live window
  on some frame.
If `all', then consider all buffers on all frames.
If `visible', then only consider buffers on all visible frames.
If `selected' or t, then only consider buffers on the selected
  frame.
If a frame, then only consider buffers on that frame."
  (if-let ((topdir (manky-toplevel)))
      (cl-flet* ((b (buffer)
                    (with-current-buffer buffer
                      (and (eq major-mode mode)
                           (equal manky--default-directory topdir)
                           (if value
                               (and manky-buffer-locked-p
                                    (equal (manky-buffer-value) value))
                             (not manky-buffer-locked-p))
                           buffer)))
                 (w (window)
                    (b (window-buffer window)))
                 (f (frame)
                    (-some #'w (window-list frame 'no-minibuf))))
        (pcase-exhaustive frame
          (`nil                   (-some #'b (buffer-list)))
          (`all                   (-some #'f (frame-list)))
          (`visible               (-some #'f (visible-frame-list)))
          ((or `selected `t)      (-some #'w (window-list (selected-frame))))
          ((guard (framep frame)) (-some #'w (window-list frame)))))
    (manky--not-inside-repository-error)))

(defun manky-mode-get-buffer (mode &optional create frame value)
  (declare (obsolete manky-get-mode-buffer "Manky 3.0.0"))
  (when create
    (error "`manky-mode-get-buffer's CREATE argument is obsolete"))
  (if-let ((topdir (manky-toplevel)))
      (--first (with-current-buffer it
                 (and (eq major-mode mode)
                      (equal manky--default-directory topdir)
                      (if value
                          (and manky-buffer-locked-p
                               (equal (manky-buffer-value) value))
                        (not manky-buffer-locked-p))))
               (if frame
                   (mapcar #'window-buffer
                           (window-list (unless (eq frame t) frame)))
                 (buffer-list)))
    (manky--not-inside-repository-error)))

(defun manky-generate-new-buffer (mode &optional value)
  (let* ((name (funcall manky-generate-buffer-name-function mode value))
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq manky--default-directory default-directory)
      (setq manky-buffer-locked-p (and value t))
      ;; (manky-restore-section-visibility-cache mode)
      )
    (when manky-uniquify-buffer-names
      (add-to-list 'uniquify-list-buffers-directory-modes mode)
      (with-current-buffer buffer
        (setq list-buffers-directory (abbreviate-file-name default-directory)))
      (let ((uniquify-buffer-name-style
             (if (memq uniquify-buffer-name-style '(nil forward))
                 'post-forward-angle-brackets
               uniquify-buffer-name-style)))
        (uniquify-rationalize-file-buffer-names
         name (file-name-directory (directory-file-name default-directory))
         buffer)))
    buffer))

(defun manky-generate-buffer-name-default-function (mode &optional value)
  "Generate buffer name for a MODE buffer in the current repository.
The returned name is based on `manky-buffer-name-format' and
takes `manky-uniquify-buffer-names' and VALUE, if non-nil, into
account."
  (let ((m (substring (symbol-name mode) 0 -5))
        (v (and value (format "%s" (if (listp value) value (list value)))))
        (n (if manky-uniquify-buffer-names
               (file-name-nondirectory
                (directory-file-name default-directory))
             (abbreviate-file-name default-directory))))
    (format-spec
     manky-buffer-name-format
     `((?m . ,m)
       (?M . ,(if (eq mode 'manky-status-mode) "manky" m))
       (?v . ,(or v ""))
       (?V . ,(if v (concat " " v) ""))
       (?t . ,n)
       (?x . ,(if manky-uniquify-buffer-names "" "*"))
       (?T . ,(if manky-uniquify-buffer-names n (concat n "*")))))))

;;; Buffer Lock

(defun manky-toggle-buffer-lock ()
  "Lock the current buffer to its value or unlock it.

Locking a buffer to its value prevents it from being reused to
display another value.  The name of a locked buffer contains its
value, which allows telling it apart from other locked buffers
and the unlocked buffer.

Not all Manky buffers can be locked to their values, for example
it wouldn't make sense to lock a status buffer.

There can only be a single unlocked buffer using a certain
major-mode per repository.  So when a buffer is being unlocked
and another unlocked buffer already exists for that mode and
repository, then the former buffer is instead deleted and the
latter is displayed in its place."
  (interactive)
  (if manky-buffer-locked-p
      (if-let ((unlocked (manky-get-mode-buffer major-mode)))
          (let ((locked (current-buffer)))
            (switch-to-buffer unlocked nil t)
            (kill-buffer locked))
        (setq manky-buffer-locked-p nil)
        (rename-buffer (funcall manky-generate-buffer-name-function
                                major-mode)))
    (if-let ((value (manky-buffer-value)))
        (if-let ((locked (manky-get-mode-buffer major-mode value)))
            (let ((unlocked (current-buffer)))
              (switch-to-buffer locked nil t)
              (kill-buffer unlocked))
          (setq manky-buffer-locked-p t)
          (rename-buffer (funcall manky-generate-buffer-name-function
                                  major-mode value)))
      (user-error "Buffer has no value it could be locked to"))))

;;; Bury Buffer

(defun manky-mode-bury-buffer (&optional kill-buffer)
  "Bury the current buffer.
With a prefix argument, kill the buffer instead.
With two prefix arguments, also kill all Manky buffers associated
with this repository.
This is done using `manky-bury-buffer-function'."
  (interactive "P")
  ;; Kill all associated Manky buffers when a double prefix arg is given.
  (when (>= (prefix-numeric-value kill-buffer) 16)
    (let ((current (current-buffer)))
      (dolist (buf (manky-mode-get-buffers))
        (unless (eq buf current)
          (kill-buffer buf)))))
  (funcall manky-bury-buffer-function kill-buffer))

(defun manky-mode-quit-window (kill-buffer)
  "Quit the selected window and bury its buffer.

This behaves similar to `quit-window', but when the window
was originally created to display a Manky buffer and the
current buffer is the last remaining Manky buffer that was
ever displayed in the selected window, then delete that
window."
  (if (or (one-window-p)
          (--first (let ((buffer (car it)))
                     (and (not (eq buffer (current-buffer)))
                          (buffer-live-p buffer)
                          (or (not (window-parameter nil 'manky-dedicated))
                              (with-current-buffer buffer
                                (derived-mode-p 'manky-mode
                                                'manky-process-mode)))))
                   (window-prev-buffers)))
      (quit-window kill-buffer)
    (let ((window (selected-window)))
      (quit-window kill-buffer)
      (when (window-live-p window)
        (delete-window window)))))

;;; Refresh Buffers

(defvar inhibit-manky-refresh nil)

(defun manky-refresh ()
  "Refresh some buffers belonging to the current repository.

Refresh the current buffer if its major mode derives from
`manky-mode', and refresh the corresponding status buffer.

Run hooks `manky-pre-refresh-hook' and `manky-post-refresh-hook'."
  (interactive)
  (unless inhibit-manky-refresh
    (unwind-protect
        (let ((start (current-time))
              (manky--refresh-cache (or manky--refresh-cache
                                        (list (cons 0 0)))))
          (when manky-refresh-verbose
            (message "Refreshing manky..."))
          (manky-run-hook-with-benchmark 'manky-pre-refresh-hook)
          (cond ((derived-mode-p 'manky-mode)
                 (manky-refresh-buffer))
                ((derived-mode-p 'tabulated-list-mode)
                 (revert-buffer)))
          (--when-let (and manky-refresh-status-buffer
                           (not (derived-mode-p 'manky-status-mode))
                           (manky-get-mode-buffer 'manky-status-mode))
            (with-current-buffer it
              (manky-refresh-buffer)))
          ;; (manky-auto-revert-buffers)

          ;; (cond
          ;;  ((and (not this-command)
          ;;        (memq last-command manky-post-commit-hook-commands))
          ;;   (manky-run-hook-with-benchmark 'manky-post-commit-hook))
          ;;  ((memq this-command manky-post-stage-hook-commands)
          ;;   (manky-run-hook-with-benchmark 'manky-post-stage-hook))
          ;;  ((memq this-command manky-post-unstage-hook-commands)
          ;;   (manky-run-hook-with-benchmark 'manky-post-unstage-hook)))
          (manky-run-hook-with-benchmark 'manky-post-refresh-hook)
          (when manky-refresh-verbose
            (message "cache %S" manky--refresh-cache)
            (message "Refreshing manky...done (%.3fs, cached %s/%s)"
                     (float-time (time-subtract (current-time) start))
                     (caar manky--refresh-cache)
                     (+ (caar manky--refresh-cache)
                        (cdar manky--refresh-cache)))))
      (run-hooks 'manky-unwind-refresh-hook))))

(defun manky-refresh-all ()
  "Refresh all buffers belonging to the current repository.

Refresh all Manky buffers belonging to the current repository,
and revert buffers that visit files located inside the current
repository.

Run hooks `manky-pre-refresh-hook' and `manky-post-refresh-hook'."
  (interactive)
  (manky-run-hook-with-benchmark 'manky-pre-refresh-hook)
  (dolist (buffer (manky-mode-get-buffers))
    (with-current-buffer buffer (manky-refresh-buffer)))
  ;; (manky-auto-revert-buffers)
  (manky-run-hook-with-benchmark 'manky-post-refresh-hook))

(defvar-local manky-refresh-start-time nil)

(defun manky-refresh-buffer ()
  "Refresh the current Manky buffer."
  (setq manky-refresh-start-time (current-time))
  (let ((refresh (intern (format "%s-refresh-buffer"
                                 (substring (symbol-name major-mode) 0 -5))))
        (manky--refresh-cache (or manky--refresh-cache (list (cons 0 0)))))
    (when (functionp refresh)
      (when manky-refresh-verbose
        (message "Refreshing buffer `%s'..." (buffer-name)))
      (let* ((buffer (current-buffer))
             (windows
              (--mapcat (with-selected-window it
                          (with-current-buffer buffer
                            (when-let ((section (manky-current-section)))
                              (list
                               (nconc (list it section)
                                      (manky-refresh-get-relative-position))))))
                        (or (get-buffer-window-list buffer nil t)
                            (list (selected-window))))))
        (deactivate-mark)
        (setq magit-section-highlight-overlays nil)
        (setq magit-section-highlighted-section nil)
        (setq magit-section-highlighted-sections nil)
        (setq magit-section-unhighlight-sections nil)
        ;; (manky-process-unset-mode-line-error-status)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (save-excursion
            (apply refresh (with-no-warnings manky-refresh-args))))
        (pcase-dolist (`(,window . ,args) windows)
          (with-selected-window window
            (with-current-buffer buffer
              (apply #'magit-section-goto-successor args))))
        (run-hooks 'manky-refresh-buffer-hook)
        (magit-section-update-highlight)
        (set-buffer-modified-p nil))
      (when manky-refresh-verbose
        (message "Refreshing buffer `%s'...done (%.3fs)" (buffer-name)
                 (float-time (time-subtract (current-time)
                                            manky-refresh-start-time)))))))

(defun manky-refresh-get-relative-position ()
  nil
  ;; (when-let ((section (manky-current-section)))
  ;;   (let ((start (oref section start)))
  ;;     (list (count-lines start (point))
  ;;           (- (point) (line-beginning-position))
  ;;           (and (manky-hunk-section-p section)
  ;;                (region-active-p)
  ;;                (progn (goto-char (line-beginning-position))
  ;;                       (when  (looking-at "^[-+]") (forward-line))
  ;;                       (while (looking-at "^[ @]") (forward-line))
  ;;                       (let ((beg (point)))
  ;;                         (cond ((looking-at "^[-+]")
  ;;                                (forward-line)
  ;;                                (while (looking-at "^[-+]") (forward-line))
  ;;                                (while (looking-at "^ ")    (forward-line))
  ;;                                (forward-line -1)
  ;;                                (regexp-quote (buffer-substring-no-properties
  ;;                                               beg (line-end-position))))
  ;;                               (t t))))))))
  )

;;; Save File-Visiting Buffers

(defvar disable-manky-save-buffers nil)

(defun manky-pre-command-hook ()
  (setq disable-manky-save-buffers nil))
(add-hook 'pre-command-hook #'manky-pre-command-hook)

;; (defvar manky-after-save-refresh-buffers nil)

;; (defun manky-after-save-refresh-buffers ()
;;   (dolist (buffer manky-after-save-refresh-buffers)
;;     (when (buffer-live-p buffer)
;;       (with-current-buffer buffer
;;         (manky-refresh-buffer))))
;;   (setq manky-after-save-refresh-buffers nil)
;;   (remove-hook 'post-command-hook 'manky-after-save-refresh-buffers))

;; (defun manky-after-save-refresh-status ()
;;   "Refresh the status buffer of the current repository.

;; This function is intended to be added to `after-save-hook'.

;; If the status buffer does not exist or the file being visited in
;; the current buffer isn't inside the working tree of a repository,
;; then do nothing.

;; Note that refreshing a Manky buffer is done by re-creating its
;; contents from scratch, which can be slow in large repositories.
;; If you are not satisfied with Manky's performance, then you
;; should obviously not add this function to that hook."
;;   (when (and (not disable-manky-save-buffers)
;;              (manky-inside-worktree-p t))
;;     (--when-let (ignore-errors (manky-get-mode-buffer 'manky-status-mode))
;;       (add-to-list 'manky-after-save-refresh-buffers it)
;;       (add-hook 'post-command-hook 'manky-after-save-refresh-buffers))))

(defun manky-maybe-save-repository-buffers ()
  "Maybe save file-visiting buffers belonging to the current repository.
Do so if `manky-save-repository-buffers' is non-nil.  You should
not remove this from any hooks, instead set that variable to nil
if you so desire."
  (when (and manky-save-repository-buffers
             (not disable-manky-save-buffers))
    (setq disable-manky-save-buffers t)
    (let ((msg (current-message)))
      (manky-save-repository-buffers
       (eq manky-save-repository-buffers 'dontask))
      (when (and msg
                 (current-message)
                 (not (equal msg (current-message))))
        (message "%s" msg)))))

(add-hook 'manky-pre-refresh-hook #'manky-maybe-save-repository-buffers)
(add-hook 'manky-pre-call-git-hook #'manky-maybe-save-repository-buffers)
(add-hook 'manky-pre-start-git-hook #'manky-maybe-save-repository-buffers)

(defvar-local manky-inhibit-refresh-save nil)

(defun manky-save-repository-buffers (&optional arg)
  "Save file-visiting buffers belonging to the current repository.
After any buffer where `buffer-save-without-query' is non-nil
is saved without asking, the user is asked about each modified
buffer which visits a file in the current repository.  Optional
argument (the prefix) non-nil means save all with no questions."
  (interactive "P")
  ;; (when-let ((topdir (manky-rev-parse-safe "--show-toplevel")))
  ;;   (let ((remote (file-remote-p topdir))
  ;;         (save-some-buffers-action-alist
  ;;          `((?Y (lambda (buffer)
  ;;                  (with-current-buffer buffer
  ;;                    (setq buffer-save-without-query t)
  ;;                    (save-buffer)))
  ;;                "to save the current buffer and remember choice")
  ;;            (?N (lambda (buffer)
  ;;                  (with-current-buffer buffer
  ;;                    (setq manky-inhibit-refresh-save t)))
  ;;                "to skip the current buffer and remember choice")
  ;;            ,@save-some-buffers-action-alist)))
  ;;     (save-some-buffers
  ;;      arg (lambda ()
  ;;            (and (not manky-inhibit-refresh-save)
  ;;                 buffer-file-name
  ;;                 ;; Avoid needlessly connecting to unrelated remotes.
  ;;                 (equal (file-remote-p buffer-file-name)
  ;;                        remote)
  ;;                 ;; For remote files this makes network requests and
  ;;                 ;; therefore has to come after the above to avoid
  ;;                 ;; unnecessarily waiting for unrelated hosts.
  ;;                 (file-exists-p (file-name-directory buffer-file-name))
  ;;                 (string-prefix-p topdir (file-truename buffer-file-name))
  ;;                 (equal (manky-rev-parse-safe "--show-toplevel")
  ;;                        topdir))))))
  )

;; ;;; Restore Window Configuration

;; (defvar manky-inhibit-save-previous-winconf nil)

;; (defvar-local manky-previous-window-configuration nil)
;; (put 'manky-previous-window-configuration 'permanent-local t)

;; (defun manky-save-window-configuration ()
;;   "Save the current window configuration.

;; Later, when the buffer is buried, it may be restored by
;; `manky-restore-window-configuration'."
;;   (if manky-inhibit-save-previous-winconf
;;       (when (eq manky-inhibit-save-previous-winconf 'unset)
;;         (setq manky-previous-window-configuration nil))
;;     (unless (get-buffer-window (current-buffer) (selected-frame))
;;       (setq manky-previous-window-configuration
;;             (current-window-configuration)))))

;; (defun manky-restore-window-configuration (&optional kill-buffer)
;;   "Bury or kill the current buffer and restore previous window configuration."
;;   (let ((winconf manky-previous-window-configuration)
;;         (buffer (current-buffer))
;;         (frame (selected-frame)))
;;     (quit-window kill-buffer (selected-window))
;;     (when (and winconf (equal frame (window-configuration-frame winconf)))
;;       (set-window-configuration winconf)
;;       (when (buffer-live-p buffer)
;;         (with-current-buffer buffer
;;           (setq manky-previous-window-configuration nil))))))

;; ;;; Buffer History

;; (defun manky-go-backward ()
;;   "Move backward in current buffer's history."
;;   (interactive)
;;   (if help-xref-stack
;;       (help-xref-go-back (current-buffer))
;;     (user-error "No previous entry in buffer's history")))

;; (defun manky-go-forward ()
;;   "Move forward in current buffer's history."
;;   (interactive)
;;   (if help-xref-forward-stack
;;       (help-xref-go-forward (current-buffer))
;;     (user-error "No next entry in buffer's history")))

;; (defun manky-insert-xref-buttons ()
;;   "Insert xref buttons."
;;   (when (or help-xref-stack help-xref-forward-stack)
;;     (when help-xref-stack
;;       (manky-xref-insert-button help-back-label 'manky-xref-backward))
;;     (when help-xref-forward-stack
;;       (when help-xref-stack
;;         (insert " "))
;;       (manky-xref-insert-button help-forward-label 'manky-xref-forward))))

;; (defun manky-xref-insert-button (label type)
;;   (manky-insert-section (button label)
;;     (insert-text-button label 'type type
;;                         'help-args (list (current-buffer)))))

;; (define-button-type 'manky-xref-backward
;;   :supertype 'help-back
;;   'mouse-face 'magit-section-highlight
;;   'help-echo (purecopy "mouse-2, RET: go back to previous history entry"))

;; (define-button-type 'manky-xref-forward
;;   :supertype 'help-forward
;;   'mouse-face 'magit-section-highlight
;;   'help-echo (purecopy "mouse-2, RET: go back to next history entry"))

;; (defvar manky-xref-modes
;;   '(manky-log-mode
;;     manky-reflog-mode
;;     manky-diff-mode
;;     manky-revision-mode)
;;   "List of modes for which to insert navigation buttons.")

;; (defun manky-xref-setup (fn args)
;;   (when (memq major-mode manky-xref-modes)
;;     (when help-xref-stack-item
;;       (push (cons (point) help-xref-stack-item) help-xref-stack)
;;       (setq help-xref-forward-stack nil))
;;     (when (called-interactively-p 'interactive)
;;       (--when-let (nthcdr 10 help-xref-stack)
;;         (setcdr it nil)))
;;     (setq help-xref-stack-item
;;           (list 'manky-xref-restore fn default-directory args))))

;; (defun manky-xref-restore (fn dir args)
;;   (setq default-directory dir)
;;   (funcall fn major-mode nil args)
;;   (manky-refresh-buffer))

;; ;;; Repository-Local Cache

;; (defvar manky-repository-local-cache nil
;;   "Alist mapping `manky-toplevel' paths to alists of key/value pairs.")

;; (defun manky-repository-local-repository ()
;;   "Return the key for the current repository."
;;   (or (bound-and-true-p manky--default-directory)
;;       (manky-toplevel)))

;; (defun manky-repository-local-set (key value &optional repository)
;;   "Set the repository-local VALUE for KEY.

;; Unless specified, REPOSITORY is the current buffer's repository.

;; If REPOSITORY is nil (meaning there is no current repository),
;; then the value is not cached, and we return nil."
;;   (let* ((repokey (or repository (manky-repository-local-repository)))
;;          (cache (assoc repokey manky-repository-local-cache)))
;;     ;; Don't cache values for a nil REPOSITORY, as the 'set' and 'get'
;;     ;; calls for some KEY may happen in unrelated contexts.
;;     (when repokey
;;       (if cache
;;           (let ((keyvalue (assoc key (cdr cache))))
;;             (if keyvalue
;;                 ;; Update pre-existing value for key.
;;                 (setcdr keyvalue value)
;;               ;; No such key in repository-local cache.
;;               (push (cons key value) (cdr cache))))
;;         ;; No cache for this repository.
;;         (push (cons repokey (list (cons key value)))
;;               manky-repository-local-cache)))))

;; (defun manky-repository-local-exists-p (key &optional repository)
;;   "Non-nil when a repository-local value exists for KEY.

;; Returns a (KEY . value) cons cell.

;; The KEY is matched using `equal'.

;; Unless specified, REPOSITORY is the current buffer's repository."
;;   (let* ((repokey (or repository (manky-repository-local-repository)))
;;          (cache (assoc repokey manky-repository-local-cache)))
;;     (and cache
;;          (assoc key (cdr cache)))))

;; (defun manky-repository-local-get (key &optional default repository)
;;   "Return the repository-local value for KEY.

;; Return DEFAULT if no value for KEY exists.

;; The KEY is matched using `equal'.

;; Unless specified, REPOSITORY is the current buffer's repository."
;;   (let ((keyvalue (manky-repository-local-exists-p key repository)))
;;     (if keyvalue
;;         (cdr keyvalue)
;;       default)))

;; (defun manky-repository-local-delete (key &optional repository)
;;   "Delete the repository-local value for KEY.

;; Unless specified, REPOSITORY is the current buffer's repository."
;;   (let* ((repokey (or repository (manky-repository-local-repository)))
;;          (cache (assoc repokey manky-repository-local-cache)))
;;     (when cache
;;       ;; There is no `assoc-delete-all'.
;;       (setf (cdr cache)
;;             (cl-delete key (cdr cache) :key #'car :test #'equal)))))

;; (defun manky-preserve-section-visibility-cache ()
;;   (when (derived-mode-p 'manky-status-mode 'manky-refs-mode)
;;     (manky-repository-local-set
;;      (cons major-mode 'manky-section-visibility-cache)
;;      manky-section-visibility-cache)))

;; (defun manky-restore-section-visibility-cache (mode)
;;   (setq manky-section-visibility-cache
;;         (manky-repository-local-get
;;          (cons mode 'manky-section-visibility-cache))))

;; (defun manky-zap-caches ()
;;   "Zap caches for the current repository.
;; Remove the repository's entry from `manky-repository-local-cache'
;; and set `manky-section-visibility-cache' to nil in all of the
;; repository's Manky buffers."
;;   (interactive)
;;   (manky-with-toplevel
;;     (setq manky-repository-local-cache
;;           (cl-delete default-directory
;;                      manky-repository-local-cache
;;                      :key #'car :test #'equal)))
;;   (dolist (buffer (manky-mode-get-buffers))
;;     (with-current-buffer buffer
;;       (setq manky-section-visibility-cache nil)))
;;   (setq manky--libgit-available-p eieio-unbound))

;;; Utilities

(defun manky-toggle-verbose-refresh ()
  "Toggle whether Manky refreshes buffers verbosely.
Enabling this helps figuring out which sections are bottlenecks.
The additional output can be found in the *Messages* buffer."
  (interactive)
  (setq manky-refresh-verbose (not manky-refresh-verbose))
  (message "%s verbose refreshing"
           (if manky-refresh-verbose "Enabled" "Disabled")))

(defun manky-run-hook-with-benchmark (hook)
  (when hook
    (if manky-refresh-verbose
        (let ((start (current-time)))
          (message "Running %s..." hook)
          (run-hooks hook)
          (message "Running %s...done (%.3fs)" hook
                   (float-time (time-subtract (current-time) start))))
      (run-hooks hook))))

;;; _
(provide 'manky-mode)
;;; manky-mode.el ends here
