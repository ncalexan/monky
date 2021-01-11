;;; monky-mode.el --- create and refresh Monky buffers  -*- lexical-binding: t -*-

;;; Commentary:

;; This library implements the abstract major-mode `monky-mode' from
;; which almost all other Monky major-modes derive.  The code in here
;; is mostly concerned with creating and refreshing Monky buffers.

;;; Code:

(require 'cl-lib)
(require 'dash)

(eval-when-compile
  (require 'subr-x))

(require 'transient)

(require 'magit-section)
(require 'monky-hg)

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

(defcustom monky-mode-hook
  '(;; monky-load-config-extensions
    )
  "Hook run when entering a mode derived from Monky mode."
  :package-version '(monky . "3.0.0")
  :group 'monky-modes
  :type 'hook
  :options '(;; monky-load-config-extensions
             bug-reference-mode))

(defcustom monky-setup-buffer-hook
  '(monky-maybe-save-repository-buffers
    monky-set-buffer-margin)
  "Hook run by `monky-setup-buffer'.

This is run right after displaying the buffer and right before
generating or updating its content.  `monky-mode-hook' and other,
more specific, `monky-mode-*-hook's on the other hand are run
right before displaying the buffer.  Usually one of these hooks
should be used instead of this one."
  :package-version '(monky . "2.3.0")
  :group 'monky-modes
  :type 'hook
  :options '(monky-maybe-save-repository-buffers
             monky-set-buffer-margin))

(defcustom monky-pre-refresh-hook '(monky-maybe-save-repository-buffers)
  "Hook run before refreshing in `monky-refresh'.

This hook, or `monky-post-refresh-hook', should be used
for functions that are not tied to a particular buffer.

To run a function with a particular buffer current, use
`monky-refresh-buffer-hook' and use `derived-mode-p'
inside your function."
  :package-version '(monky . "2.4.0")
  :group 'monky-refresh
  :type 'hook
  :options '(monky-maybe-save-repository-buffers))

(defcustom monky-post-refresh-hook nil
  "Hook run after refreshing in `monky-refresh'.

This hook, or `monky-pre-refresh-hook', should be used
for functions that are not tied to a particular buffer.

To run a function with a particular buffer current, use
`monky-refresh-buffer-hook' and use `derived-mode-p'
inside your function."
  :package-version '(monky . "2.4.0")
  :group 'monky-refresh
  :type 'hook)

(defcustom monky-display-buffer-function 'monky-display-buffer-traditional
  "The function used display a Monky buffer.

All Monky buffers (buffers whose major-modes derive from
`monky-mode') are displayed using `monky-display-buffer',
which in turn uses the function specified here."
  :package-version '(monky . "2.3.0")
  :group 'monky-buffers
  :type '(radio (function-item monky-display-buffer-traditional)
                ;; (function-item monky-display-buffer-same-window-except-diff-v1)
                ;; (function-item monky-display-buffer-fullframe-status-v1)
                ;; (function-item monky-display-buffer-fullframe-status-topleft-v1)
                ;; (function-item monky-display-buffer-fullcolumn-most-v1)
                (function-item display-buffer)
                (function :tag "Function")))

(defcustom monky-pre-display-buffer-hook '();; monky-save-window-configuration
  "Hook run by `monky-display-buffer' before displaying the buffer."
  :package-version '(monky . "2.3.0")
  :group 'monky-buffers
  :type 'hook
  :get 'monky-hook-custom-get
  :options '(monky-save-window-configuration))

(defcustom monky-post-display-buffer-hook '(monky-maybe-set-dedicated)
  "Hook run by `monky-display-buffer' after displaying the buffer."
  :package-version '(monky . "2.3.0")
  :group 'monky-buffers
  :type 'hook
  :get 'monky-hook-custom-get
  :options '(monky-maybe-set-dedicated))

(defcustom monky-generate-buffer-name-function
  'monky-generate-buffer-name-default-function
  "The function used to generate the name for a Monky buffer."
  :package-version '(monky . "2.3.0")
  :group 'monky-buffers
  :type '(radio (function-item monky-generate-buffer-name-default-function)
                (function :tag "Function")))

(defcustom monky-buffer-name-format "%x%M%v: %t%x"
  "The format string used to name Monky buffers.

The following %-sequences are supported:

`%m' The name of the major-mode, but with the `-mode' suffix
     removed.

`%M' Like \"%m\" but abbreviate `monky-status-mode' as `monky'.

`%v' The value the buffer is locked to, in parentheses, or an
     empty string if the buffer is not locked to a value.

`%V' Like \"%v\", but the string is prefixed with a space, unless
     it is an empty string.

`%t' The top-level directory of the working tree of the
     repository, or if `monky-uniquify-buffer-names' is non-nil
     an abbreviation of that.

`%x' If `monky-uniquify-buffer-names' is nil \"*\", otherwise the
     empty string.  Due to limitations of the `uniquify' package,
     buffer names must end with the path.

`%T' Obsolete, use \"%t%x\" instead.  Like \"%t\", but append an
     asterisk if and only if `monky-uniquify-buffer-names' is nil.

The value should always contain \"%m\" or \"%M\", \"%v\" or
\"%V\", and \"%t\" (or the obsolete \"%T\").

If `monky-uniquify-buffer-names' is non-nil, then the value must
end with \"%t\" or \"%t%x\" (or the obsolete \"%T\").  See issue
#2841.

This is used by `monky-generate-buffer-name-default-function'.
If another `monky-generate-buffer-name-function' is used, then
it may not respect this option, or on the contrary it may
support additional %-sequences."
  :package-version '(monky . "2.12.0")
  :group 'monky-buffers
  :type 'string)

(defcustom monky-uniquify-buffer-names t
  "Whether to uniquify the names of Monky buffers."
  :package-version '(monky . "2.3.0")
  :group 'monky-buffers
  :type 'boolean)

(defcustom monky-bury-buffer-function 'monky-restore-window-configuration
  "The function used to bury or kill the current Monky buffer."
  :package-version '(monky . "2.3.0")
  :group 'monky-buffers
  :type '(radio (function-item quit-window)
                (function-item monky-mode-quit-window)
                (function-item monky-restore-window-configuration)
                (function :tag "Function")))

(defcustom monky-prefix-use-buffer-arguments 'selected
  "Whether certain prefix commands reuse arguments active in relevant buffer.

This affects the transient prefix commands `monky-diff',
`monky-log' and `monky-show-refs'.

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

For more information see info node `(monky)Transient Arguments
and Buffer Arguments'."
  :package-version '(monky . "3.0.0")
  :group 'monky-buffers
  :group 'monky-commands
  :type '(choice
          (const :tag "always use args from buffer" always)
          (const :tag "use args from buffer if displayed in frame" selected)
          (const :tag "use args from buffer if it is current" current)
          (const :tag "never use args from buffer" never)))

(defcustom monky-direct-use-buffer-arguments 'selected
  "Whether certain commands reuse arguments active in relevant buffer.

This affects certain commands such as `monky-show-commit' that
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

For more information see info node `(monky)Transient Arguments
and Buffer Arguments'."
  :package-version '(monky . "3.0.0")
  :group 'monky-buffers
  :group 'monky-commands
  :type '(choice
          (const :tag "always use args from buffer" always)
          (const :tag "use args from buffer if displayed in frame" selected)
          (const :tag "use args from buffer if it is current" current)
          (const :tag "never use args from buffer" never)))

(defcustom monky-region-highlight-hook '(monky-diff-update-hunk-region)
  "Functions used to highlight the region.

Each function is run with the current section as only argument
until one of them returns non-nil.  If all functions return nil,
then fall back to regular region highlighting."
  :package-version '(monky . "2.1.0")
  :group 'monky-refresh
  :type 'hook
  :options '(monky-diff-update-hunk-region))

(defcustom monky-create-buffer-hook nil
  "Normal hook run after creating a new `monky-mode' buffer."
  :package-version '(monky . "2.90.0")
  :group 'monky-refresh
  :type 'hook)

(defcustom monky-refresh-buffer-hook nil
  "Normal hook for `monky-refresh-buffer' to run after refreshing."
  :package-version '(monky . "2.1.0")
  :group 'monky-refresh
  :type 'hook)

(defcustom monky-refresh-status-buffer t
  "Whether the status buffer is refreshed after running git.

When this is non-nil, then the status buffer is automatically
refreshed after running git for side-effects, in addition to the
current Monky buffer, which is always refreshed automatically.

Only set this to nil after exhausting all other options to
improve performance."
  :package-version '(monky . "2.4.0")
  :group 'monky-refresh
  :group 'monky-status
  :type 'boolean)

(defcustom monky-refresh-verbose nil
  "Whether to revert Monky buffers verbosely."
  :package-version '(monky . "2.1.0")
  :group 'monky-refresh
  :type 'boolean)

(defcustom monky-save-repository-buffers t
  "Whether to save file-visiting buffers when appropriate.

If non-nil, then all modified file-visiting buffers belonging
to the current repository may be saved before running Monky
commands and before creating or refreshing Monky buffers.
If `dontask', then this is done without user intervention, for
any other non-nil value the user has to confirm each save.

The default is t to avoid surprises, but `dontask' is the
recommended value."
  :group 'monky-essentials
  :group 'monky-buffers
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask" t)
                 (const :tag "Save without asking" dontask)))

;;; Key Bindings

(defvar monky-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map [C-return]  'monky-visit-thing)
    (define-key map (kbd "C-m") 'monky-visit-thing)
    (define-key map (kbd "C-M-i") 'monky-dired-jump)
    (define-key map [M-tab]     'magit-section-cycle-diffs)
    (define-key map (kbd   "P") 'monky-push)
    (define-key map (kbd   "k") 'monky-delete-thing)
    (define-key map (kbd   "K") 'monky-file-untrack)
    (define-key map (kbd   "i") 'monky-gitignore)
    (define-key map (kbd   "I") 'monky-gitignore)
    (define-key map (kbd "SPC") 'monky-diff-show-or-scroll-up)
    (define-key map (kbd "S-SPC") 'monky-diff-show-or-scroll-down)
    (define-key map (kbd "DEL") 'monky-diff-show-or-scroll-down)
    (define-key map "+"         'monky-diff-more-context)
    (define-key map "-"         'monky-diff-less-context)
    (define-key map "0"         'monky-diff-default-context)
    (define-key map "$" 'monky-process-buffer)
    (define-key map "%" 'monky-worktree)
    (define-key map "a" 'monky-cherry-apply)
    (define-key map "A" 'monky-cherry-pick)
    (define-key map "b" 'monky-branch)
    (define-key map "B" 'monky-bisect)
    (define-key map "c" 'monky-commit)
    (define-key map "C" 'monky-clone)
    (define-key map "d" 'monky-diff)
    (define-key map "D" 'monky-diff-refresh)
    (define-key map "e" 'monky-ediff-dwim)
    (define-key map "E" 'monky-ediff)
    (define-key map "f" 'monky-fetch)
    (define-key map "F" 'monky-pull)
    (define-key map "g" 'monky-refresh)
    (define-key map "G" 'monky-refresh-all)
    (define-key map "h" 'monky-dispatch)
    (define-key map "?" 'monky-dispatch)
    (define-key map "l" 'monky-log)
    (define-key map "L" 'monky-log-refresh)
    (define-key map "m" 'monky-merge)
    (define-key map "M" 'monky-remote)
    (define-key map "o" 'monky-submodule)
    (define-key map "O" 'monky-subtree)
    (define-key map "q" 'monky-mode-bury-buffer)
    (define-key map "r" 'monky-rebase)
    (define-key map "R" 'monky-file-rename)
    (define-key map "s" 'monky-stage-file)
    (define-key map "S" 'monky-stage-modified)
    (define-key map "t" 'monky-tag)
    (define-key map "T" 'monky-notes)
    (define-key map "u" 'monky-unstage-file)
    (define-key map "U" 'monky-unstage-all)
    (define-key map "v" 'monky-revert-no-commit)
    (define-key map "V" 'monky-revert)
    (define-key map "w" 'monky-am)
    (define-key map "W" 'monky-patch)
    (define-key map "x" 'monky-reset-quickly)
    (define-key map "X" 'monky-reset)
    (define-key map "y" 'monky-show-refs)
    (define-key map "Y" 'monky-cherry)
    (define-key map "z" 'monky-stash)
    (define-key map "Z" 'monky-stash)
    (define-key map ":" 'monky-git-command)
    (define-key map "!" 'monky-run)
    (define-key map (kbd "C-c C-c") 'monky-dispatch)
    (define-key map (kbd "C-c C-e") 'monky-edit-thing)
    (define-key map (kbd "C-c C-o") 'monky-browse-thing)
    (define-key map (kbd "C-c C-w") 'monky-browse-thing)
    (define-key map (kbd "C-x a")   'monky-add-change-log-entry)
    (define-key map (kbd "C-x 4 a") 'monky-add-change-log-entry-other-window)
    (define-key map (kbd "C-w")     'monky-copy-section-value)
    (define-key map (kbd "M-w")     'monky-copy-buffer-revision)
    ;; (define-key map [remap previous-line]      'monky-previous-line)
    ;; (define-key map [remap next-line]          'monky-next-line)
    (define-key map [remap evil-previous-line] 'evil-previous-visual-line)
    (define-key map [remap evil-next-line]     'evil-next-visual-line)
    map)
  "Parent keymap for all keymaps of modes derived from `monky-mode'.")

(defun monky-delete-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which deletes the thing at point."
  (interactive)
  (user-error "There is no thing at point that could be deleted"))

(defun monky-visit-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which visits the thing at point."
  (interactive)
  (if (eq current-transient-command 'monky-dispatch)
      (call-interactively (key-binding (this-command-keys)))
    (user-error "There is no thing at point that could be visited")))

(defun monky-edit-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which lets you edit the thing at point, likely in another buffer."
  (interactive)
  (if (eq current-transient-command 'monky-dispatch)
      (call-interactively (key-binding (this-command-keys)))
    (user-error "There is no thing at point that could be edited")))

(defun monky-browse-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which visits the thing at point using `browse-url'."
  (interactive)
  (user-error "There is no thing at point that could be browsed"))

(defvar bug-reference-map)
(with-eval-after-load 'bug-reference
  (define-key bug-reference-map [remap monky-visit-thing]
    'bug-reference-push-button))

(easy-menu-define monky-mode-menu monky-mode-map
  "Monky menu"
  '("Monky"
    ["Refresh" monky-refresh t]
    ["Refresh all" monky-refresh-all t]
    "---"
    ["Stage" monky-stage t]
    ["Stage modified" monky-stage-modified t]
    ["Unstage" monky-unstage t]
    ["Reset index" monky-reset-index t]
    ["Commit" monky-commit t]
    ["Add log entry" monky-commit-add-log t]
    ["Tag" monky-tag-create t]
    "---"
    ["Diff working tree" monky-diff-working-tree t]
    ["Diff" monky-diff t]
    ("Log"
     ["Log" monky-log-other t]
     ["Reflog" monky-reflog-other t]
     ["Extended..." monky-log t])
    "---"
    ["Cherry pick" monky-cherry-pick t]
    ["Revert commit" monky-revert t]
    "---"
    ["Ignore globally" monky-gitignore-globally t]
    ["Ignore locally" monky-gitignore-locally t]
    ["Discard" monky-discard t]
    ["Reset head and index" monky-reset-mixed t]
    ["Stash" monky-stash-both t]
    ["Snapshot" monky-snapshot-both t]
    "---"
    ["Branch..." monky-checkout t]
    ["Merge" monky-merge t]
    ["Ediff resolve" monky-ediff-resolve t]
    ["Rebase..." monky-rebase t]
    "---"
    ["Push" monky-push t]
    ["Pull" monky-pull-branch t]
    ["Remote update" monky-fetch-all t]
    ("Submodule"
     ["Submodule update" monky-submodule-update t]
     ["Submodule update and init" monky-submodule-setup t]
     ["Submodule init" monky-submodule-init t]
     ["Submodule sync" monky-submodule-sync t])
    "---"
    ("Extensions")
    "---"
    ["Display Git output" monky-process-buffer t]
    ["Quit Monky" monky-mode-bury-buffer t]))

;;; Mode

;; (defun monky-load-config-extensions ()
;;   "Load Monky extensions that are defined at the Git config layer."
;;   (dolist (ext (monky-get-all "monky.extension"))
;;     (let ((sym (intern (format "monky-%s-mode" ext))))
;;       (when (fboundp sym)
;;         (funcall sym 1)))))

(define-derived-mode monky-mode magit-section-mode "Monky"
  "Parent major mode from which Monky major modes inherit.

Monky is documented in info node `(monky)'."
  :group 'monky
  (hack-dir-local-variables-non-file-buffer)
  ;; (setq mode-line-process (monky-repository-local-get 'mode-line-process))
  ;; (setq-local bookmark-make-record-function 'monky--make-bookmark)
  )

;;; Highlighting

;;; Local Variables

(defvar-local monky-buffer-arguments nil)
(defvar-local monky-buffer-diff-args nil)
(defvar-local monky-buffer-diff-files nil)
(defvar-local monky-buffer-diff-files-suspended nil)
(defvar-local monky-buffer-file-name nil)
(defvar-local monky-buffer-files nil)
(defvar-local monky-buffer-log-args nil)
(defvar-local monky-buffer-log-files nil)
(defvar-local monky-buffer-range nil)
(defvar-local monky-buffer-range-hashed nil)
(defvar-local monky-buffer-refname nil)
(defvar-local monky-buffer-revision nil)
(defvar-local monky-buffer-revision-hash nil)
(defvar-local monky-buffer-revisions nil)
(defvar-local monky-buffer-typearg nil)
(defvar-local monky-buffer-upstream nil)

;; These variables are also used in file-visiting buffers.
;; Because the user may change the major-mode, they have
;; to be permanent buffer-local.
(put 'monky-buffer-file-name 'permanent-local t)
(put 'monky-buffer-refname 'permanent-local t)
(put 'monky-buffer-revision 'permanent-local t)
(put 'monky-buffer-revision-hash 'permanent-local t)

;; `monky-status' re-enables mode function but its refresher
;; function does not reinstate this.
(put 'monky-buffer-diff-files-suspended 'permanent-local t)

(defvar-local monky-refresh-args nil
  "Obsolete.  Possibly the arguments used to refresh the current buffer.
Some third-party packages might still use this, but Monky does not.")
(put 'monky-refresh-args 'permanent-local t)
(make-obsolete-variable 'monky-refresh-args nil "Monky 3.0.0")

(defvar monky-buffer-lock-functions nil
  "Obsolete buffer-locking support for third-party modes.
Implement the generic function `monky-buffer-value' for
your mode instead of adding an entry to this variable.")
(make-obsolete-variable 'monky-buffer-lock-functions nil "Monky 3.0.0")

(cl-defgeneric monky-buffer-value ()
  (when-let ((fn (cdr (assq major-mode monky-buffer-lock-functions))))
    (funcall fn (with-no-warnings monky-refresh-args))))

(defvar-local monky-previous-section nil)
(put 'monky-previous-section 'permanent-local t)

;;; Setup Buffer

(defmacro monky-setup-buffer (mode &optional locked &rest bindings)
  (declare (indent 2))
  `(monky-setup-buffer-internal
    ,mode ,locked
    ,(cons 'list (mapcar (pcase-lambda (`(,var ,form))
                           `(list ',var ,form))
                         bindings))))

(defun monky-setup-buffer-internal (mode locked bindings)
  (let* ((value   (and locked
                       (with-temp-buffer
                         (pcase-dolist (`(,var ,val) bindings)
                           (set (make-local-variable var) val))
                         (let ((major-mode mode))
                           (monky-buffer-value)))))
         (buffer  (monky-get-mode-buffer mode value))
         (section (and buffer (monky-current-section)))
         (created (not buffer)))
    (unless buffer
      (setq buffer (monky-with-toplevel
                     (monky-generate-new-buffer mode value))))
    (with-current-buffer buffer
      (setq monky-previous-section section)
      (funcall mode)
      ;; (monky-xref-setup 'monky-setup-buffer-internal bindings)
      (pcase-dolist (`(,var ,val) bindings)
        (set (make-local-variable var) val))
      (when created
        ;; (monky-status-goto-initial-section)
        (run-hooks 'monky-create-buffer-hook)))
    (monky-display-buffer buffer)
    (with-current-buffer buffer
      (run-hooks 'monky-setup-buffer-hook)
      (monky-refresh-buffer))
    buffer))

(defun monky-mode-setup (mode &rest args)
  "Setup up a MODE buffer using ARGS to generate its content."
  (declare (obsolete monky-setup-buffer "Monky 3.0.0"))
  (with-no-warnings
    (monky-mode-setup-internal mode args)))

(defun monky-mode-setup-internal (mode args &optional locked)
  "Setup up a MODE buffer using ARGS to generate its content.
When optional LOCKED is non-nil, then create a buffer that is
locked to its value, which is derived from MODE and ARGS."
  (declare (obsolete monky-setup-buffer "Monky 3.0.0"))
  (let* ((value   (and locked
                       (with-temp-buffer
                         (with-no-warnings
                           (setq monky-refresh-args args))
                         (let ((major-mode mode))
                           (monky-buffer-value)))))
         (buffer  (monky-get-mode-buffer mode value))
         (section (and buffer (monky-current-section)))
         (created (not buffer)))
    (unless buffer
      (setq buffer (monky-with-toplevel
                     (monky-generate-new-buffer mode value))))
    (with-current-buffer buffer
      (setq monky-previous-section section)
      (with-no-warnings
        (setq monky-refresh-args args))
      (funcall mode)
      ;; (monky-xref-setup 'monky-mode-setup-internal args)
      (when created
        ;; (monky-status-goto-initial-section)
        (run-hooks 'monky-create-buffer-hook)))
    (monky-display-buffer buffer)
    (with-current-buffer buffer
      (run-hooks 'monky-mode-setup-hook)
      (monky-refresh-buffer))))

;;; Display Buffer

(defvar monky-display-buffer-noselect nil
  "If non-nil, then `monky-display-buffer' doesn't call `select-window'.")

(defun monky-display-buffer (buffer &optional display-function)
  "Display BUFFER in some window and maybe select it.

If optional DISPLAY-FUNCTION is non-nil, then use that to display
the buffer.  Otherwise use `monky-display-buffer-function', which
is the normal case.

Then, unless `monky-display-buffer-noselect' is non-nil, select
the window which was used to display the buffer.

Also run the hooks `monky-pre-display-buffer-hook'
and `monky-post-display-buffer-hook'."
  (with-current-buffer buffer
    (run-hooks 'monky-pre-display-buffer-hook))
  (let ((window (funcall (or display-function monky-display-buffer-function)
                         buffer)))
    (unless monky-display-buffer-noselect
      (let* ((old-frame (selected-frame))
             (new-frame (window-frame window)))
        (select-window window)
        (unless (eq old-frame new-frame)
          (select-frame-set-input-focus new-frame)))))
  (with-current-buffer buffer
    (run-hooks 'monky-post-display-buffer-hook)))

(defun monky-display-buffer-traditional (buffer)
  "Display BUFFER the way this has traditionally been done."
  (display-buffer
   buffer (if (and (derived-mode-p 'monky-mode)
                   (not (memq (with-current-buffer buffer major-mode)
                              '(monky-process-mode
                                monky-revision-mode
                                monky-diff-mode
                                monky-stash-mode
                                monky-status-mode))))
              '(display-buffer-same-window)
            nil))) ; display in another window

;; (defun monky-display-buffer-same-window-except-diff-v1 (buffer)
;;   "Display BUFFER in the selected window except for some modes.
;; If a buffer's `major-mode' derives from `monky-diff-mode' or
;; `monky-process-mode', display it in another window.  Display all
;; other buffers in the selected window."
;;   (display-buffer
;;    buffer (if (with-current-buffer buffer
;;                 (derived-mode-p 'monky-diff-mode 'monky-process-mode))
;;               nil  ; display in another window
;;             '(display-buffer-same-window))))

;; (defun monky--display-buffer-fullframe (buffer alist)
;;   (when-let ((window (or (display-buffer-reuse-window buffer alist)
;;                          (display-buffer-same-window buffer alist)
;;                          (display-buffer-pop-up-window buffer alist)
;;                          (display-buffer-use-some-window buffer alist))))
;;     (delete-other-windows window)
;;     window))

;; (defun monky-display-buffer-fullframe-status-v1 (buffer)
;;   "Display BUFFER, filling entire frame if BUFFER is a status buffer.
;; Otherwise, behave like `monky-display-buffer-traditional'."
;;   (if (eq (with-current-buffer buffer major-mode)
;;           'monky-status-mode)
;;       (display-buffer buffer '(monky--display-buffer-fullframe))
;;     (monky-display-buffer-traditional buffer)))

;; (defun monky--display-buffer-topleft (buffer alist)
;;   (or (display-buffer-reuse-window buffer alist)
;;       (when-let ((window2 (display-buffer-pop-up-window buffer alist)))
;;         (let ((window1 (get-buffer-window))
;;               (buffer1 (current-buffer))
;;               (buffer2 (window-buffer window2))
;;               (w2-quit-restore (window-parameter window2 'quit-restore)))
;;           (set-window-buffer window1 buffer2)
;;           (set-window-buffer window2 buffer1)
;;           (select-window window2)
;;           ;; Swap some window state that `monky-mode-quit-window' and
;;           ;; `quit-restore-window' inspect.
;;           (set-window-prev-buffers window2 (cdr (window-prev-buffers window1)))
;;           (set-window-prev-buffers window1 nil)
;;           (set-window-parameter window2 'monky-dedicated
;;                                 (window-parameter window1 'monky-dedicated))
;;           (set-window-parameter window1 'monky-dedicated t)
;;           (set-window-parameter window1 'quit-restore
;;                                 (list 'window 'window
;;                                       (nth 2 w2-quit-restore)
;;                                       (nth 3 w2-quit-restore)))
;;           (set-window-parameter window2 'quit-restore nil)
;;           window1))))

;; (defun monky-display-buffer-fullframe-status-topleft-v1 (buffer)
;;   "Display BUFFER, filling entire frame if BUFFER is a status buffer.
;; When BUFFER derives from `monky-diff-mode' or
;; `monky-process-mode', try to display BUFFER to the top or left of
;; the current buffer rather than to the bottom or right, as
;; `monky-display-buffer-fullframe-status-v1' would.  Whether the
;; split is made vertically or horizontally is determined by
;; `split-window-preferred-function'."
;;   (display-buffer
;;    buffer
;;    (cond ((eq (with-current-buffer buffer major-mode)
;;               'monky-status-mode)
;;           '(monky--display-buffer-fullframe))
;;          ((with-current-buffer buffer
;;             (derived-mode-p 'monky-diff-mode 'monky-process-mode))
;;           '(monky--display-buffer-topleft))
;;          (t
;;           '(display-buffer-same-window)))))

;; (defun monky--display-buffer-fullcolumn (buffer alist)
;;   (when-let ((window (or (display-buffer-reuse-window buffer alist)
;;                          (display-buffer-same-window buffer alist)
;;                          (display-buffer-below-selected buffer alist))))
;;     (delete-other-windows-vertically window)
;;     window))

;; (defun monky-display-buffer-fullcolumn-most-v1 (buffer)
;;   "Display BUFFER using the full column except in some cases.
;; For most cases where BUFFER's `major-mode' derives from
;; `monky-mode', display it in the selected window and grow that
;; window to the full height of the frame, deleting other windows in
;; that column as necessary.  However, display BUFFER in another
;; window if 1) BUFFER's mode derives from `monky-process-mode', or
;; 2) BUFFER's mode derives from `monky-diff-mode', provided that
;; the mode of the current buffer derives from `monky-log-mode' or
;; `monky-cherry-mode'."
;;   (display-buffer
;;    buffer
;;    (cond ((and (or git-commit-mode
;;                    (derived-mode-p 'monky-log-mode
;;                                    'monky-cherry-mode
;;                                    'monky-reflog-mode))
;;                (with-current-buffer buffer
;;                  (derived-mode-p 'monky-diff-mode)))
;;           nil)
;;          ((with-current-buffer buffer
;;             (derived-mode-p 'monky-process-mode))
;;           nil)
;;          (t
;;           '(monky--display-buffer-fullcolumn)))))

(defun monky-maybe-set-dedicated ()
  "Mark the selected window as dedicated if appropriate.

If a new window was created to display the buffer, then remember
that fact.  That information is used by `monky-mode-quit-window',
to determine whether the window should be deleted when its last
Monky buffer is buried."
  (let ((window (get-buffer-window (current-buffer))))
    (when (and (window-live-p window)
               (not (window-prev-buffers window)))
      (set-window-parameter window 'monky-dedicated t))))

;;; Get Buffer

(defvar-local monky--default-directory nil
  "Value of `default-directory' when buffer is generated.
This exists to prevent a let-bound `default-directory' from
tricking `monky-get-mode-buffer' or `monky-mode-get-buffers'
into thinking a buffer belongs to a repo that it doesn't.")
(put 'monky--default-directory 'permanent-local t)

(defun monky-mode-get-buffers ()
  (let ((topdir (monky-toplevel)))
    (--filter (with-current-buffer it
                (and (derived-mode-p 'monky-mode)
                     (equal monky--default-directory topdir)))
              (buffer-list))))

(defvar-local monky-buffer-locked-p nil)
(put 'monky-buffer-locked-p 'permanent-local t)

(defun monky-get-mode-buffer (mode &optional value frame)
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
  (if-let ((topdir (monky-toplevel)))
      (cl-flet* ((b (buffer)
                    (with-current-buffer buffer
                      (and (eq major-mode mode)
                           (equal monky--default-directory topdir)
                           (if value
                               (and monky-buffer-locked-p
                                    (equal (monky-buffer-value) value))
                             (not monky-buffer-locked-p))
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
    (monky--not-inside-repository-error)))

(defun monky-mode-get-buffer (mode &optional create frame value)
  (declare (obsolete monky-get-mode-buffer "Monky 3.0.0"))
  (when create
    (error "`monky-mode-get-buffer's CREATE argument is obsolete"))
  (if-let ((topdir (monky-toplevel)))
      (--first (with-current-buffer it
                 (and (eq major-mode mode)
                      (equal monky--default-directory topdir)
                      (if value
                          (and monky-buffer-locked-p
                               (equal (monky-buffer-value) value))
                        (not monky-buffer-locked-p))))
               (if frame
                   (mapcar #'window-buffer
                           (window-list (unless (eq frame t) frame)))
                 (buffer-list)))
    (monky--not-inside-repository-error)))

(defun monky-generate-new-buffer (mode &optional value)
  (let* ((name (funcall monky-generate-buffer-name-function mode value))
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq monky--default-directory default-directory)
      (setq monky-buffer-locked-p (and value t))
      ;; (monky-restore-section-visibility-cache mode)
      )
    (when monky-uniquify-buffer-names
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

(defun monky-generate-buffer-name-default-function (mode &optional value)
  "Generate buffer name for a MODE buffer in the current repository.
The returned name is based on `monky-buffer-name-format' and
takes `monky-uniquify-buffer-names' and VALUE, if non-nil, into
account."
  (let ((m (substring (symbol-name mode) 0 -5))
        (v (and value (format "%s" (if (listp value) value (list value)))))
        (n (if monky-uniquify-buffer-names
               (file-name-nondirectory
                (directory-file-name default-directory))
             (abbreviate-file-name default-directory))))
    (format-spec
     monky-buffer-name-format
     `((?m . ,m)
       (?M . ,(if (eq mode 'monky-status-mode) "monky" m))
       (?v . ,(or v ""))
       (?V . ,(if v (concat " " v) ""))
       (?t . ,n)
       (?x . ,(if monky-uniquify-buffer-names "" "*"))
       (?T . ,(if monky-uniquify-buffer-names n (concat n "*")))))))

;;; Buffer Lock

(defun monky-toggle-buffer-lock ()
  "Lock the current buffer to its value or unlock it.

Locking a buffer to its value prevents it from being reused to
display another value.  The name of a locked buffer contains its
value, which allows telling it apart from other locked buffers
and the unlocked buffer.

Not all Monky buffers can be locked to their values, for example
it wouldn't make sense to lock a status buffer.

There can only be a single unlocked buffer using a certain
major-mode per repository.  So when a buffer is being unlocked
and another unlocked buffer already exists for that mode and
repository, then the former buffer is instead deleted and the
latter is displayed in its place."
  (interactive)
  (if monky-buffer-locked-p
      (if-let ((unlocked (monky-get-mode-buffer major-mode)))
          (let ((locked (current-buffer)))
            (switch-to-buffer unlocked nil t)
            (kill-buffer locked))
        (setq monky-buffer-locked-p nil)
        (rename-buffer (funcall monky-generate-buffer-name-function
                                major-mode)))
    (if-let ((value (monky-buffer-value)))
        (if-let ((locked (monky-get-mode-buffer major-mode value)))
            (let ((unlocked (current-buffer)))
              (switch-to-buffer locked nil t)
              (kill-buffer unlocked))
          (setq monky-buffer-locked-p t)
          (rename-buffer (funcall monky-generate-buffer-name-function
                                  major-mode value)))
      (user-error "Buffer has no value it could be locked to"))))

;;; Bury Buffer

(defun monky-mode-bury-buffer (&optional kill-buffer)
  "Bury the current buffer.
With a prefix argument, kill the buffer instead.
With two prefix arguments, also kill all Monky buffers associated
with this repository.
This is done using `monky-bury-buffer-function'."
  (interactive "P")
  ;; Kill all associated Monky buffers when a double prefix arg is given.
  (when (>= (prefix-numeric-value kill-buffer) 16)
    (let ((current (current-buffer)))
      (dolist (buf (monky-mode-get-buffers))
        (unless (eq buf current)
          (kill-buffer buf)))))
  (funcall monky-bury-buffer-function kill-buffer))

(defun monky-mode-quit-window (kill-buffer)
  "Quit the selected window and bury its buffer.

This behaves similar to `quit-window', but when the window
was originally created to display a Monky buffer and the
current buffer is the last remaining Monky buffer that was
ever displayed in the selected window, then delete that
window."
  (if (or (one-window-p)
          (--first (let ((buffer (car it)))
                     (and (not (eq buffer (current-buffer)))
                          (buffer-live-p buffer)
                          (or (not (window-parameter nil 'monky-dedicated))
                              (with-current-buffer buffer
                                (derived-mode-p 'monky-mode
                                                'monky-process-mode)))))
                   (window-prev-buffers)))
      (quit-window kill-buffer)
    (let ((window (selected-window)))
      (quit-window kill-buffer)
      (when (window-live-p window)
        (delete-window window)))))

;;; Refresh Buffers

(defvar inhibit-monky-refresh nil)

(defun monky-refresh ()
  "Refresh some buffers belonging to the current repository.

Refresh the current buffer if its major mode derives from
`monky-mode', and refresh the corresponding status buffer.

Run hooks `monky-pre-refresh-hook' and `monky-post-refresh-hook'."
  (interactive)
  (unless inhibit-monky-refresh
    (unwind-protect
        (let ((start (current-time))
              (monky--refresh-cache (or monky--refresh-cache
                                        (list (cons 0 0)))))
          (when monky-refresh-verbose
            (message "Refreshing monky..."))
          (monky-run-hook-with-benchmark 'monky-pre-refresh-hook)
          (cond ((derived-mode-p 'monky-mode)
                 (monky-refresh-buffer))
                ((derived-mode-p 'tabulated-list-mode)
                 (revert-buffer)))
          (--when-let (and monky-refresh-status-buffer
                           (not (derived-mode-p 'monky-status-mode))
                           (monky-get-mode-buffer 'monky-status-mode))
            (with-current-buffer it
              (monky-refresh-buffer)))
          ;; (monky-auto-revert-buffers)

          ;; (cond
          ;;  ((and (not this-command)
          ;;        (memq last-command monky-post-commit-hook-commands))
          ;;   (monky-run-hook-with-benchmark 'monky-post-commit-hook))
          ;;  ((memq this-command monky-post-stage-hook-commands)
          ;;   (monky-run-hook-with-benchmark 'monky-post-stage-hook))
          ;;  ((memq this-command monky-post-unstage-hook-commands)
          ;;   (monky-run-hook-with-benchmark 'monky-post-unstage-hook)))
          (monky-run-hook-with-benchmark 'monky-post-refresh-hook)
          (when monky-refresh-verbose
            (message "cache %S" monky--refresh-cache)
            (message "Refreshing monky...done (%.3fs, cached %s/%s)"
                     (float-time (time-subtract (current-time) start))
                     (caar monky--refresh-cache)
                     (+ (caar monky--refresh-cache)
                        (cdar monky--refresh-cache)))))
      (run-hooks 'monky-unwind-refresh-hook))))

(defun monky-refresh-all ()
  "Refresh all buffers belonging to the current repository.

Refresh all Monky buffers belonging to the current repository,
and revert buffers that visit files located inside the current
repository.

Run hooks `monky-pre-refresh-hook' and `monky-post-refresh-hook'."
  (interactive)
  (monky-run-hook-with-benchmark 'monky-pre-refresh-hook)
  (dolist (buffer (monky-mode-get-buffers))
    (with-current-buffer buffer (monky-refresh-buffer)))
  ;; (monky-auto-revert-buffers)
  (monky-run-hook-with-benchmark 'monky-post-refresh-hook))

(defvar-local monky-refresh-start-time nil)

(defun monky-refresh-buffer ()
  "Refresh the current Monky buffer."
  (setq monky-refresh-start-time (current-time))
  (let ((refresh (intern (format "%s-refresh-buffer"
                                 (substring (symbol-name major-mode) 0 -5))))
        (monky--refresh-cache (or monky--refresh-cache (list (cons 0 0)))))
    (when (functionp refresh)
      (when monky-refresh-verbose
        (message "Refreshing buffer `%s'..." (buffer-name)))
      (let* ((buffer (current-buffer))
             (windows
              (--mapcat (with-selected-window it
                          (with-current-buffer buffer
                            (when-let ((section (monky-current-section)))
                              (list
                               (nconc (list it section)
                                      (monky-refresh-get-relative-position))))))
                        (or (get-buffer-window-list buffer nil t)
                            (list (selected-window))))))
        (deactivate-mark)
        (setq magit-section-highlight-overlays nil)
        (setq magit-section-highlighted-section nil)
        (setq magit-section-highlighted-sections nil)
        (setq magit-section-unhighlight-sections nil)
        ;; (monky-process-unset-mode-line-error-status)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (save-excursion
            (apply refresh (with-no-warnings monky-refresh-args))))
        (pcase-dolist (`(,window . ,args) windows)
          (with-selected-window window
            (with-current-buffer buffer
              (apply #'magit-section-goto-successor args))))
        (run-hooks 'monky-refresh-buffer-hook)
        (magit-section-update-highlight)
        (set-buffer-modified-p nil))
      (when monky-refresh-verbose
        (message "Refreshing buffer `%s'...done (%.3fs)" (buffer-name)
                 (float-time (time-subtract (current-time)
                                            monky-refresh-start-time)))))))

(defun monky-refresh-get-relative-position ()
  nil
  ;; (when-let ((section (monky-current-section)))
  ;;   (let ((start (oref section start)))
  ;;     (list (count-lines start (point))
  ;;           (- (point) (line-beginning-position))
  ;;           (and (monky-hunk-section-p section)
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

(defvar disable-monky-save-buffers nil)

(defun monky-pre-command-hook ()
  (setq disable-monky-save-buffers nil))
(add-hook 'pre-command-hook #'monky-pre-command-hook)

;; (defvar monky-after-save-refresh-buffers nil)

;; (defun monky-after-save-refresh-buffers ()
;;   (dolist (buffer monky-after-save-refresh-buffers)
;;     (when (buffer-live-p buffer)
;;       (with-current-buffer buffer
;;         (monky-refresh-buffer))))
;;   (setq monky-after-save-refresh-buffers nil)
;;   (remove-hook 'post-command-hook 'monky-after-save-refresh-buffers))

;; (defun monky-after-save-refresh-status ()
;;   "Refresh the status buffer of the current repository.

;; This function is intended to be added to `after-save-hook'.

;; If the status buffer does not exist or the file being visited in
;; the current buffer isn't inside the working tree of a repository,
;; then do nothing.

;; Note that refreshing a Monky buffer is done by re-creating its
;; contents from scratch, which can be slow in large repositories.
;; If you are not satisfied with Monky's performance, then you
;; should obviously not add this function to that hook."
;;   (when (and (not disable-monky-save-buffers)
;;              (monky-inside-worktree-p t))
;;     (--when-let (ignore-errors (monky-get-mode-buffer 'monky-status-mode))
;;       (add-to-list 'monky-after-save-refresh-buffers it)
;;       (add-hook 'post-command-hook 'monky-after-save-refresh-buffers))))

(defun monky-maybe-save-repository-buffers ()
  "Maybe save file-visiting buffers belonging to the current repository.
Do so if `monky-save-repository-buffers' is non-nil.  You should
not remove this from any hooks, instead set that variable to nil
if you so desire."
  (when (and monky-save-repository-buffers
             (not disable-monky-save-buffers))
    (setq disable-monky-save-buffers t)
    (let ((msg (current-message)))
      (monky-save-repository-buffers
       (eq monky-save-repository-buffers 'dontask))
      (when (and msg
                 (current-message)
                 (not (equal msg (current-message))))
        (message "%s" msg)))))

(add-hook 'monky-pre-refresh-hook #'monky-maybe-save-repository-buffers)
(add-hook 'monky-pre-call-git-hook #'monky-maybe-save-repository-buffers)
(add-hook 'monky-pre-start-git-hook #'monky-maybe-save-repository-buffers)

(defvar-local monky-inhibit-refresh-save nil)

(defun monky-save-repository-buffers (&optional arg)
  "Save file-visiting buffers belonging to the current repository.
After any buffer where `buffer-save-without-query' is non-nil
is saved without asking, the user is asked about each modified
buffer which visits a file in the current repository.  Optional
argument (the prefix) non-nil means save all with no questions."
  (interactive "P")
  ;; (when-let ((topdir (monky-rev-parse-safe "--show-toplevel")))
  ;;   (let ((remote (file-remote-p topdir))
  ;;         (save-some-buffers-action-alist
  ;;          `((?Y (lambda (buffer)
  ;;                  (with-current-buffer buffer
  ;;                    (setq buffer-save-without-query t)
  ;;                    (save-buffer)))
  ;;                "to save the current buffer and remember choice")
  ;;            (?N (lambda (buffer)
  ;;                  (with-current-buffer buffer
  ;;                    (setq monky-inhibit-refresh-save t)))
  ;;                "to skip the current buffer and remember choice")
  ;;            ,@save-some-buffers-action-alist)))
  ;;     (save-some-buffers
  ;;      arg (lambda ()
  ;;            (and (not monky-inhibit-refresh-save)
  ;;                 buffer-file-name
  ;;                 ;; Avoid needlessly connecting to unrelated remotes.
  ;;                 (equal (file-remote-p buffer-file-name)
  ;;                        remote)
  ;;                 ;; For remote files this makes network requests and
  ;;                 ;; therefore has to come after the above to avoid
  ;;                 ;; unnecessarily waiting for unrelated hosts.
  ;;                 (file-exists-p (file-name-directory buffer-file-name))
  ;;                 (string-prefix-p topdir (file-truename buffer-file-name))
  ;;                 (equal (monky-rev-parse-safe "--show-toplevel")
  ;;                        topdir))))))
  )

;; ;;; Restore Window Configuration

;; (defvar monky-inhibit-save-previous-winconf nil)

;; (defvar-local monky-previous-window-configuration nil)
;; (put 'monky-previous-window-configuration 'permanent-local t)

;; (defun monky-save-window-configuration ()
;;   "Save the current window configuration.

;; Later, when the buffer is buried, it may be restored by
;; `monky-restore-window-configuration'."
;;   (if monky-inhibit-save-previous-winconf
;;       (when (eq monky-inhibit-save-previous-winconf 'unset)
;;         (setq monky-previous-window-configuration nil))
;;     (unless (get-buffer-window (current-buffer) (selected-frame))
;;       (setq monky-previous-window-configuration
;;             (current-window-configuration)))))

;; (defun monky-restore-window-configuration (&optional kill-buffer)
;;   "Bury or kill the current buffer and restore previous window configuration."
;;   (let ((winconf monky-previous-window-configuration)
;;         (buffer (current-buffer))
;;         (frame (selected-frame)))
;;     (quit-window kill-buffer (selected-window))
;;     (when (and winconf (equal frame (window-configuration-frame winconf)))
;;       (set-window-configuration winconf)
;;       (when (buffer-live-p buffer)
;;         (with-current-buffer buffer
;;           (setq monky-previous-window-configuration nil))))))

;; ;;; Buffer History

;; (defun monky-go-backward ()
;;   "Move backward in current buffer's history."
;;   (interactive)
;;   (if help-xref-stack
;;       (help-xref-go-back (current-buffer))
;;     (user-error "No previous entry in buffer's history")))

;; (defun monky-go-forward ()
;;   "Move forward in current buffer's history."
;;   (interactive)
;;   (if help-xref-forward-stack
;;       (help-xref-go-forward (current-buffer))
;;     (user-error "No next entry in buffer's history")))

;; (defun monky-insert-xref-buttons ()
;;   "Insert xref buttons."
;;   (when (or help-xref-stack help-xref-forward-stack)
;;     (when help-xref-stack
;;       (monky-xref-insert-button help-back-label 'monky-xref-backward))
;;     (when help-xref-forward-stack
;;       (when help-xref-stack
;;         (insert " "))
;;       (monky-xref-insert-button help-forward-label 'monky-xref-forward))))

;; (defun monky-xref-insert-button (label type)
;;   (monky-insert-section (button label)
;;     (insert-text-button label 'type type
;;                         'help-args (list (current-buffer)))))

;; (define-button-type 'monky-xref-backward
;;   :supertype 'help-back
;;   'mouse-face 'magit-section-highlight
;;   'help-echo (purecopy "mouse-2, RET: go back to previous history entry"))

;; (define-button-type 'monky-xref-forward
;;   :supertype 'help-forward
;;   'mouse-face 'magit-section-highlight
;;   'help-echo (purecopy "mouse-2, RET: go back to next history entry"))

;; (defvar monky-xref-modes
;;   '(monky-log-mode
;;     monky-reflog-mode
;;     monky-diff-mode
;;     monky-revision-mode)
;;   "List of modes for which to insert navigation buttons.")

;; (defun monky-xref-setup (fn args)
;;   (when (memq major-mode monky-xref-modes)
;;     (when help-xref-stack-item
;;       (push (cons (point) help-xref-stack-item) help-xref-stack)
;;       (setq help-xref-forward-stack nil))
;;     (when (called-interactively-p 'interactive)
;;       (--when-let (nthcdr 10 help-xref-stack)
;;         (setcdr it nil)))
;;     (setq help-xref-stack-item
;;           (list 'monky-xref-restore fn default-directory args))))

;; (defun monky-xref-restore (fn dir args)
;;   (setq default-directory dir)
;;   (funcall fn major-mode nil args)
;;   (monky-refresh-buffer))

;; ;;; Repository-Local Cache

;; (defvar monky-repository-local-cache nil
;;   "Alist mapping `monky-toplevel' paths to alists of key/value pairs.")

;; (defun monky-repository-local-repository ()
;;   "Return the key for the current repository."
;;   (or (bound-and-true-p monky--default-directory)
;;       (monky-toplevel)))

;; (defun monky-repository-local-set (key value &optional repository)
;;   "Set the repository-local VALUE for KEY.

;; Unless specified, REPOSITORY is the current buffer's repository.

;; If REPOSITORY is nil (meaning there is no current repository),
;; then the value is not cached, and we return nil."
;;   (let* ((repokey (or repository (monky-repository-local-repository)))
;;          (cache (assoc repokey monky-repository-local-cache)))
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
;;               monky-repository-local-cache)))))

;; (defun monky-repository-local-exists-p (key &optional repository)
;;   "Non-nil when a repository-local value exists for KEY.

;; Returns a (KEY . value) cons cell.

;; The KEY is matched using `equal'.

;; Unless specified, REPOSITORY is the current buffer's repository."
;;   (let* ((repokey (or repository (monky-repository-local-repository)))
;;          (cache (assoc repokey monky-repository-local-cache)))
;;     (and cache
;;          (assoc key (cdr cache)))))

;; (defun monky-repository-local-get (key &optional default repository)
;;   "Return the repository-local value for KEY.

;; Return DEFAULT if no value for KEY exists.

;; The KEY is matched using `equal'.

;; Unless specified, REPOSITORY is the current buffer's repository."
;;   (let ((keyvalue (monky-repository-local-exists-p key repository)))
;;     (if keyvalue
;;         (cdr keyvalue)
;;       default)))

;; (defun monky-repository-local-delete (key &optional repository)
;;   "Delete the repository-local value for KEY.

;; Unless specified, REPOSITORY is the current buffer's repository."
;;   (let* ((repokey (or repository (monky-repository-local-repository)))
;;          (cache (assoc repokey monky-repository-local-cache)))
;;     (when cache
;;       ;; There is no `assoc-delete-all'.
;;       (setf (cdr cache)
;;             (cl-delete key (cdr cache) :key #'car :test #'equal)))))

;; (defun monky-preserve-section-visibility-cache ()
;;   (when (derived-mode-p 'monky-status-mode 'monky-refs-mode)
;;     (monky-repository-local-set
;;      (cons major-mode 'monky-section-visibility-cache)
;;      monky-section-visibility-cache)))

;; (defun monky-restore-section-visibility-cache (mode)
;;   (setq monky-section-visibility-cache
;;         (monky-repository-local-get
;;          (cons mode 'monky-section-visibility-cache))))

;; (defun monky-zap-caches ()
;;   "Zap caches for the current repository.
;; Remove the repository's entry from `monky-repository-local-cache'
;; and set `monky-section-visibility-cache' to nil in all of the
;; repository's Monky buffers."
;;   (interactive)
;;   (monky-with-toplevel
;;     (setq monky-repository-local-cache
;;           (cl-delete default-directory
;;                      monky-repository-local-cache
;;                      :key #'car :test #'equal)))
;;   (dolist (buffer (monky-mode-get-buffers))
;;     (with-current-buffer buffer
;;       (setq monky-section-visibility-cache nil)))
;;   (setq monky--libgit-available-p eieio-unbound))

;;; Utilities

(defun monky-toggle-verbose-refresh ()
  "Toggle whether Monky refreshes buffers verbosely.
Enabling this helps figuring out which sections are bottlenecks.
The additional output can be found in the *Messages* buffer."
  (interactive)
  (setq monky-refresh-verbose (not monky-refresh-verbose))
  (message "%s verbose refreshing"
           (if monky-refresh-verbose "Enabled" "Disabled")))

(defun monky-run-hook-with-benchmark (hook)
  (when hook
    (if monky-refresh-verbose
        (let ((start (current-time)))
          (message "Running %s..." hook)
          (run-hooks hook)
          (message "Running %s...done (%.3fs)" hook
                   (float-time (time-subtract (current-time) start))))
      (run-hooks hook))))

;;; _
(provide 'monky-mode)
;;; monky-mode.el ends here
