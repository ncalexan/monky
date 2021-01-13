;;; manky.el --- A Mercurial/hg interface inside Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2011 Anantha Kumaran.

;; Author: Anantha Kumaran <ananthakumaran@gmail.com>
;; URL: http://github.com/ananthakumaran/manky
;; Version: 0.2
;; Keywords: mercurial, hg, tools, vc
;; Package-Requires: ((emacs "24.4") (with-editor "2.9") magit-section transient)

;; Manky is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Manky is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'bindat)
(require 'cl-lib)
(require 'dash)
(require 'ediff)
(require 'f)
(require 'subr-x)
(require 'view)
(require 'tramp)
(require 'transient)
(require 'with-editor)

(defgroup manky nil
  "Controlling Hg from Emacs."
  :prefix "manky-"
  :group 'tools)

(defcustom manky-hg-executable "hg"
  "The name of the Hg executable."
  :group 'manky
  :type 'string)

(defcustom manky-hg-standard-options '("--config" "diff.git=Off" "--config" "ui.merge=:merge")
  "Standard options when running Hg."
  :group 'manky
  :type '(repeat string))

(defcustom manky-hg-process-environment '("TERM=dumb" "HGPLAINEXCEPT=status" "LANGUAGE=C")
  "Default environment variables for hg."
  :group 'manky
  :type '(repeat string))

;; TODO
(defcustom manky-save-some-buffers t
  "Non-nil means that \\[manky-status] will save modified buffers before running.
Setting this to t will ask which buffers to save, setting it to 'dontask will
save all modified buffers without asking."
  :group 'manky
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask" t)
                 (const :tag "Save without asking" dontask)))

(defcustom manky-revert-item-confirm t
  "Require acknowledgment before reverting an item."
  :group 'manky
  :type 'boolean)

(defcustom manky-log-edit-confirm-cancellation nil
  "Require acknowledgment before canceling the log edit buffer."
  :group 'manky
  :type 'boolean)

(defcustom manky-process-popup-time -1
  "Popup the process buffer if a command takes longer than this many seconds."
  :group 'manky
  :type '(choice (const :tag "Never" -1)
                 (const :tag "Immediately" 0)
                 (integer :tag "After this many seconds")))

(defcustom manky-log-cutoff-length 100
  "The maximum number of commits to show in the log buffer."
  :group 'manky
  :type 'integer)

(defcustom manky-log-infinite-length 99999
  "Number of log used to show as maximum for `manky-log-cutoff-length'."
  :group 'manky
  :type 'integer)

(defcustom manky-log-auto-more t
  "Insert more log entries automatically when moving past the last entry.

Only considered when moving past the last entry with `manky-goto-next-section'."
  :group 'manky
  :type 'boolean)

;; (defcustom manky-incoming-repository "default"
;;   "The repository from which changes are pulled from by default."
;;   :group 'manky
;;   :type 'string)

(defcustom manky-outgoing-repository ""
  "The repository to which changes are pushed to by default."
  :group 'manky
  :type 'string)

(defcustom manky-process-type nil
  "How manky spawns Mercurial processes.
Manky can either spawn a new Mercurial process for each request or
use Mercurial's command server feature to run several commands in a
single process instances. While the former is more robust, the latter
is usually faster if Manky runs several commands."
  :group 'manky
  :type '(choice (const :tag "Single processes" :value nil)
                 (const :tag "Use command server" :value cmdserver)))

(defcustom manky-pull-args ()
  "Extra args to pass to pull."
  :group 'manky
  :type '(repeat string))

(defcustom manky-repository-paths nil
  "*Paths where to find repositories.  For each repository an alias is defined, which can then be passed to `manky-open-repository` to open the repository.

Lisp-type of this option: The value must be a list L whereas each
element of L is a 2-element list: The first element is the full
path of a directory \(string) and the second element is an
arbitrary alias \(string) for this directory which is then
displayed instead of the underlying directory."
  :group 'manky
  :initialize 'custom-initialize-default
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'ecb-minor-mode)
                            ecb-minor-mode
			    (functionp 'ecb-update-directories-buffer))
		       (ecb-update-directories-buffer))))
  :type '(repeat (cons :tag "Path with alias"
		       (string :tag "Alias")
		       (directory :tag "Path"))))

(defun manky-root-dir-descr (dir)
  "Return the name of dir if it matches a path in manky-repository-paths, otherwise return nil"
  (catch 'exit
    (dolist (root-dir manky-repository-paths)
      (let ((base-dir
	     (concat
	      (replace-regexp-in-string
	       "/$" ""
	       (replace-regexp-in-string
		"^\~" (getenv "HOME")
		(cdr root-dir)))
	      "/")))
	(when (equal base-dir dir)
	  (throw 'exit (cons (car root-dir)
			     base-dir)))))))

(defun manky-open-repository ()
  "Prompt for a repository path or alias, then display the status
buffer.  Aliases are set in manky-repository-paths."
  (interactive)
  (let* ((rootdir (condition-case nil
		      (manky-get-root-dir)
		    (error nil)))
	 (default-repo (or (manky-root-dir-descr rootdir) rootdir))
	 (msg (if default-repo
		  (concat "repository (default " (car default-repo) "): ")
		"repository: "))
	 (repo-name (completing-read msg (mapcar 'car manky-repository-paths)))
	 (repo (or (assoc repo-name manky-repository-paths) default-repo)))
    (when repo (manky-status (cdr repo)))))

(defgroup manky-faces nil
  "Customize the appearance of Manky"
  :prefix "manky-"
  :group 'faces
  :group 'manky)

(defface manky-header
  '((t :weight bold))
  "Face for generic header lines.

Many Manky faces inherit from this one by default."
  :group 'manky-faces)

(defface manky-section-title
  '((((class color) (background light)) :foreground "DarkGoldenrod4" :inherit manky-header)
    (((class color) (background  dark)) :foreground "LightGoldenrod2" :inherit manky-header))
  "Face for section titles."
  :group 'manky-faces)

(defface manky-branch
  '((t :weight bold :inherit manky-header))
  "Face for the current branch."
  :group 'manky-faces)

(defface manky-diff-title
  '((t :inherit (manky-header)))
  "Face for diff title lines."
  :group 'manky-faces)

(defface manky-diff-hunk-header
  '((((class color) (background light))
     :background "grey80"
     :foreground "grey30")
    (((class color) (background dark))
     :background "grey25"
     :foreground "grey70"))
  "Face for diff hunk header lines."
  :group 'manky-faces)

(defface manky-diff-add
  '((((class color) (background light))
     :background "#cceecc"
     :foreground "#22aa22")
    (((class color) (background dark))
     :background "#336633"
     :foreground "#cceecc"))
  "Face for lines in a diff that have been added."
  :group 'manky-faces)

(defface manky-diff-none
  '((t))
  "Face for lines in a diff that are unchanged."
  :group 'manky-faces)

(defface manky-diff-del
  '((((class color) (background light))
     :background "#eecccc"
     :foreground "#aa2222")
    (((class color) (background dark))
     :background "#663333"
     :foreground "#eecccc"))
  "Face for lines in a diff that have been deleted."
  :group 'manky-faces)

(defface manky-commit-id
  '((((class color) (background light))
     :foreground "firebrick")
    (((class color) (background dark))
     :foreground "tomato"))
  "Face for commit IDs: SHA1 codes and commit numbers."
  :group 'manky-faces)

(defface manky-log-sha1
  '((t :inherit manky-commit-id))
  "Face for the sha1 element of the log output."
  :group 'manky-faces)

(defface manky-log-message
  '((t))
  "Face for the message element of the log output."
  :group 'manky-faces)

(defface manky-log-author
  '((((class color) (background light))
     :foreground "navy")
    (((class color) (background dark))
     :foreground "cornflower blue"))
  "Face for author shown in log buffer."
  :group 'manky-faces)

(defface manky-log-head-label-local
  '((((class color) (background light))
     :box t
     :background "Grey85"
     :foreground "LightSkyBlue4")
    (((class color) (background dark))
     :box t
     :background "Grey13"
     :foreground "LightSkyBlue1"))
  "Face for local branch head labels shown in log buffer."
  :group 'manky-faces)

(defface manky-log-head-label-tags
  '((((class color) (background light))
     :box t
     :background "LemonChiffon1"
     :foreground "goldenrod4")
    (((class color) (background dark))
     :box t
     :background "LemonChiffon1"
     :foreground "goldenrod4"))
  "Face for tag labels shown in log buffer."
  :group 'manky-faces)

(defface manky-log-head-label-bookmarks
  '((((class color) (background light))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4")
    (((class color) (background dark))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4"))
  "Face for bookmark labels shown in log buffer."
  :group 'manky-faces)

(defface manky-log-head-label-phase
  '((((class color) (background light))
     :box t
     :background "light green"
     :foreground "dark olive green")
    (((class color) (background dark))
     :box t
     :background "light green"
     :foreground "dark olive green"))
  "Face for phase label shown in log buffer."
  :group 'manky-faces)

(defface manky-log-head-label-obsolete
  '((((class color) (background light))
     :box t
     :background "OrangeRed1"
     :foreground "OrangeRed4")
    (((class color) (background dark))
     :box t
     :background "OrangeRed1"
     :foreground "OrangeRed4"))
  "Face for obsolete label shown in log buffer."
  :group 'manky-faces)

(defface manky-log-date
  '((t :weight bold :inherit manky-header))
  "Face for date in log."
  :group 'manky-faces)

(defvar manky-mode-hook nil
  "Hook run by `manky-mode'.")

;;; User facing configuration

(put 'manky-mode 'mode-class 'special)

;;; Compatibilities

(cl-eval-when (load eval)
  (require 'manky-margin))

;;; Utilities

(defmacro manky-with-process-environment (&rest body)
  (declare (indent 0)
           (debug (body)))
  `(let ((process-environment (append manky-hg-process-environment
                                      process-environment)))
     ,@body))

(defmacro manky-with-refresh (&rest body)
  "Refresh manky buffers after evaluating BODY.

It is safe to call the functions which uses this macro inside of
this macro.  As it is time consuming to refresh manky buffers,
this macro enforces refresh to occur exactly once by pending
refreshes inside of this macro.  Nested calls of this
macro (possibly via functions) does not refresh buffers multiple
times.  Instead, only the outside-most call of this macro
refreshes buffers."
  (declare (indent 0)
           (debug (body)))
  `(manky-refresh-wrapper (lambda () ,@body)))

(defun manky-completing-read (&rest args)
  (apply (if (null ido-mode)
             'completing-read
           'ido-completing-read)
         args))

(defun manky-start-process (&rest args)
  (manky-with-process-environment
    (apply (if (functionp 'start-file-process)
               'start-file-process
             'start-process) args)))

(defun manky-process-file-single (&rest args)
  (manky-with-process-environment
    (apply 'process-file args)))


;; Command server
(defvar manky-process nil)
(defvar manky-process-buffer-name "*manky-process*")
(defvar manky-process-client-buffer nil)

(defvar manky-cmd-process nil)
(defvar manky-cmd-process-buffer-name "*manky-cmd-process*")
(defvar manky-cmd-process-input-buffer nil)
(defvar manky-cmd-process-input-point nil)
(defvar manky-cmd-error-message nil)
(defvar manky-cmd-hello-message nil
  "Variable to store parsed hello message.")

;; TODO: does this need to be permanent? If it's only used in manky buffers (not source file buffers), it shouldn't be.
(defvar-local manky-root-dir nil)
(put 'manky-root-dir 'permanent-local t)

(defun manky-cmdserver-sentinel (proc _change)
  (unless (memq (process-status proc) '(run stop))
    (delete-process proc)))

(defun manky-cmdserver-read-data (size)
  (with-current-buffer (process-buffer manky-cmd-process)
    (while (< (point-max) size)
      (accept-process-output manky-cmd-process 0.1 nil t))
    (let ((str (buffer-substring (point-min) (+ (point-min) size))))
      (delete-region (point-min) (+ (point-min) size))
      (goto-char (point-min))
      str)))

(defun manky-cmdserver-read ()
  "Read one channel and return cons (CHANNEL . RAW-DATA)."
  (let* ((data (bindat-unpack '((channel byte) (len u32))
                              (manky-cmdserver-read-data 5)))
         (channel (bindat-get-field data 'channel))
         (len (bindat-get-field data 'len)))
    (cons channel (manky-cmdserver-read-data len))))

(defun manky-cmdserver-unpack-int (data)
  (bindat-get-field (bindat-unpack '((field u32)) data) 'field))

(defun manky-cmdserver-unpack-string (data)
  (bindat-get-field (bindat-unpack `((field str ,(length data))) data) 'field))

(defun manky-cmdserver-write (data)
  (process-send-string manky-cmd-process
                       (concat (bindat-pack '((len u32))
                                            `((len . ,(length data))))
                               data)))

(defun manky-cmdserver-start ()
  (unless manky-root-dir
    (let (manky-process manky-process-type)
      (setq manky-root-dir (manky-get-root-dir))))

  (let ((dir manky-root-dir)
        (buf (get-buffer-create manky-cmd-process-buffer-name))
        (default-directory manky-root-dir)
        (process-connection-type nil))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (setq buffer-file-coding-system 'no-conversion)
      (set-buffer-multibyte nil)
      (erase-buffer)
      (setq view-exit-action
            #'(lambda (buffer)
                (with-current-buffer buffer
                  (bury-buffer))))
      (setq default-directory dir)
      (let ((manky-cmd-process (manky-start-process
                                "manky-hg" buf "sh" "-c"
                                (format "%s --config extensions.mq= serve --cmdserver pipe 2> /dev/null" manky-hg-executable))))
        (set-process-coding-system manky-cmd-process 'no-conversion 'no-conversion)
        (set-process-sentinel manky-cmd-process #'manky-cmdserver-sentinel)
        (setq manky-cmd-hello-message
              (manky-cmdserver-parse-hello (manky-cmdserver-read)))
        manky-cmd-process))))

(defun manky-cmdserver-parse-hello (hello-message)
  "Parse hello message to get encoding information."
  (let ((channel (car hello-message))
        (text (cdr hello-message)))
    (if (eq channel ?o)
        (progn
          (mapcar
           (lambda (s)
             (string-match "^\\([a-z0-9]+\\) *: *\\(.*\\)$" s)
             (let ((field-name (match-string 1 s))
                   (field-data (match-string 2 s)))
               (cons (intern field-name) field-data)))
           (split-string (manky-cmdserver-unpack-string text) "\n")))
      (error "unknown channel %c for hello message" channel))))

(defun manky-cmdserver-get-encoding (&optional default)
  "Get encoding stored in `manky-cmd-hello-message'."
  (let ((e (assoc 'encoding manky-cmd-hello-message)))
    (if e
        (cond
         ((string-equal (downcase (cdr e)) "ascii")
          'us-ascii)
         (t
          (intern (downcase (cdr e)))))
      default)))

(defun manky-cmdserver-runcommand (&rest cmd-and-args)
  (setq manky-cmd-error-message nil)
  (with-current-buffer (process-buffer manky-cmd-process)
    (setq buffer-read-only nil)
    (erase-buffer))
  (process-send-string manky-cmd-process "runcommand\n")
  (manky-cmdserver-write (mapconcat #'identity cmd-and-args "\0"))
  (let* ((inhibit-read-only t)
         (start (point))
         (result
          (catch 'finished
            (while t
              (let* ((result (manky-cmdserver-read))
                     (channel (car result))
                     (text (cdr result)))
                (cond
                 ((eq channel ?o)
                  (insert (manky-cmdserver-unpack-string text)))
                 ((eq channel ?r)
                  (throw 'finished
                         (manky-cmdserver-unpack-int text)))
                 ((eq channel ?e)
                  (setq manky-cmd-error-message
                        (concat manky-cmd-error-message text)))
                 ((memq channel '(?I ?L))
                  (with-current-buffer manky-cmd-process-input-buffer
                    (let* ((max (if (eq channel ?I)
                                    (point-max)
                                  (save-excursion
                                    (goto-char manky-cmd-process-input-point)
                                    (line-beginning-position 2))))
                           (maxreq (manky-cmdserver-unpack-int text))
                           (len (min (- max manky-cmd-process-input-point)
                                     maxreq))
                           (end (+ manky-cmd-process-input-point len)))
                      (manky-cmdserver-write
                       (buffer-substring manky-cmd-process-input-point end))
                      (setq manky-cmd-process-input-point end))))
                 (t
                  (setq manky-cmd-error-message
                        (format "Unsupported channel: %c" channel)))))))))
    (decode-coding-region start (point)
                          (manky-cmdserver-get-encoding 'utf-8))
    result))

(defun manky-cmdserver-process-file (program infile buffer display &rest args)
  "Same as `process-file' but uses the currently active hg command-server."
  (if (or infile display)
      (apply #'manky-process-file-single program infile buffer display args)
    (let ((stdout (if (consp buffer) (car buffer) buffer))
          (stderr (and (consp buffer) (cadr buffer))))
      (if (eq stdout t) (setq stdout (current-buffer)))
      (if (eq stderr t) (setq stderr stdout))
      (let ((result
             (if stdout
                 (with-current-buffer stdout
                   (apply #'manky-cmdserver-runcommand args))
               (with-temp-buffer
                 (apply #'manky-cmdserver-runcommand args)))))
        (cond
         ((bufferp stderr)
          (when manky-cmd-error-message
            (with-current-buffer stderr
              (insert manky-cmd-error-message))))
         ((stringp stderr)
          (with-temp-file stderr
            (when manky-cmd-error-message
              (insert manky-cmd-error-message)))))
        result))))

(defun manky-process-file (&rest args)
  "Same as `process-file' in the current hg environment.
This function either calls `manky-cmdserver-process-file' or
`manky-process-file-single' depending on whether the hg
command-server should be used."
  ;; (message "manky-process-file: %s" args)
  (apply (cond
          (manky-cmd-process #'manky-cmdserver-process-file)
          ;; ((eq manky-process-type 'cmdserver)
          ;;  (error "No process started (forget `manky-with-process`?)"))
          (t #'manky-process-file-single))
         args))

(defmacro manky-with-process (&rest body)
  (declare (indent 0)
	   (debug (body)))
  `(let ((outer (not manky-cmd-process)))
     (when (and outer (eq manky-process-type 'cmdserver))
       (setq manky-cmd-process (manky-cmdserver-start)))
     (unwind-protect
	 (progn ,@body)
       (when (and manky-cmd-process outer (eq manky-process-type 'cmdserver))
	 (delete-process manky-cmd-process)
	 (setq manky-cmd-process nil)))))



(defvar manky-bug-report-url "http://github.com/ananthakumaran/manky/issues")
(defun manky-bug-report (str)
  (message "Unknown error: %s\nPlease file a bug at %s"
           str manky-bug-report-url))

(defun manky-string-starts-with-p (string prefix)
  (eq (compare-strings string nil (length prefix) prefix nil nil) t))

(defun manky-trim-line (str)
  (if (string= str "")
      nil
    (if (equal (elt str (- (length str) 1)) ?\n)
        (substring str 0 (- (length str) 1))
      str)))

(defun manky-delete-line (&optional end)
  "Delete the text in current line.
If END is non-nil, deletes the text including the newline character"
  (let ((end-point (if end
                       (1+ (point-at-eol))
                     (point-at-eol))))
    (delete-region (point-at-bol) end-point)))

(defun manky-split-lines (str)
  (if (string= str "")
      nil
    (let ((lines (nreverse (split-string str "\n"))))
      (if (string= (car lines) "")
          (setq lines (cdr lines)))
      (nreverse lines))))

(defun manky-put-line-property (prop val)
  (put-text-property (line-beginning-position) (line-beginning-position 2)
                     prop val))

(defun manky-parse-args (command)
  (require 'pcomplete)
  (car (with-temp-buffer
         (insert command)
         (pcomplete-parse-buffer-arguments))))

(defun manky-prefix-p (prefix list)
  "Return non-nil if PREFIX is a prefix of LIST.
PREFIX and LIST should both be lists.

If the car of PREFIX is the symbol '*, then return non-nil if the cdr of PREFIX
is a sublist of LIST (as if '* matched zero or more arbitrary elements of LIST)"
  (or (null prefix)
      (if (eq (car prefix) '*)
          (or (manky-prefix-p (cdr prefix) list)
              (and (not (null list))
                   (manky-prefix-p prefix (cdr list))))
        (and (not (null list))
             (equal (car prefix) (car list))
             (manky-prefix-p (cdr prefix) (cdr list))))))

(defun manky-wash-sequence (func)
  "Run FUNC until end of buffer is reached.

FUNC should leave point at the end of the modified region"
  (while (and (not (eobp))
              (funcall func))))

(defun manky-goto-line (line)
  "Like `goto-line' but doesn't set the mark."
  (save-restriction
    (widen)
    (goto-char 1)
    (forward-line (1- line))))

;;; Key bindings

(defvar manky-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "n") 'manky-goto-next-section)
    (define-key map (kbd "p") 'manky-goto-previous-section)
    (define-key map (kbd "RET") 'manky-visit-item)
    (define-key map (kbd "TAB") 'manky-toggle-section)
    (define-key map (kbd "SPC") 'manky-show-item-or-scroll-up)
    (define-key map (kbd "DEL") 'manky-show-item-or-scroll-down)
    (define-key map (kbd "g") 'manky-refresh)
    (define-key map (kbd "$") 'manky-display-process)
    (define-key map (kbd ":") 'manky-hg-command)
    (define-key map (kbd "l") 'manky-log)
    (define-key map (kbd "b") 'manky-branches)
    (define-key map (kbd "Q") 'manky-queue)
    (define-key map (kbd "q") 'manky-quit-window)

    (define-key map (kbd "M-1") 'manky-section-show-level-1-all)
    (define-key map (kbd "M-2") 'manky-section-show-level-2-all)
    (define-key map (kbd "M-3") 'manky-section-show-level-3-all)
    (define-key map (kbd "M-4") 'manky-section-show-level-4-all)
    map))

(defvar manky-status-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "s") 'manky-stage-item)
    (define-key map (kbd "S") 'manky-stage-all)
    (define-key map (kbd "u") 'manky-unstage-item)
    (define-key map (kbd "U") 'manky-unstage-all)
    ;; (define-key map (kbd "a") 'manky-commit-amend)
    (define-key map (kbd "c") 'manky-commit)
    (define-key map (kbd "e") 'manky-ediff-item)
    (define-key map (kbd "y") 'manky-bookmark-create)
    (define-key map (kbd "C") 'manky-checkout)
    (define-key map (kbd "M") 'manky-merge)
    (define-key map (kbd "B") 'manky-backout)
    (define-key map (kbd "P") 'manky-push)
    (define-key map (kbd "f") 'manky-pull)
    (define-key map (kbd "k") 'manky-discard-item)
    (define-key map (kbd "m") 'manky-resolve-item)
    (define-key map (kbd "r") 'manky-rebase)
    (define-key map (kbd "x") 'manky-unresolve-item)
    (define-key map (kbd "X") 'manky-reset-tip)
    (define-key map (kbd "A") 'manky-addremove-all)
    (define-key map (kbd "L") 'manky-rollback)
    ;; TODO: make this contingent on the shelve extension.
    (define-key map (kbd "z") 'manky-shelve)
    map))

(defvar manky-log-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-w") 'manky-copy-section-value)
    (define-key map (kbd "e") 'manky-log-show-more-entries)
    (define-key map (kbd "C") 'manky-checkout-item)
    (define-key map (kbd "M") 'manky-merge-item)
    (define-key map (kbd "B") 'manky-backout-item)
    (define-key map (kbd "i") 'manky-qimport-item)
    (define-key map (kbd "E") 'manky-histedit-item)
    (define-key map (kbd "r") 'manky-rebase)
    (define-key map (kbd "R") 'manky-mozilla-review-item)
    (define-key map (kbd "u") 'manky-prune-item)
    map))

(defvar manky-blame-mode-map
  (let ((map (make-keymap)))
    map))

(defvar manky-branches-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-w") 'manky-copy-section-value)
    (define-key map (kbd "C") 'manky-checkout-item)
    (define-key map (kbd "M") 'manky-merge-item)
    map))

(defvar manky-commit-mode-map
  (let ((map (make-keymap)))
    map))

(defvar manky-pre-log-edit-window-configuration nil)
(defvar manky-log-edit-client-buffer nil)
(defvar manky-log-edit-operation nil)
(defvar manky-log-edit-info nil)

(defvar manky-log-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'manky-log-edit-commit)
    (define-key map (kbd "C-c C-k") 'manky-log-edit-cancel-log-message)
    (define-key map (kbd "C-x C-s")
      (lambda ()
        (interactive)
        (message "Not saved. Use C-c C-c to finalize this %s." manky-log-edit-operation)))
    map))

(defvar-local manky-refresh-function nil)
(defvar-local manky-refresh-args nil)

(require 'manky-section)

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
           (section (manky-with-section section-title section-type
                      (if buffer-title
                          (insert (propertize buffer-title 'face 'manky-section-title) "\n"))
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
                            (goto-char (point-max)))))))
      (when (= body-beg (point))
        (manky-cancel-section section))
      section)))

(defun manky-hg-section (section-title-and-type buffer-title washer &rest args)
  (apply #'manky-insert-section
         section-title-and-type
         buffer-title
         washer
         manky-hg-executable
         (append manky-hg-standard-options args)))

;;; Running commands

(defun manky-set-mode-line-process (str)
  (let ((pr (if str (concat " " str) "")))
    (save-excursion
      (manky-for-all-buffers (lambda ()
                               (setq mode-line-process pr))))))

(defun manky-process-indicator-from-command (comps)
  (if (manky-prefix-p (cons manky-hg-executable manky-hg-standard-options)
                      comps)
      (setq comps (nthcdr (+ (length manky-hg-standard-options) 1) comps)))
  (car comps))

(defun manky-run* (cmd-and-args
		   &optional logline noerase noerror nowait input)
  (if (and manky-process
           (get-buffer manky-process-buffer-name))
      (error "Hg is already running"))
  (let ((cmd (car cmd-and-args))
        (args (cdr cmd-and-args))
        (dir default-directory)
        (buf (get-buffer-create manky-process-buffer-name))
        (successp nil))
    (with-editor
      (manky-set-mode-line-process
       (manky-process-indicator-from-command cmd-and-args))
      (setq manky-process-client-buffer (current-buffer))
      (with-current-buffer buf
        (view-mode 1)
        (set (make-local-variable 'view-no-disable-on-exit) t)
        (setq view-exit-action
              (lambda (buffer)
                (with-current-buffer buffer
                  (bury-buffer))))
        (setq buffer-read-only t)
        (let ((inhibit-read-only t))
          (setq default-directory dir)
          (if noerase
              (goto-char (point-max))
            (erase-buffer))
          (insert "$ " (or logline
                           (mapconcat #'identity cmd-and-args " "))
                  "\n")
          (cond (nowait
                 (setq manky-process
                       (let ((process-connection-type nil))
                         (apply 'manky-start-process cmd buf cmd args)))
                 (set-process-sentinel manky-process 'manky-process-sentinel)
                 (with-editor-set-process-filter manky-process 'manky-process-filter)
                 (when input
                   (with-current-buffer input
                     (process-send-region manky-process
                                          (point-min) (point-max))
                     (process-send-eof manky-process)
                     (sit-for 0.1 t)))
                 (cond ((= manky-process-popup-time 0)
                        (pop-to-buffer (process-buffer manky-process)))
                       ((> manky-process-popup-time 0)
                        (run-with-timer
                         manky-process-popup-time nil
                         (function
                          (lambda (buf)
                            (with-current-buffer buf
                              (when manky-process
                                (display-buffer (process-buffer manky-process))
                                (goto-char (point-max))))))
                         (current-buffer))))
                 (setq successp t))
  	      (manky-cmd-process
  	       (let ((manky-cmd-process-input-buffer input)
  		     (manky-cmd-process-input-point (and input
  						         (with-current-buffer input
  						           (point-min)))))
  		 (setq successp
  		       (equal (apply #'manky-cmdserver-runcommand (cdr cmd-and-args)) 0))
  		 (manky-set-mode-line-process nil)
  		 (manky-need-refresh manky-process-client-buffer)))
                (input
                 (with-current-buffer input
                   (setq default-directory dir)
                   (setq manky-process
                         ;; Don't use a pty, because it would set icrnl
                         ;; which would modify the input (issue #20).
                         (let ((process-connection-type nil))
                           (apply 'manky-start-process cmd buf cmd args)))
                   (with-editor-set-process-filter manky-process 'manky-process-filter)
                   (process-send-region manky-process
                                        (point-min) (point-max))
                   (process-send-eof manky-process)
                   (while (equal (process-status manky-process) 'run)
                     (sit-for 0.1 t))
                   (setq successp
                         (equal (process-exit-status manky-process) 0))
                   (setq manky-process nil))
                 (manky-set-mode-line-process nil)
                 (manky-need-refresh manky-process-client-buffer))
                (t
                 (setq successp
                       (equal (apply 'manky-process-file-single cmd nil buf nil args) 0))
                 (manky-set-mode-line-process nil)
                 (manky-need-refresh manky-process-client-buffer))))
        (or successp
            noerror
            (error
             (or manky-cmd-error-message
  	       (manky-abort-message (get-buffer manky-process-buffer-name))
                 "Hg failed")))
        successp))))

(defun manky-process-sentinel (process event)
  (let ((msg (format "Hg %s." (substring event 0 -1)))
        (successp (string-match "^finished" event)))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert msg "\n")
        (message msg)))
    (when (not successp)
      (let ((msg (manky-abort-message (process-buffer process))))
        (when msg
          (message msg))))
    (setq manky-process nil)
    (manky-set-mode-line-process nil)
    (if (buffer-live-p manky-process-client-buffer)
        (with-current-buffer manky-process-client-buffer
          (manky-with-refresh
            (manky-need-refresh manky-process-client-buffer))))))

(defun manky-abort-message (buffer)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^abort: \\(.*\\)" paragraph-separate) nil t)
        (match-string 1)))))

;; TODO password?

(defun manky-process-filter (proc string)
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
      (goto-char (process-mark proc))
      (insert string)
      (set-marker (process-mark proc) (point)))))


(defun manky-run-hg (&rest args)
  (manky-with-refresh
    (manky-run* (append (cons manky-hg-executable
                              manky-hg-standard-options)
                        args))))

(defun manky-run-hg-sync (&rest args)
  (message "Running %s %s"
           manky-hg-executable
           (mapconcat #'identity args " "))
  (manky-run* (append (cons manky-hg-executable
			    manky-hg-standard-options)
		      args)))

(defun manky-run-hg-async (&rest args)
  (message "Running %s %s"
           manky-hg-executable
           (mapconcat #'identity args " "))
  (manky-run* (append (cons manky-hg-executable
                            manky-hg-standard-options)
                      args)
              nil nil nil t))

(defun manky-run-async-with-input (input cmd &rest args)
  (message "hg: %S" (cons cmd args))
  (manky-run* (cons cmd args) nil nil nil t input))

(defun manky-display-process ()
  "Display output from most recent hg command."
  (interactive)
  (unless (get-buffer manky-process-buffer-name)
    (user-error "No Hg commands have run"))
  (display-buffer manky-process-buffer-name))

(defun manky-hg-command (command)
  "Perform arbitrary Hg COMMAND."
  (interactive "sRun hg like this: ")
  (let ((args (manky-parse-args command))
        (manky-process-popup-time 0))
    (manky-with-refresh
      (manky-run* (append (cons manky-hg-executable
                                manky-hg-standard-options)
                          args)
                  nil nil nil t))))

;;; Actions

(defmacro manky-section-action (opname &rest clauses)
  "Refresh manky buffers after executing action defined in CLAUSES.

See `manky-section-case' for the definition of HEAD and CLAUSES and
`manky-with-refresh' for how the buffers are refreshed."
  (declare (indent 1)
           (debug (form &rest (sexp body))))
  `(manky-with-refresh
     (manky-section-case ,opname ,@clauses)))

(defun manky-visit-item (&optional other-window)
  "Visit current item.
With a prefix argument, visit in other window."
  (interactive (list current-prefix-arg))
  (let ((ff (if other-window 'find-file-other-window 'find-file)))
    (manky-section-action "visit"
      ((file)
       (funcall ff (manky-section-info (manky-current-section))))
      ((diff)
       (funcall ff (manky-diff-item-file (manky-current-section))))
      ((hunk)
       (let ((file (manky-diff-item-file (manky-hunk-item-diff (manky-current-section))))
             (line (manky-hunk-item-target-line (manky-current-section))))
         (funcall ff file)
         (goto-char (point-min))
         (forward-line (1- line))))
      ((commit)
       (manky-show-commit (manky-section-info (manky-current-section))))
      ((longer)
       (manky-log-show-more-entries))
      ((queue)
       (manky-qqueue (manky-section-info (manky-current-section))))
      ((branch)
       (manky-checkout (manky-section-info (manky-current-section))))
      ((shelf)
       (manky-show-shelf
	(manky-section-info (manky-current-section)))))))

(defun manky-ediff-item ()
  "Open the ediff merge editor on the item."
  (interactive)
  (manky-section-action "ediff"
    ((merged diff)
     (if (eq (manky-diff-item-kind (manky-current-section)) 'unresolved)
	 (manky-ediff-merged (manky-current-section))
       (user-error "Already resolved.  Unresolve first.")))
    ((unmodified diff)
     (user-error "Cannot ediff an unmodified file during a merge."))
    ((staged diff)
     (user-error "Already staged"))
    ((changes diff)
     (manky-ediff-changes (manky-current-section)))))

(defun manky-ediff-merged (item)
  (let* ((file (manky-diff-item-file item))
	 (file-path (concat (manky-get-root-dir) file)))
    (condition-case nil
	(manky-run-hg-sync "resolve" "--tool" "internal:dump" file)
      (error nil))
    (condition-case nil
	(ediff-merge-files-with-ancestor
	 (concat file-path ".local")
	 (concat file-path ".other")
	 (concat file-path ".base")
	 nil file)
      (error nil))
    (delete-file (concat file-path ".local"))
    (delete-file (concat file-path ".other"))
    (delete-file (concat file-path ".base"))
    (delete-file (concat file-path ".orig"))))

(defun manky-ediff-changes (item)
  (ediff-revision
   (concat (manky-get-root-dir)
	   (manky-diff-item-file item))))

(defvar manky-staged-all-files nil)
(defvar manky-old-staged-files '())
(defvar-local manky-staged-files nil)

(defun manky-stage-all ()
  "Add all items in Changes to the staging area."
  (interactive)
  (manky-with-refresh
    (setq manky-staged-all-files t)
    (manky-refresh-buffer)))

(defun manky-stage-item ()
  "Add the item at point to the staging area."
  (interactive)
  (manky-section-action "stage"
    ((untracked file)
     (manky-run-hg "add" (manky-section-info (manky-current-section))))
    ((untracked)
     (manky-run-hg "add"))
    ((missing file)
     (manky-run-hg "remove" "--after" (manky-section-info (manky-current-section))))
    ((changes diff)
     (manky-stage-file (manky-section-title (manky-current-section)))
     (manky-refresh-buffer))
    ((changes)
     (manky-stage-all))
    ((staged diff)
     (user-error "Already staged"))
    ((unmodified diff)
     (user-error "Cannot partially commit a merge"))
    ((merged diff)
     (user-error "Cannot partially commit a merge"))))

(defun manky-unstage-all ()
  "Remove all items from the staging area"
  (interactive)
  (manky-with-refresh
    (setq manky-staged-files '())
    (manky-refresh-buffer)))

(defun manky-unstage-item ()
  "Remove the item at point from the staging area."
  (interactive)
  (manky-with-process
    (manky-section-action "unstage"
      ((staged diff)
       (manky-unstage-file (manky-section-title (manky-current-section)))
       (manky-refresh-buffer))
      ((staged)
       (manky-unstage-all))
      ((changes diff)
       (user-error "Already unstaged")))))

;;; Updating

(defun manky-hg-pull (args)
  "Run hg pull. The manky-pull-args variable contains extra arguments to pass to hg."
  ;; (let ((remote (if current-prefix-arg
  ;;                   (manky-read-remote "Pull from : ")
  ;;                 manky-incoming-repository)))
    (apply #'manky-run-hg-async
	   "pull" (append manky-pull-args args)))

;;;###autoload (autoload 'manky-pull "manky" nil t)
(define-transient-command manky-pull ()
  "Pull changes from a remote repository to a local one."
  ;; :info-manual "(magit)Initiating a Commit"
  ;; :man-page "git-commit"
  ["Arguments"
   ("-u" "Update"     "--update")
   ("=e" "Rebase"     "--rebase")
   ("-r" "Revision"   "--rev")
   ("-b" "Branch"     "--branch")
   ("-B" "Bookmark"   "--bookmark")
  ]
  [["Actions"
    ("p" "Default"          manky-pull-default)
    ("o" "Other"            manky-pull-other)
   ]]
  ;; ["Edit"
  ;;  ("f" "Fixup"          magit-commit-fixup)
  ;;  ("s" "Squash"         magit-commit-squash)
  ;;  ("A" "Augment"        magit-commit-augment)
  ;;  (6 "x" "Absorb changes" magit-commit-absorb)]
  ;; [""
  ;;  ("F" "Instant fixup"  magit-commit-instant-fixup)
  ;;  ("S" "Instant squash" magit-commit-instant-squash)]
  ;; ]
;; (when (not (or manky-staged-files (manky-merge-p)))
;;   (if (y-or-n-p "Nothing staged. Stage and commit all changes? ")
;;       (manky-stage-all)
;;     (user-error "Nothing staged")))
)

(defun manky-pull-arguments ()
  (transient-args 'manky-pull))

(defun manky-pull-default (args)
  (interactive (list
                (manky-pull-arguments)))
  (manky-hg-pull args))

(defun manky-pull-other (args remote)
  (interactive (list
                (manky-pull-arguments)
                (manky-read-remote "Pull from: ")
                ))
  (manky-hg-pull (append args (list remote))))

  ;; (let ((remote (if current-prefix-arg
  ;;                   (manky-read-remote "Pull from : ")
  ;;                 manky-incoming-repository)))


(defun manky-remotes ()
  (mapcar #'car (manky-hg-config-section "paths")))

(defun manky-read-remote (prompt)
  (manky-completing-read prompt
                         (manky-remotes)))

(defun manky-read-revision (prompt)
  (let ((revision (read-string prompt)))
    (unless (manky-hg-revision-p revision)
      (error "%s is not a revision" revision))
    revision))

(defun manky-push ()
  "Pushes current branch to the default path."
  (interactive)
  (let* ((branch (manky-current-branch))
         (remote (if current-prefix-arg
                     (manky-read-remote
                      (format "Push branch %s to : " branch))
                   manky-outgoing-repository)))
    (if (string= "" remote)
        (manky-run-hg-async "push" "--branch" branch)
      (manky-run-hg-async "push" "--branch" branch remote))))

(defun manky-checkout (node)
  (interactive (list (manky-read-revision "Update to: ")))
  (manky-run-hg "update" node))

(defun manky-merge (node)
  (interactive (list (manky-read-revision "Merge with: ")))
  (manky-run-hg "merge" node))

(defun manky-histedit (node)
  (interactive (list (manky-read-revision "Edit history starting from: ")))
  (manky-run-hg-async "histedit" "--rev" node
                      "--config" "ui.interface.histedit=text"
                      "--config" "histedit.linelen=200" ;; "--config" "histedit.summary-template={desc|firstline}"
                      ))

(defun manky-reset-tip ()
  (interactive)
  (when (yes-or-no-p "Discard all uncommitted changes? ")
    (manky-run-hg "update" "--clean")))

(defun manky-addremove-all ()
  (interactive)
  (manky-run-hg "addremove"))

(defun manky-rollback ()
  (interactive)
  (manky-run-hg "rollback"))

;;; Merging

(defun manky-unresolve-item ()
  "Mark the item at point as unresolved."
  (interactive)
  (manky-section-action "unresolve"
    ((merged diff)
     (if (eq (manky-diff-item-kind (manky-current-section)) 'resolved)
         (manky-run-hg "resolve" "--unmark" (manky-diff-item-file (manky-current-section)))
       (user-error "Already unresolved")))))

(defun manky-resolve-item ()
  "Mark the item at point as resolved."
  (interactive)
  (manky-section-action "resolve"
    ((merged diff)
     (if (eq (manky-diff-item-kind (manky-current-section)) 'unresolved)
         (manky-run-hg "resolve" "--mark" (manky-diff-item-file (manky-current-section)))
       (user-error "Already resolved")))))

;; History

(defun manky-backout (revision)
  "Runs hg backout."
  (interactive (list (manky-read-revision "Backout : ")))
  (manky-pop-to-log-edit 'backout revision))

(defun manky-backout-item ()
  "Backout the revision represented by current item."
  (interactive)
  (manky-section-action "backout"
    ((log commits commit)
     (manky-backout (manky-section-info (manky-current-section))))))

(defun manky-show-item-or-scroll-up ()
  (interactive)
  (manky-section-action "show"
    ((commit)
     (manky-show-commit (manky-section-info (manky-current-section)) nil #'scroll-up))
    (t
     (scroll-up))))

(defun manky-show-item-or-scroll-down ()
  (interactive)
  (manky-section-action "show"
    ((commit)
     (manky-show-commit (manky-section-info (manky-current-section)) nil #'scroll-down))
    (t
     (scroll-down))))

;;; Miscellaneous

(defun manky-revert-file (file)
  (when (or (not manky-revert-item-confirm)
	    (yes-or-no-p (format "Revert %s? " file)))
    (manky-run-hg "revert" "--no-backup" file)
    (let ((file-buf (find-buffer-visiting
		     (concat (manky-get-root-dir) file))))
      (if file-buf
	  (save-current-buffer
	    (set-buffer file-buf)
	    (revert-buffer t t t))))))

(defun manky-discard-item ()
  "Delete the file if not tracked, otherwise revert it."
  (interactive)
  (manky-section-action "discard"
    ((untracked file)
     (when (yes-or-no-p (format "Delete %s? " (manky-section-info (manky-current-section))))
       (delete-file (manky-section-info (manky-current-section)))
       (manky-refresh-buffer)))
    ((untracked changes)
     (when (yes-or-no-p (format "Delete %s? " (manky-section-info (manky-current-section))))
       (delete-file (manky-section-info (manky-current-section)))
       (manky-refresh-buffer)))
    ((hunk)
     (let* ((file (manky-diff-item-file (manky-hunk-item-diff (manky-current-section))))
            (target (find-file-noselect file))
            (dffn (lambda (&rest r) file))
            (reverse t))
       (with-current-buffer target
         ;; There's wiggle room here (no pun intended): perhaps query
         ;; the user if they want to try discarding the hunk anyway?
         (when (buffer-modified-p)
           (user-error "Cannot discard hunk: buffer has been modified")))
       (cl-letf (((symbol-function 'diff-find-file-name) dffn))
         (diff-apply-hunk reverse))
       (with-current-buffer target
         (basic-save-buffer))
       (manky-refresh-buffer)))
    ((untracked)
     (message "untracked %s" (manky-section-info (manky-current-section))))
    ((changes diff)
     (manky-revert-file (manky-diff-item-file (manky-current-section))))
    ((staged diff)
     (manky-revert-file (manky-diff-item-file (manky-current-section))))
    ((missing file)
     (manky-revert-file (manky-section-info (manky-current-section))))
    ((shelf)
     (manky-delete-shelf (manky-section-info (manky-current-section))))))

(defun manky-quit-window (&optional kill-buffer)
  "Bury the buffer and delete its window.  With a prefix argument, kill the
buffer instead."
  (interactive "P")
  (quit-window kill-buffer (selected-window)))

;;; Refresh

(defun manky-revert-buffers (dir &optional ignore-modtime)
  (dolist (buffer (buffer-list))
    (when (and buffer
               (buffer-file-name buffer)
               (manky-string-starts-with-p (buffer-file-name buffer) dir)
               (file-readable-p (buffer-file-name buffer))
               (or ignore-modtime (not (verify-visited-file-modtime buffer)))
               (not (buffer-modified-p buffer)))
      (with-current-buffer buffer
        (condition-case var
            (revert-buffer t t t)
          (error (let ((signal-data (cadr var)))
                   (cond (t (manky-bug-report signal-data))))))))))

(defvar manky-refresh-needing-buffers nil)
(defvar manky-refresh-pending nil)

(defun manky-refresh-wrapper (func)
  "A helper function for `manky-with-refresh'."
  (manky-with-process
    (if manky-refresh-pending
        (funcall func)
      (let* ((dir default-directory)
             (status-buffer (manky-find-status-buffer dir))
             (manky-refresh-needing-buffers nil)
             (manky-refresh-pending t))
        (unwind-protect
            (funcall func)
          (when manky-refresh-needing-buffers
            (manky-revert-buffers dir)
            (dolist (b (cl-adjoin status-buffer
                                  manky-refresh-needing-buffers))
              (manky-refresh-buffer b))))))))

(defun manky-need-refresh (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (setq manky-refresh-needing-buffers
          (cl-adjoin buffer manky-refresh-needing-buffers))))

(defun manky-refresh ()
  "Refresh current buffer to match repository state.
Also revert every unmodified buffer visiting files
in the corresponding directory."
  (interactive)
  (manky-with-refresh
    (manky-need-refresh)))

(defun manky-refresh-buffer (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((old-line (line-number-at-pos))
           (old-section (manky-current-section))
           (old-path (and old-section
                          (manky-section-path old-section)))
           (section-line (and old-section
                              (count-lines
                               (manky-section-beginning old-section)
                               (point)))))
      (if manky-refresh-function
          (apply manky-refresh-function
                 manky-refresh-args))
      (let ((s (and old-path (manky-find-section old-path manky-top-section))))
        (cond (s
               (goto-char (manky-section-beginning s))
               (forward-line section-line))
              (t
               (manky-goto-line old-line)))
        (dolist (w (get-buffer-window-list (current-buffer)))
          (set-window-point w (point)))))))

(defvar manky-last-point nil)

(defun manky-remember-point ()
  (setq manky-last-point (point)))

(defun manky-invisible-region-end (pos)
  (while (and (not (= pos (point-max))) (invisible-p pos))
    (setq pos (next-char-property-change pos)))
  pos)

(defun manky-invisible-region-start (pos)
  (while (and (not (= pos (point-min))) (invisible-p pos))
    (setq pos (1- (previous-char-property-change pos))))
  pos)

(defun manky-correct-point-after-command ()
  "Move point outside of invisible regions.

Emacs often leaves point in invisible regions, it seems.  To fix
this, we move point ourselves and never let Emacs do its own
adjustments.

When point has to be moved out of an invisible region, it can be
moved to its end or its beginning.  We usually move it to its
end, except when that would move point back to where it was
before the last command."
  (if (invisible-p (point))
      (let ((end (manky-invisible-region-end (point))))
        (goto-char (if (= end manky-last-point)
                       (manky-invisible-region-start (point))
                     end))))
  (setq disable-point-adjustment t))

(defun manky-post-command-hook ()
  (manky-correct-point-after-command))

;;; Manky mode

(define-derived-mode manky-mode special-mode "Manky"
  "View the status of a mercurial repository.

\\{manky-mode-map}"
  (setq buffer-read-only t)
  (setq mode-line-process "")
  (setq truncate-lines t)
  (add-hook 'pre-command-hook #'manky-remember-point nil t)
  (add-hook 'post-command-hook #'manky-post-command-hook t t))

(defvar-local manky-submode nil)

(defun manky-mode-init (dir submode refresh-func &rest refresh-args)
  (manky-mode)
  (setq default-directory dir
        manky-submode submode
        manky-refresh-function refresh-func
        manky-refresh-args refresh-args)
  (manky-refresh-buffer))


;;; Hg utils

(defmacro manky-with-temp-file (file &rest body)
  "Create a temporary file name, evaluate BODY and delete the file."
  (declare (indent 1)
           (debug (symbolp body)))
  `(let ((,file (make-temp-file "manky-temp-file")))
     (unwind-protect
         (progn ,@body)
       (delete-file ,file))))

(defun manky-hg-insert (args)
  (insert (manky-hg-output args)))

(defun manky-hg-output (args)
  (manky-with-temp-file stderr
    (save-current-buffer
      (with-temp-buffer
        (unless (eq 0 (apply #'manky-process-file
                             manky-hg-executable
                             nil (list t stderr) nil
                             (append manky-hg-standard-options args)))
          (error (with-temp-buffer
                   (insert-file-contents stderr)
                   (buffer-string))))
        (buffer-string)))))

(defun manky-hg-string (&rest args)
  (manky-trim-line (manky-hg-output args)))

(defun manky-hg-lines (&rest args)
  (manky-split-lines (manky-hg-output args)))

(defun manky-hg-exit-code (&rest args)
  (apply #'manky-process-file manky-hg-executable nil nil nil
         (append manky-hg-standard-options args)))

(defun manky-hg-revision-p (revision)
  (eq 0 (manky-hg-exit-code "identify" "--rev" revision)))

;; TODO needs cleanup
(defun manky-get-root-dir ()
  (if (and (featurep 'tramp)
	   (tramp-tramp-file-p default-directory))
      (manky-get-tramp-root-dir)
    (manky-get-local-root-dir)))

(defun manky-get-local-root-dir ()
  (let ((root (manky-hg-string "root")))
    (if root
	(concat root "/")
      (user-error "Not inside a hg repo"))))

(defun manky-get-tramp-root-dir ()
  (let ((root (manky-hg-string "root"))
        (tramp-path (vconcat (tramp-dissect-file-name default-directory))))
    (if root
        (progn (aset tramp-path 6 root)
               (concat (apply 'tramp-make-tramp-file-name (cdr (append tramp-path nil)))
                       "/"))
      (user-error "Not inside a hg repo"))))

(defun manky-find-buffer (submode &optional dir)
  (let ((rootdir (expand-file-name (or dir (manky-get-root-dir)))))
    (cl-find-if (lambda (buf)
                  (with-current-buffer buf
                    (and default-directory
                         (equal (expand-file-name default-directory) rootdir)
                         (eq major-mode 'manky-mode)
                         (eq manky-submode submode))))
                (buffer-list))))

(defun manky-find-status-buffer (&optional dir)
  (manky-find-buffer 'status dir))

(defun manky-for-all-buffers (func &optional dir)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if (and (eq major-mode 'manky-mode)
               (or (null dir)
                   (equal default-directory dir)))
          (funcall func)))))

(defun manky-hg-config ()
  "Return an alist of ((section . key) . value)"
  (mapcar (lambda (line)
            (string-match "^\\([^.]*\\)\.\\([^=]*\\)=\\(.*\\)$" line)
            (cons (cons (match-string 1 line)
                        (match-string 2 line))
                  (match-string 3 line)))
          (manky-hg-lines "debugconfig")))

(defun manky-hg-config-section (section)
  "Return an alist of (name . value) for section"
  (mapcar (lambda (item)
            (cons (cdar item) (cdr item)))
          (cl-remove-if-not (lambda (item)
                              (equal section (caar item)))
                            (manky-hg-config))))

(defvar manky-el-directory
  (file-name-directory (or load-file-name default-directory))
  "The parent directory of manky.el")

(defun manky-get-style-path (filename)
  (concat (file-name-as-directory (concat manky-el-directory "style"))
          filename))

(defvar manky-hg-style-log-graph
  (manky-get-style-path "log-graph"))

(defvar manky-hg-style-log-commit
  (manky-get-style-path "log-commit"))

(defvar manky-hg-style-files-status
  (manky-get-style-path "files-status"))

(defvar manky-hg-style-tags
  (manky-get-style-path "tags"))

(defun manky-hg-log-tags (revision &rest args)
  (apply #'manky-hg-lines "log"
         "--style" manky-hg-style-tags
         "--rev" revision args))

(defun manky-qtip-p ()
  "Return non-nil if the current revision is qtip"
  (let ((rev (replace-regexp-in-string "\\+$" ""
                                       (manky-hg-string "identify" "--id"))))
    (let ((manky-cmd-process nil))      ; use single process
      (member "qtip" (manky-hg-log-tags rev "--config" "extensions.mq=")))))


;;; Washers

(defun manky-wash-status-lines (callback)
  "For every status line in the current buffer, remove it and call CALLBACK.
CALLBACK is called with the status and the associated filename."
  (while (and (not (eobp))
              (looking-at "\\([A-Z!? ]\\) \\([^\t\n]+\\)$"))
    (let ((status (cl-case (string-to-char (match-string-no-properties 1))
                    (?M 'modified)
                    (?A 'new)
                    (?R 'removed)
                    (?C 'clean)
                    (?! 'missing)
                    (?? 'untracked)
                    (?I 'ignored)
                    (?U 'unresolved)
                    (t nil)))
          (file (match-string-no-properties 2)))
      (manky-delete-line t)
      (funcall callback status file))))

;; File

(defun manky-wash-files ()
  (let ((empty t))
    (manky-wash-status-lines
     (lambda (_status file)
       (setq empty nil)
       (manky-with-section file 'file
         (manky-set-section-info file)
         (insert file "\n"))))
    (unless empty
      (insert "\n"))))

;; Hunk

(defun manky-hunk-item-diff (hunk)
  (let ((diff (manky-section-parent hunk)))
    (or (eq (manky-section-type diff) 'diff)
        (error "Huh?  Parent of hunk not a diff"))
    diff))

(defun manky-hunk-item-target-line (hunk)
  (save-excursion
    (beginning-of-line)
    (let ((line (line-number-at-pos)))
      (goto-char (manky-section-beginning hunk))
      (if (not (looking-at "@@+ .* \\+\\([0-9]+\\),[0-9]+ @@+"))
          (error "Hunk header not found"))
      (let ((target (string-to-number (match-string 1))))
        (forward-line)
        (while (< (line-number-at-pos) line)
          ;; XXX - deal with combined diffs
          (if (not (looking-at "-"))
              (setq target (+ target 1)))
          (forward-line))
        target))))

(defun manky-wash-hunk ()
  (if (looking-at "\\(^@+\\)[^@]*@+")
      (let ((n-columns (1- (length (match-string 1))))
            (head (match-string 0)))
        (manky-with-section head 'hunk
          (add-text-properties (match-beginning 0) (1+ (match-end 0))
                               '(face manky-diff-hunk-header))
          (forward-line)
          (while (not (or (eobp)
                          (looking-at "^diff\\|^@@")))
            (let ((prefix (buffer-substring-no-properties
                           (point) (min (+ (point) n-columns) (point-max)))))
              (cond ((string-match "\\+" prefix)
                     (manky-put-line-property 'face 'manky-diff-add))
                    ((string-match "-" prefix)
                     (manky-put-line-property 'face 'manky-diff-del))
                    (t
                     (manky-put-line-property 'face 'manky-diff-none))))
            (forward-line))))
    nil))

;; Diff

(defvar manky-hide-diffs nil)

(defun manky-diff-item-kind (diff)
  (car (manky-section-info diff)))

(defun manky-diff-item-file (diff)
  (cadr (manky-section-info diff)))

(defun manky-diff-line-file ()
  (cond ((looking-at "^diff -r \\([^ ]*\\) \\(-r \\([^ ]*\\) \\)?\\(.*\\)$")
         (match-string-no-properties 4))
	((looking-at (rx "diff --git a/" (group (+? anything)) " b/"))
	 (match-string-no-properties 1))
        (t
         nil)))

(defun manky-wash-diff-section (&optional status file)
  (let ((case-fold-search nil))
   (cond ((looking-at "^diff ")
	  (let* ((file (manky-diff-line-file))
		 (end (save-excursion
		        (forward-line)
		        (if (search-forward-regexp "^diff \\|^@@" nil t)
			    (goto-char (match-beginning 0))
			  (goto-char (point-max)))
		        (point-marker)))
		 (status (or status
			     (cond
			      ((save-excursion
				 (search-forward-regexp "^--- /dev/null" end t))
			       'new)
			      ((save-excursion
				 (search-forward-regexp "^+++ /dev/null" end t))
			       'removed)
			      (t 'modified)))))
	    (manky-set-section-info (list status file))
	    (manky-insert-diff-title status file)
            ;; Remove the 'diff ...' text and '+++' text, as it's redundant.
            (delete-region (point) end)
	    (let ((manky-section-hidden-default nil))
	      (manky-wash-sequence #'manky-wash-hunk))))
	 ;; sometimes diff returns empty output
	 ((and status file)
	  (manky-set-section-info (list status file))
	  (manky-insert-diff-title status file))
	 (t nil))))

(defun manky-wash-diff ()
  (let ((manky-section-hidden-default manky-hide-diffs))
    (manky-with-section nil 'diff
      (manky-wash-diff-section))))

(defun manky-wash-diffs ()
  (manky-wash-sequence #'manky-wash-diff))

(defun manky-insert-diff (file &optional status cmd)
  (let ((p (point)))
    (manky-hg-insert (list (or cmd "diff") "--unified" "4" file))
    (if (not (eq (char-before) ?\n))
        (insert "\n"))
    (save-restriction
      (narrow-to-region p (point))
      (goto-char p)
      (manky-wash-diff-section status file)
      (goto-char (point-max)))))

(defun manky-insert-diff-title (status file)
  (insert
   (format "%-10s %s\n"
          (propertize
           (symbol-name status)
           'face
           (if (eq status 'unresolved) 'warning 'manky-diff-title))
          (propertize file 'face 'manky-diff-title))))

;;; Changes

(defun manky-wash-changes ()
  (let ((changes-p nil))
    (manky-wash-status-lines
     (lambda (status file)
       (let ((manky-section-hidden-default manky-hide-diffs))
         (if (or manky-staged-all-files
                 (member file manky-old-staged-files))
             (manky-stage-file file)
           (manky-with-section file 'diff
             (manky-insert-diff file status))
           (setq changes-p t)))))
    (when changes-p
      (insert "\n"))))


(defun manky-insert-changes ()
  (let ((manky-hide-diffs t))
    (setq manky-old-staged-files (cl-copy-list manky-staged-files))
    (setq manky-staged-files '())
    (manky-hg-section 'changes "Changes:" #'manky-wash-changes
                      "status" "--modified" "--added" "--removed")))

;; Staged Changes

(defun manky-stage-file (file)
  (if (not (member file manky-staged-files))
      (setq manky-staged-files (cons file manky-staged-files))))

(defun manky-unstage-file (file)
  (setq manky-staged-files (delete file manky-staged-files)))

(defun manky-insert-staged-changes ()
  (when manky-staged-files
    (manky-with-section 'staged nil
      (insert (propertize "Staged changes:" 'face 'manky-section-title) "\n")
      (let ((manky-section-hidden-default t))
        (dolist (file manky-staged-files)
          (manky-with-section file 'diff
            (manky-insert-diff file)))))
    (insert "\n"))
  (setq manky-staged-all-files nil))

(defun manky-extensions ()
  "Return a list of all the enabled mercurial extensions."
  (let* ((config
          (string-trim (shell-command-to-string "hg config extensions")))
         (lines
          (split-string config "\n"))
         extensions)
    (dolist (line lines)
      (unless (string-match-p (rx "!" eos) line)
        (setq line (string-remove-prefix "extensions." line))
        (setq line (string-remove-suffix "=" line)))
      (push line extensions))
    (nreverse extensions)))
;;; Parents

(defvar-local manky-parents nil)

(defun manky-merge-p ()
  (> (length manky-parents) 1))

(defun manky-wash-parent ()
  (if (looking-at "changeset:\s*\\([0-9]+\\):\\([0-9a-z]+\\)")
      (let ((changeset (match-string 2))
	    (line (buffer-substring (line-beginning-position) (line-end-position))))
        (push changeset manky-parents)

	;; Remove the plain text 'changeset: ...' and replace it with
	;; propertized text, plus a section that knows the changeset
	;; (so RET shows the full commit).
	(manky-with-section 'commit nil
	  (manky-set-section-info changeset)
	  (manky-delete-line t)
	  (insert line "\n")

	  (put-text-property
           (match-beginning 1)
           (match-end 1)
           'face
           'manky-commit-id)
          (put-text-property
           (match-beginning 2)
           (match-end 2)
           'face
           'manky-commit-id))

        (while (not (or (eobp)
                        (looking-at "changeset:\s*\\([0-9]+\\):\\([0-9a-z]+\\)")))
          (forward-line))
        t)
    nil))

(defun manky-wash-parents ()
  (manky-wash-sequence #'manky-wash-parent))

(defun manky-insert-parents ()
  (manky-hg-section 'parents "Parents:"
                    #'manky-wash-parents "parents"))

;;; Merged Files

(defvar-local manky-merged-files nil)

(defun manky-wash-merged-files ()
  (let ((empty t))
    (manky-wash-status-lines
     (lambda (status file)
       (setq empty nil)
       (let ((manky-section-hidden-default manky-hide-diffs))
        (push file manky-merged-files)
        ;; XXX hg uses R for resolved and removed status
        (let ((status (if (eq status 'unresolved)
                           'unresolved
                        'resolved)))
           (manky-with-section file 'diff
             (manky-insert-diff file status))))))
    (unless empty
      (insert "\n"))))

(defun manky-wash-histedit-state ()
  (while (and (not (eobp))
              (not (looking-at-p "[[:space:]]*$")))
    (forward-line 1))
  (while (not (eobp))
    (manky-delete-line t)))

;; (manky-wash-status-lines
;;  (lambda (_status file)
;;    (let ((manky-section-hidden-default manky-hide-diffs))
;;      (when (not (member file manky-merged-files))
;;        (manky-with-section file 'diff
;;          (manky-insert-diff file)))))))

(defun manky-file-contents (file &optional noerror)
  (with-temp-buffer
    (condition-case nil
	    (progn
	      (insert-file-contents file)
	      (buffer-string))
	  (file-error
       (funcall (if noerror #'message #'user-error)
                "Unable to read file %S"
                file)
	   nil))))

(defun manky-insert-histedit-state ()
  ;; Edit history between 6051e609a4a8 and f06bac4905c9
  (when (manky-histedit-in-progress-p)
    (let ((p (point)))
      (pcase-let ((`(,filename ,len)
                   (insert-file-contents (f-join (manky-get-root-dir) ".hg" "histedit-last-edit.txt"))))
        (unless (re-search-forward "# Edit history between \\([0-9a-z]+\\) and \\([0-9a-z]+\\)")
          (error "Malformed histedit-last-edit.txt"))
        (let ((title (format "Histedit between %s and %s" (match-string 1) (match-string 2))))
          (beginning-of-line)
          (delete-region (point) (point-max))
          (goto-char p)
          (manky-with-section title 'histedit-state
            (insert (propertize title 'face 'manky-section-title) "\n")
            (goto-char (point-max))))))))

  ;;         ))))))

  ;;   (defun insert-histedit-state ()
  ;;       (forward-char len)))
  ;;   (let ((section (manky-insert-section
  ;;                   'histedit-state
  ;;                   "Histedit between XXX"
  ;;                   #'manky-wash-histedit-state
  ;;                   #'insert-histedit-state
  ;;                   )))
  ;;     ;; (setf (manky-section-title section) "Test")
  ;;     section)))


  ;; ;;  (append manky-hg-standard-options args)))
  ;; ;; ;; ;; (let ((manky-hide-diffs t))
  ;; ;; ;; ;;   (setq manky-merged-files '())
  ;; ;; ;; (manky-hg-section 'merged "Merged Files:" #'manky-wash-merged-files
  ;; ;; ;;                   "resolve" "--list")))

(defun manky-insert-merged-files ()
  (let ((manky-hide-diffs t))
    (setq manky-merged-files '())
    (manky-hg-section 'merged "Merged Files:" #'manky-wash-merged-files
                      "resolve" "--list")))

;;; Unmodified Files

(defun manky-wash-unmodified-files ()
  (manky-wash-status-lines
   (lambda (_status file)
     (let ((manky-section-hidden-default manky-hide-diffs))
       (when (not (member file manky-merged-files))
         (manky-with-section file 'diff
           (manky-insert-diff file)))))))

(defun manky-insert-resolved-files ()
  (let ((manky-hide-diffs t))
    (manky-hg-section 'unmodified "Unmodified files during merge:" #'manky-wash-unmodified-files
                      "status" "--modified" "--added" "--removed")))
;;; Status mode

(defun manky-wash-json-status ()
  (let ((p (point))
        (j (json-read)))
    (delete-region p (point))

    (manky-with-section "Untracked files:" 'untracked
      (cl-loop for item across j
               when (s-equals? (alist-get 'status item) "?")
               do
               (message "item %s" item)
               (insert
                (format
                 "%s   %s\n"
                 "untracked "
                 (alist-get 'path item)))))

    (manky-with-section "Missing files:" 'missing
      (cl-loop for item across j
               when (s-equals? (alist-get 'status item) "R")
               do
               (insert
                (format
                 "%s   %s\n"
                 "missing   "
                 (alist-get 'path item)))))

    (manky-with-section "Changes:" 'changes
      (cl-loop for item across j
               when (or (and (not (alist-get 'status item))
                             (alist-get 'path item))
                        (-contains? '("M" "A" "R" "U") (alist-get 'status item)))
               do
               ;; (message "item %s" (type-of (alist-get 'status item)))
               (insert
                (format
                 "%s   %s\n"
                 (or (and (alist-get 'resolved item)   "resolved  ")
                     (and (alist-get 'unresolved item) "unresolved")
                     (alist-get 'status item))
                 (alist-get 'path item)))))

    (manky-with-section "Unfinished:" 'unfinished
      (cl-loop for item across j
               when (alist-get 'unfinished item)
               do
               ;; (message "item %s" item)
               (insert
                (format
                 "%s   %s\n"
                 (alist-get 'unfinished item)
                 (alist-get 'unfinishedmsg item)))))
))

(defun manky-refresh-status ()
  (setq manky-parents nil
        manky-merged-files nil)
  (manky-create-buffer-sections
    (manky-with-section 'status nil
      (manky-insert-parents)

      (manky-hg-section 'changes "Changes:"
                        #'manky-wash-json-status
                        "--config" "commands.status.verbose=1"
                        "status" "-Tjson")

      (when (manky-histedit-in-progress-p)
        (manky-insert-histedit-state))
      (if (manky-merge-p)
          (progn
            (manky-insert-merged-files)
            (manky-insert-resolved-files))
        (manky-insert-changes)
        (manky-insert-staged-changes)
        (manky-insert-shelves)
        ))))

(define-minor-mode manky-status-mode
  "Minor mode for hg status.

\\{manky-status-mode-map}"
  :group manky
  :init-value ()
  :lighter ()
  :keymap manky-status-mode-map)

;;;###autoload
(defun manky-status (&optional directory)
  "Show the status of Hg repository."
  (interactive)
  (manky-with-process
    (let* ((rootdir (or directory (manky-get-root-dir)))
           (buf (or (manky-find-status-buffer rootdir)
                    (generate-new-buffer
                     (concat "*manky: "
                             (file-name-nondirectory
                              (directory-file-name rootdir)) "*")))))
      (pop-to-buffer buf)
      (manky-mode-init rootdir 'status #'manky-refresh-status)
      (manky-status-mode t))))

;;; Log mode

(define-minor-mode manky-log-mode
  "Minor mode for hg log.

\\{manky-log-mode-map}"
  :group manky
  :init-value ()
  :lighter ()
  :keymap manky-log-mode-map)

(defvar manky-log-buffer-name "*manky-log*")

(defun manky-propertize-labels (label-list &rest properties)
  "Propertize labels (tag/branch/bookmark/...) in LABEL-LIST.

PROPERTIES is the arguments for the function `propertize'."
  (apply #'concat
         (apply #'append
                (mapcar (lambda (l)
                          (unless (or (string= l "") (string= l "None"))
                            (list (apply #'propertize l properties) " ")))
                        label-list))))

;; (defun manky-set-window-margin (&optional window)
;;   (when (or window (setq window (get-buffer-window)))
;;     (with-selected-window window
;;       (set-window-margins
;;        nil (car (window-margins))
;;        (and 24))))) ;; (magit-buffer-margin-p)
;;        ;; (nth 2 magit-buffer-margin))))))

(defun manky-present-log-line (width graph id branches tags bookmarks phase author date obsolete message)
  (let* ((hg-info (concat
                   (propertize (substring id 0 8) 'face 'manky-log-sha1)
                   " "
                   graph
                   (manky-propertize-labels branches 'face 'manky-log-head-label-local)
                   (manky-propertize-labels tags 'face 'manky-log-head-label-tags)
                   (manky-propertize-labels bookmarks 'face 'manky-log-head-label-bookmarks)
                   (unless (or (string= phase "") (string= phase "public"))
                     (manky-propertize-labels `(,phase) 'face 'manky-log-head-label-phase))
                   (unless (or (string= obsolete "") (string= obsolete "stable"))
                     (manky-propertize-labels `(,obsolete) 'face 'manky-log-head-label-obsolete))))
         (total-space-left (max 0 (- width (length hg-info))))
         (author-date-space-taken (+ 16 (min 10 (length author))))
         (message-space-left (number-to-string (max 0 (- total-space-left author-date-space-taken 1))))
         ;; (msg-format (concat "%-" message-space-left "." message-space-left "s"))
         (msg (format "%s" ;; msg-format
                      message)))
    (let* ((shortened-msg (if (< 3 (length msg))
                              (concat (substring msg 0 -3) "...")
                            msg))
           (msg (if (>= (string-to-number message-space-left) (length message))
                   msg
                  shortened-msg)))
      (concat
       hg-info
       (propertize msg 'face 'manky-log-message)
))))

;;;###autoload (autoload 'manky-log "manky" nil t)
(define-transient-command manky-log ()
  "Show revision history."
  ;; :info-manual "(magit)Initiating a Commit"
  ;; :man-page "git-commit"
  ["Arguments"
   ("=h" "Show hidden revisions"   "--hidden") ;; TODO: support this.
   ;; ("-e" "Allow empty commit"                     "--allow-empty")
   ;; ("-v" "Show diff of changes to be committed"   ("-v" "--verbose"))
   ;; ("-n" "Disable hooks"                          ("-n" "--no-verify"))
   ;; ("-R" "Claim authorship and reset author date" "--reset-author")
   ;; (magit:--author :description "Override the author")
   ;; (7 "-D" "Override the author date" "--date=" transient-read-date)
   ;; ("-s" "Add Signed-off-by line"                 ("-s" "--signoff"))
   ;; (5 magit:--gpg-sign)
   ;; (magit-commit:--reuse-message)
   ]
  [["Actions"
    ("l" "Current"          manky-log-current-branch)
    ("a" "All"              manky-log-all)
   ;;("o" "Other"            manky-log-other)
    ("r" "Revset"           manky-log-revset)
    ("c" "Containing"       manky-log-containing)
    ("o" "Obsolete"         manky-log-obsolete)
   ]]
  ;; ["Edit"
  ;;  ("f" "Fixup"          magit-commit-fixup)
  ;;  ("s" "Squash"         magit-commit-squash)
  ;;  ("A" "Augment"        magit-commit-augment)
  ;;  (6 "x" "Absorb changes" magit-commit-absorb)]
  ;; [""
  ;;  ("F" "Instant fixup"  magit-commit-instant-fixup)
  ;;  ("S" "Instant squash" magit-commit-instant-squash)]
  ;; ]
;; (when (not (or manky-staged-files (manky-merge-p)))
;;   (if (y-or-n-p "Nothing staged. Stage and commit all changes? ")
;;       (manky-stage-all)
;;     (user-error "Nothing staged")))
)

(defun manky-log-arguments ()
  (transient-args 'manky-log))

(defun manky-log-current-branch (args)
  (interactive (list
                (manky-log-arguments)))
  (manky-log-setup-buffer (append args (list "--rev" "ancestors(.)"))))

(defun manky-log-buffer-file (args)
  "View a log of commits that affected the current file."
  (interactive (list
                (manky-log-arguments)))
  (manky-log-setup-buffer (append args (list "--rev" "ancestors(.)" "--" (buffer-file-name)))))

(defun manky-log-all (args)
  (interactive (list
                (manky-log-arguments)))
  (manky-log-setup-buffer (append args (list))))

(defun manky-log-containing (args rev)
  (interactive (list
                (manky-log-arguments)
                (manky-section-case "log"
                  ((branch)
                   (manky-section-info (manky-current-section)))
                  ((log commits commit)
                   (manky-section-info (manky-current-section))))))
  (manky-log-setup-buffer (append args (list "--rev" (format "!public() & (::%s | %s::)" rev rev)))))

(defun manky-log-obsolete (args rev)
  (interactive (list
                (manky-log-arguments)
                (manky-section-case "log"
                  ((branch)
                   (manky-section-info (manky-current-section)))
                  ((log commits commit)
                   (manky-section-info (manky-current-section))))))
  (manky-log-setup-buffer (append args (list "--rev" rev)) 'olog))

(defvar manky-log-revset-history '())

(defun manky-log-revset (revset args)
  (interactive (list
                (read-string "Revset: " nil 'manky-log-revset-history)
                (manky-log-arguments)))
  (manky-log-setup-buffer (append args (list "--rev" revset))))

(defun manky-log-setup-buffer (args &optional cmd)
  (manky-with-process
    (let ((topdir (manky-get-root-dir))
          (refresh-func (pcase cmd
                          ('olog #'manky-refresh-olog-buffer)
                          (_ #'manky-refresh-log-buffer))))
      (pop-to-buffer manky-log-buffer-name)
      (setq default-directory topdir
            manky-root-dir topdir)
      (manky-mode-init topdir 'log (funcall refresh-func args))
      (manky-log-mode t)
      (manky-set-buffer-margin)
      ;; (dolist (window (get-buffer-window-list nil nil 0))
      ;;   (with-selected-window window
      ;;     (manky-set-window-margin)
      ;;     (add-hook 'window-configuration-change-hook
      ;;               'manky-set-window-margin nil t)))
      )))

(defvar manky-log-graph-node-re
  "^\\([\\/@ox%\\*+-|\s]+\s*\\)")

(defvar manky-log-graph-re
  (concat
   manky-log-graph-node-re " "          ; 1. graph node
   "\\([a-z0-9]\\{40\\}\\) "            ; 2. id
   "<branches>\\(.?*\\)</branches>"     ; 3. branches
   "<tags>\\(.?*\\)</tags>"             ; 4. tags
   "<bookmarks>\\(.?*\\)</bookmarks>"   ; 5. bookmarks
   "<phase>\\(.?*\\)</phase>"           ; 6. phase
   "<author>\\(.*?\\)</author>"         ; 7. author
   "<manky-date>\\([0-9]+\\).?*</manky-date>" ; 8. date
   "<obsolete>\\(.?*\\)</obsolete>"     ; 9. obsolete
   "\\(.*\\)$"                          ; 10. msg
   ))

(defun manky-decode-xml-entities (str)
  (setq str (replace-regexp-in-string "&lt;" "<" str))
  (setq str (replace-regexp-in-string "&gt;" ">" str))
  (setq str (replace-regexp-in-string "&amp;" "&" str))
  str)

(defun manky-xml-items-to-list (xml-like tag)
  "Convert XML-LIKE string which has repeated TAG items into a list of strings.

Example:
    (manky-xml-items-to-list \"<tag>A</tag><tag>B</tag>\" \"tag\")
    ; => (\"A\" \"B\")
"
  (mapcar #'manky-decode-xml-entities
          (split-string (replace-regexp-in-string
                         (format "^<%s>\\|</%s>$" tag tag) "" xml-like)
                        (format "</%s><%s>" tag tag))))

(defvar manky-log-count ()
  "Internal var used to count the number of logs actually added in a buffer.")

(defun manky--author-name (s)
  "Extract the name from a Mercurial author string."
  (save-match-data
    (cond
     ((string-match
       ;; If S contains a name followed by an email addres, take just the name.
       (rx (group (1+ (not (any "<"))))
           " <")
       s)
      (match-string 1 s))
     ((string-match
       ;; If S is just an email, take the username.
       (rx (group (1+ (not (any "@"))))
           "@")
       s)
      (match-string 1 s))
     (t s))))

(defcustom manky-ellipsis ?…
  "Character used to abbreviate text.

Currently this is used to abbreviate author names in the margin
and in process buffers to elide `manky-hg-global-arguments'."
  :package-version '(manky . "0.3.0")
  :group 'manky-miscellaneous
  :type 'character)

(defun manky-log-format-author-margin (author date)
  (manky-make-margin-overlay
   (concat (propertize
            (truncate-string-to-width
             (or author "")
             manky-buffer-margin-author-width
             nil ?\s (make-string 1 manky-ellipsis))
            'face 'manky-log-author)
           " "
           (propertize
            date
            'face 'manky-log-date))))

(defun manky-wash-log-line ()
  (if (looking-at manky-log-graph-re)
      (let ((width (window-total-width))
            (graph (match-string 1))
            (id (match-string 2))
            (branches (match-string 3))
            (tags (match-string 4))
            (bookmarks (match-string 5))
            (phase (match-string 6))
            (author (manky--author-name (match-string 7)))
            (date (format-time-string "%Y-%m-%d" (seconds-to-time (string-to-number (match-string 8)))))
            (obsolete (match-string 9))
            (msg (match-string 10)))
        (manky-delete-line)
        (manky-with-section id 'commit
          (insert (manky-present-log-line
                   width
                   graph id
                   (manky-xml-items-to-list branches "branch")
                   (manky-xml-items-to-list tags "tag")
                   (manky-xml-items-to-list bookmarks "bookmark")
                   (manky-decode-xml-entities phase)
                   (manky-decode-xml-entities author)
                   (manky-decode-xml-entities date)
                   (manky-decode-xml-entities obsolete)
                   (manky-decode-xml-entities msg)))
          (manky-log-format-author-margin author date)
          (manky-set-section-info id)
          (when manky-log-count (cl-incf manky-log-count))
          (forward-line)
          (when (looking-at (concat manky-log-graph-node-re "$"))
            (let ((graph (match-string 1)))
              (insert "         ")
              (forward-line))))
        t)
    nil))

(defun manky-wash-logs ()
  (let ((manky-old-top-section nil))
    (manky-wash-sequence #'manky-wash-log-line)))

(defmacro manky-create-log-buffer-sections (&rest body)
  "Empty current buffer of text and manky's section, and then evaluate BODY.

if the number of logs inserted in the buffer is `manky-log-cutoff-length'
insert a line to tell how to insert more of them"
  (declare (indent 0)
           (debug (body)))
  `(let ((manky-log-count 0))
     (manky-create-buffer-sections
       (manky-with-section 'log nil
         ,@body
         (if (= manky-log-count manky-log-cutoff-length)
           (manky-with-section "longer"  'longer
             (insert "type \"e\" to show more logs\n")))))))

(defun manky-log-show-more-entries (&optional arg)
  "Grow the number of log entries shown.

With no prefix optional ARG, show twice as much log entries.
With a numerical prefix ARG, add this number to the number of shown log entries.
With a non numeric prefix ARG, show all entries"
  (interactive "P")
  (make-local-variable 'manky-log-cutoff-length)
  (cond
   ((numberp arg)
    (setq manky-log-cutoff-length (+ manky-log-cutoff-length arg)))
   (arg
    (setq manky-log-cutoff-length manky-log-infinite-length))
   (t (setq manky-log-cutoff-length (* manky-log-cutoff-length 2))))
  (manky-refresh))

(defun manky-refresh-log-buffer (args)
  (lambda ()
    (manky-create-log-buffer-sections
      (apply #'manky-hg-section
       'commits
       (if-let ((path (and (-contains-p args "--") (car (last path)))))
           (format "Commits affecting %s:"
                   (file-relative-name path manky-root-dir))
         "Commits:")
       #'manky-wash-logs
       "log"
       "--config" "extensions.graphlog="
       "-G"
       "--limit" (number-to-string manky-log-cutoff-length)
       "--style" manky-hg-style-log-graph
       args))))

       ;; (if revs "--rev" "")
       ;; (if revs revs "")
       ;; (if path path "")))))

(defun manky-refresh-olog-buffer (args)
  (lambda ()
    (manky-create-log-buffer-sections
      (apply #'manky-hg-section
       'commits
       (if-let ((path (and (-contains-p args "--") (car (last path)))))
           (format "Commits affecting %s:"
                   (file-relative-name path manky-root-dir))
         "Commits:")
       #'manky-wash-logs
       "olog"
       "-G"
       args))))

(defun manky-next-sha1 (pos)
  "Return position of next sha1 after given position POS"
  (while (and pos
              (not (equal (get-text-property pos 'face) 'manky-log-sha1)))
    (setq pos (next-single-property-change pos 'face)))
  pos)

(defun manky-previous-sha1 (pos)
  "Return position of previous sha1 before given position POS"
  (while (and pos
              (not (equal (get-text-property pos 'face) 'manky-log-sha1)))
    (setq pos (previous-single-property-change pos 'face)))
  pos)

;;; Blame mode
(define-minor-mode manky-blame-mode
  "Minor mode for hg blame.

\\{manky-blame-mode-map}"
  :group manky
  :init-value ()
  :lighter ()
  :keymap manky-blame-mode-map)

(defun manky-present-blame-line (author changeset text)
  (concat author
	  " "
	  (propertize changeset 'face 'manky-log-sha1)
	  ": "
	  text))

(defvar manky-blame-re
  (concat
   "\\(.*\\) "               ; author
   "\\([a-f0-9]\\{12\\}\\):" ; changeset
   "\\(.*\\)$"               ; text
   ))

(defun manky-wash-blame-line ()
  (if (looking-at manky-blame-re)
      (let ((author (match-string 1))
	    (changeset (match-string 2))
	    (text (match-string 3)))
	(manky-delete-line)
	(manky-with-section changeset 'commit
	  (insert (manky-present-blame-line author changeset text))
	  (manky-set-section-info changeset)
	  (forward-line))
	t)))

(defun manky-wash-blame ()
  (manky-wash-sequence #'manky-wash-blame-line))

(defun manky-refresh-blame-buffer (file-name)
  (manky-create-buffer-sections
    (manky-with-section file-name 'blame
      (manky-hg-section nil nil
			#'manky-wash-blame
			"blame"
			"--user"
			"--changeset"
			file-name))))

(defun manky-blame-current-file ()
  (interactive)
  (manky-with-process
    (let ((file-name (buffer-file-name))
	  (topdir (manky-get-root-dir))
          (line-num (line-number-at-pos))
          (column (current-column)))
      (pop-to-buffer
       (format "*manky-blame: %s*"
               (file-name-nondirectory buffer-file-name)))
      (manky-mode-init topdir 'blame #'manky-refresh-blame-buffer file-name)
      (manky-blame-mode t)
      ;; Put point on the same line number as the original file.
      (forward-line (1- line-num))
      (while (and (not (looking-at ":")) (not (eolp)))
        (forward-char))
      ;; Step over the blame information columns.
      (forward-char (length ":  "))
      ;; Put point at the same column as the original file.
      (forward-char column))))

;;; Commit mode

(define-minor-mode manky-commit-mode
  "Minor mode to view a hg commit.

\\{manky-commit-mode-map}"

  :group manky
  :init-value ()
  :lighter ()
  :keymap manky-commit-mode-map)

(defvar manky-commit-buffer-name "*manky-commit*")

(defun manky-empty-buffer-p (buffer)
  (with-current-buffer buffer
    (< (length (buffer-string)) 1)))

(defun manky-show-commit (commit &optional select scroll)
  (manky-with-process
    (when (manky-section-p commit)
      (setq commit (manky-section-info commit)))
    (unless (and commit
                 (manky-hg-revision-p commit))
      (error "%s is not a commit" commit))
    (let ((topdir (manky-get-root-dir))
          (buffer (get-buffer-create manky-commit-buffer-name)))
      (cond
       ((and scroll
	     (not (manky-empty-buffer-p buffer)))
        (let ((win (get-buffer-window buffer)))
          (cond ((not win)
                 (display-buffer buffer))
                (t
                 (with-selected-window win
                   (funcall scroll))))))
       (t
        (display-buffer buffer)
        (with-current-buffer buffer
          (manky-mode-init topdir 'commit
                           #'manky-refresh-commit-buffer commit)
          (manky-commit-mode t))))
      (if select
          (pop-to-buffer buffer)))))

(defun manky-refresh-commit-buffer (commit)
  (manky-create-buffer-sections
    (manky-hg-section nil nil
                      #'manky-wash-commit
                      "--debug"
                      ;; "--verbose"
                      "log"
                      "--style" manky-hg-style-log-commit
                      "--stat"
                      "--patch"
                      "--rev" commit)))

(defun manky-wash-commit ()
  (save-excursion
    (manky-wash-parent))
  (let ((case-fold-search nil))
   (while (and (not (eobp)) (not (looking-at "^diff ")) )
     (forward-line))
   (when (looking-at "^diff ")
     (manky-wash-diffs))))

;;; Branch mode
(define-minor-mode manky-branches-mode
  "Minor mode for hg branch.

\\{manky-branches-mode-map}"
  :group manky
  :init-value ()
  :lighter ()
  :keymap manky-branches-mode-map)

(defvar manky-branches-buffer-name "*manky-branches*")

(defvar manky-branch-re "^\\(.*[^\s]\\)\s* \\([0-9]+\\):\\([0-9a-z]\\{12\\}\\)\\(.*\\)$")

(defvar-local manky-current-branch-name nil)

(defun manky-present-branch-line (name rev node status)
  (concat rev " : "
          (propertize node 'face 'manky-log-sha1) " "
          (if (equal name manky-current-branch-name)
              (propertize name 'face 'manky-branch)
            name)
          " "
          status))

(defun manky-wash-branch-line ()
  (if (looking-at manky-branch-re)
      (let ((name (match-string 1))
            (rev (match-string 2))
            (node (match-string 3))
            (status (match-string 4)))
        (manky-delete-line)
        (manky-with-section name 'branch
          (insert (manky-present-branch-line name rev node status))
          (manky-set-section-info node)
          (forward-line))
        t)
    nil))

(defun manky-wash-branches ()
  (manky-wash-sequence #'manky-wash-branch-line))

(defun manky-refresh-branches-buffer ()
  (setq manky-current-branch-name (manky-current-branch))
  (manky-create-buffer-sections
    (manky-with-section 'buffer nil
      (manky-hg-section nil "Branches:"
                        #'manky-wash-branches
                        "branches"))))

(defun manky-current-branch ()
  (manky-hg-string "branch"))

(defun manky-branches ()
  (interactive)
  (let ((topdir (manky-get-root-dir)))
    (pop-to-buffer manky-branches-buffer-name)
    (manky-mode-init topdir 'branches #'manky-refresh-branches-buffer)
    (manky-branches-mode t)))

(defun manky-checkout-item ()
  "Checkout the revision represented by current item."
  (interactive)
  (manky-section-action "checkout"
    ((branch)
     (manky-checkout (manky-section-info (manky-current-section))))
    ((log commits commit)
     (manky-checkout (manky-section-info (manky-current-section))))))

(defun manky-merge-item ()
  "Merge the revision represented by current item."
  (interactive)
  (manky-section-action "merge"
    ((branch)
     (manky-merge (manky-section-info (manky-current-section))))
    ((log commits commit)
     (manky-merge (manky-section-info (manky-current-section))))))

(defun manky-histedit-item ()
  "Edit history starting from the revision represented by current item."
  (interactive)
  (manky-section-action "histedit"
    ((branch)
     (manky-histedit (manky-section-info (manky-current-section))))
    ((log commits commit)
     (manky-histedit (manky-section-info (manky-current-section))))))

(defun manky-histedit-in-progress-p ()
  "Return t if a histedit is in progress."
  (file-exists-p (f-join (manky-get-root-dir) ".hg" "histedit-state")))

(defun manky-rebase-in-progress-p ()
  "Return t if a rebase is in progress."
  (file-exists-p (f-join (manky-get-root-dir) ".hg" "rebasestate")))

;; (defun manky-hg-rebase (source dest)
;;   (interactive (list
;;                 (manky-read-revision "Rebase source: ")
;;                 (manky-read-revision "Onto dest: ")))
;;   (manky-run-hg-async "rebase" "-s" source "-d" dest "--tool" ":merge3"))

;; (defun manky-rebase-item (dest)
;;   "Rebase the revision represented by current item."
;;   (interactive
;;    (list
;;     (manky-read-revision (format "Rebase source %s onto dest : "
;;                          (manky-section-case "rebase"
;;                            ((branch)
;;                             (manky-section-info (manky-current-section)))
;;                            ((log commits commit)
;;                             (manky-section-info (manky-current-section))))))))
;;   (manky-section-action "rebase"
;;     ((branch)
;;      (manky-hg-rebase (manky-section-info (manky-current-section)) dest))
;;     ((log commits commit)
;;      (manky-hg-rebase (manky-section-info (manky-current-section)) dest))))

;; If we have a region, we should use -r r1 -r r2 ...
;; If not, we should default to -s but allow -b.  These are exclusive.

;; (define-infix-argument sf:--path ()
;;   :description "Path"
;;   :class 'transient-option
;;   :key "-p"
;;   :history-key 'sf-path
;;   ;; :format "%v)"
;;   :argument "path=")
;;   ;; :reader 'magit-transient-read-person)

(defun manky-region-range ()
  (interactive)
  (if-let
      ((      (region-active-p))
       (newer (manky-section-info (manky-section-at (manky-next-sha1 (region-beginning)))))
       (older (manky-section-info (manky-section-at (manky-previous-sha1 (- (region-end) 1)))))
       (      (not (string= newer older))))
      (list older newer)
    (list (manky-section-info (manky-current-section)))))

(defun manky-region-active-p ()
  "Whether there is an active region spanning more than one commit."
  (when-let ((range (manky-region-range)))
    (> (length range) 1)))

;;;###autoload (autoload 'sft "searchfox" nil t)
(define-transient-command manky-rebase ()
  "Move changeset (and descendants) to a different branch."
  ;; :man-page "git-push"
  ["Arguments"
   :if-not manky-rebase-in-progress-p
   ("-k" "keep original changesets" ("-k" "--keep"))
   ]
  ;; [
  ;;  :if manky-region-active-p
  ;;  ("-r" "rebase these revisions" ("-r" "--rev"))
  ;;  ;; (sf:--path)
  ;;  ]
  ;; [
  ;;  :if-not manky-region-active-p
  ;;  ("-s" "rebase the specified changesets and their descendants" ("-s" "--source"))
  ;;  ("-b" "rebase everything from branching point of specified changeset" ("-b" "--base"))
  ;;  ;; (sf:--path)
  ;;  ]

  ["Rebase"
   :if-not manky-rebase-in-progress-p
   [("i" "interactively (histedit)" manky-histedit-item)
    ("s" "rebase specified source changeset and its descendants onto dest" manky-rebase-source-onto-dest)
    ("b" "rebase everything from branching point of specified changeset onto dest" manky-rebase-base-onto-dest)
    ("r" "rebase revision(s) onto dest" manky-rebase-rev-onto-dest)]
    ;; ("b" "rebase everything from branching point of specified changeset onto dest" manky-rebase-base-onto-dest)]

   ;; [("m" "to modify a commit" manky-rebase-edit-commit)
   ;;  ("w" "to reword a commit" manky-rebase-reword-commit)
   ;;  ("k" "to remove a commit" manky-rebase-remove-commit)
   ;;  ("f" "to autosquash"      manky-rebase-autosquash)]
   ]

  ["Actions"
   :if manky-rebase-in-progress-p
   ("r" "Continue" manky-rebase-continue)
   ("s" "Stop"     manky-rebase-stop)
   ;; ("e" "Edit"     magit-rebase-edit)
   ("a" "Abort"    manky-rebase-abort)])


;;;###autoload
(defun manky-rebase-continue (&optional noedit)
  "Restart the current rebasing operation.
In some cases this pops up a commit message buffer for you do
edit.  With a prefix argument the old message is reused as-is."
  (interactive "P")
  (unless (manky-rebase-in-progress-p)
    (user-error "No rebase in progress"))
  (when (y-or-n-p "Continue this rebase ")
    (manky-run-hg "rebase" "--continue")))

  ;; (if (manky-rebase-in-progress-p)
  ;;     ;; (if (magit-anything-unstaged-p t)
  ;;     ;;     (user-error "Cannot continue rebase with unstaged changes")
  ;;     ;;   (when (and (magit-anything-staged-p)
  ;;     ;;              (file-exists-p (magit-git-dir "rebase-merge"))
  ;;     ;;              (not (member (magit-toplevel)
  ;;     ;;                           magit--rebase-public-edit-confirmed)))
  ;;     ;;     (magit-commit-amend-assert))
  ;;     ;;   (if noedit
  ;;     ;;       (let ((process-environment process-environment))
  ;;     ;;         (push "GIT_EDITOR=true" process-environment)
  ;;     ;;         (magit-run-git-async (magit--rebase-resume-command) "--continue")
  ;;     ;;         (set-process-sentinel magit-this-process
  ;;     ;;                               #'magit-sequencer-process-sentinel)
  ;;     ;;         magit-this-process)
  ;;     ;;     (magit-run-git-sequencer (magit--rebase-resume-command) "--continue")))
  ;;   (user-error "No rebase in progress")))

;;;###autoload
(defun manky-rebase-stop ()
  "Stop the current rebase operation, leaving the work in progress."
  (interactive)
  (unless (manky-rebase-in-progress-p)
    (user-error "No rebase in progress"))
  (when (y-or-n-p "Stop this rebase ")
    (manky-run-hg "rebase" "--stop")))

;;;###autoload
(defun manky-rebase-abort ()
  "Abort the current rebase operation, restoring the original branch."
  (interactive)
  (unless (manky-rebase-in-progress-p)
    (user-error "No rebase in progress"))
  (when (y-or-n-p "Abort this rebase ")
    (manky-run-hg "rebase" "--abort")))


(defun manky-current-line-string ()
  (let ((s (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (forward-line 1)
    s))

(defun manky-read-histedit-state ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at "v1")
      (user-error "Not a v1 histedit-state file"))
    (forward-line 4)
    ;; v1
    ;; parentctxnode
    ;; topmost
    ;; keep

    ;; rules
    (let*
        ((rule-count (string-to-number (manky-current-line-string)))
         (rules (cl-loop repeat rule-count
                         collect
                         (list (manky-current-line-string)
                               (manky-current-line-string))))
         (subs-count (string-to-number (manky-current-line-string)))
         (subs (cl-loop repeat subs-count
                        collect
                        (let ((line (manky-current-line-string)))
                          (list (substring line 0 40)
                                (substring line 41 80))))))
      (list rules subs))))
  

(defun manky-rebase-arguments ()
  (transient-args 'manky-rebase))

(defun manky-read-rebase-dest-revision (type)
  (interactive)
  (manky-read-revision (concat "Rebase "
                               type
                               " "
                               (manky-section-case "rebase"
                                 ((branch)
                                  (manky-section-info (manky-current-section)))
                                 ((log commits commit)
                                  (manky-section-info (manky-current-section))))
                               " onto dest: ")))

(defun manky-hg-rebase (args)
  ;; (interactive (list
  ;;               (manky-read-revision "Rebase source: ")
  ;;               (manky-read-revision "Onto dest: ")))
  (apply #'manky-run-hg-async "rebase" "--tool" ":merge3" args))

(defun manky-rebase-source-onto-dest (dest args)
  (interactive (list
                (manky-read-rebase-dest-revision "source")
                (manky-rebase-arguments)))
  (let ((rev (manky-section-case "rebase"
                  ((branch)
                   (manky-section-info (manky-current-section)))
                  ((log commits commit)
                   (manky-section-info (manky-current-section))))))
    ;; (message "Rebasing... %S" args)

    ;; (when-let ((arg (--remove-first (member it (list "--base" "--rev")) args)))
    ;;   (setq args (concat args (list arg (car (manky-region-range))))))
    
    ;; (message "rev-onto-dest %S" args)
    (manky-hg-rebase (append args (list "--source" rev "--dest" dest)))
    ;; (message "Rebasing... done")
    ))

(defun manky-rebase-base-onto-dest (dest args)
  (interactive (list
                (manky-read-rebase-dest-revision "base")
                (manky-rebase-arguments)))
  (let ((rev (manky-section-case "rebase"
                  ((branch)
                   (manky-section-info (manky-current-section)))
                  ((log commits commit)
                   (manky-section-info (manky-current-section))))))
    ;; (message "Rebasing... %S" args)

    ;; (when-let ((arg (--remove-first (member it (list "--base" "--source")) args)))
    ;;   (setq args (concat args (list arg (car (manky-region-range))))))
    
    ;; (message "source-onto-dest %S" args)
    (manky-hg-rebase (append args (list "--base" rev "--dest" dest)))
    ;; (message "Rebasing... done")
    ))

(defun manky-rebase-rev-onto-dest (dest args)
  (interactive (list
                (manky-read-rebase-dest-revision "rev")
                (manky-rebase-arguments)))
  (let ((rev (manky-section-case "rebase"
                  ((branch)
                   (manky-section-info (manky-current-section)))
                  ((log commits commit)
                   (manky-section-info (manky-current-section))))))
    ;; (message "Rebasing... %S" args)

    ;; (when-let ((arg (--remove-first (member it (list "--base" "--source")) args)))
    ;;   (setq args (concat args (list arg (car (manky-region-range))))))
    
    ;; (message "source-onto-dest %S" args)
    (manky-hg-rebase (append args (list "--rev" rev "--dest" dest)))
    ;; (message "Rebasing... done")
    ))

;; ;;;###autoload
;; (defun sft-search-literal (&rest args)
;;   "Push the current branch to a branch read in the minibuffer."
;;   (interactive
;;    ;; (--if-let (magit-get-current-branch)
;;    ;;     (list (magit-read-remote-branch (format "Push %s to" it)
;;    ;;                                     nil nil it 'confirm)
;;    (sft-arguments))
;;      ;; (user-error "No branch is checked out")))
;;   (message "sft-search-literal %s" args)
;;   ;; (magit-git-push (magit-get-current-branch) target args)
;; )

;; ;;;###autoload
;; (defun sft-search-regexp (&rest args)
;;   "Push the current branch to a branch read in the minibuffer."
;;   (interactive
;;    ;; (--if-let (magit-get-current-branch)
;;    ;;     (list (magit-read-remote-branch (format "Push %s to" it)
;;    ;;                                     nil nil it 'confirm)
;;    (sft-arguments))
;;      ;; (user-error "No branch is checked out")))
;;   (message "sft-search-regexp %s" args)
;;   ;; (magit-git-push (magit-get-current-branch) target args)
;; )

(defun manky-prune (node-1 &optional node-2)
  "Prune revision NODE-1 or topological revision range NODE-2::NODE-1."
  (manky-run-hg "prune" "--rev"
                (if node-2 (concat node-2 "::" node-1) node-1)))

(defun manky-prune-item ()
  "Prune the revision(s) represented by the current item or region."
  (interactive)
  (manky-section-action "prune"
    ((log commits commit)
     (if (region-active-p)
	       (manky-prune
	        (manky-section-info (manky-section-at (manky-next-sha1 (region-beginning))))
	        (manky-section-info (manky-section-at (manky-previous-sha1 (- (region-end) 1)))))
       (manky-prune (manky-section-info (manky-current-section)))))))

(defun manky-mozilla-review (node-1 &optional node-2)
  "`moz-phab submit` revision NODE-1 or topological revision range NODE-1::NODE-2."
  (let ((default-directory (manky-get-root-dir))
        (nodes (if (and node-2 (not (string= node-1 node-2)))
                   (list node-2 node-1)
                 (list "--single" node-1))))
    (manky-with-refresh
      ;; TODO: use a compilation buffer?  Use the *manky-process* buffer?
      (apply 'start-process "moz-phab" "*moz-phab*" "moz-phab" "submit"
             nodes))))

(defun manky-mozilla-review-item ()
  "`moz-phab submit` the revision(s) represented by the current item or region."
  (interactive)
  (manky-section-action "review"
    ((log commits commit)
     (if (region-active-p)
	       (manky-mozilla-review
	        (manky-section-info (manky-section-at (manky-next-sha1 (region-beginning))))
	        (manky-section-info (manky-section-at (manky-previous-sha1 (- (region-end) 1)))))
       (manky-mozilla-review (manky-section-info (manky-current-section)))))))

;;; Log edit mode

(define-derived-mode manky-log-edit-mode text-mode "Manky Log Edit")

(defun manky-restore-pre-log-edit-window-configuration ()
  (when manky-pre-log-edit-window-configuration
    (set-window-configuration manky-pre-log-edit-window-configuration)
    (setq manky-pre-log-edit-window-configuration nil)))

(defun manky-log-edit-commit ()
  "Finish edit and commit."
  (interactive)
  (when (= (buffer-size) 0)
    (user-error "No %s message" manky-log-edit-operation))
  (let ((commit-buf (current-buffer)))
    (cl-case manky-log-edit-operation
      ('commit
       (with-current-buffer (manky-find-status-buffer default-directory)
         (apply #'manky-run-async-with-input commit-buf
                manky-hg-executable
                (append manky-hg-standard-options
                        (list "commit" "--logfile" "-")
                        manky-staged-files))))
      ('amend
       (with-current-buffer (manky-find-status-buffer default-directory)
         (apply #'manky-run-async-with-input commit-buf
                manky-hg-executable
                (append manky-hg-standard-options
                        (list "commit" "--amend" "--logfile" "-")
                        manky-staged-files))))
      ('reword
       (with-current-buffer (manky-find-status-buffer default-directory)
         (apply #'manky-run-async-with-input commit-buf
                manky-hg-executable
                (append manky-hg-standard-options
                        (list "commit" "--amend" "--logfile" "-")
                        (list "--exclude" "**")))))
      ('backout
       (with-current-buffer manky-log-edit-client-buffer
         (manky-run-async-with-input commit-buf
                                   manky-hg-executable
                                   "backout"
                                   "--merge"
                                   "--logfile" "-"
                                   manky-log-edit-info)))
      ('qnew
       (with-current-buffer manky-log-edit-client-buffer
         (manky-run-async-with-input commit-buf
                                     manky-hg-executable
                                     "qnew" manky-log-edit-info
                                     "--config" "extensions.mq="
                                     "--logfile" "-")))
      ('qrefresh
       (with-current-buffer manky-log-edit-client-buffer
         (apply #'manky-run-async-with-input commit-buf
                manky-hg-executable "qrefresh"
                "--config" "extensions.mq="
                "--logfile" "-"
                (append manky-staged-files manky-queue-staged-files))))
      ('qreorder
       (let* ((queue-buffer (manky-find-buffer 'queue))
	      (series (with-current-buffer queue-buffer
			(manky-patch-series-file))))
	(with-current-buffer manky-log-edit-buffer-name
	  (write-region (point-min) (point-max) series))
	(with-current-buffer queue-buffer
	  (manky-refresh))))))
  (erase-buffer)
  (bury-buffer)
  (manky-restore-pre-log-edit-window-configuration))

(defun manky-log-edit-cancel-log-message ()
  "Abort edits and erase commit message being composed."
  (interactive)
  (when (or (not manky-log-edit-confirm-cancellation)
            (yes-or-no-p
             "Really cancel editing the log (any changes will be lost)?"))
    (erase-buffer)
    (bury-buffer)
    (manky-restore-pre-log-edit-window-configuration)))

(defun manky-prepare-to-log-edit (operation &optional info)
  (let ((dir default-directory)
        (buf (get-buffer-create manky-log-edit-buffer-name)))
    (setq manky-pre-log-edit-window-configuration
          (current-window-configuration)
          manky-log-edit-operation operation
          manky-log-edit-client-buffer (current-buffer)
          manky-log-edit-info info)
    (with-current-buffer buf
      (setq default-directory dir))
    buf))

(defun manky-pop-to-log-edit (operation &optional info)
  (let ((buf (manky-prepare-to-log-edit operation info)))
    (pop-to-buffer buf)
    (manky-log-edit-mode)
    (message "Type C-c C-c to %s (C-c C-k to cancel)." manky-log-edit-operation)))

(defun manky-log-edit ()
  "Bring up a buffer to allow editing of commit messages."
  (interactive)
  (when (not (or manky-staged-files (manky-merge-p)))
    (if (y-or-n-p "Nothing staged. Stage and commit all changes? ")
        (manky-stage-all)
      (user-error "Nothing staged")))
  (manky-pop-to-log-edit 'commit))

(defun manky-commit-amend ()
  "Amends previous commit.
Brings up a buffer to allow editing of commit message."
  (interactive)
  ;; get last commit message
  (with-current-buffer (get-buffer-create manky-log-edit-buffer-name)
    (manky-hg-insert
     (list "log"
           "--template" "{desc}" "-r" ".")))
  (manky-pop-to-log-edit 'amend))

;;;###autoload (autoload 'manky-commit "manky" nil t)
(define-transient-command manky-commit ()
  "Create a new commit or replace an existing commit."
  ;; :info-manual "(magit)Initiating a Commit"
  ;; :man-page "git-commit"
  ["Arguments"
   ;; ("-a" "Stage all modified and deleted files"   ("-a" "--all"))
   ;; ("-e" "Allow empty commit"                     "--allow-empty")
   ;; ("-v" "Show diff of changes to be committed"   ("-v" "--verbose"))
   ;; ("-n" "Disable hooks"                          ("-n" "--no-verify"))
   ;; ("-R" "Claim authorship and reset author date" "--reset-author")
   ;; (magit:--author :description "Override the author")
   ;; (7 "-D" "Override the author date" "--date=" transient-read-date)
   ;; ("-s" "Add Signed-off-by line"                 ("-s" "--signoff"))
   ;; (5 magit:--gpg-sign)
   ;; (magit-commit:--reuse-message)
   ]
  [["Create"
    ("c" "Commit"         manky-commit-create)]
   ["Edit HEAD"
    ("e" "Extend"         manky-commit-extend)
    ("w" "Reword"         manky-commit-reword)
    ("a" "Amend"          manky-commit-amend)
    ;; (6 "n" "Reshelve"     magit-commit-reshelve)
    ]
   ["Edit"
    ;; ("f" "Fixup"          magit-commit-fixup)
    ;; ("s" "Squash"         magit-commit-squash)
    ;; ("A" "Augment"        magit-commit-augment)
    ("x" "Absorb changes" manky-commit-absorb)]
   ;; [""
   ;;  ("F" "Instant fixup"  magit-commit-instant-fixup)
   ;;  ("S" "Instant squash" magit-commit-instant-squash)]
   ]
  ;; (when (not (or manky-staged-files (manky-merge-p)))
  ;;   (if (y-or-n-p "Nothing staged. Stage and commit all changes? ")
  ;;       (manky-stage-all)
  ;;     (user-error "Nothing staged")))
)

(defun manky-commit-arguments ()
  (transient-args 'manky-commit))

(defalias 'manky-commit-create 'manky-log-edit)

(defun manky-commit-extend ()
  "Extends previous commit.
Amend but does not bring up a buffer to allow editing of commit message."
  (interactive)
  ;; get last commit message
  (with-current-buffer (manky-prepare-to-log-edit 'amend)
    (erase-buffer)
    (manky-hg-insert
     (list "log"
           "--template" "{desc}" "-r" "."))
    (manky-log-edit-commit)))

(defun manky-commit-reword ()
  "Reword the commit message of the previous commit.
Brings up a buffer to allow editing of commit message."
  (interactive)
  ;; get last commit message
  (with-current-buffer (manky-prepare-to-log-edit 'reword)
    (erase-buffer)
    (manky-hg-insert
     (list "log"
           "--template" "{desc}" "-r" "."))
    (manky-pop-to-log-edit 'reword)))

(defun manky-commit-absorb ()
  "Incorporate corrections into the stack of draft changesets."
  (interactive)
  (apply #'manky-run-hg-async
         "absorb"
         "--apply-changes"
         manky-staged-files)
  (manky-refresh-buffer
   (manky-find-status-buffer default-directory)))

;; (defun manky-commit-create (args)
;;   (interactive (list (manky-commit-arguments)))
;;   (message "args: %S" args)
;;   (manky-log-edit))

;; (defun manky-commit-create (args)
;;   (interactive (list (manky-commit-arguments)))
;;   (message "args: %S" args)
;;   (manky-log-edit))

(defun manky-bookmark-create (bookmark-name)
  "Create a bookmark at the current location"
  (interactive "sBookmark name: ")
  (manky-run-hg-async "bookmark" bookmark-name))

(defun manky-killall-manky-buffers ()
  (interactive)
  (cl-flet ((manky-buffer-p (b) (string-match "\*manky\\(:\\|-\\).*" (buffer-name b))))
    (let ((manky-buffers (cl-remove-if-not #'manky-buffer-p (buffer-list))))
      (cl-loop for mb in manky-buffers
               do
               (kill-buffer mb)))))

;;;###autoload
(defun manky-copy-section-value ()
  "Save the value of the current section for later use.

Save the section value to the `kill-ring'.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above."
  (interactive)
  (cond
   ((use-region-p)
    (copy-region-as-kill nil nil 'region))
   (t
    (manky-section-action "copy-section-value"
      ((branch)
       (kill-new (message "%s" (manky-section-info (manky-current-section)))))
      ((log commits commit)
       (kill-new (message "%s" (manky-section-info (manky-current-section)))))))))

;; (cl-eval-when (load eval)
  (require 'manky-queue)
  (require 'manky-shelve)
;; )

(provide 'manky)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; manky.el ends here
