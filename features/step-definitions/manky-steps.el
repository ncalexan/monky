;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(When "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(Then "^I should have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(And "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(But "^I should not have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))


(defun ecukes-ansi-clear (string)
  "Like `ansi-color-filter-apply' with extension for movement."
  (ansi-color-filter-apply
   (->> string
     (replace-regexp-in-string "\n\u001b\\[[0-9]+A" "")
     (replace-regexp-in-string "\u001b\\[[0-9]+[BCD]" ""))))

(defun ecukes-should-match (needle haystack)
  (should (s-contains? needle haystack)))

(When "^I run hg \"\\([^\"]*\\)\"$"
  (lambda (command)
    (let* ((buffer-name "*hg-output*")
           (buffer
            (progn
              (when (get-buffer buffer-name)
                (kill-buffer buffer-name))
              (get-buffer-create buffer-name)))
           (default-directory (file-name-as-directory manky-repo-path))
           (args
            (unless (equal command "")
              (s-split " " command)))
           (exit-code
            (apply
             'call-process
             (append (list manky-hg-executable nil buffer nil) args))))
      (with-current-buffer buffer
        (let ((content (ecukes-ansi-clear (buffer-string))))
          (cond ((= exit-code 0)
                 (setq ecukes-stdout content))
                (t
                 (setq ecukes-stderr content))))))))

(Then "^I should see command output:$"
  (lambda (expected)
    (ecukes-should-match expected ecukes-stdout)))

(Then "^I should see command error:$"
  (lambda (expected)
    (ecukes-should-match expected ecukes-stderr)))

(When "^I start with a fresh repository$"
  (lambda ()
    (when (f-dir? manky-repo-path)
      (f-delete manky-repo-path 'force))
    (f-mkdir manky-repo-path)
    (let* ((default-directory (file-name-as-directory manky-repo-path)))
      (When "I run hg \"init\"")
      (switch-to-buffer (manky-status)))))
