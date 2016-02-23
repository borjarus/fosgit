;;; fosgit-commit.el --- create Git commits  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2016  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This library implements commands for creating Git commits.  These
;; commands just initiate the commit, support for writing the commit
;; messages is implemented in `git-commit.el'.

;;; Code:

(require 'fosgit)
(require 'fosgit-sequence)

(eval-when-compile (require 'epa)) ; for `epa-protocol'
(eval-when-compile (require 'epg))
(declare-function epg-sub-key-id 'epg)
(declare-function epg-key-sub-key-list 'epg)
(declare-function epg-key-user-id-list 'epg)
(declare-function epg-user-id-string 'epg)
(declare-function epg-decode-dn 'epg)
(declare-function epg-list-keys 'epg)

;;; Options

(defcustom fosgit-commit-arguments nil
  "The arguments used when committing."
  :group 'fosgit-commands
  :type '(repeat (string :tag "Argument")))

(defcustom fosgit-commit-ask-to-stage 'verbose
  "Whether to ask to stage everything when committing and nothing is staged."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-commands
  :type '(choice (const :tag "Ask showing diff" verbose)
                 (const :tag "Ask" t)
                 (const :tag "Don't ask" nil)))

(defcustom fosgit-commit-show-diff t
  "Whether the relevant diff is automatically shown when committing."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-commands
  :type 'boolean)

(defcustom fosgit-commit-extend-override-date t
  "Whether using `fosgit-commit-extend' changes the committer date."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-commands
  :type 'boolean)

(defcustom fosgit-commit-reword-override-date t
  "Whether using `fosgit-commit-reword' changes the committer date."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-commands
  :type 'boolean)

(defcustom fosgit-commit-squash-confirm t
  "Whether the commit targeted by squash and fixup has to be confirmed.
When non-nil then the commit at point (if any) is used as default
choice, otherwise it has to be confirmed.  This option only
affects `fosgit-commit-squash' and `fosgit-commit-fixup'.  The
\"instant\" variants always require confirmation because making
an error while using those is harder to recover from."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-commands
  :type 'boolean)

;;; Code

(defun fosgit-commit-popup (&optional arg)
  "Popup console for commit commands."
  (interactive "P")
  (--if-let (fosgit-commit-message-buffer)
      (switch-to-buffer it)
    (fosgit-invoke-popup 'fosgit-commit-popup nil arg)))

(defvar fosgit-commit-popup
  '(:variable fosgit-commit-arguments
    :man-page "git-commit"
    :switches ((?a "Stage all modified and deleted files"   "--all")
               (?e "Allow empty commit"                     "--allow-empty")
               (?v "Show diff of changes to be committed"   "--verbose")
               (?n "Bypass git hooks"                       "--no-verify")
               (?s "Add Signed-off-by line"                 "--signoff")
               (?R "Claim authorship and reset author date" "--reset-author"))
    :options  ((?A "Override the author"  "--author=")
               (?S "Sign using gpg"       "--gpg-sign=" fosgit-read-gpg-secret-key)
               (?C "Reuse commit message" "--reuse-message="))
    :actions  ((?c "Commit"         fosgit-commit)
               (?e "Extend"         fosgit-commit-extend)
               (?f "Fixup"          fosgit-commit-fixup)
               (?F "Instant Fixup"  fosgit-commit-instant-fixup) nil
               (?w "Reword"         fosgit-commit-reword)
               (?s "Squash"         fosgit-commit-squash)
               (?S "Instant Squash" fosgit-commit-instant-squash) nil
               (?a "Amend"          fosgit-commit-amend)
               (?A "Augment"        fosgit-commit-augment))
    :max-action-columns 4
    :default-action fosgit-commit))

(fosgit-define-popup-keys-deferred 'fosgit-commit-popup)

(defun fosgit-commit-arguments nil
  (if (eq fosgit-current-popup 'fosgit-commit-popup)
      fosgit-current-popup-args
    fosgit-commit-arguments))

(defun fosgit-commit-message-buffer ()
  (let* ((find-file-visit-truename t) ; git uses truename of COMMIT_EDITMSG
         (topdir (fosgit-toplevel)))
    (--first (equal topdir (with-current-buffer it
                             (and git-commit-mode (fosgit-toplevel))))
             (append (buffer-list (selected-frame))
                     (buffer-list)))))

;;;###autoload
(defun fosgit-commit (&optional args)
  "Create a new commit on HEAD.
With a prefix argument amend to the commit at HEAD instead.
\n(git commit [--amend] ARGS)"
  (interactive (if current-prefix-arg
                   (list (cons "--amend" (fosgit-commit-arguments)))
                 (list (fosgit-commit-arguments))))
  (when (setq args (fosgit-commit-assert args))
    (fosgit-run-git-with-editor "commit" args)))

;;;###autoload
(defun fosgit-commit-amend (&optional args)
  "Amend the last commit.
\n(git commit --amend ARGS)"
  (interactive (list (fosgit-commit-arguments)))
  (fosgit-run-git-with-editor "commit" "--amend" args))

;;;###autoload
(defun fosgit-commit-extend (&optional args override-date)
  "Amend the last commit, without editing the message.

With a prefix argument keep the committer date, otherwise change
it.  The option `fosgit-commit-extend-override-date' can be used
to inverse the meaning of the prefix argument.  \n(git commit
--amend --no-edit)"
  (interactive (list (fosgit-commit-arguments)
                     (if current-prefix-arg
                         (not fosgit-commit-extend-override-date)
                       fosgit-commit-extend-override-date)))
  (when (setq args (fosgit-commit-assert args (not override-date)))
    (let ((process-environment process-environment))
      (unless override-date
        (setenv "GIT_COMMITTER_DATE" (fosgit-rev-format "%cD")))
      (fosgit-run-git-with-editor "commit" "--amend" "--no-edit" args))))

;;;###autoload
(defun fosgit-commit-reword (&optional args override-date)
  "Reword the last commit, ignoring staged changes.

With a prefix argument keep the committer date, otherwise change
it.  The option `fosgit-commit-reword-override-date' can be used
to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.
\n(git commit --amend --only)"
  (interactive (list (fosgit-commit-arguments)
                     (if current-prefix-arg
                         (not fosgit-commit-reword-override-date)
                       fosgit-commit-reword-override-date)))
  (let ((process-environment process-environment))
    (unless override-date
      (setenv "GIT_COMMITTER_DATE" (fosgit-rev-format "%cD")))
    (fosgit-run-git-with-editor "commit" "--amend" "--only" args)))

;;;###autoload
(defun fosgit-commit-fixup (&optional commit args)
  "Create a fixup commit.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `fosgit-commit-squash-confirm'."
  (interactive (list (fosgit-commit-at-point)
                     (fosgit-commit-arguments)))
  (fosgit-commit-squash-internal "--fixup" commit args))

;;;###autoload
(defun fosgit-commit-squash (&optional commit args)
  "Create a squash commit, without editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `fosgit-commit-squash-confirm'."
  (interactive (list (fosgit-commit-at-point)
                     (fosgit-commit-arguments)))
  (fosgit-commit-squash-internal "--squash" commit args))

;;;###autoload
(defun fosgit-commit-augment (&optional commit args)
  "Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `fosgit-commit-squash-confirm'."
  (interactive (list (fosgit-commit-at-point)
                     (fosgit-commit-arguments)))
  (fosgit-commit-squash-internal "--squash" commit args nil t))

;;;###autoload
(defun fosgit-commit-instant-fixup (&optional commit args)
  "Create a fixup commit targeting COMMIT and instantly rebase."
  (interactive (list (fosgit-commit-at-point)
                     (fosgit-commit-arguments)))
  (fosgit-commit-squash-internal "--fixup" commit args t))

;;;###autoload
(defun fosgit-commit-instant-squash (&optional commit args)
  "Create a squash commit targeting COMMIT and instantly rebase."
  (interactive (list (fosgit-commit-at-point)
                     (fosgit-commit-arguments)))
  (fosgit-commit-squash-internal "--squash" commit args t))

(defun fosgit-commit-squash-internal
    (option commit &optional args rebase edit confirmed)
  (-when-let (args (fosgit-commit-assert args t))
    (if (and commit
             (or confirmed
                 (not (or rebase
                          current-prefix-arg
                          fosgit-commit-squash-confirm))))
        (let ((fosgit-commit-show-diff nil))
          (fosgit-run-git-with-editor "commit"
                                     (unless edit "--no-edit")
                                     (concat option "=" commit)
                                     args))
      (fosgit-log-select
        `(lambda (commit)
           (fosgit-commit-squash-internal ,option commit ',args ,rebase ,edit t)
           ,@(when rebase
               `((fosgit-rebase-interactive-1 commit
                     (list "--autosquash" "--autostash")
                   "" "true"))))
        (format "Type %%p on a commit to %s into it,"
                (substring option 2)))
      (when fosgit-commit-show-diff
        (let ((fosgit-display-buffer-noselect t))
          (apply #'fosgit-diff-staged nil (fosgit-diff-arguments)))))))

(defun fosgit-commit-assert (args &optional strict)
  (cond
   ((or (fosgit-anything-staged-p)
        (and (fosgit-anything-unstaged-p)
             ;; ^ Everything of nothing is still nothing.
             (member "--all" args))
        (and (not strict)
             ;; ^ For amend variants that don't make sense otherwise.
             (or (member "--amend" args)
                 (member "--allow-empty" args))))
    (or args (list "--")))
   ((and (fosgit-rebase-in-progress-p)
         (not (fosgit-anything-unstaged-p))
         (y-or-n-p "Nothing staged.  Continue in-progress rebase? "))
    (fosgit-run-git-sequencer "rebase" "--continue")
    nil)
   ((and (file-exists-p (fosgit-git-dir "MERGE_MSG"))
         (not (fosgit-anything-unstaged-p)))
    (or args (list "--")))
   ((not (fosgit-anything-unstaged-p))
    (user-error "Nothing staged (or unstaged)"))
   (fosgit-commit-ask-to-stage
    (when (eq fosgit-commit-ask-to-stage 'verbose)
      (fosgit-diff-unstaged))
    (prog1 (when (y-or-n-p "Nothing staged.  Stage and commit everything? ")
             (fosgit-run-git "add" "-u" ".")
             (or args (list "--")))
      (when (and (eq fosgit-commit-ask-to-stage 'verbose)
                 (derived-mode-p 'fosgit-diff-mode))
        (fosgit-mode-bury-buffer))))
   (t
    (user-error "Nothing staged"))))

(defun fosgit-commit-diff ()
  (--when-let (and git-commit-mode
                   fosgit-commit-show-diff
                   (pcase last-command
                     (`fosgit-commit
                      (apply-partially 'fosgit-diff-staged nil))
                     (`fosgit-commit-amend  'fosgit-diff-while-amending)
                     (`fosgit-commit-reword 'fosgit-diff-while-amending)))
    (condition-case nil
        (let ((fosgit-inhibit-save-previous-winconf 'unset)
              (fosgit-display-buffer-noselect t)
              (inhibit-quit nil))
          (message "Diffing changes to be committed (C-g to abort diffing)")
          (funcall it (car (fosgit-diff-arguments))))
      (quit))))

(add-hook 'server-switch-hook 'fosgit-commit-diff)

(add-to-list 'with-editor-server-window-alist
             (cons git-commit-filename-regexp 'switch-to-buffer))

(defvar fosgit-gpg-secret-key-hist nil)

(defun fosgit-read-gpg-secret-key (prompt &optional _initial-input)
  (require 'epa)
  (let ((keys (--map (list (epg-sub-key-id (car (epg-key-sub-key-list it)))
                           (-when-let (id-obj (car (epg-key-user-id-list it)))
                             (let    ((id-str (epg-user-id-string id-obj)))
                               (if (stringp id-str)
                                   id-str
                                 (epg-decode-dn id-obj)))))
                     (epg-list-keys (epg-make-context epa-protocol) nil t))))
    (fosgit-completing-read prompt keys nil nil nil 'fosgit-gpg-secret-key-hist
                           (car (or fosgit-gpg-secret-key-hist keys)))))

(defvar fosgit-commit-add-log-insert-function 'fosgit-commit-add-log-insert
  "Used by `fosgit-commit-add-log' to insert a single entry.")

(defun fosgit-commit-add-log ()
  "Add a stub for the current change into the commit message buffer.
If no commit is in progress, then initiate it.  Use the function
specified by variable `fosgit-commit-add-log-insert-function' to
actually insert the entry."
  (interactive)
  (let ((hunk (fosgit-section-when 'hunk it))
        (log (fosgit-commit-message-buffer)) buf pos)
    (save-window-excursion
      (call-interactively #'fosgit-diff-visit-file)
      (setq buf (current-buffer)
            pos (point)))
    (unless log
      (unless (fosgit-commit-assert nil)
        (user-error "Abort"))
      (fosgit-commit)
      (while (not (setq log (fosgit-commit-message-buffer)))
        (sit-for 0.01)))
    (save-excursion
      (with-current-buffer buf
        (goto-char pos)
        (funcall fosgit-commit-add-log-insert-function log
                 (fosgit-file-relative-name)
                 (and hunk (add-log-current-defun)))))))

(defun fosgit-commit-add-log-insert (buffer file defun)
  (with-current-buffer buffer
    (undo-boundary)
    (goto-char (point-max))
    (while (re-search-backward (concat "^" comment-start) nil t))
    (cond ((re-search-backward (format "* %s\\(?: (\\([^)]+\\))\\)?: " file)
                               nil t)
           (when (equal (match-string 1) defun)
             (setq defun nil))
           (re-search-forward ": "))
          (t
           (when (re-search-backward "^[\\*(].+\n" nil t)
             (goto-char (match-end 0)))
           (while (re-search-forward "^[^\\*#\n].*\n" nil t))
           (if defun
               (progn (insert (format "* %s (%s): \n" file defun))
                      (setq defun nil))
             (insert (format "* %s: \n" file)))
           (backward-char)
           (unless (looking-at "\n[\n\\']")
             (insert ?\n)
             (backward-char))))
    (when defun
      (forward-line)
      (let ((limit (save-excursion
                     (and (re-search-forward "^\\*" nil t)
                          (point)))))
        (unless (or (looking-back (format "(%s): " defun)
                                  (line-beginning-position))
                    (re-search-forward (format "^(%s): " defun) limit t))
          (while (re-search-forward "^[^\\*#\n].*\n" limit t))
          (insert (format "(%s): \n" defun))
          (backward-char))))))

;;; fosgit-commit.el ends soon
(provide 'fosgit-commit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-commit.el ends here
