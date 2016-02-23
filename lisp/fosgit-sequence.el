;;; fosgit-sequence.el --- history manipulation in Magit  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2016  The Magit Project Contributors
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

;; Support for Git commands that replay commits and help the user make
;; changes along the way.  Supports `cherry-pick', `revert', `rebase',
;; `rebase--interactive' and `am'.

;;; Code:

(require 'fosgit)

;;; Options
;;;; Faces

(defface fosgit-sequence-pick
  '((t :inherit default))
  "Face used in sequence sections."
  :group 'fosgit-faces)

(defface fosgit-sequence-stop
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background dark))  :foreground "DarkSeaGreen2"))
  "Face used in sequence sections."
  :group 'fosgit-faces)

(defface fosgit-sequence-part
  '((((class color) (background light)) :foreground "Goldenrod4")
    (((class color) (background dark))  :foreground "LightGoldenrod2"))
  "Face used in sequence sections."
  :group 'fosgit-faces)

(defface fosgit-sequence-head
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background dark))  :foreground "LightSkyBlue1"))
  "Face used in sequence sections."
  :group 'fosgit-faces)

(defface fosgit-sequence-drop
  '((((class color) (background light)) :foreground "IndianRed")
    (((class color) (background dark))  :foreground "IndianRed"))
  "Face used in sequence sections."
  :group 'fosgit-faces)

(defface fosgit-sequence-done
  '((t :inherit fosgit-hash))
  "Face used in sequence sections."
  :group 'fosgit-faces)

(defface fosgit-sequence-onto
  '((t :inherit fosgit-sequence-done))
  "Face used in sequence sections."
  :group 'fosgit-faces)

;;; Common

;;;###autoload
(defun fosgit-sequencer-continue ()
  "Resume the current cherry-pick or revert sequence."
  (interactive)
  (if (fosgit-sequencer-in-progress-p)
      (if (fosgit-anything-unstaged-p t)
          (user-error "Cannot continue due to unstaged changes")
        (fosgit-run-git-sequencer
         (if (fosgit-revert-in-progress-p) "revert" "cherry-pick") "--continue"))
    (user-error "No cherry-pick or revert in progress")))

;;;###autoload
(defun fosgit-sequencer-skip ()
  "Skip the stopped at commit during a cherry-pick or revert sequence."
  (interactive)
  (if (fosgit-sequencer-in-progress-p)
      (progn (fosgit-call-git "reset" "--hard")
             (fosgit-sequencer-continue))
    (user-error "No cherry-pick or revert in progress")))

;;;###autoload
(defun fosgit-sequencer-abort ()
  "Abort the current cherry-pick or revert sequence.
This discards all changes made since the sequence started."
  (interactive)
  (if (fosgit-sequencer-in-progress-p)
      (fosgit-run-git-sequencer
       (if (fosgit-revert-in-progress-p) "revert" "cherry-pick") "--abort")
    (user-error "No cherry-pick or revert in progress")))

(defun fosgit-sequencer-in-progress-p ()
  (or (fosgit-cherry-pick-in-progress-p)
      (fosgit-revert-in-progress-p)))

;;; Cherry-Pick

;;;###autoload (autoload 'fosgit-cherry-pick-popup "fosgit-sequence" nil t)
(fosgit-define-popup fosgit-cherry-pick-popup
  "Popup console for cherry-pick commands."
  'fosgit-commands
  :man-page "git-cherry-pick"
  :switches '((?s "Add Signed-off-by lines"            "--signoff")
              (?e "Edit commit messages"               "--edit")
              (?x "Reference cherry in commit message" "-x")
              (?F "Attempt fast-forward"               "--ff")
              (?m "Reply merge relative to parent"     "--mainline="))
  :options  '((?s "Strategy" "--strategy="))
  :actions  '((?A "Cherry Pick"  fosgit-cherry-pick)
              (?a "Cherry Apply" fosgit-cherry-apply))
  :sequence-actions '((?A "Continue" fosgit-sequencer-continue)
                      (?s "Skip"     fosgit-sequencer-skip)
                      (?a "Abort"    fosgit-sequencer-abort))
  :sequence-predicate 'fosgit-sequencer-in-progress-p
  :default-arguments '("--ff"))

(defun fosgit-cherry-pick-read-args (prompt)
  (list (or (nreverse (fosgit-region-values 'commit))
            (fosgit-read-other-branch-or-commit prompt))
        (fosgit-cherry-pick-arguments)))

;;;###autoload
(defun fosgit-cherry-pick (commit &optional args)
  "Cherry-pick COMMIT.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then pick all of them,
without prompting."
  (interactive (fosgit-cherry-pick-read-args "Cherry-pick"))
  (fosgit-assert-one-parent (car (if (listp commit)
                                    commit
                                  (split-string commit "\\.\\.")))
                           "cherry-pick")
  (fosgit-run-git-sequencer "cherry-pick" args commit))

;;;###autoload
(defun fosgit-cherry-apply (commit &optional args)
  "Apply the changes in COMMIT but do not commit them.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then apply all of them,
without prompting."
  (interactive (fosgit-cherry-pick-read-args "Apply changes from commit"))
  (fosgit-assert-one-parent commit "cherry-pick")
  (fosgit-run-git-sequencer "cherry-pick" "--no-commit"
                           (remove "--ff" args) commit))

(defun fosgit-cherry-pick-in-progress-p ()
  ;; .git/sequencer/todo does not exist when there is only one commit left.
  (file-exists-p (fosgit-git-dir "CHERRY_PICK_HEAD")))

;;; Revert

;;;###autoload (autoload 'fosgit-revert-popup "fosgit-sequence" nil t)
(fosgit-define-popup fosgit-revert-popup
  "Popup console for revert commands."
  'fosgit-commands
  :man-page "git-revert"
  :switches '((?s "Add Signed-off-by lines" "--signoff"))
  :options  '((?s "Strategy" "--strategy="))
  :actions  '((?V "Revert commit(s)" fosgit-revert)
              (?v "Revert changes"   fosgit-revert-no-commit))
  :sequence-actions '((?V "Continue" fosgit-sequencer-continue)
                      (?s "Skip"     fosgit-sequencer-skip)
                      (?a "Abort"    fosgit-sequencer-abort))
  :sequence-predicate 'fosgit-sequencer-in-progress-p)

(defun fosgit-revert-read-args (prompt)
  (list (or (fosgit-region-values 'commit)
            (fosgit-read-branch-or-commit prompt))
        (fosgit-revert-arguments)))

;;;###autoload
(defun fosgit-revert (commit &optional args)
  "Revert COMMIT by creating a new commit.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting."
  (interactive (fosgit-revert-read-args "Revert commit"))
  (fosgit-assert-one-parent commit "revert")
  (fosgit-run-git-sequencer "revert" args commit))

;;;###autoload
(defun fosgit-revert-no-commit (commit &optional args)
  "Revert COMMIT by applying it in reverse to the worktree.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting."
  (interactive (fosgit-revert-read-args "Revert changes"))
  (fosgit-assert-one-parent commit "revert")
  (fosgit-run-git-sequencer "revert" "--no-commit" args commit))

(defun fosgit-revert-in-progress-p ()
  ;; .git/sequencer/todo does not exist when there is only one commit left.
  (file-exists-p (fosgit-git-dir "REVERT_HEAD")))

;;; Patch

;;;###autoload (autoload 'fosgit-am-popup "fosgit-sequence" nil t)
(fosgit-define-popup fosgit-am-popup
  "Popup console for mailbox commands."
  'fosgit-commands
  :man-page "git-am"
  :switches '((?3 "Fall back on 3way merge"           "--3way")
              (?s "Add Signed-off-by lines"           "--signoff")
              (?c "Remove text before scissors line"  "--scissors")
              (?k "Inhibit removal of email cruft"    "--keep")
              (?b "Limit removal of email cruft"      "--keep-non-patch")
              (?d "Use author date as committer date"
                  "--committer-date-is-author-date")
              (?D "Use committer date as author date" "--ignore-date"))
  :options  '((?p "Remove leading slashes from paths" "-p"
                  fosgit-popup-read-number))
  :actions  '((?w "Apply patches" fosgit-am-apply-patches)
              (?m "Apply maildir" fosgit-am-apply-maildir))
  :default-arguments '("--3way")
  :default-actions 'fosgit-am-apply-patches
  :sequence-actions '((?w "Continue" fosgit-am-continue)
                      (?s "Skip"     fosgit-am-skip)
                      (?a "Abort"    fosgit-am-abort))
  :sequence-predicate 'fosgit-am-in-progress-p)

;;;###autoload
(defun fosgit-am-apply-patches (&optional files args)
  "Apply the patches FILES."
  (interactive (list (or (fosgit-region-values 'file)
                         (list (let ((default (fosgit-file-at-point)))
                                 (read-file-name
                                  (if default
                                      (format "Apply patch (%s): " default)
                                    "Apply patch: ")
                                  nil default))))
                     (fosgit-am-arguments)))
  (fosgit-run-git-sequencer "am" args "--" (mapcar 'expand-file-name files)))

;;;###autoload
(defun fosgit-am-apply-maildir (&optional maildir args)
  "Apply the patches from MAILDIR."
  (interactive (list (read-file-name "Apply mbox or Maildir: ")
                     (fosgit-am-arguments)))
  (fosgit-run-git-sequencer "am" args (expand-file-name maildir)))

;;;###autoload
(defun fosgit-am-continue ()
  "Resume the current patch applying sequence."
  (interactive)
  (if (fosgit-am-in-progress-p)
      (if (fosgit-anything-unstaged-p t)
          (error "Cannot continue due to unstaged changes")
        (fosgit-run-git-sequencer "am" "--continue"))
    (user-error "Not applying any patches")))

;;;###autoload
(defun fosgit-am-skip ()
  "Skip the stopped at patch during a patch applying sequence."
  (interactive)
  (if (fosgit-am-in-progress-p)
      (fosgit-run-git-sequencer "am" "--skip")
    (user-error "Not applying any patches")))

;;;###autoload
(defun fosgit-am-abort ()
  "Abort the current patch applying sequence.
This discards all changes made since the sequence started."
  (interactive)
  (if (fosgit-am-in-progress-p)
      (fosgit-run-git "am" "--abort")
    (user-error "Not applying any patches")))

(defun fosgit-am-in-progress-p ()
  (file-exists-p (fosgit-git-dir "rebase-apply/applying")))

;;; Rebase

;;;###autoload (autoload 'fosgit-rebase-popup "fosgit-sequence" nil t)
(fosgit-define-popup fosgit-rebase-popup
  "Key menu for rebasing."
  'fosgit-commands
  :man-page "git-rebase"
  :switches '((?k "Keep empty commits"    "--keep-empty")
              (?p "Preserve merges"       "--preserve-merges")
              (?c "Lie about author date" "--committer-date-is-author-date")
              (?a "Autosquash"            "--autosquash")
              (?A "Autostash"             "--autostash")
              (?i "Interactive"           "--interactive"))
  :actions  '((lambda ()
                (concat (propertize "Rebase " 'face 'fosgit-popup-heading)
                        (propertize (or (fosgit-get-current-branch) "HEAD")
                                    'face 'fosgit-branch-local)
                        (propertize " onto" 'face 'fosgit-popup-heading)))
              (?p (lambda ()
                    (--when-let (fosgit-get-push-branch) (concat it "\n")))
                  fosgit-rebase-onto-pushremote)
              (?u (lambda ()
                    (--when-let (fosgit-get-upstream-branch) (concat it "\n")))
                  fosgit-rebase-onto-upstream)
              (?e "elsewhere"               fosgit-rebase)
              "Rebase"
              (?i "interactively"      fosgit-rebase-interactive)
              (?m "to edit a commit"   fosgit-rebase-edit-commit)
              (?s "subset"             fosgit-rebase-subset)
              (?w "to reword a commit" fosgit-rebase-reword-commit) nil
              (?f "to autosquash"      fosgit-rebase-autosquash))
  :sequence-actions '((?r "Continue" fosgit-rebase-continue)
                      (?s "Skip"     fosgit-rebase-skip)
                      (?e "Edit"     fosgit-rebase-edit)
                      (?a "Abort"    fosgit-rebase-abort))
  :sequence-predicate 'fosgit-rebase-in-progress-p
  :max-action-columns 2)

(defun fosgit-git-rebase (target args)
  (fosgit-run-git-sequencer "rebase" target args))

;;;###autoload
(defun fosgit-rebase-onto-pushremote (args)
  "Rebase the current branch onto `branch.<name>.pushRemote'.
If that variable is unset, then rebase onto `remote.pushDefault'."
  (interactive (list (fosgit-rebase-arguments)))
  (--if-let (fosgit-get-current-branch)
      (-if-let (remote (fosgit-get-push-remote it))
          (if (member remote (fosgit-list-remotes))
              (fosgit-git-rebase (concat remote "/" it) args)
            (user-error "Remote `%s' doesn't exist" remote))
        (user-error "No push-remote is configured for %s" it))
    (user-error "No branch is checked out")))

;;;###autoload
(defun fosgit-rebase-onto-upstream (args)
  "Rebase the current branch onto its upstream branch."
  (interactive (list (fosgit-rebase-arguments)))
  (--if-let (fosgit-get-current-branch)
      (-if-let (target (fosgit-get-upstream-branch it))
          (fosgit-git-rebase target args)
        (user-error "No upstream is configured for %s" it))
    (user-error "No branch is checked out")))

;;;###autoload
(defun fosgit-rebase (target args)
  "Rebase the current branch onto a branch read in the minibuffer.
All commits that are reachable from head but not from the
selected branch TARGET are being rebased."
  (interactive (list (fosgit-read-other-branch-or-commit "Rebase onto")
                     (fosgit-rebase-arguments)))
  (message "Rebasing...")
  (fosgit-git-rebase target args)
  (message "Rebasing...done"))

;;;###autoload
(defun fosgit-rebase-subset (newbase start args)
  "Rebase a subset of the current branches history onto a new base.
Rebase commits from START to `HEAD' onto NEWBASE.
START has to be selected from a list of recent commits."
  (interactive (list (fosgit-read-other-branch-or-commit
                      "Rebase subset onto" nil
                      (fosgit-get-upstream-branch))
                     nil
                     (fosgit-rebase-arguments)))
  (if start
      (progn (message "Rebasing...")
             (fosgit-run-git-sequencer "rebase" "--onto" newbase start args)
             (message "Rebasing...done"))
    (fosgit-log-select
      `(lambda (commit)
         (fosgit-rebase-subset ,newbase (concat commit "^") (list ,@args)))
      (concat "Type %p on a commit to rebase it "
              "and commits above it onto " newbase ","))))

(defun fosgit-rebase-interactive-1 (commit args message &optional editor)
  (declare (indent 2))
  (when commit
    (if (eq commit :merge-base)
        (setq commit (--if-let (fosgit-get-upstream-branch)
                         (fosgit-git-string "merge-base" it "HEAD")
                       nil))
      (when (fosgit-git-failure "merge-base" "--is-ancestor" commit "HEAD")
        (user-error "%s isn't an ancestor of HEAD" commit))
      (if (fosgit-commit-parents commit)
          (setq commit (concat commit "^"))
        (setq args (cons "--root" args)))))
  (when (and commit
             (fosgit-git-lines "rev-list" "--merges" (concat commit "..HEAD")))
    (fosgit-read-char-case "Proceed despite merge in rebase range?  " nil
      (?c "[c]ontinue")
      (?s "[s]elect other" (setq commit nil))
      (?a "[a]bort" (user-error "Quit"))))
  (if commit
      (let ((process-environment process-environment))
        (when editor
          (setenv "GIT_SEQUENCE_EDITOR" editor))
        (fosgit-run-git-sequencer "rebase" "-i" args
                                 (unless (member "--root" args) commit)))
    (fosgit-log-select
      `(lambda (commit)
         (fosgit-rebase-interactive-1 commit (list ,@args) ,message ,editor))
      message)))

;;;###autoload
(defun fosgit-rebase-interactive (commit args)
  "Start an interactive rebase sequence."
  (interactive (list (fosgit-commit-at-point)
                     (fosgit-rebase-arguments)))
  (fosgit-rebase-interactive-1 commit args
    "Type %p on a commit to rebase it and all commits above it,"))

;;;###autoload
(defun fosgit-rebase-autosquash (args)
  "Combine squash and fixup commits with their intended targets."
  (interactive (list (fosgit-rebase-arguments)))
  (fosgit-rebase-interactive-1 :merge-base (cons "--autosquash" args)
    "Type %p on a commit to squash into it and then rebase as necessary,"
    "true"))

;;;###autoload
(defun fosgit-rebase-edit-commit (commit args)
  "Edit a single older commit using rebase."
  (interactive (list (fosgit-commit-at-point)
                     (fosgit-rebase-arguments)))
  (fosgit-rebase-interactive-1 commit args
    "Type %p on a commit to edit it,"
    "perl -i -p -e '++$x if not $x and s/^pick/edit/'"))

;;;###autoload
(defun fosgit-rebase-reword-commit (commit args)
  "Reword a single older commit using rebase."
  (interactive (list (fosgit-commit-at-point)
                     (fosgit-rebase-arguments)))
  (fosgit-rebase-interactive-1 commit args
    "Type %p on a commit to reword its message,"
    "perl -i -p -e '++$x if not $x and s/^pick/reword/'"))

;;;###autoload
(defun fosgit-rebase-continue ()
  "Restart the current rebasing operation."
  (interactive)
  (if (fosgit-rebase-in-progress-p)
      (if (fosgit-anything-unstaged-p t)
          (user-error "Cannot continue rebase with unstaged changes")
        (fosgit-run-git-sequencer "rebase" "--continue"))
    (user-error "No rebase in progress")))

;;;###autoload
(defun fosgit-rebase-skip ()
  "Skip the current commit and restart the current rebase operation."
  (interactive)
  (if (fosgit-rebase-in-progress-p)
      (fosgit-run-git-sequencer "rebase" "--skip")
    (user-error "No rebase in progress")))

;;;###autoload
(defun fosgit-rebase-edit ()
  "Edit the todo list of the current rebase operation."
  (interactive)
  (if (fosgit-rebase-in-progress-p)
      (fosgit-run-git-sequencer "rebase" "--edit-todo")
    (user-error "No rebase in progress")))

;;;###autoload
(defun fosgit-rebase-abort ()
  "Abort the current rebase operation, restoring the original branch."
  (interactive)
  (if (fosgit-rebase-in-progress-p)
      (fosgit-run-git "rebase" "--abort")
    (user-error "No rebase in progress")))

(defun fosgit-rebase-in-progress-p ()
  "Return t if a rebase is in progress."
  (or (file-exists-p (fosgit-git-dir "rebase-merge"))
      (file-exists-p (fosgit-git-dir "rebase-apply/onto"))))

;;; Sections

(defun fosgit-insert-sequencer-sequence ()
  "Insert section for the on-going cherry-pick or revert sequence.
If no such sequence is in progress, do nothing."
  (let ((picking (fosgit-cherry-pick-in-progress-p)))
    (when (or picking (fosgit-revert-in-progress-p))
      (fosgit-insert-section (sequence)
        (fosgit-insert-heading (if picking "Cherry Picking" "Reverting"))
        (-when-let (lines (cdr (fosgit-file-lines (fosgit-git-dir "sequencer/todo"))))
          (dolist (line (nreverse lines))
            (when (string-match "^\\(pick\\|revert\\) \\([^ ]+\\) \\(.*\\)$" line)
              (fosgit-bind-match-strings (cmd hash msg) line
                (fosgit-insert-section (commit hash)
                  (insert (propertize cmd 'face 'fosgit-sequence-pick)
                          " " (propertize hash 'face 'fosgit-hash)
                          " " msg "\n"))))))
        (fosgit-sequence-insert-sequence
         (fosgit-file-line (fosgit-git-dir (if picking
                                             "CHERRY_PICK_HEAD"
                                           "REVERT_HEAD")))
         (fosgit-file-line (fosgit-git-dir "sequencer/head")))
        (insert "\n")))))

(defun fosgit-insert-am-sequence ()
  "Insert section for the on-going patch applying sequence.
If no such sequence is in progress, do nothing."
  (when (fosgit-am-in-progress-p)
    (fosgit-insert-section (rebase-sequence)
      (fosgit-insert-heading "Applying patches")
      (let ((patches (nreverse (fosgit-rebase-patches)))
            patch commit)
        (while patches
          (setq patch (pop patches)
                commit (fosgit-rev-verify-commit
                        (cadr (split-string (fosgit-file-line patch)))))
          (cond ((and commit patches)
                 (fosgit-sequence-insert-commit
                  "pick" commit 'fosgit-sequence-pick))
                (patches
                 (fosgit-sequence-insert-am-patch
                  "pick" patch 'fosgit-sequence-pick))
                (commit
                 (fosgit-sequence-insert-sequence commit "ORIG_HEAD"))
                (t
                 (fosgit-sequence-insert-am-patch
                  "stop" patch 'fosgit-sequence-stop)
                 (fosgit-sequence-insert-sequence nil "ORIG_HEAD")))))
      (insert ?\n))))

(defun fosgit-sequence-insert-am-patch (type patch face)
  (fosgit-insert-section (file patch)
    (insert (propertize type 'face face)
            ?\s (propertize (file-name-nondirectory patch) 'face 'fosgit-hash)
            ?\n)))

(defun fosgit-insert-rebase-sequence ()
  "Insert section for the on-going rebase sequence.
If no such sequence is in progress, do nothing."
  (when (fosgit-rebase-in-progress-p)
    (let* ((interactive (file-directory-p (fosgit-git-dir "rebase-merge")))
           (dir  (if interactive "rebase-merge/" "rebase-apply/"))
           (name (-> (concat dir "head-name") fosgit-git-dir fosgit-file-line))
           (onto (-> (concat dir "onto")      fosgit-git-dir fosgit-file-line))
           (onto (or (fosgit-rev-name onto name)
                     (fosgit-rev-name onto "refs/heads/*") onto))
           (name (or (fosgit-rev-name name "refs/heads/*") name)))
      (fosgit-insert-section (rebase-sequence)
        (fosgit-insert-heading (format "Rebasing %s onto %s" name onto))
        (if interactive
            (fosgit-rebase-insert-merge-sequence)
          (fosgit-rebase-insert-apply-sequence))
        (fosgit-sequence-insert-sequence
         (fosgit-file-line
          (fosgit-git-dir
           (concat dir (if interactive "stopped-sha" "original-commit"))))
         onto (--map (cadr (split-string it))
                     (fosgit-file-lines (fosgit-git-dir "rebase-merge/done"))))
        (insert ?\n)))))

(defun fosgit-rebase-insert-merge-sequence ()
  (dolist (line (nreverse
                 (fosgit-file-lines
                  (fosgit-git-dir "rebase-merge/git-rebase-todo"))))
    (when (string-match "^\\([^# ]+\\) \\([^ ]+\\) .*$" line)
      (fosgit-bind-match-strings (action hash) line
        (fosgit-sequence-insert-commit action hash 'fosgit-sequence-pick)))))

(defun fosgit-rebase-insert-apply-sequence ()
  (dolist (patch (nreverse (cdr (fosgit-rebase-patches))))
    (fosgit-sequence-insert-commit
     "pick" (cadr (split-string (fosgit-file-line patch))) 'fosgit-sequence-pick)))

(defun fosgit-rebase-patches ()
  (directory-files (fosgit-git-dir "rebase-apply") t "^[0-9]\\{4\\}$"))

(defun fosgit-sequence-insert-sequence (stop onto &optional orig)
  (let ((head (fosgit-rev-parse "HEAD")) done)
    (setq onto (if onto (fosgit-rev-parse onto) head))
    (setq done (fosgit-git-lines "log" "--format=%H" (concat onto "..HEAD")))
    (when (and stop (not (member stop done)))
      (let ((id (fosgit-patch-id stop)))
        (--if-let (--first (equal (fosgit-patch-id it) id) done)
            (setq stop it)
          (cond
           ((--first (fosgit-rev-equal it stop) done)
            ;; The commit's testament has been executed.
            (fosgit-sequence-insert-commit "void" stop 'fosgit-sequence-drop))
           ;; The faith of the commit is still undecided...
           ((fosgit-anything-unmerged-p)
            ;; ...and time travel isn't for the faint of heart.
            (fosgit-sequence-insert-commit "join" stop 'fosgit-sequence-part))
           ((fosgit-anything-modified-p t)
            ;; ...and the dust hasn't settled yet...
            (fosgit-sequence-insert-commit
             (let ((staged   (fosgit-commit-tree "oO" nil "HEAD"))
                   (unstaged (fosgit-commit-worktree "oO" "--reset")))
               (cond
                ;; ...but we could end up at the same tree just by committing.
                ((or (fosgit-rev-equal staged   stop)
                     (fosgit-rev-equal unstaged stop)) "goal")
                ;; ...but the changes are still there, untainted.
                ((or (equal (fosgit-patch-id staged)   id)
                     (equal (fosgit-patch-id unstaged) id)) "same")
                ;; ...and some changes are gone and/or others were added.
                (t "work")))
             stop 'fosgit-sequence-part))
           ;; The commit is definitely gone...
           ((--first (fosgit-rev-equal it stop) done)
            ;; ...but all of its changes are still in effect.
            (fosgit-sequence-insert-commit "poof" stop 'fosgit-sequence-drop))
           (t
            ;; ...and some changes are gone and/or other changes were added.
            (fosgit-sequence-insert-commit "gone" stop 'fosgit-sequence-drop)))
          (setq stop nil))))
    (dolist (rev done)
      (apply 'fosgit-sequence-insert-commit
             (cond ((equal rev stop)
                    ;; ...but its reincarnation lives on.
                    ;; Or it didn't die in the first place.
                    (list (if (and (equal rev head)
                                   (equal (fosgit-patch-id (concat stop "^"))
                                          (fosgit-patch-id (car (last orig 2)))))
                              "stop" ; We haven't done anything yet.
                            "same")  ; There are new commits.
                          rev (if (equal rev head)
                                  'fosgit-sequence-head
                                'fosgit-sequence-stop)))
                   ((equal rev head)
                    (list "done" rev 'fosgit-sequence-head))
                   (t
                    (list "done" rev 'fosgit-sequence-done)))))
    (fosgit-sequence-insert-commit "onto" onto
                                (if (equal onto head)
                                    'fosgit-sequence-head
                                  'fosgit-sequence-onto))))

(defun fosgit-sequence-insert-commit (type hash face)
  (fosgit-insert-section (commit hash)
    (insert (propertize type 'face face)    ?\s
            (fosgit-format-rev-summary hash) ?\n)))

;;; fosgit-sequence.el ends soon
(provide 'fosgit-sequence)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-sequence.el ends here
