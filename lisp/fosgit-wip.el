;;; fosgit-wip.el --- commit snapshots to work-in-progress refs  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2016  The Magit Project Contributors
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

;; This library defines tree global modes which automatically commit
;; snapshots to branch specific work-in-progress refs before and after
;; making changes, and two commands which can be used to do so on
;; demand.

;;; Code:

(require 'fosgit-core)
(require 'fosgit-log)

;;; Options

(defgroup fosgit-wip nil
  "Automatically commit to work-in-progress refs."
  :group 'fosgit-extensions)

(defcustom fosgit-wip-after-save-local-mode-lighter " sWip"
  "Lighter for Fosgit-Wip-After-Save-Local mode."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-wip
  :type 'string)

(defcustom fosgit-wip-after-apply-mode-lighter " aWip"
  "Lighter for Fosgit-Wip-After-Apply mode."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-wip
  :type 'string)

(defcustom fosgit-wip-before-change-mode-lighter " cWip"
  "Lighter for Fosgit-Wip-Before-Change mode."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-wip
  :type 'string)

(defcustom fosgit-wip-namespace "refs/wip/"
  "Namespace used for work-in-progress refs.
The wip refs are named \"<namespace/>index/<branchref>\"
and \"<namespace/>wtree/<branchref>\".  When snapshots
are created while the `HEAD' is detached then \"HEAD\"
is used as `branch-ref'."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-wip
  :type 'string)

;;; Modes

(define-minor-mode fosgit-wip-after-save-local-mode
  "After saving, also commit to a worktree work-in-progress ref.

After saving the current file-visiting buffer this mode also
commits the changes to the worktree work-in-progress ref for
the current branch.

This mode should be enabled globally by turning on the globalized
variant `fosgit-wip-after-save-mode'."
  :package-version '(fosgit . "2.1.0")
  :lighter fosgit-wip-after-save-local-mode-lighter
  (if fosgit-wip-after-save-local-mode
      (if (and buffer-file-name (fosgit-inside-worktree-p))
          (add-hook 'after-save-hook 'fosgit-wip-commit-buffer-file t t)
        (setq fosgit-wip-after-save-local-mode nil)
        (user-error "Need a worktree and a file"))
    (remove-hook 'after-save-hook 'fosgit-wip-commit-buffer-file t)))

(defun fosgit-wip-after-save-local-mode-turn-on ()
  (and buffer-file-name
       (ignore-errors (fosgit-inside-worktree-p))
       (fosgit-file-tracked-p buffer-file-name)
       (fosgit-wip-after-save-local-mode)))

;;;###autoload
(define-globalized-minor-mode fosgit-wip-after-save-mode
  fosgit-wip-after-save-local-mode fosgit-wip-after-save-local-mode-turn-on
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-wip)

(defun fosgit-wip-commit-buffer-file ()
  "Commit visited file to a worktree work-in-progress ref.

Also see `fosgit-wip-after-save-mode' which calls this function
automatically whenever a buffer visiting a tracked file is saved."
  (interactive)
  (--when-let (fosgit-wip-get-ref)
    (fosgit-with-toplevel
      (let ((file (file-relative-name buffer-file-name)))
        (fosgit-wip-commit-worktree
         it (list file) (if (called-interactively-p 'any)
                            (format "wip-save %s after save" file)
                          (format "autosave %s after save" file)))))))

;;;###autoload
(define-minor-mode fosgit-wip-after-apply-mode
  "Commit to work-in-progress refs.

After applying a change using any \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected files to the current wip refs.  For each branch there
may be two wip refs; one contains snapshots of the files as found
in the worktree and the other contains snapshots of the entries
in the index."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-wip
  :lighter fosgit-wip-after-change-mode-lighter
  :global t)

(defun fosgit-wip-commit-after-apply (&optional files msg)
  (when fosgit-wip-after-apply-mode
    (fosgit-wip-commit files msg)))

;;;###autoload
(define-minor-mode fosgit-wip-before-change-mode
  "Commit to work-in-progress refs before certain destructive changes.

Before invoking a revert command or an \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected tracked files to the current wip refs.  For each branch
there may be two wip refs; one contains snapshots of the files
as found in the worktree and the other contains snapshots of the
entries in the index.

Only changes to files which could potentially be affected by the
command which is about to be called are committed."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-wip
  :lighter fosgit-wip-before-change-mode-lighter
  :global t)

(defun fosgit-wip-commit-before-change (&optional files msg)
  (when fosgit-wip-before-change-mode
    (fosgit-with-toplevel
      (fosgit-wip-commit files msg))))

;;; Core

(defun fosgit-wip-commit (&optional files msg)
  "Commit all tracked files to the work-in-progress refs.

Interactively, commit all changes to all tracked files using
a generic commit message.  With a prefix-argument the commit
message is read in the minibuffer.

Non-interactively, only commit changes to FILES using MSG as
commit message."
  (interactive (list nil (if current-prefix-arg
                             (fosgit-read-string "Wip commit message")
                           "wip-save tracked files")))
  (--when-let (fosgit-wip-get-ref)
    (fosgit-wip-commit-index it files msg)
    (fosgit-wip-commit-worktree it files msg)))

(defun fosgit-wip-commit-index (ref files msg &optional cached-only)
  (let* ((wipref (concat fosgit-wip-namespace "index/" ref))
         (parent (fosgit-wip-get-parent ref wipref)))
    (when (fosgit-git-failure "diff-index" "--quiet"
                             (and cached-only "--cached")
                             parent "--" files)
      (fosgit-wip-update-wipref wipref (fosgit-git-string "write-tree")
                               parent files msg "index"))))

(defun fosgit-wip-commit-worktree (ref files msg)
  (let* ((wipref (concat fosgit-wip-namespace "wtree/" ref))
         (parent (fosgit-wip-get-parent ref wipref))
         (tree (fosgit-with-temp-index parent "--reset"
                 (if files
                     (fosgit-call-git "add" "--" files)
                   (fosgit-with-toplevel
                     (fosgit-call-git "add" "-u" ".")))
                 (fosgit-git-string "write-tree"))))
    (when (fosgit-git-failure "diff-tree" "--quiet" parent tree "--" files)
      (fosgit-wip-update-wipref wipref tree parent files msg "worktree"))))

(defun fosgit-wip-update-wipref (wipref tree parent files msg start-msg)
  (let ((len (length files)))
    (unless (and msg (not (= (aref msg 0) ?\s)))
      (setq msg (concat
                 (cond ((= len 0) "autosave tracked files")
                       ((> len 1) (format "autosave %s files" len))
                       (t (concat "autosave "
                                  (file-relative-name (car files)
                                                      (fosgit-toplevel)))))
                 msg)))
    (unless (equal parent wipref)
      (setq start-msg (concat "restart autosaving " start-msg))
      (fosgit-update-ref wipref start-msg
                        (fosgit-git-string "commit-tree" "-p" parent
                                          "-m" start-msg
                                          (concat parent "^{tree}")))
      (setq parent wipref))
    (fosgit-update-ref wipref msg
                      (fosgit-git-string "commit-tree" tree
                                        "-p" parent "-m" msg))))

(defun fosgit-wip-get-ref ()
  (let ((ref (or (fosgit-git-string "symbolic-ref" "HEAD") "HEAD")))
    (when (fosgit-rev-verify ref)
      ref)))

(defun fosgit-wip-get-parent (ref wipref)
  (if (and (fosgit-rev-verify wipref)
           (equal (fosgit-git-string "merge-base" wipref ref)
                  (fosgit-rev-verify ref)))
      wipref
    ref))

;;; Log

(defun fosgit-wip-log-current (branch args files count)
  "Show log for the current branch and its wip refs.
With a negative prefix argument only show the worktree wip ref.
The absolute numeric value of the prefix argument controls how
many \"branches\" of each wip ref are shown."
  (interactive
   (nconc (list (or (fosgit-get-current-branch) "HEAD"))
          (fosgit-log-arguments)
          (list (prefix-numeric-value current-prefix-arg))))
  (fosgit-wip-log branch args files count))

(defun fosgit-wip-log (branch args files count)
  "Show log for a branch and its wip refs.
With a negative prefix argument only show the worktree wip ref.
The absolute numeric value of the prefix argument controls how
many \"branches\" of each wip ref are shown."
  (interactive
   (nconc (list (fosgit-completing-read
                 "Log branch and its wip refs"
                 (-snoc (fosgit-list-local-branch-names) "HEAD")
                 nil t nil 'fosgit-revision-history
                 (or (fosgit-branch-at-point)
                     (fosgit-get-current-branch)
                     "HEAD")))
          (fosgit-log-arguments)
          (list (prefix-numeric-value current-prefix-arg))))
  (unless (equal branch "HEAD")
    (setq branch (concat "refs/heads/" branch)))
  (fosgit-log (nconc (list branch)
                    (fosgit-wip-log-get-tips
                     (concat fosgit-wip-namespace "wtree/" branch)
                     (abs count))
                    (and (>= count 0)
                         (fosgit-wip-log-get-tips
                          (concat fosgit-wip-namespace "index/" branch)
                          (abs count))))
             args files))

(defun fosgit-wip-log-get-tips (wipref count)
  (-when-let (reflog (fosgit-git-lines "reflog" wipref))
    (let (tips)
      (while (and reflog (> count 1))
        (setq reflog (cl-member "^[^ ]+ [^:]+: restart autosaving"
                                reflog :test #'string-match-p))
        (when (and (cadr reflog)
                   (string-match "^[^ ]+ \\([^:]+\\)" (cadr reflog)))
          (push (match-string 1 (cadr reflog)) tips))
        (setq reflog (cddr reflog))
        (cl-decf count))
      (cons wipref (nreverse tips)))))

;;; fosgit-wip.el ends soon
(provide 'fosgit-wip)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-wip.el ends here
