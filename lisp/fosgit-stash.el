;;; fosgit-stash.el --- stash support for Fosgit  -*- lexical-binding: t -*-

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

;; Support for Git stashes.

;;; Code:

(require 'fosgit)

;;; Commands

;;;###autoload (autoload 'fosgit-stash-popup "fosgit-stash" nil t)
(fosgit-define-popup fosgit-stash-popup
  "Popup console for stash commands."
  'fosgit-commands
  :man-page "git-stash"
  :switches '((?u "Also save untracked files" "--include-untracked")
              (?a "Also save untracked and ignored files" "--all"))
  :actions  '((?z "Save"               fosgit-stash)
              (?Z "Snapshot"           fosgit-snapshot)
              (?p "Pop"                fosgit-stash-pop)
              (?i "Save index"         fosgit-stash-index)
              (?I "Snapshot index"     fosgit-snapshot-index)
              (?a "Apply"              fosgit-stash-apply)
              (?w "Save worktree"      fosgit-stash-worktree)
              (?W "Snapshot worktree"  fosgit-snapshot-worktree)
              (?l "List"               fosgit-stash-list)
              (?x "Save keeping index" fosgit-stash-keep-index)
              (?r "Snapshot to wipref" fosgit-wip-commit)
              (?v "Show"               fosgit-stash-show)
              (?b "Branch"             fosgit-stash-branch)
              (?k "Drop"               fosgit-stash-drop) nil
              (?f "Format patch"       fosgit-stash-format-patch))
  :default-action 'fosgit-stash
  :max-action-columns 3)

;;;###autoload
(defun fosgit-stash (message &optional include-untracked)
  "Create a stash of the index and working tree.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive (fosgit-stash-read-args))
  (fosgit-stash-save message t t include-untracked t))

;;;###autoload
(defun fosgit-stash-index (message)
  "Create a stash of the index only.
Unstaged and untracked changes are not stashed.  The stashed
changes are applied in reverse to both the index and the
worktree.  This command can fail when the worktree is not clean.
Applying the resulting stash has the inverse effect."
  (interactive (list (fosgit-stash-read-message)))
  (fosgit-stash-save message t nil nil t 'worktree))

;;;###autoload
(defun fosgit-stash-worktree (message &optional include-untracked)
  "Create a stash of the working tree only.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive (fosgit-stash-read-args))
  (fosgit-stash-save message nil t include-untracked t 'index))

;;;###autoload
(defun fosgit-stash-keep-index (message &optional include-untracked)
  "Create a stash of the index and working tree, keeping index intact.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive (fosgit-stash-read-args))
  (fosgit-stash-save message t t include-untracked t 'index))

(defun fosgit-stash-read-args ()
  (list (fosgit-stash-read-message)
        (fosgit-stash-read-untracked)))

(defun fosgit-stash-read-untracked ()
  (let ((prefix (prefix-numeric-value current-prefix-arg))
        (args   (fosgit-stash-arguments)))
    (cond ((or (= prefix 16) (member "--all" args)) 'all)
          ((or (= prefix  4) (member "--include-untracked" args)) t))))

(defun fosgit-stash-read-message ()
  (let* ((default (format "On %s: "
                          (or (fosgit-get-current-branch) "(no branch)")))
         (input (fosgit-read-string "Stash message" default)))
    (if (equal input default)
        (concat default (fosgit-rev-format "%h %s"))
      input)))

;;;###autoload
(defun fosgit-snapshot (&optional include-untracked)
  "Create a snapshot of the index and working tree.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive (fosgit-snapshot-read-args))
  (fosgit-snapshot-save t t include-untracked t))

;;;###autoload
(defun fosgit-snapshot-index ()
  "Create a snapshot of the index only.
Unstaged and untracked changes are not stashed."
  (interactive)
  (fosgit-snapshot-save t nil nil t))

;;;###autoload
(defun fosgit-snapshot-worktree (&optional include-untracked)
  "Create a snapshot of the working tree only.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive (fosgit-snapshot-read-args))
  (fosgit-snapshot-save nil t include-untracked t))

(defun fosgit-snapshot-read-args ()
  (list (fosgit-stash-read-untracked)))

(defun fosgit-snapshot-save (index worktree untracked &optional refresh)
  (fosgit-stash-save (concat "WIP on " (fosgit-stash-summary))
                    index worktree untracked refresh t))

;;;###autoload
(defun fosgit-stash-apply (stash)
  "Apply a stash to the working tree.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index."
  (interactive (list (fosgit-read-stash "Apply stash" t)))
  (if (= (fosgit-call-git "stash" "apply" "--index" stash) 0)
      (fosgit-refresh)
    (fosgit-run-git "stash" "apply" stash)))

(defun fosgit-stash-pop (stash)
  "Apply a stash to the working tree and remove it from stash list.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index
and forgo removing the stash."
  (interactive (list (fosgit-read-stash "Apply pop" t)))
  (if (= (fosgit-call-git "stash" "apply" "--index" stash) 0)
      (fosgit-stash-drop stash)
    (fosgit-run-git "stash" "apply" stash)))

;;;###autoload
(defun fosgit-stash-drop (stash)
  "Remove a stash from the stash list.
When the region is active offer to drop all contained stashes."
  (interactive (list (--if-let (fosgit-region-values 'stash)
                         (fosgit-confirm t nil "Drop %i stashes" it)
                       (fosgit-read-stash "Drop stash"))))
  (dolist (stash (if (listp stash)
                     (nreverse (prog1 stash (setq stash (car stash))))
                   (list stash)))
    (message "Deleted refs/%s (was %s)" stash
             (fosgit-rev-parse "--short" stash))
    (fosgit-call-git "reflog" "delete" "--updateref" "--rewrite" stash))
  (-when-let (ref (and (string-match "\\(.+\\)@{[0-9]+}$" stash)
                       (match-string 1 stash)))
    (unless (string-match "^refs/" ref)
      (setq ref (concat "refs/" ref)))
    (unless (fosgit-rev-verify (concat ref "@{0}"))
      (fosgit-run-git "update-ref" "-d" ref)))
  (fosgit-refresh))

;;;###autoload
(defun fosgit-stash-clear (ref)
  "Remove all stashes saved in REF's reflog by deleting REF."
  (interactive
   (let ((ref (or (fosgit-section-when 'stashes) "refs/stash")))
     (if (fosgit-confirm t (format "Drop all stashes in %s" ref))
         (list ref)
       (user-error "Abort"))))
  (fosgit-run-git "update-ref" "-d" ref))

;;;###autoload
(defun fosgit-stash-branch (stash branch)
  "Create and checkout a new BRANCH from STASH."
  (interactive (list (fosgit-read-stash "Branch stash" t)
                     (fosgit-read-string-ns "Branch name")))
  (fosgit-run-git "stash" "branch" branch stash))

;;;###autoload
(defun fosgit-stash-format-patch (stash)
  "Create a patch from STASH"
  (interactive (list (fosgit-read-stash "Create patch from stash" t)))
  (with-temp-file (fosgit-rev-format "0001-%f.patch" stash)
    (fosgit-git-insert "stash" "show" "-p" stash))
  (fosgit-refresh))

;;; Plumbing

(defun fosgit-stash-save (message index worktree untracked
                                 &optional refresh keep noerror ref)
  (if (or (and index     (fosgit-staged-files t))
          (and worktree  (fosgit-modified-files t))
          (and untracked (fosgit-untracked-files (eq untracked 'all))))
      (fosgit-with-toplevel
        (fosgit-stash-store message (or ref "refs/stash")
                           (fosgit-stash-create message index worktree untracked))
        (if (eq keep 'worktree)
            (with-temp-buffer
              (fosgit-git-insert "diff" "--cached")
              (fosgit-run-git-with-input
               "apply" "--reverse" "--cached" "--ignore-space-change" "-")
              (fosgit-run-git-with-input
               "apply" "--reverse" "--ignore-space-change" "-"))
          (unless (eq keep t)
            (if (eq keep 'index)
                (fosgit-call-git "checkout" "--" ".")
              (fosgit-call-git "reset" "--hard" "HEAD"))
            (when untracked
              (fosgit-call-git "clean" "-f" (and (eq untracked 'all) "-x")))))
        (when refresh
          (fosgit-refresh)))
    (unless noerror
      (user-error "No %s changes to save" (cond ((not index)  "unstaged")
                                                ((not worktree) "staged")
                                                (t "local"))))))

(defun fosgit-stash-store (message ref commit)
  (fosgit-update-ref ref message commit t))

(defun fosgit-stash-create (message index worktree untracked)
  (unless (fosgit-rev-parse "--verify" "HEAD")
    (error "You do not have the initial commit yet"))
  (let ((fosgit-git-global-arguments (nconc (list "-c" "commit.gpgsign=false")
                                           fosgit-git-global-arguments))
        (default-directory (fosgit-toplevel))
        (summary (fosgit-stash-summary))
        (head "HEAD"))
    (when (and worktree (not index))
      (setq head (fosgit-commit-tree "pre-stash index" nil "HEAD")))
    (or (setq index (fosgit-commit-tree (concat "index on " summary) nil head))
        (error "Cannot save the current index state"))
    (and untracked
         (setq untracked (fosgit-untracked-files (eq untracked 'all)))
         (setq untracked (fosgit-with-temp-index nil nil
                           (or (and (fosgit-update-files untracked)
                                    (fosgit-commit-tree
                                     (concat "untracked files on " summary)))
                               (error "Cannot save the untracked files")))))
    (fosgit-with-temp-index index "-m"
      (when worktree
        (or (fosgit-update-files (fosgit-git-items "diff" "-z" "--name-only" head))
            (error "Cannot save the current worktree state")))
      (or (fosgit-commit-tree message nil head index untracked)
          (error "Cannot save the current worktree state")))))

(defun fosgit-stash-summary ()
  (concat (or (fosgit-get-current-branch) "(no branch)")
          ": " (fosgit-rev-format "%h %s")))

;;; Sections

(defvar fosgit-stashes-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap fosgit-delete-thing] 'fosgit-stash-clear)
    map)
  "Keymap for `stashes' section.")

(defvar fosgit-stash-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap fosgit-visit-thing]  'fosgit-stash-show)
    (define-key map [remap fosgit-delete-thing] 'fosgit-stash-drop)
    (define-key map "a"  'fosgit-stash-apply)
    (define-key map "A"  'fosgit-stash-pop)
    map)
  "Keymap for `stash' sections.")

(fosgit-define-section-jumper fosgit-jump-to-stashes
  "Stashes" stashes "refs/stash")

(cl-defun fosgit-insert-stashes (&optional (ref   "refs/stash")
                                          (heading "Stashes:"))
  "Insert `stashes' section showing reflog for \"refs/stash\".
If optional REF is non-nil show reflog for that instead.
If optional HEADING is non-nil use that as section heading
instead of \"Stashes:\"."
  (when (fosgit-rev-verify ref)
    (fosgit-insert-section (stashes ref (not fosgit-status-expand-stashes))
      (fosgit-insert-heading heading)
      (fosgit-git-wash (apply-partially 'fosgit-log-wash-log 'stash)
        "reflog" "--format=%gd %at %gs" ref))))

;;; List Stashes

;;;###autoload
(defun fosgit-stash-list ()
  "List all stashes in a buffer."
  (interactive)
  (fosgit-mode-setup #'fosgit-stashes-mode "refs/stash"))

(define-derived-mode fosgit-stashes-mode fosgit-reflog-mode "Fosgit Stashes"
  "Mode for looking at lists of stashes."
  :group 'fosgit-log
  (hack-dir-local-variables-non-file-buffer))

(cl-defun fosgit-stashes-refresh-buffer (ref)
  (fosgit-insert-section (stashesbuf)
    (fosgit-insert-heading (if (equal ref "refs/stash")
                              "Stashes:"
                            (format "Stashes [%s]:" ref)))
    (fosgit-git-wash (apply-partially 'fosgit-log-wash-log 'stash)
      "reflog" "--format=%gd %at %gs" ref)))

;;; Show Stash

(defcustom fosgit-stash-sections-hook
  '(fosgit-insert-stash-worktree
    fosgit-insert-stash-index
    fosgit-insert-stash-untracked)
  "Hook run to insert sections into stash buffers."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-log
  :type 'hook)

;;;###autoload
(defun fosgit-stash-show (stash &optional args files)
  "Show all diffs of a stash in a buffer."
  (interactive (cons (or (and (not current-prefix-arg)
                              (fosgit-stash-at-point))
                         (fosgit-read-stash "Show stash"))
                     (-let [(args files) (fosgit-diff-arguments)]
                       (list (delete "--stat" args) files))))
  (fosgit-mode-setup #'fosgit-stash-mode stash nil args files))

(define-derived-mode fosgit-stash-mode fosgit-diff-mode "Fosgit Stash"
  "Mode for looking at individual stashes."
  :group 'fosgit-diff
  (hack-dir-local-variables-non-file-buffer))

(defun fosgit-stash-refresh-buffer (stash _const _args _files)
  (setq header-line-format
        (concat
         "\s" (propertize (capitalize stash) 'face 'fosgit-section-heading)
         "\s" (fosgit-rev-format "%s" stash)))
  (fosgit-insert-section (stash)
    (run-hooks 'fosgit-stash-sections-hook)))

(defun fosgit-stash-insert-section (commit range message &optional files)
  (fosgit-insert-section (commit commit)
    (fosgit-insert-heading message)
    (fosgit-git-wash #'fosgit-diff-wash-diffs
      "diff" range "-p" "--no-prefix"
      (nth 2 fosgit-refresh-args)
      "--" (or files (nth 3 fosgit-refresh-args)))))

(defun fosgit-insert-stash-index ()
  "Insert section showing the index commit of the stash."
  (let ((stash (car fosgit-refresh-args)))
    (fosgit-stash-insert-section (format "%s^2" stash)
                                (format "%s^..%s^2" stash stash)
                                "Index")))

(defun fosgit-insert-stash-worktree ()
  "Insert section showing the worktree commit of the stash."
  (let ((stash (car fosgit-refresh-args)))
    (fosgit-stash-insert-section stash
                                (format "%s^2..%s" stash stash)
                                "Working tree")))

(defun fosgit-insert-stash-untracked ()
  "Insert section showing the untracked files commit of the stash."
  (let ((stash (car fosgit-refresh-args))
        (rev   (concat (car fosgit-refresh-args) "^3")))
    (when (fosgit-rev-verify rev)
      (fosgit-stash-insert-section (format "%s^3" stash)
                                  (format "%s^..%s^3" stash stash)
                                  "Untracked files"
                                  (fosgit-git-items "ls-tree" "-z" "--name-only"
                                                   "--full-tree" rev)))))

;;; fosgit-stash.el ends soon
(provide 'fosgit-stash)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-stash.el ends here
