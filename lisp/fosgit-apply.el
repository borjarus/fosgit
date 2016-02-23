;;; fosgit-apply.el --- apply Git diffs  -*- lexical-binding: t -*-

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

;; This library implements commands for applying Git diffs or parts
;; of such a diff.  The supported "apply variants" are apply, stage,
;; unstage, discard, and reverse - more than Git itself knows about,
;; at least at the porcelain level.

;;; Code:

(require 'fosgit-core)
(require 'fosgit-diff)
(require 'fosgit-wip)

;; For `fosgit-apply'
(declare-function fosgit-anti-stage 'fosgit-rockstar)
(declare-function fosgit-am-popup 'fosgit-sequence)
;; For `fosgit-discard-files'
(declare-function fosgit-checkout-stage 'fosgit)
(declare-function fosgit-checkout-read-stage 'fosgit)
(defvar auto-revert-verbose)

(require 'dired)

;;; Options

(defcustom fosgit-delete-by-moving-to-trash t
  "Whether Fosgit uses the system's trash can."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit
  :type 'boolean)

(defcustom fosgit-unstage-committed t
  "Whether unstaging a committed change reverts it instead.

A committed change cannot be unstaged, because staging and
unstaging are actions that are concern with the differences
between the index and the working tree, not with committed
changes.

If this option is non-nil (the default), then typing \"u\"
(`fosgit-unstage') on a committed change, causes it to be
reversed in the index but not the working tree.  For more
information see command `fosgit-reverse-in-index'."
  :package-version '(fosgit . "2.4.1")
  :group 'fosgit-commands
  :type 'boolean)

;;; Commands
;;;; Apply

(defun fosgit-apply (&rest args)
  "Apply the change at point.
With a prefix argument and if necessary, attempt a 3-way merge."
  (interactive (and current-prefix-arg (list "--3way")))
  (--when-let (fosgit-apply--get-selection)
    (pcase (list (fosgit-diff-type) (fosgit-diff-scope))
      (`(,(or `unstaged `staged) ,_)
       (user-error "Change is already in the working tree"))
      (`(untracked ,(or `file `files))
       (fosgit-am-popup))
      (`(,_ region) (fosgit-apply-region it args))
      (`(,_   hunk) (fosgit-apply-hunk   it args))
      (`(,_  hunks) (fosgit-apply-hunks  it args))
      (`(,_   file) (fosgit-apply-diff   it args))
      (`(,_  files) (fosgit-apply-diffs  it args)))))

(defun fosgit-apply-diffs (sections &rest args)
  (setq sections (fosgit-apply--get-diffs sections))
  (fosgit-apply-patch sections args
                     (mapconcat
                      (lambda (s)
                        (concat (fosgit-diff-file-header s)
                                (buffer-substring (fosgit-section-content s)
                                                  (fosgit-section-end s))))
                      sections "")))

(defun fosgit-apply-diff (section &rest args)
  (setq section (car (fosgit-apply--get-diffs (list section))))
  (fosgit-apply-patch section args
                     (concat (fosgit-diff-file-header section)
                             (buffer-substring (fosgit-section-content section)
                                               (fosgit-section-end section)))))

(defun fosgit-apply-hunks (sections &rest args)
  (let ((section (fosgit-section-parent (car sections))))
    (when (string-match "^diff --cc" (fosgit-section-value section))
      (user-error "Cannot un-/stage resolution hunks.  Stage the whole file"))
    (fosgit-apply-patch section args
                       (concat (fosgit-section-diff-header section)
                               (mapconcat
                                (lambda (s)
                                  (buffer-substring (fosgit-section-start s)
                                                    (fosgit-section-end s)))
                                sections "")))))

(defun fosgit-apply-hunk (section &rest args)
  (when (string-match "^diff --cc" (fosgit-section-parent-value section))
    (user-error "Cannot un-/stage resolution hunks.  Stage the whole file"))
  (fosgit-apply-patch (fosgit-section-parent section) args
                     (concat (fosgit-diff-file-header section)
                             (buffer-substring (fosgit-section-start section)
                                               (fosgit-section-end section)))))

(defun fosgit-apply-region (section &rest args)
  (unless (fosgit-diff-context-p)
    (user-error "Not enough context to apply region.  Increase the context"))
  (when (string-match "^diff --cc" (fosgit-section-parent-value section))
    (user-error "Cannot un-/stage resolution hunks.  Stage the whole file"))
  (fosgit-apply-patch (fosgit-section-parent section) args
                     (concat (fosgit-diff-file-header section)
                             (fosgit-diff-hunk-region-patch section args))))

(defun fosgit-apply-patch (section:s args patch)
  (let* ((files (if (atom section:s)
                    (list (fosgit-section-value section:s))
                  (mapcar 'fosgit-section-value section:s)))
         (command (symbol-name this-command))
         (command (if (and command (string-match "^fosgit-\\([^-]+\\)" command))
                      (match-string 1 command)
                    "apply")))
    (when (and fosgit-wip-before-change-mode (not inhibit-fosgit-refresh))
      (fosgit-wip-commit-before-change files (concat " before " command)))
    (with-temp-buffer
      (insert patch)
      (fosgit-run-git-with-input
       "apply" args "-p0"
       (unless (fosgit-diff-context-p) "--unidiff-zero")
       "--ignore-space-change" "-"))
    (unless inhibit-fosgit-refresh
      (when fosgit-wip-after-apply-mode
        (fosgit-wip-commit-after-apply files (concat " after " command)))
      (fosgit-refresh))))

(defun fosgit-apply--get-selection ()
  (or (fosgit-region-sections 'hunk 'file)
      (let ((section (fosgit-current-section)))
        (pcase (fosgit-section-type section)
          ((or `hunk `file) section)
          ((or `staged `unstaged `untracked
               `stashed-index `stashed-worktree `stashed-untracked)
           (fosgit-section-children section))
          (_ (user-error "Cannot apply this, it's not a change"))))))

(defun fosgit-apply--get-diffs (sections)
  (fosgit-section-case
    ([file diffstat]
     (--map (or (fosgit-get-section
                 (append `((file . ,(fosgit-section-value it)))
                         (fosgit-section-ident fosgit-root-section)))
                (error "Cannot get required diff headers"))
            sections))
    (t sections)))

;;;; Stage

(defun fosgit-stage ()
  "Add the change at point to the staging area."
  (interactive)
  (--when-let (fosgit-apply--get-selection)
    (pcase (list (fosgit-diff-type) (fosgit-diff-scope))
      (`(untracked     ,_) (fosgit-stage-untracked))
      (`(unstaged  region) (fosgit-apply-region it "--cached"))
      (`(unstaged    hunk) (fosgit-apply-hunk   it "--cached"))
      (`(unstaged   hunks) (fosgit-apply-hunks  it "--cached"))
      (`(unstaged    file) (fosgit-stage-1 "-u" (list (fosgit-section-value it))))
      (`(unstaged   files) (fosgit-stage-1 "-u" (fosgit-region-values)))
      (`(unstaged    list) (fosgit-stage-1 "-u"))
      (`(staged        ,_) (user-error "Already staged"))
      (`(committed     ,_) (user-error "Cannot stage committed changes"))
      (`(undefined     ,_) (user-error "Cannot stage this change")))))

;;;###autoload
(defun fosgit-stage-file (file)
  "Stage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be staged.  Otherwise stage the file at point without
requiring confirmation."
  (interactive
   (let* ((atpoint (fosgit-section-when (file)))
          (current (fosgit-file-relative-name))
          (choices (nconc (fosgit-modified-files)
                          (fosgit-untracked-files)))
          (default (car (member (or atpoint current) choices))))
     (list (if (or current-prefix-arg (not default))
               (fosgit-completing-read "Stage file" choices
                                      nil t nil nil default)
             default))))
  (fosgit-with-toplevel
    (fosgit-stage-1 nil (list file))))

;;;###autoload
(defun fosgit-stage-modified (&optional all)
  "Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files.
\('git add --update|--all .')."
  (interactive (progn (unless (or (not (fosgit-anything-staged-p))
                                  (fosgit-confirm 'stage-all-changes))
                        (user-error "Abort"))
                      (list current-prefix-arg)))
  (fosgit-with-toplevel
    (fosgit-stage-1 (if all "--all" "-u"))))

(defun fosgit-stage-1 (arg &optional files)
  (fosgit-wip-commit-before-change files " before stage")
  (fosgit-run-git "add" arg (if files (cons "--" files) "."))
  (when fosgit-auto-revert-mode
    (mapc #'fosgit-turn-on-auto-revert-mode-if-desired files))
  (fosgit-wip-commit-after-apply files " after stage"))

(defun fosgit-stage-untracked ()
  (let* ((section (fosgit-current-section))
         (files (pcase (fosgit-diff-scope)
                  (`file  (list (fosgit-section-value section)))
                  (`files (fosgit-region-values))
                  (`list  (fosgit-untracked-files))))
         plain repos)
    (dolist (file files)
      (if (fosgit-git-repo-p file t)
          (push file repos)
        (push file plain)))
    (fosgit-wip-commit-before-change files " before stage")
    (when plain
      (fosgit-run-git "add" "--" plain)
      (when fosgit-auto-revert-mode
        (mapc #'fosgit-turn-on-auto-revert-mode-if-desired plain)))
    (dolist (repo repos)
      (save-excursion
        (goto-char (fosgit-section-start
                    (fosgit-get-section
                     `((file . ,repo) (untracked) (status)))))
        (call-interactively 'fosgit-submodule-add)))
    (fosgit-wip-commit-after-apply files " after stage")))

;;;; Unstage

(defun fosgit-unstage ()
  "Remove the change at point from the staging area."
  (interactive)
  (--when-let (fosgit-apply--get-selection)
    (pcase (list (fosgit-diff-type) (fosgit-diff-scope))
      (`(untracked     ,_) (user-error "Cannot unstage untracked changes"))
      (`(unstaged      ,_) (user-error "Already unstaged"))
      (`(staged    region) (fosgit-apply-region it "--reverse" "--cached"))
      (`(staged      hunk) (fosgit-apply-hunk   it "--reverse" "--cached"))
      (`(staged     hunks) (fosgit-apply-hunks  it "--reverse" "--cached"))
      (`(staged      file) (fosgit-unstage-1 (list (fosgit-section-value it))))
      (`(staged     files) (fosgit-unstage-1 (fosgit-region-values)))
      (`(staged      list) (fosgit-unstage-all))
      (`(committed     ,_) (if fosgit-unstage-committed
                               (fosgit-reverse-in-index)
                             (user-error "Cannot unstage committed changes")))
      (`(undefined     ,_) (user-error "Cannot unstage this change")))))

;;;###autoload
(defun fosgit-unstage-file (file)
  "Unstage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be unstaged.  Otherwise unstage the file at point
without requiring confirmation."
  (interactive
   (let* ((atpoint (fosgit-section-when (file)))
          (current (fosgit-file-relative-name))
          (choices (fosgit-staged-files))
          (default (car (member (or atpoint current) choices))))
     (list (if (or current-prefix-arg (not default))
               (fosgit-completing-read "Unstage file" choices
                                      nil t nil nil default)
             default))))
  (fosgit-with-toplevel
    (fosgit-unstage-1 (list file))))

(defun fosgit-unstage-1 (files)
  (fosgit-wip-commit-before-change files " before unstage")
  (if (fosgit-no-commit-p)
      (fosgit-run-git "rm" "--cached" "--" files)
    (fosgit-run-git "reset" "HEAD" "--" files))
  (fosgit-wip-commit-after-apply files " after unstage"))

;;;###autoload
(defun fosgit-unstage-all ()
  "Remove all changes from the staging area."
  (interactive)
  (when (or (and (not (fosgit-anything-unstaged-p))
                 (not (fosgit-untracked-files)))
            (fosgit-confirm 'unstage-all-changes))
    (fosgit-wip-commit-before-change nil " before unstage")
    (fosgit-run-git "reset" "HEAD" "--")
    (fosgit-wip-commit-after-apply nil " after unstage")))

;;;; Discard

(defun fosgit-discard ()
  "Remove the change at point."
  (interactive)
  (--when-let (fosgit-apply--get-selection)
    (pcase (list (fosgit-diff-type) (fosgit-diff-scope))
      (`(committed ,_) (user-error "Cannot discard committed changes"))
      (`(undefined ,_) (user-error "Cannot discard this change"))
      (`(,_    region) (fosgit-discard-region it))
      (`(,_      hunk) (fosgit-discard-hunk   it))
      (`(,_     hunks) (fosgit-discard-hunks  it))
      (`(,_      file) (fosgit-discard-file   it))
      (`(,_     files) (fosgit-discard-files  it))
      (`(,_      list) (fosgit-discard-files  it)))))

(defun fosgit-discard-region (section)
  (when (fosgit-confirm 'discard "Discard region")
    (fosgit-discard-apply section 'fosgit-apply-region)))

(defun fosgit-discard-hunk (section)
  (when (fosgit-confirm 'discard "Discard hunk")
    (fosgit-discard-apply section 'fosgit-apply-hunk)))

(defun fosgit-discard-apply (section apply)
  (if (eq (fosgit-diff-type section) 'unstaged)
      (funcall apply section "--reverse")
    (if (fosgit-anything-unstaged-p
         nil (if (eq (fosgit-section-type section) 'file)
                 (fosgit-section-value section)
               (fosgit-section-parent-value section)))
        (progn (let ((inhibit-fosgit-refresh t))
                 (funcall apply section "--reverse" "--cached")
                 (funcall apply section "--reverse"))
               (fosgit-refresh))
      (funcall apply section "--reverse" "--index"))))

(defun fosgit-discard-hunks (sections)
  (when (fosgit-confirm 'discard
          (format "Discard %s hunks from %s"
                  (length sections)
                  (fosgit-section-parent-value (car sections))))
    (fosgit-discard-apply-n sections 'fosgit-apply-hunks)))

(defun fosgit-discard-apply-n (sections apply)
  (let ((section (car sections)))
    (if (eq (fosgit-diff-type section) 'unstaged)
        (funcall apply sections "--reverse")
      (if (fosgit-anything-unstaged-p
           nil (if (eq (fosgit-section-type section) 'file)
                   (fosgit-section-value section)
                 (fosgit-section-parent-value section)))
          (progn (let ((inhibit-fosgit-refresh t))
                   (funcall apply sections "--reverse" "--cached")
                   (funcall apply sections "--reverse"))
                 (fosgit-refresh))
        (funcall apply sections "--reverse" "--index")))))

(defun fosgit-discard-file (section)
  (fosgit-discard-files (list section)))

(defun fosgit-discard-files (sections)
  (let ((auto-revert-verbose nil)
        (type (fosgit-diff-type (car sections)))
        (status (fosgit-file-status))
        files delete resurrect rename discard discard-new resolve)
    (dolist (section sections)
      (let ((file (fosgit-section-value section)))
        (push file files)
        (pcase (cons (pcase type
                       (`staged ?X)
                       (`unstaged ?Y)
                       (`untracked ?Z))
                     (cddr (assoc file status)))
          (`(?Z) (--each (fosgit-untracked-files nil file)
                   (push it delete)))
          ((or `(?Z ?? ??) `(?Z ?! ?!)) (push file delete))
          ((or `(?Z ?D ? ) `(,_ ?D ?D)) (push file delete))
          ((or `(,_ ?U ,_) `(,_ ,_ ?U)) (push file resolve))
          (`(,_ ?A ?A)                  (push file resolve))
          (`(?X ?M ,(or ?  ?M ?D)) (push section discard))
          (`(?Y ,_         ?M    ) (push section discard))
          (`(?X ?A         ?M    ) (push file discard-new))
          (`(?X ?C         ?M    ) (push file discard-new))
          (`(?X ?A ,(or ?     ?D)) (push file delete))
          (`(?X ?C ,(or ?     ?D)) (push file delete))
          (`(?X ?D ,(or ?  ?M   )) (push file resurrect))
          (`(?Y ,_            ?D ) (push file resurrect))
          (`(?X ?R ,(or ?  ?M ?D)) (push file rename)))))
    (unwind-protect
        (let ((inhibit-fosgit-refresh t))
          (fosgit-wip-commit-before-change files " before discard")
          (when resolve
            (dolist (file (nreverse resolve))
              (fosgit-checkout-stage file (fosgit-checkout-read-stage file))))
          (fosgit-discard-files--resurrect (nreverse resurrect))
          (fosgit-discard-files--delete    (nreverse delete) status)
          (fosgit-discard-files--rename    (nreverse rename) status)
          (fosgit-discard-files--discard   (nreverse discard)
                                          (nreverse discard-new))
          (fosgit-wip-commit-after-apply files " after discard"))
      (fosgit-refresh))))

(defun fosgit-discard-files--resurrect (files)
  (when (fosgit-confirm-files 'resurrect files)
    (if (eq (fosgit-diff-type) 'staged)
        (fosgit-call-git "reset"  "--" files)
      (fosgit-call-git "checkout" "--" files))))

(defun fosgit-discard-files--delete (files status)
  (when (if fosgit-delete-by-moving-to-trash
            (fosgit-confirm-files 'trash files)
          (fosgit-confirm-files 'delete files))
    (let ((delete-by-moving-to-trash fosgit-delete-by-moving-to-trash))
      (dolist (file files)
        (if (memq (fosgit-diff-type) '(unstaged untracked))
            (dired-delete-file file dired-recursive-deletes
                               fosgit-delete-by-moving-to-trash)
          (pcase (nth 3 (assoc file status))
            (?  (delete-file file t)
                (fosgit-call-git "rm" "--cached" "--" file))
            (?M (let ((temp (fosgit-git-string "checkout-index" "--temp" file)))
                  (string-match
                   (format "\\(.+?\\)\t%s" (regexp-quote file)) temp)
                  (rename-file (match-string 1 temp)
                               (setq temp (concat file ".~{index}~")))
                  (delete-file temp t))
                (fosgit-call-git "rm" "--cached" "--force" "--" file))
            (?D (fosgit-call-git "checkout" "--" file)
                (delete-file file t)
                (fosgit-call-git "rm" "--cached" "--force" "--" file))))))))

(defun fosgit-discard-files--rename (files status)
  (when (fosgit-confirm 'rename "Undo rename %s" "Undo %i renames"
          (mapcar (lambda (file)
                    (setq file (assoc file status))
                    (format "%s -> %s" (cadr file) (car file)))
                  files))
    (dolist (file files)
      (let ((orig (cadr (assoc file status))))
        (if (file-exists-p file)
            (fosgit-call-git "mv" file orig)
          (fosgit-call-git "rm" "--cached" "--" file)
          (fosgit-call-git "reset" "--" orig))))))

(defun fosgit-discard-files--discard (sections new-files)
  (let ((files (mapcar #'fosgit-section-value sections)))
    (when (fosgit-confirm-files
           'discard (append files new-files)
           (format "Discard %s changes in" (fosgit-diff-type)))
      (if (eq (fosgit-diff-type (car sections)) 'unstaged)
          (fosgit-call-git "checkout" "--" files)
        (when new-files
          (fosgit-call-git "add"   "--" new-files)
          (fosgit-call-git "reset" "--" new-files))
        (let ((binaries (fosgit-staged-binary-files)))
          (when binaries
            (setq sections
                  (--filter (not (member (fosgit-section-value it) binaries))
                            sections)))
          (if (= (length sections) 1)
              (fosgit-discard-apply (car sections) 'fosgit-apply-diff)
            (fosgit-discard-apply-n sections 'fosgit-apply-diffs))
          (when binaries
            (let ((modified (fosgit-modified-files t)))
              (setq binaries (--separate (member it modified) binaries)))
            (when (cadr binaries)
              (fosgit-call-git "reset" "--" (cadr binaries)))
            (when (car binaries)
              (user-error
               (concat
                "Cannot discard staged changes to binary files, "
                "which also have unstaged changes.  Unstage instead.")))))))))

;;;; Reverse

(defun fosgit-reverse (&rest args)
  "Reverse the change at point in the working tree."
  (interactive (and current-prefix-arg (list "--3way")))
  (--when-let (fosgit-apply--get-selection)
    (pcase (list (fosgit-diff-type) (fosgit-diff-scope))
      (`(untracked ,_) (user-error "Cannot reverse untracked changes"))
      (`(unstaged  ,_) (user-error "Cannot reverse unstaged changes"))
      (`(,_    region) (fosgit-reverse-region it args))
      (`(,_      hunk) (fosgit-reverse-hunk   it args))
      (`(,_     hunks) (fosgit-reverse-hunks  it args))
      (`(,_      file) (fosgit-reverse-file   it args))
      (`(,_     files) (fosgit-reverse-files  it args))
      (`(,_      list) (fosgit-reverse-files  it args)))))

(defun fosgit-reverse-region (section args)
  (when (fosgit-confirm 'reverse "Reverse region")
    (apply 'fosgit-apply-region section "--reverse" args)))

(defun fosgit-reverse-hunk (section args)
  (when (fosgit-confirm 'reverse "Reverse hunk")
    (apply 'fosgit-apply-hunk section "--reverse" args)))

(defun fosgit-reverse-hunks (sections args)
  (when (fosgit-confirm 'reverse
          (format "Reverse %s hunks from %s"
                  (length sections)
                  (fosgit-section-parent-value (car sections))))
    (fosgit-apply-hunks sections "--reverse" args)))

(defun fosgit-reverse-file (section args)
  (fosgit-reverse-files (list section) args))

(defun fosgit-reverse-files (sections args)
  (-let [(binaries sections)
         (let ((bs (fosgit-staged-binary-files)))
           (--separate (member (fosgit-section-value it) bs) sections))]
    (when (fosgit-confirm-files 'reverse (mapcar #'fosgit-section-value sections))
      (if (= (length sections) 1)
          (fosgit-apply-diff (car sections) "--reverse" args)
        (fosgit-apply-diffs sections "--reverse" args)))
    (when binaries
      (user-error "Cannot reverse binary files"))))

(defun fosgit-reverse-in-index (&rest args)
  "Reverse the change at point in the index but not the working tree.

Use this command to extract a change from `HEAD', while leaving
it in the working tree, so that it can later be committed using
a separate commit.  A typical workflow would be:

0. Optionally make sure that there are no uncommitted changes.
1. Visit the `HEAD' commit and navigate to the change that should
   not have been included in that commit.
2. Type \"u\" (`fosgit-unstage') to reverse it in the index.
   This assumes that `fosgit-unstage-committed-changes' is non-nil.
3. Type \"c e\" to extend `HEAD' with the staged changes,
   including those that were already staged before.
4. Optionally stage the remaining changes using \"s\" or \"S\"
   and then type \"c c\" to create a new commit."
  (interactive)
  (fosgit-reverse (cons "--cached" args)))

;;; fosgit-apply.el ends soon
(provide 'fosgit-apply)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-apply.el ends here
