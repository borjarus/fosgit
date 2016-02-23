;;; fosgit.el --- A Git porcelain inside Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2016  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Marius Vollmer <marius.vollmer@gmail.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;;	Kyle Meyer        <kyle@kyleam.com>
;;	Noam Postavsky    <npostavs@users.sourceforge.net>
;; Former-Maintainers:
;;	Nicolas Dudebout  <nicolas.dudebout@gatech.edu>
;;	Peter J. Weisberg <pj@irregularexpressions.net>
;;	Phil Jackson      <phil@shellarchive.co.uk>
;;	Rémi Vanicat      <vanicat@debian.org>
;;	Yann Hodique      <yann.hodique@gmail.com>

;; Package-Requires: ((emacs "24.4") (async "20150909.2257") (dash "20151021.113") (with-editor "20160128.1201") (git-commit "20160119.1409") (magit-popup "20160119.1409"))
;; Keywords: git tools vc
;; Homepage: https://github.com/magit/magit

;; Fosgit requires at least GNU Emacs 24.4 and Git 1.9.4.

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

;; Fosgit is an interface to the version control system Git,
;; implemented as an Emacs package.  Fosgit aspires to be a complete
;; Git porcelain.  While we cannot (yet) claim, that Fosgit wraps and
;; improves upon each and every Git command, it is complete enough to
;; allow even experienced Git users to perform almost all of their
;; daily version control tasks directly from within Emacs.  While many
;; fine Git clients exist, only Fosgit and Git itself deserve to be
;; called porcelains.

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'with-editor)
(require 'git-commit)
(require 'fosgit-core)
(require 'fosgit-diff)
(require 'fosgit-apply)
(require 'fosgit-log)

(require 'format-spec)
(require 'package nil t) ; used in `fosgit-version'

(eval-when-compile (require 'dired-x))
(declare-function dired-jump 'dired-x)
(eval-when-compile (require 'eshell))
(declare-function eshell-parse-arguments 'eshell)
(eval-when-compile (require 'message))
(declare-function message-goto-body 'message)

(defconst fosgit--minimal-git "1.9.4")
(defconst fosgit--minimal-emacs "24.4")

;;; Options
;;;; Status Mode

(defgroup fosgit-status nil
  "Inspect and manipulate Git repositories."
  :group 'fosgit-modes)

(defcustom fosgit-status-mode-hook nil
  "Hook run after entering Fosgit-Status mode."
  :group 'fosgit-status
  :type 'hook)

(defcustom fosgit-status-headers-hook
  '(fosgit-insert-error-header
    fosgit-insert-diff-filter-header
    fosgit-insert-head-branch-header
    fosgit-insert-upstream-branch-header
    fosgit-insert-push-branch-header
    fosgit-insert-tags-header)
  "Hook run to insert headers into the status buffer.

This hook is run by `fosgit-insert-status-headers', which in turn
has to be a member of `fosgit-insert-status-sections' to be used
at all."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-status
  :type 'hook
  :options '(fosgit-insert-error-header
             fosgit-insert-diff-filter-header
             fosgit-insert-repo-header
             fosgit-insert-remote-header
             fosgit-insert-head-branch-header
             fosgit-insert-upstream-branch-header
             fosgit-insert-push-branch-header
             fosgit-insert-tags-header))

(defcustom fosgit-status-sections-hook
  '(fosgit-insert-status-headers
    fosgit-insert-merge-log
    fosgit-insert-rebase-sequence
    fosgit-insert-am-sequence
    fosgit-insert-sequencer-sequence
    fosgit-insert-bisect-output
    fosgit-insert-bisect-rest
    fosgit-insert-bisect-log
    fosgit-insert-untracked-files
    fosgit-insert-unstaged-changes
    fosgit-insert-staged-changes
    fosgit-insert-stashes
    fosgit-insert-unpulled-from-upstream
    fosgit-insert-unpulled-from-pushremote
    fosgit-insert-unpushed-to-upstream
    fosgit-insert-unpushed-to-pushremote)
  "Hook run to insert sections into a status buffer."
  :package-version '(fosgit . "2.4.0")
  :group 'fosgit-status
  :type 'hook)

(defvar fosgit-status-refresh-hook nil
  "Hook run after a status buffer has been refreshed.")

(make-obsolete-variable 'fosgit-status-refresh-hook "\
use `fosgit-pre-refresh-hook', `fosgit-post-refresh-hook',
  `fosgit-refresh-buffer-hook', or `fosgit-status-mode-hook' instead.

  If you want to run a function every time the status buffer is
  refreshed, in order to do something with that buffer, then use:

    (add-hook 'fosgit-refresh-buffer-hook
              (lambda ()
                (when (derived-mode-p 'fosgit-status-mode)
                  ...)))

  If your hook function should run regardless of whether the
  status buffer exists or not, then use `fosgit-pre-refresh-hook'
  or `fosgit-post-refresh-hook'.

  If your hook function only has to be run once, when the buffer
  is first created, then `fosgit-status-mode-hook' instead.
" "Fosgit 2.4.0")

(defcustom fosgit-status-expand-stashes t
  "Whether the list of stashes is expanded initially."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-status
  :type 'boolean)

(defcustom fosgit-status-show-hashes-in-headers nil
  "Whether headers in the status buffer show hashes.
The functions which respect this option are
`fosgit-insert-head-branch-header',
`fosgit-insert-upstream-branch-header', and
`fosgit-insert-push-branch-header'."
  :package-version '(fosgit . "2.4.0")
  :group 'fosgit-status
  :type 'boolean)

;;;; Refs Mode

(defgroup fosgit-refs nil
  "Inspect and manipulate Git branches and tags."
  :group 'fosgit-modes)

(defcustom fosgit-refs-mode-hook nil
  "Hook run after entering Fosgit-Refs mode."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-refs
  :type 'hook)

(defcustom fosgit-refs-sections-hook
  '(fosgit-insert-error-header
    fosgit-insert-branch-description
    fosgit-insert-local-branches
    fosgit-insert-remote-branches
    fosgit-insert-tags)
  "Hook run to insert sections into a references buffer."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-refs
  :type 'hook)

(defcustom fosgit-refs-show-commit-count nil
  "Whether to show commit counts in Fosgit-Refs mode buffers.

all    Show counts for branches and tags.
branch Show counts for branches only.
nil    Never show counts.

To change the value in an existing buffer use the command
`fosgit-refs-show-commit-count'"
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-refs
  :safe (lambda (val) (memq val '(all branch nil)))
  :type '(choice (const all    :tag "For branches and tags")
                 (const branch :tag "For branches only")
                 (const nil    :tag "Never")))
(put 'fosgit-refs-show-commit-count 'safe-local-variable 'symbolp)
(put 'fosgit-refs-show-commit-count 'permanent-local t)

(defcustom fosgit-refs-show-margin 'branch
  "Whether to initially show the margin in refs buffers.

When non-nil the committer name and date are initially displayed
in the margin of refs buffers.  The margin can be shown or hidden
in the current buffer using the command `fosgit-toggle-margin'."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-refs
  :safe (lambda (val) (memq val '(all branch nil)))
  :type '(choice (const all    :tag "For branches and tags")
                 (const branch :tag "For branches only")
                 (const nil    :tag "Never")))

(defcustom fosgit-visit-ref-create nil
  "Whether `fosgit-visit-ref' may create new branches.

When this is non-nil, then \"visiting\" a remote branch in a
refs buffer works by creating a new local branch which tracks
the remote branch and then checking out the new local branch."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-refs
  :group 'fosgit-commands
  :type 'boolean)

;;;; Miscellaneous

(defcustom fosgit-branch-read-upstream-first t
  "When creating a branch, read upstream before name of new branch."
  :package-version '(fosgit . "2.2.0")
  :group 'fosgit-commands
  :type 'boolean)

(defcustom fosgit-branch-prefer-remote-upstream nil
  "Whether to favor remote upstreams when creating new branches.

When a new branch is created, Fosgit offers the branch, commit, or
stash as the default starting point of the new branch.  If there
is no such thing at point, then it falls back to offer the
current branch as starting-point.  The user may then accept that
default or pick something else.

If the chosen starting-point is a branch, then it may also be set
as the upstream of the new branch, depending on the value of the
Git variable `branch.autoSetupMerge'.  By default this is done
for remote branches, but not for local branches.

You might prefer to always use some remote branch as upstream.
If the chosen starting-point is (1) a local branch, (2) whose
name is a member of the value of this option, (3) the upstream of
that local branch is a remote branch with the same name, and (4)
that remote branch can be fast-forwarded to the local branch,
then the chosen branch is used as starting-point, but its own
upstream is used as the upstream of the new branch.

Assuming the chosen branch matches these conditions you would end
up with with e.g.:

  feature --upstream--> origin/master

instead of

  feature --upstream--> master --upstream--> origin/master

Which you prefer is a matter of personal preference.  If you do
prefer the former, then you should add branches such as \"master\",
\"next\", and \"maint\" to the value of this options."
  :package-version '(fosgit . "2.4.0")
  :group 'fosgit-commands
  :type '(repeat string))

(defcustom fosgit-repository-directories nil
  "Directories containing Git repositories.
Fosgit checks these directories for Git repositories and offers
them as choices when `fosgit-status' is used with a prefix
argument."
  :group 'fosgit
  :type '(repeat string))

(defcustom fosgit-repository-directories-depth 3
  "The maximum depth to look for Git repositories.
When looking for a Git repository below the directories in
`fosgit-repository-directories', only descend this many levels
deep."
  :group 'fosgit
  :type 'integer)

;;;; Faces

(defface fosgit-header-line
  '((t :inherit fosgit-section-heading))
  "Face for the `header-line'."
  :group 'fosgit-faces)

(defface fosgit-dimmed
  '((((class color) (background light)) :foreground "grey50")
    (((class color) (background  dark)) :foreground "grey50"))
  "Face for text that shouldn't stand out."
  :group 'fosgit-faces)

(defface fosgit-hash
  '((((class color) (background light)) :foreground "grey60")
    (((class color) (background  dark)) :foreground "grey40"))
  "Face for the sha1 part of the log output."
  :group 'fosgit-faces)

(defface fosgit-tag
  '((((class color) (background light)) :foreground "Goldenrod4")
    (((class color) (background  dark)) :foreground "LightGoldenrod2"))
  "Face for tag labels shown in log buffer."
  :group 'fosgit-faces)

(defface fosgit-branch-remote
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background  dark)) :foreground "DarkSeaGreen2"))
  "Face for remote branch head labels shown in log buffer."
  :group 'fosgit-faces)

(defface fosgit-branch-local
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for local branches."
  :group 'fosgit-faces)

(defface fosgit-branch-current
  '((((class color) (background light)) :inherit fosgit-branch-local :box t)
    (((class color) (background  dark)) :inherit fosgit-branch-local :box t))
  "Face for current branch."
  :group 'fosgit-faces)

(defface fosgit-head
  '((((class color) (background light)) :inherit fosgit-branch-local)
    (((class color) (background  dark)) :inherit fosgit-branch-local))
  "Face for the symbolic ref \"HEAD\"."
  :group 'fosgit-faces)

(defface fosgit-refname
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
  "Face for refnames without a dedicated face."
  :group 'fosgit-faces)

(defface fosgit-refname-stash
  '((t :inherit fosgit-refname))
  "Face for wip refnames."
  :group 'fosgit-faces)

(defface fosgit-refname-wip
  '((t :inherit fosgit-refname))
  "Face for wip refnames."
  :group 'fosgit-faces)

(defface fosgit-signature-good
  '((t :foreground "green"))
  "Face for good signatures."
  :group 'fosgit-faces)

(defface fosgit-signature-bad
  '((t :foreground "red"))
  "Face for bad signatures."
  :group 'fosgit-faces)

(defface fosgit-signature-untrusted
  '((t :foreground "cyan"))
  "Face for good untrusted signatures."
  :group 'fosgit-faces)

(defface fosgit-cherry-unmatched
  '((t :foreground "cyan"))
  "Face for unmatched cherry commits."
  :group 'fosgit-faces)

(defface fosgit-cherry-equivalent
  '((t :foreground "magenta"))
  "Face for equivalent cherry commits."
  :group 'fosgit-faces)

(defface fosgit-filename
  '((t :weight normal))
  "Face for filenames."
  :group 'fosgit-faces)

;;; Inspect
;;;; Status Mode
;;;;; Status Core

(defvar fosgit-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map fosgit-mode-map)
    (define-key map "jz" 'fosgit-jump-to-stashes)
    (define-key map "jt" 'fosgit-jump-to-tracked)
    (define-key map "jn" 'fosgit-jump-to-untracked)
    (define-key map "ju" 'fosgit-jump-to-unstaged)
    (define-key map "js" 'fosgit-jump-to-staged)
    (define-key map "jfu" 'fosgit-jump-to-unpulled-from-upstream)
    (define-key map "jfp" 'fosgit-jump-to-unpulled-from-pushremote)
    (define-key map "jpu" 'fosgit-jump-to-unpushed-to-upstream)
    (define-key map "jpp" 'fosgit-jump-to-unpushed-to-pushremote)
    map)
  "Keymap for `fosgit-status-mode'.")

(eval-after-load 'dired-x
  '(define-key fosgit-status-mode-map [remap dired-jump] 'fosgit-dired-jump))

(define-derived-mode fosgit-status-mode fosgit-mode "Fosgit"
  "Mode for looking at Git status.

This mode is documented in info node `(fosgit)Status buffer'.

\\<fosgit-mode-map>\
Type \\[fosgit-refresh] to refresh the current buffer.
Type \\[fosgit-section-toggle] to expand or hide the section at point.
Type \\[fosgit-visit-thing] to visit the change or commit at point.

Type \\[fosgit-dispatch-popup] to see available prefix popups.

Staging and applying changes is documented in info node
`(fosgit)Staging and unstaging' and info node `(fosgit)Applying'.

\\<fosgit-hunk-section-map>Type \
\\[fosgit-apply] to apply the change at point, \
\\[fosgit-stage] to stage,
\\[fosgit-unstage] to unstage, \
\\[fosgit-discard] to discard, or \
\\[fosgit-reverse] to reverse it.

\\<fosgit-status-mode-map>\
Type \\[fosgit-commit-popup] to create a commit.

\\{fosgit-status-mode-map}"
  :group 'fosgit-status
  (hack-dir-local-variables-non-file-buffer))

;;;###autoload
(defun fosgit-status (&optional directory)
  "Show the status of the current Git repository in a buffer.
With a prefix argument prompt for a repository to be shown.
With two prefix arguments prompt for an arbitrary directory.
If that directory isn't the root of an existing repository
then offer to initialize it as a new repository."
  (interactive
   (list (and (or current-prefix-arg (not (fosgit-toplevel)))
              (fosgit-read-repository
               (>= (prefix-numeric-value current-prefix-arg) 16)))))
  (if directory
      (let ((toplevel (fosgit-toplevel directory)))
        (setq directory (file-name-as-directory (expand-file-name directory)))
        (if (and toplevel (string-equal directory toplevel))
            (fosgit-status-internal directory)
          (when (y-or-n-p
                 (if toplevel
                     (format "%s is a repository.  Create another in %s? "
                             toplevel directory)
                   (format "Create repository in %s? " directory)))
            (fosgit-init directory))))
    (fosgit-status-internal default-directory)))

(put 'fosgit-status 'interactive-only 'fosgit-status-internal)

;;;###autoload
(defun fosgit-status-internal (directory)
  (fosgit-tramp-asserts directory)
  (let ((default-directory directory))
    (fosgit-mode-setup #'fosgit-status-mode)))

;;;;; Standard Status Sections

(defvar fosgit-status-sections-hook-1 nil)

(defun fosgit-status-refresh-buffer ()
  (fosgit-git-exit-code "update-index" "--refresh")
  (fosgit-insert-section (status)
    (if (-all-p #'functionp fosgit-status-sections-hook)
        (run-hooks 'fosgit-status-sections-hook)
      (message "`fosgit-status-sections-hook' contains entries that are \
no longer valid.\nUsing standard value instead.  Please re-configure")
      (sit-for 5)
      (let ((fosgit-status-sections-hook-1
             (eval (car (get 'fosgit-status-sections-hook 'standard-value)))))
        (run-hooks 'fosgit-status-sections-hook-1))))
  (run-hooks 'fosgit-status-refresh-hook))

(defun fosgit-insert-status-headers ()
  "Insert header sections appropriate for `fosgit-status-mode' buffers.
The sections are inserted by running the functions on the hook
`fosgit-status-headers-hook'."
  (if (fosgit-rev-verify "HEAD")
      (fosgit-insert-headers fosgit-status-headers-hook)
    (insert "In the beginning there was darkness\n\n")))

(defun fosgit-insert-error-header ()
  "Insert the message about the Git error that just occured.

This function is only aware of the last error that occur when Git
was run for side-effects.  If, for example, an error occurs while
generating a diff, then that error won't be inserted.  Refreshing
the status buffer causes this section to disappear again."
  (when fosgit-this-error
    (fosgit-insert-section (error 'git)
      (insert (propertize (format "%-10s" "GitError! ")
                          'face 'fosgit-section-heading))
      (insert (propertize fosgit-this-error 'face 'font-lock-warning-face))
      (-when-let (key (car (where-is-internal 'fosgit-process-buffer)))
        (insert (format "  [Type `%s' for details]" (key-description key))))
      (insert ?\n))
    (setq fosgit-this-error nil)))

(cl-defun fosgit-insert-head-branch-header
    (&optional (branch (fosgit-get-current-branch)))
  "Insert a header line about BRANCH.
When BRANCH is nil, use the current branch or, if none, the
detached `HEAD'."
  (let ((output (fosgit-rev-format "%h %s" (or branch "HEAD"))))
    (string-match "^\\([^ ]+\\) \\(.*\\)" output)
    (fosgit-bind-match-strings (commit summary) output
      (if branch
          (fosgit-insert-section (branch branch)
            (insert (format "%-10s" "Head: "))
            (when fosgit-status-show-hashes-in-headers
              (insert (propertize commit 'face 'fosgit-hash) ?\s))
            (insert (propertize branch 'face 'fosgit-branch-local))
            (insert ?\s summary ?\n))
        (fosgit-insert-section (commit commit)
          (insert (format "%-10s" "Head: "))
          (insert (propertize commit 'face 'fosgit-hash))
          (insert ?\s summary ?\n))))))

(cl-defun fosgit-insert-upstream-branch-header
    (&optional (branch (fosgit-get-current-branch))
               (pull   (fosgit-get-upstream-branch branch))
               keyword)
  "Insert a header line about branch usually pulled into current branch."
  (when pull
    (fosgit-insert-section (branch pull)
      (insert (format "%-10s"
                      (or keyword
                          (if (fosgit-get-boolean "branch" branch "rebase")
                              "Rebase: "
                            "Merge: "))))
      (--when-let (and fosgit-status-show-hashes-in-headers
                       (fosgit-rev-format "%h" pull))
        (insert (propertize it 'face 'fosgit-hash) ?\s))
      (insert (propertize pull 'face
                          (if (string= (fosgit-get "branch" branch "remote") ".")
                              'fosgit-branch-local
                            'fosgit-branch-remote)))
      (insert ?\s)
      (if (fosgit-rev-verify pull)
          (insert (or (fosgit-rev-format "%s" pull) ""))
        (insert (propertize "is missing" 'face 'font-lock-warning-face)))
      (insert ?\n))))

(cl-defun fosgit-insert-push-branch-header
    (&optional (branch (fosgit-get-current-branch))
               (push   (fosgit-get-push-branch branch)))
  "Insert a header line about the branch the current branch is pushed to."
  (when push
    (fosgit-insert-section (branch push)
      (insert (format "%-10s" "Push: "))
      (--when-let (and fosgit-status-show-hashes-in-headers
                       (fosgit-rev-format "%h" push))
        (insert (propertize it 'face 'fosgit-hash) ?\s))
      (insert (propertize push 'face 'fosgit-branch-remote) ?\s)
      (if (fosgit-rev-verify push)
          (insert (or (fosgit-rev-format "%s" push) ""))
        (insert (propertize "is missing" 'face 'font-lock-warning-face)))
      (insert ?\n))))

(defun fosgit-insert-tags-header ()
  "Insert a header line about the current and/or next tag."
  (let* ((this-tag (fosgit-get-current-tag nil t))
         (next-tag (fosgit-get-next-tag nil t))
         (this-cnt (cadr this-tag))
         (next-cnt (cadr next-tag))
         (this-tag (car this-tag))
         (next-tag (car next-tag))
         (both-tags (and this-tag next-tag t)))
    (when (or this-tag next-tag)
      (fosgit-insert-section (tag (or this-tag next-tag))
        (insert (format "%-10s" (if both-tags "Tags: " "Tag: ")))
        (when this-tag
          (insert (fosgit-format-status-tag-sentence this-tag this-cnt nil)))
        (when both-tags
          (insert ", "))
        (when next-tag
          (insert (fosgit-format-status-tag-sentence next-tag next-cnt t)))
        (insert ?\n)))))

(defun fosgit-format-status-tag-sentence (tag count next)
  (concat (propertize tag 'face 'fosgit-tag)
          (and (> count 0)
               (format " (%s)"
                       (propertize (format "%s" count) 'face
                                   (if next 'fosgit-tag 'fosgit-branch-local))))))

(defun fosgit-insert-diff-filter-header ()
  "Insert a header line showing the effective diff filters."
  (when fosgit-diff-section-file-args
    (fosgit-insert-section (filter 'diff)
      (insert (propertize (format "%-10s" "Filter! ")
                          'face 'fosgit-section-heading))
      (insert (mapconcat #'identity fosgit-diff-section-file-args " "))
      (insert ?\n))))

(fosgit-define-section-jumper fosgit-jump-to-untracked "Untracked files" untracked)

(defvar fosgit-untracked-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap fosgit-delete-thing] 'fosgit-discard)
    (define-key map "s" 'fosgit-stage)
    map)
  "Keymap for the `untracked' section.")

(defun fosgit-insert-untracked-files ()
  "Maybe insert a list or tree of untracked files.
Do so depending on the value of `status.showUntrackedFiles'."
  (let ((show (or (fosgit-get "status.showUntrackedFiles") "normal")))
    (unless (equal show "no")
      (if (equal show "all")
          (-when-let (files (fosgit-untracked-files))
            (fosgit-insert-section (untracked)
              (fosgit-insert-heading "Untracked files:")
              (fosgit-insert-un/tracked-files-1 files nil)
              (insert ?\n)))
        (-when-let
            (files (--mapcat (and (eq (aref it 0) ??)
                                  (list (substring it 3)))
                             (fosgit-git-items "status" "-z" "--porcelain")))
          (fosgit-insert-section (untracked)
            (fosgit-insert-heading "Untracked files:")
            (dolist (file files)
              (fosgit-insert-section (file file)
                (insert (propertize file 'face 'fosgit-filename) ?\n))))
          (insert ?\n))))))

(defun fosgit-insert-un/tracked-files-1 (files directory)
  (while (and files (string-prefix-p (or directory "") (car files)))
    (let ((dir (file-name-directory (car files))))
      (if (equal dir directory)
          (let ((file (pop files)))
            (fosgit-insert-section (file file)
              (insert (propertize file 'face 'fosgit-filename) ?\n)))
        (fosgit-insert-section (file dir t)
          (insert (propertize dir 'file 'fosgit-filename) ?\n)
          (fosgit-insert-heading)
          (setq files (fosgit-insert-un/tracked-files-1 files dir))))))
  files)

;;;;; Auxiliary Status Sections

(fosgit-define-section-jumper fosgit-jump-to-tracked "Tracked files" tracked)

(defun fosgit-insert-tracked-files ()
  "Insert a tree of tracked files."
  (-when-let (files (fosgit-list-files))
    (fosgit-insert-section (tracked nil t)
      (fosgit-insert-heading "Tracked files:")
      (fosgit-insert-un/tracked-files-1 files nil)
      (insert ?\n))))

(defun fosgit-insert-user-header ()
  "Insert a header line about the current user."
  (let ((name  (fosgit-get "user.name"))
        (email (fosgit-get "user.email")))
    (when (and name email)
      (fosgit-insert-section (user name)
        (insert (format "%-10s" "User: "))
        (insert (propertize name 'face 'fosgit-log-author))
        (insert " <" email ">\n")))))

(defun fosgit-insert-repo-header ()
  "Insert a header line showing the path to the repository top-level."
  (let ((topdir (fosgit-toplevel)))
    (fosgit-insert-section (repo topdir)
      (insert (format "%-10s%s\n" "Repo: " (abbreviate-file-name topdir))))))

(defun fosgit-insert-remote-header ()
  "Insert a header line about the remote of the current branch.

If no remote is configured for the current branch, then fall back
showing the \"origin\" remote, or if that does not exist the first
remote in alphabetic order."
  (--when-let (or (fosgit-get-remote)
                  (let ((remotes (fosgit-list-remotes)))
                    (or (car (member "origin" remotes))
                        (car remotes))))
    (fosgit-insert-section (remote it)
      (insert (format "%-10s" "Remote: "))
      (insert (propertize it 'face 'fosgit-branch-remote) ?\s)
      (insert (fosgit-get "remote" it "url") ?\n))))

;;;;; Status Miscellaneous

(defun ido-enter-fosgit-status ()
  "Drop into `fosgit-status' from file switching.

To make this command available use something like:

  (add-hook \\='ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd \"C-x g\") \\='ido-enter-fosgit-status)))

Starting with Emacs 25.1 the Ido keymaps are defined just once
instead of every time Ido is invoked, so now you can modify it
like pretty much every other keymap:

  (define-key ido-common-completion-map
    (kbd \"C-x g\") 'ido-enter-fosgit-status)"
  (interactive)
  (with-no-warnings ; FIXME these are internal variables
    (setq ido-exit 'fallback fallback 'fosgit-status))
  (exit-minibuffer))

(defun fosgit-status-maybe-update-revision-buffer (&optional _)
  "When moving in the status buffer, update the revision buffer.
If there is no revision buffer in the same frame, then do nothing."
  (when (derived-mode-p 'fosgit-status-mode)
    (fosgit-log-maybe-update-revision-buffer-1)))

(defun fosgit-status-maybe-update-blob-buffer (&optional _)
  "When moving in the status buffer, update the blob buffer.
If there is no blob buffer in the same frame, then do nothing."
  (when (derived-mode-p 'fosgit-status-mode)
    (fosgit-log-maybe-update-blob-buffer-1)))

;;;; Refs Mode

(defvar fosgit-refs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map fosgit-mode-map)
    (define-key map "\C-y" 'fosgit-refs-set-show-commit-count)
    map)
  "Keymap for `fosgit-refs-mode'.")

(define-derived-mode fosgit-refs-mode fosgit-mode "Fosgit Refs"
  "Mode which lists and compares references.

This mode is documented in info node `(fosgit)References buffer'.

\\<fosgit-mode-map>\
Type \\[fosgit-refresh] to refresh the current buffer.
Type \\[fosgit-section-toggle] to expand or hide the section at point.
Type \\[fosgit-visit-thing] or \\[fosgit-diff-show-or-scroll-up] \
to visit the commit or branch at point.

Type \\[fosgit-branch-popup] to see available branch commands.
Type \\[fosgit-merge-popup] to merge the branch or commit at point.
Type \\[fosgit-cherry-pick-popup] to apply the commit at point.
Type \\[fosgit-reset] to reset HEAD to the commit at point.

\\{fosgit-refs-mode-map}"
  :group 'fosgit-refs
  (hack-dir-local-variables-non-file-buffer))

;;;###autoload (autoload 'fosgit-show-refs-popup "fosgit" nil t)
(fosgit-define-popup fosgit-show-refs-popup
  "Popup console for `fosgit-show-refs'."
  'fosgit-refs
  :man-page "git-branch"
  :switches '((?m "Merged to HEAD"            "--merged")
              (?M "Merged to master"          "--merged=master")
              (?n "Not merged to HEAD"        "--no-merged")
              (?N "Not merged to master"      "--no-merged=master"))
  :options  '((?c "Contains"   "--contains="  fosgit-read-branch-or-commit)
              (?m "Merged"     "--merged="    fosgit-read-branch-or-commit)
              (?n "Not merged" "--no-merged=" fosgit-read-branch-or-commit))
  :actions  '((?y "Show refs, comparing them with HEAD"
                  fosgit-show-refs-head)
              (?c "Show refs, comparing them with current branch"
                  fosgit-show-refs-current)
              (?o "Show refs, comparing them with other branch"
                  fosgit-show-refs))
  :default-action 'fosgit-show-refs-head
  :use-prefix 'popup)

;;;###autoload
(defun fosgit-show-refs-head (&optional args)
  "List and compare references in a dedicated buffer.
Refs are compared with `HEAD'."
  (interactive (list (fosgit-show-refs-arguments)))
  (fosgit-show-refs nil args))

;;;###autoload
(defun fosgit-show-refs-current (&optional args)
  "List and compare references in a dedicated buffer.
Refs are compared with the current branch or `HEAD' if
it is detached."
  (interactive (list (fosgit-show-refs-arguments)))
  (fosgit-show-refs (fosgit-get-current-branch) args))

;;;###autoload
(defun fosgit-show-refs (&optional ref args)
  "List and compare references in a dedicated buffer.
Refs are compared with a branch read form the user."
  (interactive (list (fosgit-read-other-branch "Compare with")
                     (fosgit-show-refs-arguments)))
  (fosgit-mode-setup #'fosgit-refs-mode ref args))

(defun fosgit-refs-refresh-buffer (&rest _ignore)
  (setq fosgit-set-buffer-margin-refresh (not fosgit-show-margin))
  (unless (fosgit-rev-verify (or (car fosgit-refresh-args) "HEAD"))
    (setq fosgit-refs-show-commit-count nil))
  (fosgit-insert-section (branchbuf)
    (run-hooks 'fosgit-refs-sections-hook)))

(defun fosgit-insert-branch-description ()
  "Insert header containing the description of the current branch.
Insert a header line with the name and description of the
current branch.  The description is taken from the Git variable
`branch.<NAME>.description'; if that is undefined then no header
line is inserted at all."
  (let ((branch (fosgit-get-current-branch)))
    (--when-let (fosgit-git-lines
                 "config" (format "branch.%s.description" branch))
      (fosgit-insert-section (branchdesc branch t)
        (fosgit-insert-heading branch ": " (car it))
        (insert (mapconcat 'identity (cdr it) "\n"))
        (insert "\n\n")))))

(defconst fosgit-refs-branch-line-re
  (concat "^"
          "\\(?:[ \\*]\\) "
          "\\(?1:([^)]+)\\|[^ ]+?\\)"       ; branch
          "\\(?: +\\)"
          "\\(?2:[0-9a-fA-F]+\\) "          ; sha1
          "\\(?:\\["
          "\\(?4:[^:]+\\)"                  ; upstream
          "\\(?:: \\(?:"
          "\\(?7:gone\\)\\|"                ; gone
          "\\(?:ahead \\(?5:[0-9]+\\)\\)?"  ; ahead
          "\\(?:, \\)?"
          "\\(?:behind \\(?6:[0-9]+\\)\\)?" ; behind
          "\\)\\)?"
          "\\] \\)?"
          "\\(?3:.*\\)"))                   ; message

(defvar fosgit-refs-local-branch-format "%4c %-25n %U%m\n"
  "Format used for local branches in refs buffers.")
(defvar fosgit-refs-remote-branch-format "%4c %-25n %m\n"
  "Format used for remote branches in refs buffers.")
(defvar fosgit-refs-tags-format "%4c %-25n %m\n"
  "Format used for tags in refs buffers.")
(defvar fosgit-refs-indent-cherry-lines 3
  "Indentation of cherries in refs buffers.")

(defvar fosgit-branch-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap fosgit-visit-thing]  'fosgit-visit-ref)
    (define-key map [remap fosgit-delete-thing] 'fosgit-branch-delete)
    (define-key map "R" 'fosgit-branch-rename)
    map)
  "Keymap for `branch' sections.")

(defvar fosgit-remote-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap fosgit-delete-thing] 'fosgit-remote-remove)
    (define-key map "R" 'fosgit-remote-rename)
    map)
  "Keymap for `remote' sections.")

(defun fosgit-refs-set-show-commit-count ()
  "Change for which refs the commit count is shown."
  (interactive)
  (setq-local fosgit-refs-show-commit-count
              (fosgit-read-char-case "Show commit counts for " nil
                (?a "[a]ll refs" 'all)
                (?b "[b]ranches only" t)
                (?n "[n]othing" nil)))
  (fosgit-refresh))

(defun fosgit-visit-ref ()
  "Visit the reference or revision at point.

In most places use `fosgit-show-commit' to visit the reference or
revision at point.

In `fosgit-refs-mode', when there is a reference at point, instead
checkout that reference.  When option `fosgit-visit-ref-create' is
non-nil and point is on remote branch, then create a local branch
with the same name and check it out.

With a prefix argument only focus on the reference at point, i.e.
the commit counts and cherries are updated to be relative to that
reference, but it is not checked out."
  (interactive)
  (if (derived-mode-p 'fosgit-refs-mode)
      (fosgit-section-case
        (([branch * branchbuf]
          [tag    * branchbuf])
         (let ((ref (fosgit-section-value (fosgit-current-section))))
           (if current-prefix-arg
               (fosgit-show-refs ref)
             (if (fosgit-section-when [branch remote])
                 (let ((start ref)
                       (arg "-b"))
                   (string-match "^[^/]+/\\(.+\\)" ref)
                   (setq ref (match-string 1 ref))
                   (when (fosgit-branch-p ref)
                     (if (yes-or-no-p
                          (format "Branch %s already exists.  Recreate it?" ref))
                         (setq arg "-B")
                       (user-error "Abort")))
                   (fosgit-run-git "checkout" arg ref start))
               (fosgit-run-git "checkout" ref))
             (setcar fosgit-refresh-args ref)
             (fosgit-refresh))))
        ([commit * branchbuf]
         (call-interactively #'fosgit-show-commit)))
    (call-interactively #'fosgit-show-commit)))

(defun fosgit-insert-local-branches ()
  "Insert sections showing all local branches."
  (fosgit-insert-section (local nil)
    (fosgit-insert-heading "Branches:")
    (let ((current  (fosgit-get-current-branch))
          (branches (fosgit-list-local-branch-names)))
      (dolist (line (fosgit-git-lines "branch" "-vv"
                                     (cadr fosgit-refresh-args)))
        (string-match fosgit-refs-branch-line-re line)
        (fosgit-bind-match-strings
            (branch hash message upstream ahead behind gone) line
          (when (string-match-p "(HEAD detached" branch)
            (setq branch nil))
          (fosgit-insert-branch
           branch fosgit-refs-local-branch-format current branches
           'fosgit-branch-local hash message upstream ahead behind gone))))
    (insert ?\n)))

(defun fosgit-insert-remote-branches ()
  "Insert sections showing all remote-tracking branches."
  (dolist (remote (fosgit-list-remotes))
    (fosgit-insert-section (remote remote)
      (fosgit-insert-heading
        (let ((pull (fosgit-get "remote" remote "url"))
              (push (fosgit-get "remote" remote "pushurl")))
          (format "%s (%s):" (capitalize remote)
                  (concat pull (and pull push ", ") push))))
      (let ((current  (fosgit-get-current-branch))
            (branches (fosgit-list-local-branch-names)))
        (dolist (line (fosgit-git-lines "branch" "-vvr"
                                       (cadr fosgit-refresh-args)))
          (when (string-match fosgit-refs-branch-line-re line)
            (fosgit-bind-match-strings (branch hash message) line
              (when (string-match-p (format "^%s/" remote) branch)
                (fosgit-insert-branch
                 branch fosgit-refs-remote-branch-format current branches
                 'fosgit-branch-remote hash message))))))
      (insert ?\n))))

(defun fosgit-insert-branch (branch format &rest args)
  "For internal use, don't add to a hook."
  (unless fosgit-refs-show-commit-count
    (setq format (replace-regexp-in-string "%[0-9]\\([cC]\\)" "%1\\1" format t)))
  (if (equal branch "HEAD")
      (fosgit-insert-section it (commit (fosgit-rev-parse "HEAD") t)
        (apply #'fosgit-insert-branch-1 it nil format args))
    (fosgit-insert-section it (branch branch t)
      (apply #'fosgit-insert-branch-1 it branch format args))))

(defun fosgit-insert-branch-1
    (section branch format current branches face
             &optional hash message upstream ahead behind gone)
  "For internal use, don't add to a hook."
  (let* ((head  (or (car fosgit-refresh-args) current "HEAD"))
         (count (and branch
                     (fosgit-refs-format-commit-count branch head format)))
         (mark  (cond ((or (equal branch head)
                           (and (not branch) (equal head "HEAD")))
                       (if (equal branch current)
                           (propertize "@" 'face 'fosgit-head)
                         (propertize "#" 'face 'fosgit-tag)))
                      ((equal branch current)
                       (propertize "." 'face 'fosgit-head)))))
    (when upstream
      (setq upstream (propertize upstream 'face
                                 (if (member upstream branches)
                                     'fosgit-branch-local
                                   'fosgit-branch-remote))))
    (fosgit-insert-heading
      (format-spec
       format
       `((?a . ,(or ahead ""))
         (?b . ,(or behind ""))
         (?c . ,(or mark count ""))
         (?C . ,(or mark " "))
         (?h . ,(or (propertize hash 'face 'fosgit-hash) ""))
         (?m . ,(or message ""))
         (?n . ,(propertize (or branch "(detached)") 'face face))
         (?u . ,(or upstream ""))
         (?U . ,(if upstream
                    (format (propertize "[%s%s] " 'face 'fosgit-dimmed)
                            upstream
                            (cond
                             (gone
                              (concat ": " (propertize gone 'face 'error)))
                             ((or ahead behind)
                              (concat ": "
                                      (and ahead (format "ahead %s" ahead))
                                      (and ahead behind ", ")
                                      (and behind (format "behind %s" behind))))
                             (t "")))
                  "")))))
    (when fosgit-show-margin
      (fosgit-refs-format-margin branch))
    (fosgit-refs-insert-cherry-commits head branch section)))

(defvar fosgit-tag-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap fosgit-visit-thing]  'fosgit-visit-ref)
    (define-key map [remap fosgit-delete-thing] 'fosgit-tag-delete)
    map)
  "Keymap for `tag' sections.")

(defun fosgit-insert-tags ()
  "Insert sections showing all tags."
  (-when-let (tags (fosgit-git-lines "tag" "-l" "-n"))
    (fosgit-insert-section (tags)
      (fosgit-insert-heading "Tags:")
      (let ((head (or (car fosgit-refresh-args)
                      (fosgit-get-current-branch)
                      "HEAD"))
            (format (if fosgit-refs-show-commit-count
                        fosgit-refs-tags-format
                      (replace-regexp-in-string
                       "%[0-9]\\([cC]\\)" "%1\\1" fosgit-refs-tags-format t))))
        (dolist (tag (nreverse tags))
          (string-match "^\\([^ \t]+\\)[ \t]+\\([^ \t\n].*\\)?" tag)
          (let* ((message (match-string 2 tag))
                 (tag     (match-string 1 tag))
                 (count   (fosgit-refs-format-commit-count tag head format t))
                 (mark    (and (equal tag head)
                               (propertize "#" 'face 'fosgit-tag))))
            (fosgit-insert-section section (tag tag t)
              (fosgit-insert-heading
               (format-spec format
                            `((?n . ,(propertize tag 'face 'fosgit-tag))
                              (?c . ,(or mark count ""))
                              (?m . ,(or message "")))))
              (when (and fosgit-show-margin
                         (eq fosgit-refs-show-margin 'all))
                (fosgit-refs-format-margin (concat tag "^{commit}")))
              (fosgit-refs-insert-cherry-commits head tag section)))))
      (insert ?\n))))

(defun fosgit-refs-insert-cherry-commits (head ref section)
  (if (fosgit-section-hidden section)
      (setf (fosgit-section-washer section)
            (apply-partially #'fosgit-refs-insert-cherry-commits-1
                             head ref section))
    (fosgit-refs-insert-cherry-commits-1 head ref section)))

(defun fosgit-refs-insert-cherry-commits-1 (head ref section)
  (let ((start (point)))
    (fosgit-git-wash (apply-partially 'fosgit-log-wash-log 'cherry)
      "cherry" "-v" "--abbrev" head ref fosgit-refresh-args)
    (unless (= (point) start)
      (insert (propertize "\n" 'fosgit-section section)))))

(defun fosgit-refs-format-commit-count (ref head format &optional tag-p)
  (and (string-match-p "%-?[0-9]+c" format)
       (if tag-p
           (eq fosgit-refs-show-commit-count 'all)
         fosgit-refs-show-commit-count)
       (let ((count (cadr (fosgit-rev-diff-count head ref))))
         (and (> count 0)
              (propertize (number-to-string count) 'face 'fosgit-dimmed)))))

(defun fosgit-refs-format-margin (commit)
  (save-excursion
    (goto-char (line-beginning-position 0))
    (let ((line (fosgit-rev-format "%ct%cN" commit)))
      (fosgit-format-log-margin (substring line 10)
                               (substring line 0 10)))))

;;;; Files

;;;###autoload
(defun fosgit-find-file (rev file)
  "View FILE from REV.
Switch to a buffer visiting blob REV:FILE,
creating one if none already exists."
  (interactive (fosgit-find-file-read-args "Find file"))
  (switch-to-buffer (fosgit-find-file-noselect rev file)))

;;;###autoload
(defun fosgit-find-file-other-window (rev file)
  "View FILE from REV, in another window.
Like `fosgit-find-file', but create a new window or reuse an
existing one."
  (interactive (fosgit-find-file-read-args "Find file in other window"))
  (switch-to-buffer-other-window (fosgit-find-file-noselect rev file)))

(defun fosgit-find-file-read-args (prompt)
  (let  ((rev (fosgit-read-branch-or-commit "Find file from revision")))
    (list rev (fosgit-read-file-from-rev rev prompt))))

(defvar fosgit-read-file-hist nil)

(defun fosgit-read-file-from-rev (rev prompt &optional default)
  (let ((files (fosgit-revision-files rev)))
    (fosgit-completing-read
     prompt files nil t nil 'fosgit-read-file-hist
     (car (member (or default (fosgit-current-file)) files)))))

(defun fosgit-read-changed-file (rev-or-range prompt &optional default)
  (fosgit-read-file-choice
   prompt
   (fosgit-changed-files rev-or-range)
   default
   (concat "No file changed in " rev-or-range)))

(defun fosgit-get-revision-buffer (rev file &optional create)
  (funcall (if create 'get-buffer-create 'get-buffer)
           (format "%s.~%s~" file (subst-char-in-string ?/ ?_ rev))))

(defun fosgit-get-revision-buffer-create (rev file)
  (fosgit-get-revision-buffer rev file t))

(defvar fosgit-find-file-hook nil)

(defun fosgit-find-file-noselect (rev file)
  "Read FILE from REV into a buffer and return the buffer.
FILE must be relative to the top directory of the repository."
  (let ((topdir (fosgit-toplevel)))
    (when (file-name-absolute-p file)
      (setq file (file-relative-name file topdir)))
    (or (fosgit-get-revision-buffer rev file)
        (with-current-buffer (fosgit-get-revision-buffer-create rev file)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (fosgit-git-insert "cat-file" "-p" (concat rev ":" file)))
          (setq fosgit-buffer-revision  (fosgit-rev-format "%H" rev)
                fosgit-buffer-refname   rev
                fosgit-buffer-file-name (expand-file-name file topdir))
          (let ((buffer-file-name fosgit-buffer-file-name))
            (normal-mode t))
          (setq buffer-read-only t)
          (set-buffer-modified-p nil)
          (goto-char (point-min))
          (fosgit-blob-mode 1)
          (run-hooks 'fosgit-find-file-hook)
          (current-buffer)))))

(defvar fosgit-find-index-hook nil)

(defun fosgit-find-file-index-noselect (file &optional revert)
  "Read FILE from the index into a buffer and return the buffer.
FILE must to be relative to the top directory of the repository."
  (let* ((bufname (concat file ".~{index}~"))
         (origbuf (get-buffer bufname))
         (default-directory (fosgit-toplevel)))
    (with-current-buffer (get-buffer-create bufname)
      (when (or (not origbuf) revert
                (y-or-n-p (format "%s already exists; revert it? " bufname)))
        (let ((inhibit-read-only t)
              (temp (car (split-string
                          (or (fosgit-git-string "checkout-index" "--temp" file)
                              (error "Error making temp file"))
                          "\t"))))
          (erase-buffer)
          (insert-file-contents temp nil nil nil t)
          (delete-file temp)))
      (setq fosgit-buffer-revision  "{index}"
            fosgit-buffer-refname   "{index}"
            fosgit-buffer-file-name (expand-file-name file))
      (let ((buffer-file-name fosgit-buffer-file-name))
        (normal-mode t))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (run-hooks 'fosgit-find-index-hook)
      (current-buffer))))

(defun fosgit-update-index ()
  "Update the index with the contents of the current buffer.
The current buffer has to be visiting a file in the index, which
is done using `fosgit-find-index-noselect'."
  (interactive)
  (let ((file (fosgit-file-relative-name)))
    (unless (equal fosgit-buffer-refname "{index}")
      (user-error "%s isn't visiting the index" file))
    (if (y-or-n-p (format "Update index with contents of %s" (buffer-name)))
        (let ((index (make-temp-file "index"))
              (buffer (current-buffer)))
          (when fosgit-wip-before-change-mode
            (fosgit-wip-commit-before-change (list file) " before un-/stage"))
          (with-temp-file index
            (insert-buffer-substring buffer))
          (fosgit-call-git "update-index" "--cacheinfo"
                          (substring (fosgit-git-string "ls-files" "-s" file) 0 6)
                          (fosgit-git-string "hash-object" "-t" "blob" "-w"
                                            (concat "--path=" file)
                                            "--" index)
                          file)
          (set-buffer-modified-p nil)
          (when fosgit-wip-after-apply-mode
            (fosgit-wip-commit-after-apply (list file) " after un-/stage")))
      (message "Abort")))
  (--when-let (fosgit-mode-get-buffer 'fosgit-status-mode)
    (with-current-buffer it (fosgit-refresh)))
  t)

;;;###autoload
(defun fosgit-dired-jump (&optional other-window)
  "Visit file at point using Dired.
With a prefix argument, visit in other window.  If there
is no file at point then instead visit `default-directory'."
  (interactive "P")
  (dired-jump other-window (--if-let (fosgit-file-at-point)
                               (expand-file-name it)
                             default-directory)))

;;;###autoload
(defun fosgit-checkout-file (rev file)
  "Checkout FILE from REV."
  (interactive
   (let ((rev (fosgit-read-branch-or-commit
               "Checkout from revision" fosgit-buffer-revision)))
     (list rev (fosgit-read-file-from-rev rev "Checkout file"))))
  (fosgit-with-toplevel
    (fosgit-run-git "checkout" rev "--" file)))

;;; Manipulate
;;;; Init

;;;###autoload
(defun fosgit-init (directory)
  "Initialize a Git repository, then show its status.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally."
  (interactive
   (let ((directory (file-name-as-directory
                     (expand-file-name
                      (read-directory-name "Create repository in: ")))))
     (-when-let (toplevel (fosgit-toplevel directory))
       (setq toplevel (expand-file-name toplevel))
       (unless (y-or-n-p (if (string-equal toplevel directory)
                             (format "Reinitialize existing repository %s? "
                                     directory)
                           (format "%s is a repository.  Create another in %s? "
                                   toplevel directory)))
         (user-error "Abort")))
     (list directory)))
  ;; `git init' does not understand the meaning of "~"!
  (fosgit-call-git "init" (fosgit-convert-git-filename
                          (expand-file-name directory)))
  (fosgit-status-internal directory))

;;;; Branch
;;;;; Branch Popup

;;;###autoload (autoload 'fosgit-branch-popup "fosgit" nil t)
(fosgit-define-popup fosgit-branch-popup
  "Popup console for branch commands."
  'fosgit-commands
  :man-page "git-branch"
  :variables '("Configure existing branches"
               (?d "branch.%s.description"
                   fosgit-edit-branch*description
                   fosgit-format-branch*description)
               (?u "branch.%s.merge"
                   fosgit-set-branch*merge/remote
                   fosgit-format-branch*merge/remote)
               (?r "branch.%s.rebase"
                   fosgit-cycle-branch*rebase
                   fosgit-format-branch*rebase)
               (?p "branch.%s.pushRemote"
                   fosgit-cycle-branch*pushRemote
                   fosgit-format-branch*pushRemote)
               "Configure repository defaults"
               (?\M-r "pull.rebase"
                      fosgit-cycle-pull.rebase
                      fosgit-format-pull.rebase)
               (?\M-p "remote.pushDefault"
                      fosgit-cycle-remote.pushDefault
                      fosgit-format-remote.pushDefault)
               "Configure branch creation"
               (?U "branch.autoSetupMerge"
                   fosgit-cycle-branch*autoSetupMerge
                   fosgit-format-branch*autoSetupMerge)
               (?R "branch.autoSetupRebase"
                   fosgit-cycle-branch*autoSetupRebase
                   fosgit-format-branch*autoSetupRebase))
  :actions '((?c "Create and checkout" fosgit-branch-and-checkout)
             (?b "Checkout"            fosgit-checkout)
             (?n "Create"              fosgit-branch)
             (?m "Rename"              fosgit-branch-rename)
             (?s "Create spin-off"     fosgit-branch-spinoff)
             (?x "Reset"               fosgit-branch-reset) nil
             (?k "Delete"              fosgit-branch-delete))
  :default-action 'fosgit-checkout
  :max-action-columns 2
  :setup-function 'fosgit-branch-popup-setup)

(defun fosgit-branch-popup-setup (val def)
  (fosgit-popup-default-setup val def)
  (use-local-map (copy-keymap fosgit-popup-mode-map))
  (dolist (ev (-filter #'fosgit-popup-event-p (fosgit-popup-get :variables)))
    (local-set-key (vector (fosgit-popup-event-key ev))
                   'fosgit-invoke-popup-action)))

;;;;; Branch Actions

;;;###autoload
(defun fosgit-checkout (revision)
  "Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch then that becomes the current
branch.  If it is something else then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.
\n(git checkout REVISION)."
  (interactive (list (fosgit-read-other-branch-or-commit "Checkout")))
  (fosgit-run-git "checkout" revision))

;;;###autoload
(defun fosgit-branch (branch start-point &optional args)
  "Create BRANCH at branch or revision START-POINT.
\n(git branch [ARGS] BRANCH START-POINT)."
  (interactive (fosgit-branch-read-args "Create branch"))
  (fosgit-call-git "branch" args branch start-point)
  (--when-let (and (fosgit-get-upstream-branch branch)
                   (fosgit-get-indirect-upstream-branch start-point))
    (fosgit-call-git "branch" (concat "--set-upstream-to=" it) branch))
  (fosgit-refresh))

;;;###autoload
(defun fosgit-branch-and-checkout (branch start-point &optional args)
  "Create and checkout BRANCH at branch or revision START-POINT.
\n(git checkout [ARGS] -b BRANCH START-POINT)."
  (interactive (fosgit-branch-read-args "Create and checkout branch"))
  (if (string-match-p "^stash@{[0-9]+}$" start-point)
      (fosgit-run-git "stash" "branch" branch start-point)
    (fosgit-call-git "checkout" args "-b" branch start-point)
    (--when-let (and (fosgit-get-upstream-branch branch)
                     (fosgit-get-indirect-upstream-branch start-point))
      (fosgit-call-git "branch" (concat "--set-upstream-to=" it) branch))
    (fosgit-refresh)))

(defun fosgit-branch-read-args (prompt)
  (let ((args (fosgit-branch-arguments)) start branch)
    (cond (fosgit-branch-read-upstream-first
           (setq start  (fosgit-read-starting-point prompt))
           (setq branch (fosgit-read-string-ns
                         "Branch name"
                         (and (member start (fosgit-list-remote-branch-names))
                              (mapconcat #'identity
                                         (cdr (split-string start "/"))
                                         "/")))))
          (t
           (setq branch (fosgit-read-string-ns "Branch name"))
           (setq start  (fosgit-read-starting-point prompt))))
    (list branch start args)))

;;;###autoload
(defun fosgit-branch-spinoff (branch &rest args)
  "Create new branch from the unpushed commits.

Create and checkout a new branch starting at and tracking the
current branch.  That branch in turn is reset to the last commit
it shares with its upstream.  If the current branch has no
upstream or no unpushed commits, then the new branch is created
anyway and the previously current branch is not touched.

This is useful to create a feature branch after work has already
began on the old branch (likely but not necessarily \"master\").

If the current branch is a member of the value of option
`fosgit-branch-prefer-remote-upstream' (which see), then the
current branch will be used as the starting point as usual, but
the upstream of the starting-point may be used as the upstream
of the new branch, instead of the starting-point itself."
  (interactive (list (fosgit-read-string "Spin off branch")
                     (fosgit-branch-arguments)))
  (when (fosgit-branch-p branch)
    (user-error "Branch %s already exists" branch))
  (-if-let (current (fosgit-get-current-branch))
      (let (tracked base)
        (fosgit-call-git "checkout" args "-b" branch current)
        (--when-let (fosgit-get-indirect-upstream-branch current)
          (fosgit-call-git "branch" "--set-upstream-to" it branch))
        (when (and (setq tracked (fosgit-get-upstream-branch current))
                   (setq base (fosgit-git-string "merge-base" current tracked))
                   (not (fosgit-rev-eq base current)))
          (fosgit-call-git "update-ref" "-m"
                          (format "reset: moving to %s" base)
                          (concat "refs/heads/" current) base))
        (fosgit-refresh))
    (fosgit-run-git "checkout" "-b" branch)))

;;;###autoload
(defun fosgit-branch-reset (branch to &optional args set-upstream)
  "Reset a branch to the tip of another branch or any other commit.

When the branch being reset is the current branch, then do a
hard reset.  If there are any uncommitted changes, then the user
has to confirming the reset because those changes would be lost.

This is useful when you have started work on a feature branch but
realize it's all crap and want to start over.

When resetting to another branch and a prefix argument is used,
then also set the target branch as the upstream of the branch
that is being reset."
  (interactive
   (let* ((atpoint (fosgit-branch-at-point))
          (branch  (fosgit-read-local-branch "Reset branch" atpoint)))
     (list branch
           (fosgit-completing-read (format "Reset %s to" branch)
                                  (delete branch (fosgit-list-branch-names))
                                  nil nil nil 'fosgit-revision-history
                                  (or (and (not (equal branch atpoint)) atpoint)
                                      (fosgit-get-upstream-branch branch)))
           (fosgit-branch-arguments)
           current-prefix-arg)))
  (unless (member "--force" args)
    (setq args (cons "--force" args)))
  (if (equal branch (fosgit-get-current-branch))
      (if (and (fosgit-anything-modified-p)
               (not (yes-or-no-p "Uncommitted changes will be lost.  Proceed?")))
          (user-error "Abort")
        (fosgit-reset-hard to)
        (when (and set-upstream (fosgit-branch-p to))
          (fosgit-set-branch*merge/remote branch to)))
    (fosgit-branch branch to args)))

;;;###autoload
(defun fosgit-branch-delete (branches &optional force)
  "Delete one or multiple branches.
If the region marks multiple branches, then offer to delete
those, otherwise prompt for a single branch to be deleted,
defaulting to the branch at point."
  (interactive
   (let ((branches (fosgit-region-values 'branch))
         (force current-prefix-arg))
     (if (if (> (length branches) 1)
             (fosgit-confirm t nil "Delete %i branches" branches)
           (setq branches
                 (list (fosgit-read-branch (if current-prefix-arg
                                              "Force delete branch"
                                            "Delete branch")
                                          (fosgit-get-previous-branch)))))
         (unless force
           (--when-let (-intersection
                        (-union (fosgit-list-unmerged-branches)
                                (fosgit-list-unmerged-to-upstream-branches))
                        branches)
             (if (fosgit-confirm 'delete-unmerged-branch
                   "Delete unmerged branch %s"
                   "Delete %i unmerged branches" it)
                 (setq force t)
               (or (setq branches (-difference branches it))
                   (user-error "Abort")))))
       (user-error "Abort"))
     (list branches force)))
  (let* ((refs (-map #'fosgit-ref-fullname branches))
         (ambiguous (--filter (not it) refs)))
    (when ambiguous
      (user-error
       "%s ambiguous.  Please cleanup using git directly."
       (let ((len (length ambiguous)))
         (cond
          ((= len 1)
           (format "%s is" (--first (not (fosgit-ref-fullname it)) branches)))
          ((= len (length refs))
           (format "These %s names are" len))
          (t
           (format "%s of these names are" len))))))
    (cond
     ((string-match "^refs/remotes/\\([^/]+\\)" (car refs))
      (let* ((remote (match-string 1 (car refs)))
             (offset (1+ (length remote))))
        (fosgit-run-git-async
         "push" remote (--map (concat ":" (substring it offset)) branches))))
     ((> (length branches) 1)
      (fosgit-run-git "branch" (if force "-D" "-d")
                     (delete (fosgit-get-current-branch) branches)))
     (t ; And now for something completely different.
      (let* ((branch (car branches))
             (prompt (format "Branch %s is checked out.  " branch)))
        (when (equal branch (fosgit-get-current-branch))
          (pcase (if (or (equal branch "master")
                         (not (fosgit-rev-verify "master")))
                     (fosgit-read-char-case prompt nil
                       (?d "[d]etach HEAD & delete" 'detach)
                       (?a "[a]bort"                'abort))
                   (fosgit-read-char-case prompt nil
                     (?d "[d]etach HEAD & delete"     'detach)
                     (?c "[c]heckout master & delete" 'master)
                     (?a "[a]bort"                    'abort)))
            (`detach (fosgit-call-git "checkout" "--detach"))
            (`master (fosgit-call-git "checkout" "master"))
            (`abort  (user-error "Abort")))
          (setq force t))
        (fosgit-run-git "branch" (if force "-D" "-d") branch))))))

(put 'fosgit-branch-delete 'interactive-only t)

;;;###autoload
(defun fosgit-branch-rename (old new &optional force)
  "Rename branch OLD to NEW.
With prefix, forces the rename even if NEW already exists.
\n(git branch -m|-M OLD NEW)."
  (interactive
   (let ((branch (fosgit-read-local-branch "Rename branch")))
     (list branch
           (fosgit-read-string-ns (format "Rename branch '%s' to" branch))
           current-prefix-arg)))
  (unless (string= old new)
    (fosgit-run-git "branch" (if force "-M" "-m") old new)))

;;;;; Branch Variables

;;;###autoload
(defun fosgit-edit-branch*description (branch)
  "Edit the description of the current branch.
With a prefix argument edit the description of another branch.

The description for the branch named NAME is stored in the Git
variable `branch.<name>.description'."
  (interactive
   (list (or (and (not current-prefix-arg)
                  (fosgit-get-current-branch))
             (fosgit-read-local-branch "Edit branch description"))))
  (fosgit-run-git-with-editor "branch" "--edit-description" branch))

(defun fosgit-edit-branch*description-check-buffers ()
  (and buffer-file-name
       (string-match-p "/BRANCH_DESCRIPTION\\'" buffer-file-name)
       (add-hook 'with-editor-post-finish-hook
                 (lambda ()
                   (when (derived-mode-p 'fosgit-popup-mode)
                     (fosgit-refresh-popup-buffer)))
                 nil t)))

(add-hook 'find-file-hook 'fosgit-edit-branch*description-check-buffers)

(defun fosgit-format-branch*description ()
  (let* ((branch (or (fosgit-get-current-branch) "<name>"))
         (width (+ (length branch) 19))
         (var (format "branch.%s.description" branch)))
    (concat var " " (make-string (- width (length var)) ?\s)
            (-if-let (value (fosgit-get var))
                (propertize value 'face 'fosgit-popup-option-value)
              (propertize "unset" 'face 'fosgit-popup-disabled-argument)))))

;;;###autoload
(defun fosgit-set-branch*merge/remote (branch upstream)
  "Set or unset the upstream of the current branch.
With a prefix argument do so for another branch.

When the branch in question already has an upstream then simply
unsets it.  Invoke this command again to set another upstream.

Together the Git variables `branch.<name>.remote' and
`branch.<name>.merge' define the upstream branch of the local
branch named NAME.  The value of `branch.<name>.remote' is the
name of the upstream remote.  The value of `branch.<name>.merge'
is the full reference of the upstream branch, on the remote.

Non-interactively, when UPSTREAM is non-nil, then always set it
as the new upstream, regardless of whether another upstream was
already set.  When nil, then always unset."
  (interactive
   (let ((branch (or (and (not current-prefix-arg)
                          (fosgit-get-current-branch))
                     (fosgit-read-local-branch "Change upstream of branch"))))
     (list branch (and (not (fosgit-get-upstream-branch branch))
                       (fosgit-read-upstream-branch)))))
  (if upstream
      (-let (((remote . merge) (fosgit-split-branch-name upstream))
             (branch (fosgit-get-current-branch)))
        (fosgit-call-git "config" (format "branch.%s.remote" branch) remote)
        (fosgit-call-git "config" (format "branch.%s.merge"  branch)
                        (concat "refs/heads/" merge)))
    (fosgit-call-git "branch" "--unset-upstream" branch))
  (when (called-interactively-p 'any)
    (fosgit-refresh)))

(defun fosgit-format-branch*merge/remote ()
  (let* ((branch (or (fosgit-get-current-branch) "<name>"))
         (width (+ (length branch) 20))
         (varM (format "branch.%s.merge" branch))
         (varR (format "branch.%s.remote" branch))
         (face (if (equal (fosgit-get varR) ".")
                   'fosgit-branch-local
                 'fosgit-branch-remote)))
    (concat varM (make-string (- width (length varM)) ?\s)
            (-if-let (value (fosgit-get varM))
                (propertize value 'face face)
              (propertize "unset" 'face 'fosgit-popup-disabled-argument))
            "\n   " varR (make-string (- width (length varR)) ?\s)
            (-if-let (value (fosgit-get varR))
                (propertize value 'face face)
              (propertize "unset" 'face 'fosgit-popup-disabled-argument)))))

;;;###autoload
(defun fosgit-cycle-branch*rebase (branch)
  "Cycle the value of `branch.<name>.rebase' for the current branch.
With a prefix argument cycle the value for another branch.

The Git variables `branch.<name>.rebase' controls whether pulling
into the branch named NAME is done by rebasing that branch onto
the fetched branch or by merging that branch.

When `true' then pulling is done by rebasing.
When `false' then pulling is done by merging.

When that variable is undefined then the value of `pull.rebase'
is used instead.  It defaults to `false'."
  (interactive
   (list (or (and (not current-prefix-arg)
                  (fosgit-get-current-branch))
             (fosgit-read-local-branch "Cycle branch.<name>.rebase for"))))
  (fosgit-popup-set-variable (format "branch.%s.rebase" branch)
                            '("true" "false")
                            "false" "pull.rebase"))

(defun fosgit-format-branch*rebase ()
  (let ((branch (or (fosgit-get-current-branch) "<name>")))
    (fosgit-popup-format-variable (format "branch.%s.rebase" branch)
                                 '("true" "false")
                                 "false" "pull.rebase"
                                 (+ (length branch) 20))))

;;;###autoload
(defun fosgit-cycle-branch*pushRemote (branch)
  "Cycle the value of `branch.<name>.pushRemote' for the current branch.
With a prefix argument cycle the value for another branch.

The Git variable `branch.<name>.pushRemote' specifies the remote
that the branch named NAME is usually pushed to.  The value has
to be the name of an existing remote.

If that variable is undefined, then the value of the Git variable
`remote.pushDefault' is used instead, provided that it is defined,
which by default it is not."
  (interactive
   (list (or (and (not current-prefix-arg)
                  (fosgit-get-current-branch))
             (fosgit-read-local-branch "Cycle branch.<name>.pushRemote for"))))
  (fosgit-popup-set-variable (format "branch.%s.pushRemote" branch)
                            (fosgit-list-remotes)
                            "remote.pushDefault"))

(defun fosgit-format-branch*pushRemote ()
  (let ((branch (or (fosgit-get-current-branch) "<name>")))
    (fosgit-popup-format-variable (format "branch.%s.pushRemote" branch)
                                 (fosgit-list-remotes)
                                 nil "remote.pushDefault"
                                 (+ (length branch) 20))))

;;;###autoload
(defun fosgit-cycle-pull.rebase ()
  "Cycle the repository-local value of `pull.rebase'.

The Git variable `pull.rebase' specifies whether pulling is done
by rebasing or by merging.  It can be overwritten using the Git
variable `branch.<name>.rebase'.

When `true' then pulling is done by rebasing.
When `false' (the default) then pulling is done by merging."
  (interactive)
  (fosgit-popup-set-variable "pull.rebase" '("true" "false") "false"))

(defun fosgit-format-pull.rebase ()
  (fosgit-popup-format-variable "pull.rebase" '("true" "false") "false" nil 19))

;;;###autoload
(defun fosgit-cycle-remote.pushDefault ()
  "Cycle the repository-local value of `remote.pushDefault'.

The Git variable `remote.pushDefault' specifies the remote that
local branches are usually pushed to.  It can be overwritten
using the Git variable `branch.<name>.pushRemote'."
  (interactive)
  (fosgit-popup-set-variable "remote.pushDefault" (fosgit-list-remotes)))

(defun fosgit-format-remote.pushDefault ()
  (fosgit-popup-format-variable "remote.pushDefault"
                               (fosgit-list-remotes) nil nil 19))

;;;###autoload
(defun fosgit-cycle-branch*autoSetupMerge ()
  "Cycle the repository-local value of `branch.autoSetupMerge'.

The Git variable `branch.autoSetupMerge' under what circumstances
creating a branch (named NAME) should result in the variables
`branch.<name>.merge' and `branch.<name>.remote' being set
according to the starting point used to create the branch.  If
the starting point isn't a branch, then these variables are never
set.

When `always' then the variables are set regardless of whether
the starting point is a local or a remote branch.

When `true' (the default) then the variable are set when the
starting point is a remote branch, but not when it is a local
branch.

When `false' then the variables are never set."
  (interactive)
  (fosgit-popup-set-variable "branch.autoSetupMerge"
                            '("always" "true" "false") "true"))

(defun fosgit-format-branch*autoSetupMerge ()
  (fosgit-popup-format-variable "branch.autoSetupMerge"
                               '("always" "true" "false") "true" nil 23))

;;;###autoload
(defun fosgit-cycle-branch*autoSetupRebase ()
  "Cycle the repository-local value of `branch.autoSetupRebase'.

The Git variable `branch.autoSetupRebase' specifies whether
creating a branch (named NAME) should result in the variable
`branch.<name>.rebase' being set to `true'.

When `always' then the variable is set regardless of whether the
starting point is a local or a remote branch.

When `local' then the variable are set when the starting point
is a local branch, but not when it is a remote branch.

When `remote' then the variable are set when the starting point
is a remote branch, but not when it is a local branch.

When `never' (the default) then the variable is never set."
  (interactive)
  (fosgit-popup-set-variable "branch.autoSetupRebase"
                            '("always" "local" "remote" "never") "never"))

(defun fosgit-format-branch*autoSetupRebase ()
  (fosgit-popup-format-variable "branch.autoSetupRebase"
                               '("always" "local" "remote" "never")
                               "never" nil 23))

;;;; Merge

;;;###autoload (autoload 'fosgit-merge-popup "fosgit" nil t)
(fosgit-define-popup fosgit-merge-popup
  "Popup console for merge commands."
  'fosgit-commands
  :man-page "git-merge"
  :switches '((?f "Fast-forward only" "--ff-only")
              (?n "No fast-forward"   "--no-ff")
              (?s "Squash"            "--squash"))
  :options  '((?s "Strategy" "--strategy="))
  :actions  '((?m "Merge"                  fosgit-merge)
              (?e "Merge and edit message" fosgit-merge-editmsg)
              (?p "Preview merge"          fosgit-merge-preview)
              (?n "Merge but don't commit" fosgit-merge-nocommit))
  :sequence-actions   '((?m "Commit merge" fosgit-commit)
                        (?a "Abort merge"  fosgit-merge-abort))
  :sequence-predicate 'fosgit-merge-state
  :default-action 'fosgit-merge
  :max-action-columns 2)

;;;###autoload
(defun fosgit-merge (rev &optional args nocommit)
  "Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

\(git merge --no-edit|--no-commit [ARGS] REV)"
  (interactive (list (fosgit-read-other-branch-or-commit "Merge")
                     (fosgit-merge-arguments)
                     current-prefix-arg))
  (fosgit-merge-assert)
  (fosgit-run-git "merge" (if nocommit "--no-commit" "--no-edit") args rev))

;;;###autoload
(defun fosgit-merge-editmsg (rev &optional args)
  "Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.
\n(git merge --edit [ARGS] rev)"
  (interactive (list (fosgit-read-other-branch-or-commit "Merge")
                     (fosgit-merge-arguments)))
  (fosgit-merge-assert)
  (with-editor "GIT_EDITOR"
    (let ((fosgit-process-popup-time -1))
      (fosgit-run-git-async "merge" "--edit" args rev))))

;;;###autoload
(defun fosgit-merge-nocommit (rev &optional args)
  "Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.
\n(git merge --no-commit [ARGS] rev)"
  (interactive (list (fosgit-read-other-branch-or-commit "Merge")
                     (fosgit-merge-arguments)))
  (fosgit-merge-assert)
  (fosgit-run-git "merge" "--no-commit" args rev))

;;;###autoload
(defun fosgit-merge-preview (rev)
  "Preview result of merging REV into the current branch."
  (interactive (list (fosgit-read-other-branch-or-commit "Preview merge")))
  (fosgit-mode-setup #'fosgit-merge-preview-mode rev))

(define-derived-mode fosgit-merge-preview-mode fosgit-diff-mode "Fosgit Merge"
  "Mode for previewing a merge."
  :group 'fosgit-diff
  (hack-dir-local-variables-non-file-buffer))

(defun fosgit-merge-preview-refresh-buffer (rev)
  (let* ((branch (fosgit-get-current-branch))
         (head (or branch (fosgit-rev-verify "HEAD"))))
    (setq header-line-format
          (propertize (format "Preview merge of %s into %s"
                              rev (or branch "HEAD"))
                      'face 'fosgit-header-line))
    (fosgit-insert-section (diffbuf)
      (fosgit-git-wash #'fosgit-diff-wash-diffs
        "merge-tree" (fosgit-git-string "merge-base" head rev) head rev))))

;;;###autoload
(defun fosgit-merge-abort ()
  "Abort the current merge operation.
\n(git merge --abort)"
  (interactive)
  (if (file-exists-p (fosgit-git-dir "MERGE_HEAD"))
      (when (fosgit-confirm 'abort-merge)
        (fosgit-run-git-async "merge" "--abort"))
    (user-error "No merge in progress")))

(defun fosgit-checkout-stage (file arg)
  "During a conflict checkout and stage side, or restore conflict."
  (interactive
   (let ((file (fosgit-completing-read "Checkout file"
                                      (fosgit-tracked-files) nil nil nil
                                      'fosgit-read-file-hist
                                      (fosgit-current-file))))
     (cond ((member file (fosgit-unmerged-files))
            (list file (fosgit-checkout-read-stage file)))
           ((yes-or-no-p (format "Restore conflicts in %s? " file))
            (list file "--merge"))
           (t
            (user-error "Quit")))))
  (pcase (cons arg (cddr (car (fosgit-file-status file))))
    ((or `("--ours"   ?D ,_)
         `("--theirs" ,_ ?D))
     (fosgit-run-git "rm" "--" file))
    (_ (if (equal arg "--merge")
           ;; This fails if the file was deleted on one
           ;; side.  And we cannot do anything about it.
           (fosgit-run-git "checkout" "--merge" "--" file)
         (fosgit-call-git "checkout" arg "--" file)
         (fosgit-run-git "add" "-u" "--" file)))))

(defun fosgit-merge-state ()
  (file-exists-p (fosgit-git-dir "MERGE_HEAD")))

(defun fosgit-merge-assert ()
  (or (not (fosgit-anything-modified-p))
      (fosgit-confirm 'merge-dirty
        "Merging with dirty worktree is risky.  Continue")
      (user-error "Abort")))

(defun fosgit-checkout-read-stage (file)
  (fosgit-read-char-case (format "For %s checkout: " file) t
    (?o "[o]ur stage"   "--ours")
    (?t "[t]heir stage" "--theirs")
    (?c "[c]onflict"    "--merge")))

(defun fosgit-insert-merge-log ()
  "Insert section for the on-going merge.
Display the heads that are being merged.
If no merge is in progress, do nothing."
  (-when-let (heads (mapcar 'fosgit-get-shortname
                            (fosgit-file-lines (fosgit-git-dir "MERGE_HEAD"))))
    (fosgit-insert-section (commit (car heads))
      (fosgit-insert-heading
        (format "Merging %s:" (mapconcat 'identity heads ", ")))
      (fosgit-insert-log
       (concat (fosgit-git-string "merge-base" "--octopus" "HEAD" (car heads))
               ".." (car heads))
       (let ((args fosgit-log-section-arguments))
         (unless (member "--decorate=full" fosgit-log-section-arguments)
           (push "--decorate=full" args))
         args)))))

;;;; Reset

;;;###autoload
(defun fosgit-reset-index (commit)
  "Reset the index to COMMIT.
Keep the head and working tree as-is, so if COMMIT refers to the
head this effectively unstages all changes.
\n(git reset COMMIT)"
  (interactive (list (fosgit-read-branch-or-commit "Reset index to")))
  (fosgit-reset-internal nil commit "."))

;;;###autoload
(defun fosgit-reset (commit &optional hard)
  "Reset the head and index to COMMIT, but not the working tree.
With a prefix argument also reset the working tree.
\n(git reset --mixed|--hard COMMIT)"
  (interactive (list (fosgit-read-branch-or-commit
                      (if current-prefix-arg
                          "Hard reset to"
                        "Reset head to"))
                     current-prefix-arg))
  (fosgit-reset-internal (if hard "--hard" "--mixed") commit))

;;;###autoload
(defun fosgit-reset-head (commit)
  "Reset the head and index to COMMIT, but not the working tree.
\n(git reset --mixed COMMIT)"
  (interactive (list (fosgit-read-branch-or-commit "Reset head to")))
  (fosgit-reset-internal "--mixed" commit))

;;;###autoload
(defun fosgit-reset-soft (commit)
  "Reset the head to COMMIT, but not the index and working tree.
\n(git reset --soft REVISION)"
  (interactive (list (fosgit-read-branch-or-commit "Soft reset to")))
  (fosgit-reset-internal "--soft" commit))

;;;###autoload
(defun fosgit-reset-hard (commit)
  "Reset the head, index, and working tree to COMMIT.
\n(git reset --hard REVISION)"
  (interactive (list (fosgit-read-branch-or-commit "Hard reset to")))
  (fosgit-reset-internal "--hard" commit))

(defun fosgit-reset-internal (arg commit &optional path)
  (when (and (not (member arg '("--hard" nil)))
             (equal (fosgit-rev-parse commit)
                    (fosgit-rev-parse "HEAD~")))
    (with-temp-buffer
      (fosgit-git-insert "show" "-s" "--format=%B" "HEAD")
      (when git-commit-major-mode
        (funcall git-commit-major-mode))
      (git-commit-setup-font-lock)
      (git-commit-save-message)))
  (let ((cmd (if (and (equal commit "HEAD") (not arg)) "unstage" "reset")))
    (fosgit-wip-commit-before-change nil (concat " before " cmd))
    (fosgit-run-git "reset" arg commit "--" path)
    (when (equal cmd "unstage")
      (fosgit-wip-commit-after-apply nil " after unstage"))))

;;;; Files

(defun fosgit-file-rename (file newname)
  "Rename the FILE to NEWNAME.
If FILE isn't tracked in Git fallback to using `rename-file'."
  (interactive
   (let* ((file (fosgit-read-file "Rename file"))
          (newname (read-file-name (format "Rename %s to file: " file))))
     (list (expand-file-name file (fosgit-toplevel))
           (expand-file-name newname))))
  (if (fosgit-file-tracked-p file)
      (let ((oldbuf (get-file-buffer file)))
        (when (and oldbuf (buffer-modified-p oldbuf))
          (user-error "Save %s before moving it" file))
        (when (file-exists-p newname)
          (user-error "%s already exists" newname))
        (fosgit-run-git "mv" file newname)
        (when oldbuf
          (with-current-buffer oldbuf
            (let ((buffer-read-only buffer-read-only))
              (set-visited-file-name newname))
            (if (fboundp 'vc-refresh-state)
                (vc-refresh-state)
              (with-no-warnings
                (vc-find-file-hook))))))
    (rename-file file newname current-prefix-arg)
    (fosgit-refresh)))

(defun fosgit-file-untrack (file)
  "Untrack FILE.
Stop tracking FILE in Git but do not remove it from the working
tree."
  (interactive (list (fosgit-read-tracked-file "Untrack file")))
  (fosgit-run-git "rm" "--cached" "--" file))

(defun fosgit-file-delete (file &optional force)
  "Delete FILE.
With a prefix argument FORCE do so even when FILE has uncommitted
changes.

If FILE isn't tracked in Git fallback to using `delete-file'."
  (interactive (list (fosgit-read-file "Delete file")))
  (if (fosgit-file-tracked-p file)
      (fosgit-run-git "rm" (and force "--force") "--" file)
    (delete-file (expand-file-name file (fosgit-toplevel)) t)
    (fosgit-refresh)))

(defun fosgit-read-tracked-file (prompt)
  (fosgit-read-file prompt t))

(defun fosgit-read-file (prompt &optional tracked-only)
  (let ((choices (nconc (fosgit-list-files)
                        (unless tracked-only (fosgit-untracked-files)))))
    (fosgit-completing-read prompt choices nil t nil nil
                           (car (member (or (fosgit-section-when (file))
                                            (fosgit-file-relative-name
                                             nil tracked-only))
                                        choices)))))

(defun fosgit-read-files (prompt initial-contents)
  (mapconcat 'identity
             (completing-read-multiple (or prompt "File,s: ")
                                       (fosgit-list-files)
                                       nil nil initial-contents) ","))

(defun fosgit-read-file-choice (prompt files &optional error default)
  "Read file from FILES.

If FILES has only one member, return that instead of prompting.
If FILES has no members, give a user error.  ERROR can be given
to provide a more informative error.

If DEFAULT is non-nil, use this as the default value instead of
`fosgit-current-file'."
  (pcase (length files)
    (0 (user-error (or error "No file choices")))
    (1 (car files))
    (_ (fosgit-completing-read
        prompt files nil t nil 'fosgit-read-file-hist
        (car (member (or default (fosgit-current-file)) files))))))

;;; Miscellaneous
;;;; Tag

;;;###autoload (autoload 'fosgit-tag-popup "fosgit" nil t)
(fosgit-define-popup fosgit-tag-popup
  "Popup console for tag commands."
  'fosgit-commands
  :man-page "git-tag"
  :switches '((?a "Annotate" "--annotate")
              (?s "Sign"     "--sign")
              (?f "Force"    "--force"))
  :actions  '((?t "Create"   fosgit-tag)
              (?k "Delete"   fosgit-tag-delete)
              (?p "Prune"    fosgit-tag-prune))
  :default-action 'fosgit-tag)

;;;###autoload
(defun fosgit-tag (name rev &optional args)
  "Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.
\n(git tag [--annotate] NAME REV)"
  (interactive (list (fosgit-read-tag "Tag name")
                     (fosgit-read-branch-or-commit "Place tag on")
                     (let ((args (fosgit-tag-arguments)))
                       (when current-prefix-arg
                         (cl-pushnew "--annotate" args))
                       args)))
  (fosgit-run-git-with-editor "tag" args name rev))

;;;###autoload
(defun fosgit-tag-delete (tags)
  "Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.
\n(git tag -d TAGS)"
  (interactive (list (--if-let (fosgit-region-values 'tag)
                         (fosgit-confirm t nil "Delete %i tags" it)
                       (fosgit-read-tag "Delete tag" t))))
  (fosgit-run-git "tag" "-d" tags))

(defun fosgit-tag-prune (tags remote-tags remote)
  "Offer to delete tags missing locally from REMOTE, and vice versa."
  (interactive
   (let* ((remote (fosgit-read-remote "Prune tags using remote"))
          (tags   (fosgit-list-tags))
          (rtags  (prog2 (message "Determining remote tags...")
                      (fosgit-remote-list-tags remote)
                    (message "Determining remote tags...done")))
          (ltags  (-difference tags rtags))
          (rtags  (-difference rtags tags)))
     (unless (or ltags rtags)
       (message "Same tags exist locally and remotely"))
     (unless (fosgit-confirm t "Delete %s locally"
               "Delete %i tags locally" ltags)
       (setq ltags nil))
     (unless (fosgit-confirm t "Delete %s from remote"
               "Delete %i tags from remote" rtags)
       (setq rtags nil))
     (list ltags rtags remote)))
  (when tags
    (fosgit-call-git "tag" "-d" tags))
  (when remote-tags
    (fosgit-run-git-async "push" remote (--map (concat ":" it) remote-tags))))

;;;; Notes

;;;###autoload (autoload 'fosgit-notes-popup "fosgit" nil t)
(fosgit-define-popup fosgit-notes-popup
  "Popup console for notes commands."
  'fosgit-commands
  :man-page "git-tag"
  :switches '("Switch for prune"
              (?n "Dry run"          "--dry-run"))
  :options  '("Option for edit and remove"
              (?r "Manipulate ref"   "--ref="      fosgit-notes-popup-read-ref)
              "Option for merge"
              (?s "Merge strategy"   "--strategy="))
  :actions  '((?T "Edit"             fosgit-notes-edit)
              (?r "Remove"           fosgit-notes-remove)
              (?m "Merge"            fosgit-notes-merge)
              (?p "Prune"            fosgit-notes-prune)
              (?s "Set ref"          fosgit-notes-set-ref)
              (?S "Set display refs" fosgit-notes-set-display-refs))
  :sequence-actions '((?c "Commit merge" fosgit-notes-merge-commit)
                      (?a "Abort merge"  fosgit-notes-merge-abort))
  :sequence-predicate 'fosgit-notes-merging-p
  :default-action 'fosgit-notes-edit)

(defun fosgit-notes-edit (commit &optional ref)
  "Edit the note attached to COMMIT.
REF is the notes ref used to store the notes.

Interactively or when optional REF is nil use the value of Git
variable `core.notesRef' or \"refs/notes/commits\" if that is
undefined."
  (interactive (fosgit-notes-read-args "Edit notes"))
  (fosgit-run-git-with-editor "notes" (and ref (concat "--ref=" ref))
                             "edit" commit))

(defun fosgit-notes-remove (commit &optional ref)
  "Remove the note attached to COMMIT.
REF is the notes ref from which the note is removed.

Interactively or when optional REF is nil use the value of Git
variable `core.notesRef' or \"refs/notes/commits\" if that is
undefined."
  (interactive (fosgit-notes-read-args "Remove notes"))
  (fosgit-run-git-with-editor "notes" (and ref (concat "--ref=" ref))
                             "remove" commit))

(defun fosgit-notes-merge (ref)
  "Merge the notes ref REF into the current notes ref.

The current notes ref is the value of Git variable
`core.notesRef' or \"refs/notes/commits\" if that is undefined.

When there are conflict, then they have to resolved in the
temporary worktree \".git/NOTES_MERGE_WORKTREE\".  When
done use `fosgit-notes-merge-commit' to finish.  To abort
use `fosgit-notes-merge-abort'."
  (interactive (list (fosgit-read-string-ns "Merge reference")))
  (fosgit-run-git-with-editor "notes" "merge" ref))

(defun fosgit-notes-merge-commit ()
  "Commit the current notes ref merge.
Also see `fosgit-notes-merge'."
  (interactive)
  (fosgit-run-git-with-editor "notes" "merge" "--commit"))

(defun fosgit-notes-merge-abort ()
  "Abort the current notes ref merge.
Also see `fosgit-notes-merge'."
  (interactive)
  (fosgit-run-git-with-editor "notes" "merge" "--abort"))

(defun fosgit-notes-prune (&optional dry-run)
  "Remove notes about unreachable commits."
  (interactive (list (and (member "--dry-run" (fosgit-notes-arguments)) t)))
  (when dry-run
    (fosgit-process-buffer))
  (fosgit-run-git-with-editor "notes" "prune" (and dry-run "--dry-run")))

(defun fosgit-notes-set-ref (ref &optional global)
  "Set the current notes ref to REF.
The ref is made current by setting the value of the Git variable
`core.notesRef'.  With a prefix argument GLOBAL change the global
value, else the value in the current repository.  When this is
undefined, then \"refs/notes/commit\" is used.

Other `fosgit-notes-*' commands, as well as the sub-commands
of Git's `note' command, default to operate on that ref."
  (interactive
   (list (fosgit-completing-read "Set notes ref"
                                (nconc (list "refs/" "refs/notes/")
                                       (fosgit-list-notes-refnames))
                                nil nil
                                (--when-let (fosgit-get "core.notesRef")
                                  (if (string-match "^refs/notes/\\(.+\\)" it)
                                      (match-string 1 it)
                                    it)))
         current-prefix-arg))
  (if ref
      (fosgit-run-git "config" (and global "--global") "core.notesRef"
                     (if (string-prefix-p "refs/" ref)
                         ref
                       (concat "refs/notes/" ref)))
    (fosgit-run-git "config" (and global "--global")
                   "--unset" "core.notesRef")))

(defun fosgit-notes-set-display-refs (refs &optional global)
  "Set notes refs to be display in addition to \"core.notesRef\".
REFS is a colon separated list of notes refs.  The values are
stored in the Git variable `notes.displayRef'.  With a prefix
argument GLOBAL change the global values, else the values in
the current repository."
  (interactive
   (list (fosgit-completing-read "Set additional notes ref(s)"
                                (nconc (list "refs/" "refs/notes/")
                                       (fosgit-list-notes-refnames))
                                nil nil
                                (mapconcat #'identity
                                           (fosgit-get-all "notes.displayRef")
                                           ":"))
         current-prefix-arg))
  (when (and refs (atom refs))
    (setq refs (split-string refs ":")))
  (when global
    (setq global "--global"))
  (fosgit-git-success "config" "--unset-all" global "notes.displayRef")
  (dolist (ref refs)
    (fosgit-call-git "config" "--add" global "notes.displayRef" ref))
  (fosgit-refresh))

(defun fosgit-notes-read-args (prompt)
 (list (fosgit-read-branch-or-commit prompt)
       (--when-let (--first (string-match "^--ref=\\(.+\\)" it)
                            (fosgit-notes-arguments))
         (match-string 1 it))))

(defun fosgit-notes-popup-read-ref (prompt &optional initial-input)
  (fosgit-completing-read prompt (nconc (list "refs/" "refs/notes/")
                                       (fosgit-list-notes-refnames))
                         nil nil initial-input))

(defun fosgit-notes-merging-p ()
  (let ((dir (fosgit-git-dir "NOTES_MERGE_WORKTREE")))
    (and (file-directory-p dir)
         (directory-files dir nil "^[^.]"))))

;;;; File Mode

(defvar fosgit-file-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-xg"    'fosgit-status)
    (define-key map "\C-x\M-g" 'fosgit-dispatch-popup)
    (define-key map "\C-c\M-g" 'fosgit-file-popup)
    map)
  "Keymap for `fosgit-file-mode'.")

(fosgit-define-popup fosgit-file-popup
  "Popup console for Fosgit commands in file-visiting buffers."
  :actions '((?s "Stage"   fosgit-stage-file)
             (?l "Log"     fosgit-log-buffer-file)
             (?c "Commit"  fosgit-commit-popup)
             (?u "Unstage" fosgit-unstage-file)
             (?b "Blame"   fosgit-blame-popup) nil nil
             (?p "Find blob" fosgit-blob-previous))
  :max-action-columns 3)

(defvar fosgit-file-mode-lighter "")

(define-minor-mode fosgit-file-mode
  "Enable some Fosgit features in file-visiting buffers.

Currently this only adds the following key bindings.
\n\\{fosgit-file-mode-map}"
  :package-version '(fosgit . "2.2.0")
  :lighter fosgit-file-mode-lighter
  :keymap  fosgit-file-mode-map)

(defun fosgit-file-mode-turn-on ()
  (and buffer-file-name
       (ignore-errors (fosgit-inside-worktree-p))
       (fosgit-file-mode)))

;;;###autoload
(define-globalized-minor-mode global-fosgit-file-mode
  fosgit-file-mode fosgit-file-mode-turn-on
  :package-version '(fosgit . "2.2.0")
  :group 'fosgit-modes)

;;;; Blob Mode

(defvar fosgit-blob-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'fosgit-blob-next)
    (define-key map "p" 'fosgit-blob-previous)
    (define-key map "q" 'fosgit-kill-this-buffer)
    map)
  "Keymap for `fosgit-blob-mode'.")

(define-minor-mode fosgit-blob-mode
  "Enable some Fosgit features in blob-visiting buffers.

Currently this only adds the following key bindings.
\n\\{fosgit-blob-mode-map}"
  :package-version '(fosgit . "2.3.0"))

(defun fosgit-blob-next ()
  "Visit the next blob which modified the current file."
  (interactive)
  (if fosgit-buffer-file-name
      (fosgit-blob-visit (or (fosgit-blob-successor fosgit-buffer-revision
                                                  fosgit-buffer-file-name)
                            fosgit-buffer-file-name)
                        (line-number-at-pos))
    (if (buffer-file-name (buffer-base-buffer))
        (user-error "You have reached the end of time")
      (user-error "Buffer isn't visiting a file or blob"))))

(defun fosgit-blob-previous ()
  "Visit the previous blob which modified the current file."
  (interactive)
  (-if-let (file (or fosgit-buffer-file-name
                     (buffer-file-name (buffer-base-buffer))))
      (--if-let (fosgit-blob-ancestor fosgit-buffer-revision file)
          (fosgit-blob-visit it (line-number-at-pos))
        (user-error "You have reached the beginning of time"))
    (user-error "Buffer isn't visiting a file or blob")))

(defun fosgit-blob-visit (blob-or-file line)
  (if (stringp blob-or-file)
      (find-file blob-or-file)
    (-let [(rev file) blob-or-file]
      (fosgit-find-file rev file)
      (let ((str (fosgit-rev-format "%ct%s" rev)))
        (message "%s (%s ago)" (substring str 10)
                 (fosgit-format-duration
                  (abs (truncate (- (float-time)
                                    (string-to-number
                                     (substring str 0 10)))))
                  fosgit-duration-spec)))))
  (goto-char (point-min))
  (forward-line (1- line)))

(defun fosgit-blob-ancestor (rev file)
  (let ((lines (fosgit-with-toplevel
                 (fosgit-git-lines "log" "-2" "--format=%H" "--name-only"
                                  "--follow" (or rev "HEAD") "--" file))))
    (if rev (cddr lines) (butlast lines 2))))

(defun fosgit-blob-successor (rev file)
  (let ((lines (fosgit-with-toplevel
                 (fosgit-git-lines "log" "--format=%H" "--name-only" "--follow"
                                  "HEAD" "--" file))))
    (catch 'found
      (while lines
        (if (equal (nth 2 lines) rev)
            (throw 'found (list (nth 0 lines) (nth 1 lines)))
          (setq lines (nthcdr 2 lines)))))))

(defun fosgit-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;;;; Dispatch Popup

;;;###autoload (autoload 'fosgit-dispatch-popup "fosgit" nil t)
(fosgit-define-popup fosgit-dispatch-popup
  "Popup console for dispatching other popups."
  'fosgit-commands nil nil
  :actions '("Popup and dwim commands"
             (?A "Cherry-picking"  fosgit-cherry-pick-popup)
             (?b "Branching"       fosgit-branch-popup)
             (?B "Bisecting"       fosgit-bisect-popup)
             (?c "Committing"      fosgit-commit-popup)
             (?d "Diffing"         fosgit-diff-popup)
             (?D "Change diffs"    fosgit-diff-refresh-popup)
             (?e "Ediff dwimming"  fosgit-ediff-dwim)
             (?E "Ediffing"        fosgit-ediff-popup)
             (?f "Fetching"        fosgit-fetch-popup)
             (?F "Pulling"         fosgit-pull-popup)
             (?l "Logging"         fosgit-log-popup)
             (?m "Merging"         fosgit-merge-popup)
             (?M "Remoting"        fosgit-remote-popup)
             (?o "Submodules"      fosgit-submodule-popup)
             (?P "Pushing"         fosgit-push-popup)
             (?r "Rebasing"        fosgit-rebase-popup)
             (?t "Tagging"         fosgit-tag-popup)
             (?T "Notes"           fosgit-notes-popup)
             (?V "Reverting"       fosgit-revert-popup)
             (?w "Apply patches"   fosgit-am-popup)
             (?W "Format patches"  fosgit-patch-popup)
             (?y "Show Refs"       fosgit-show-refs-popup)
             (?z "Stashing"        fosgit-stash-popup)
             (?! "Running"         fosgit-run-popup)
             "Applying changes"
             (?a "Apply"           fosgit-apply)
             (?s "Stage"           fosgit-stage)
             (?u "Unstage"         fosgit-unstage)
             nil
             (?v "Reverse"         fosgit-reverse)
             (?S "Stage all"       fosgit-stage-modified)
             (?U "Unstage all"     fosgit-unstage-all)
             nil
             (?k "Discard"         fosgit-discard)
             "\
 g      refresh current buffer
 TAB    toggle section at point
 RET    visit thing at point

 C-h m  show all key bindings" nil)
  :max-action-columns 4)

;;;; Git Popup

(defvar fosgit-git-command-history nil)

;;;###autoload (autoload 'fosgit-run-popup "fosgit" nil t)
(fosgit-define-popup fosgit-run-popup
  "Popup console for running raw Git commands."
  'fosgit-commands nil nil
  :actions '((?! "Git Subcommand (in topdir)" fosgit-git-command-topdir)
             (?k "Gitk"                       fosgit-run-gitk)
             (?p "Git Subcommand (in pwd)"    fosgit-git-command)
             (?a "Gitk --all"                 fosgit-run-gitk-all)
             (?s "Shell command (in topdir)"  fosgit-shell-command-topdir)
             (?b "Gitk --branches"            fosgit-run-gitk-branches)
             (?S "Shell command (in pwd)"     fosgit-shell-command)
             (?g "Git Gui"                    fosgit-run-git-gui))
  :default-action 'fosgit-git-command
  :max-action-columns 2)

;;;###autoload
(defun fosgit-git-command (args directory)
  "Execute a Git subcommand asynchronously, displaying the output.
With a prefix argument run Git in the root of the current
repository, otherwise in `default-directory'."
  (interactive (fosgit-read-shell-command "Git subcommand (pwd: %s)"))
  (require 'eshell)
  (with-temp-buffer
    (insert args)
    (setq args (mapcar 'eval (eshell-parse-arguments (point-min)
                                                     (point-max))))
    (setq default-directory directory)
    (let ((fosgit-git-global-arguments
           ;; A human will want globbing by default.
           (remove "--literal-pathspecs"
                   fosgit-git-global-arguments)))
     (fosgit-run-git-async args)))
  (fosgit-process-buffer))

;;;###autoload
(defun fosgit-git-command-topdir (args directory)
  "Execute a Git subcommand asynchronously, displaying the output.
Run Git in the top-level directory of the current repository.
\n(fn)" ; arguments are for internal use
  (interactive (fosgit-read-shell-command "Git subcommand (pwd: %s)" t))
  (fosgit-git-command args directory))

;;;###autoload
(defun fosgit-shell-command (args directory)
  "Execute a shell command asynchronously, displaying the output.
With a prefix argument run the command in the root of the current
repository, otherwise in `default-directory'."
  (interactive (fosgit-read-shell-command "Shell command (pwd: %s)"))
  (require 'eshell)
  (with-temp-buffer
    (insert args)
    (setq args (mapcar 'eval (eshell-parse-arguments (point-min)
                                                     (point-max))))
    (setq default-directory directory)
    (apply #'fosgit-start-process (car args) nil (cdr args)))
  (fosgit-process-buffer))

;;;###autoload
(defun fosgit-shell-command-topdir (args directory)
  "Execute a shell command asynchronously, displaying the output.
Run the command in the top-level directory of the current repository.
\n(fn)" ; arguments are for internal use
  (interactive (fosgit-read-shell-command "Shell command (pwd: %s)" t))
  (fosgit-shell-command args directory))

(defun fosgit-read-shell-command (prompt &optional root)
  (let ((dir (if (or root current-prefix-arg)
                 (or (fosgit-toplevel)
                     (user-error "Not inside a Git repository"))
               default-directory)))
    (list (fosgit-read-string (format prompt (abbreviate-file-name dir))
                             nil 'fosgit-git-command-history)
          dir)))

;;;; Read Repository

(defun fosgit-read-repository (&optional read-directory-name)
  "Read a Git repository in the minibuffer, with completion.

The completion choices are the basenames of top-levels of
repositories found in the directories specified by option
`fosgit-repository-directories'.  In case of name conflicts
the basenames are prefixed with the name of the respective
parent directories.  The returned value is the actual path
to the selected repository.

With prefix argument simply read a directory name using
`read-directory-name'."
  (if (and (not read-directory-name) fosgit-repository-directories)
      (let* ((repos (fosgit-list-repos-uniquify
                     (--map (cons (file-name-nondirectory it) it)
                            (fosgit-list-repos))))
             (reply (fosgit-completing-read "Git repository" repos)))
        (file-name-as-directory
         (or (cdr (assoc reply repos))
             (if (file-directory-p reply)
                 (expand-file-name reply)
               (user-error "Not a repository or a directory: %s" reply)))))
    (file-name-as-directory
     (read-directory-name "Git repository: "
                          (or (fosgit-toplevel) default-directory)))))

(defun fosgit-list-repos ()
  (--mapcat (fosgit-list-repos-1 it fosgit-repository-directories-depth)
            fosgit-repository-directories))

(defun fosgit-list-repos-1 (directory depth)
  (cond ((file-readable-p (expand-file-name ".git" directory))
         (list directory))
        ((and (> depth 0) (file-accessible-directory-p directory))
         (--mapcat (when (file-directory-p it)
                     (fosgit-list-repos-1 it (1- depth)))
                   (directory-files directory t "^[^.]" t)))))

(defun fosgit-list-repos-uniquify (alist)
  (let (result (dict (make-hash-table :test 'equal)))
    (dolist (a (delete-dups alist))
      (puthash (car a) (cons (cdr a) (gethash (car a) dict)) dict))
    (maphash
     (lambda (key value)
       (if (= (length value) 1)
           (push (cons key (car value)) result)
         (setq result
               (append result
                       (fosgit-list-repos-uniquify
                        (--map (cons (concat
                                      key "\\"
                                      (file-name-nondirectory
                                       (directory-file-name
                                        (substring it 0 (- (length key))))))
                                     it)
                               value))))))
     dict)
    result))

;;;; Revision Stack

(defvar fosgit-revision-stack nil)

(defcustom fosgit-pop-revision-stack-format
  '("[%N: %h] " "%N: %H\n   %s\n" "\\[\\([0-9]+\\)[]:]")
  "Control how `fosgit-pop-revision-stack' inserts a revision.

The command `fosgit-pop-revision-stack' inserts a representation
of the revision last pushed to the `fosgit-revision-stack' into
the current buffer.  It inserts text at point and/or near the end
of the buffer, and removes the consumed revision from the stack.

The entries on the stack have the format (HASH TOPLEVEL) and this
option has the format (POINT-FORMAT EOB-FORMAT INDEX-REGEXP), all
of which may be nil or a string (though either one of EOB-FORMAT
or POINT-FORMAT should be a string, and if INDEX-REGEXP is
non-nil, then the two formats should be too).

First INDEX-REGEXP is used to find the previously inserted entry,
by searching backward from point.  The first submatch must match
the index number.  That number is incremented by one, and becomes
the index number of the entry to be inserted.  If you don't want
to number the inserted revisions, then use nil for INDEX-REGEXP.

If INDEX-REGEXP is non-nil then both POINT-FORMAT and EOB-FORMAT
should contain \"%N\", which is replaced with the number that was
determined in the previous step.

Both formats, if non-nil and after removing %N, are then expanded
using `git show --format=FORMAT ...' inside TOPLEVEL.

The expansion of POINT-FORMAT is inserted at point, and the
expansion of EOB-FORMAT is inserted at the end of the buffer (if
the buffer ends with a comment, then it is inserted right before
that)."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-commands
  :type '(list (choice (string :tag "Insert at point format")
                       (cons (string :tag "Insert at point format")
                             (repeat (string :tag "Argument to git show")))
                       (const :tag "Don't insert at point" nil))
               (choice (string :tag "Insert at eob format")
                       (cons (string :tag "Insert at eob format")
                             (repeat (string :tag "Argument to git show")))
                       (const :tag "Don't insert at eob" nil))
               (choice (regexp :tag "Find index regexp")
                       (const :tag "Don't number entries" nil))))

(defun fosgit-pop-revision-stack (rev toplevel)
  "Insert a representation of a revision into the current buffer.

Pop a revision from the `fosgit-revision-stack' and insert it into
the current buffer according to `fosgit-pop-revision-stack-format'.
Revisions can be put on the stack using `fosgit-copy-section-value'
and `fosgit-copy-buffer-revision'.

If the stack is empty or with a prefix argument instead read a
revision in the minibuffer.  By using the minibuffer history this
allows selecting an item which was popped earlier or to insert an
arbitrary reference or revision without first pushing it onto the
stack.

When reading the revision from the minibuffer, then it might not
be possible to guess the correct repository.  When this command
is called inside a repository (e.g. while composing a commit
message), then that repository is used.  Otherwise (e.g. while
composing an email) then the repository recorded for the top
element of the stack is used (even though we insert another
revision).  If not called inside a repository and with an empty
stack, or with two prefix arguments, then read the repository in
the minibuffer too."
  (interactive
   (if (or current-prefix-arg (not fosgit-revision-stack))
       (let ((default-directory
               (or (and (not (= (prefix-numeric-value current-prefix-arg) 16))
                        (or (fosgit-toplevel)
                            (cadr (car fosgit-revision-stack))))
                   (fosgit-read-repository))))
         (list (fosgit-read-branch-or-commit "Insert revision")
               default-directory))
     (push (caar fosgit-revision-stack) fosgit-revision-history)
     (pop fosgit-revision-stack)))
  (if rev
      (-let [(pnt-format eob-format idx-format) fosgit-pop-revision-stack-format]
        (let ((default-directory toplevel)
              (idx (and idx-format
                        (save-excursion
                          (if (re-search-backward idx-format nil t)
                              (number-to-string
                               (1+ (string-to-number (match-string 1))))
                            "1"))))
              pnt-args eob-args)
          (when (listp pnt-format)
            (setq pnt-args (cdr pnt-format)
                  pnt-format (car pnt-format)))
          (when (listp eob-format)
            (setq eob-args (cdr eob-format)
                  eob-format (car eob-format)))
          (when pnt-format
            (when idx-format
              (setq pnt-format
                    (replace-regexp-in-string "%N" idx pnt-format t t)))
            (fosgit-rev-insert-format pnt-format rev pnt-args)
            (backward-delete-char 1))
          (when eob-format
            (when idx-format
              (setq eob-format
                    (replace-regexp-in-string "%N" idx eob-format t t)))
            (save-excursion
              (goto-char (point-max))
              (skip-syntax-backward ">s-")
              (beginning-of-line)
              (if (and comment-start (looking-at comment-start))
                  (while (looking-at comment-start)
                    (forward-line -1))
                (forward-line)
                (unless (= (current-column) 0)
                  (insert ?\n)))
              (insert ?\n)
              (fosgit-rev-insert-format eob-format rev eob-args)
              (backward-delete-char 1)))))
    (user-error "Revision stack is empty")))

(define-key git-commit-mode-map
  (kbd "C-c C-w") 'fosgit-pop-revision-stack)

(defun fosgit-copy-section-value ()
  "Save the value of the current section for later use.

Save the section value to the `kill-ring', and, provided that
the current section is a commit, branch, or tag section, push
the (referenced) revision to the `fosgit-revision-stack' for use
with `fosgit-pop-revision-stack'.

When the current section is a branch or a tag, and a prefix
argument is used, then save the revision at its tip to the
`kill-ring' instead of the reference name.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above."
  (interactive)
  (if (region-active-p)
      (copy-region-as-kill (mark) (point) 'region)
    (-when-let* ((section (fosgit-current-section))
                 (value (fosgit-section-value section)))
      (fosgit-section-case
        ((branch commit module-commit tag)
         (let ((default-directory default-directory) ref)
           (fosgit-section-case
             ((branch tag)
              (setq ref value))
             (module-commit
              (setq default-directory
                    (file-name-as-directory
                     (expand-file-name (fosgit-section-parent-value section)
                                       (fosgit-toplevel))))))
           (setq value (fosgit-rev-parse value))
           (push (list value default-directory) fosgit-revision-stack)
           (kill-new (message "%s" (or (and current-prefix-arg ref)
                                       value)))))
        (t (kill-new (message "%s" value)))))))

(defun fosgit-copy-buffer-revision ()
  "Save the revision of the current buffer for later use.

Save the revision shown in the current buffer to the `kill-ring'
and push it to the `fosgit-revision-stack'.

This command is mainly intended for use in `fosgit-revision-mode'
buffers, the only buffers where it is always unambiguous exactly
which revision should be saved.

Most other Fosgit buffers usually show more than one revision, in
some way or another, so this command has to select one of them,
and that choice might not always be the one you think would have
been the best pick.

In such buffers it is often more useful to save the value of
the current section instead, using `fosgit-copy-section-value'.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above."
  (interactive)
  (if (region-active-p)
      (copy-region-as-kill (mark) (point) 'region)
    (-when-let (rev (cond ((memq major-mode '(fosgit-cherry-mode
                                              fosgit-log-select-mode
                                              fosgit-reflog-mode
                                              fosgit-refs-mode
                                              fosgit-revision-mode
                                              fosgit-stash-mode
                                              fosgit-stashes-mode))
                           (car fosgit-refresh-args))
                          ((memq major-mode '(fosgit-diff-mode
                                              fosgit-log-mode))
                           (let ((r (caar fosgit-refresh-args)))
                             (if (string-match "\\.\\.\\.?\\(.+\\)" r)
                                 (match-string 1 r)
                               r)))
                          ((eq major-mode 'fosgit-status-mode) "HEAD")))
      (when (fosgit-rev-verify-commit rev)
        (setq rev (fosgit-rev-parse rev))
        (push (list rev default-directory) fosgit-revision-stack)
        (kill-new (message "%s" rev))))))

;;; fosgit.el ends soon

(defconst fosgit-font-lock-keywords
  (eval-when-compile
    `((,(concat "(\\(fosgit-define-section-jumper\\)\\_>"
                "[ \t'\(]*"
                "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
       (1 'font-lock-keyword-face)
       (2 'font-lock-function-name-face nil t))
      (,(concat "(" (regexp-opt '("fosgit-insert-section"
                                  "fosgit-section-case"
                                  "fosgit-section-when"
                                  "fosgit-bind-match-strings"
                                  "fosgit-with-temp-index"
                                  "fosgit-with-blob"
                                  "fosgit-with-toplevel") t)
                "\\_>")
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode fosgit-font-lock-keywords)

(defvar fosgit-version 'undefined
  "The version of Fosgit that you're using.
Use the function by the same name instead of this variable.")

;;;###autoload
(defun fosgit-version ()
  "Return the version of Fosgit currently in use.
When called interactive also show the used versions of Fosgit,
Git, and Emacs in the echo area."
  (interactive)
  (let ((fosgit-git-global-arguments nil)
        (toplib (or load-file-name buffer-file-name))
        debug)
    (unless (and toplib
                 (equal (file-name-nondirectory toplib) "fosgit.el"))
      (setq toplib (locate-library "fosgit.el")))
    (push toplib debug)
    (when toplib
      (let* ((topdir (file-name-directory toplib))
             (gitdir (expand-file-name
                      ".git" (file-name-directory
                              (directory-file-name topdir))))
             (static (expand-file-name "fosgit-version.el" topdir)))
        (or (progn
              (push 'repo debug)
              (when (and (file-exists-p gitdir)
                         ;; It is a repo, but is it the Fosgit repo?
                         (file-exists-p
                          (expand-file-name "../lisp/fosgit.el" gitdir)))
                (push t debug)
                ;; Inside the repo the version file should only exist
                ;; while running make.
                (unless noninteractive
                  (ignore-errors (delete-file static)))
                (setq fosgit-version
                      (let ((default-directory topdir))
                        (fosgit-git-string "describe" "--tags" "--dirty")))))
            (progn
              (push 'static debug)
              (when (file-exists-p static)
                (push t debug)
                (load-file static)
                fosgit-version))
            (when (featurep 'package)
              (push 'elpa debug)
              (ignore-errors
                (--when-let (assq 'fosgit package-alist)
                  (push t debug)
                  (setq fosgit-version
                        (and (fboundp 'package-desc-version)
                             (package-version-join
                              (package-desc-version (cadr it)))))))))))
    (if (stringp fosgit-version)
        (when (called-interactively-p 'any)
          (message "Fosgit %s, Git %s, Emacs %s"
                   (or fosgit-version "(unknown)")
                   (or (fosgit-git-version t) "(unknown)")
                   emacs-version))
      (setq debug (reverse debug))
      (setq fosgit-version 'error)
      (when fosgit-version
        (push fosgit-version debug))
      (message "Cannot determine Fosgit's version %S" debug))
    fosgit-version))

(defun fosgit-startup-asserts ()
  (let ((version (fosgit-git-version)))
    (when (and version
               (version< version fosgit--minimal-git)
               (not (equal (getenv "TRAVIS") "true")))
      (display-warning 'fosgit (format "\
Fosgit requires Git >= %s, you are using %s.

If this comes as a surprise to you, because you do actually have
a newer version installed, then that probably means that the
older version happens to appear earlier on the `$PATH'.  If you
always start Emacs from a shell, then that can be fixed in the
shell's init file.  If you start Emacs by clicking on an icon,
or using some sort of application launcher, then you probably
have to adjust the environment as seen by graphical interface.
For X11 something like ~/.xinitrc should work.

If you use Tramp to work inside remote Git repositories, then you
have to make sure a suitable Git is used on the remote machines
too.\n" fosgit--minimal-git version) :error)))
  (when (version< emacs-version fosgit--minimal-emacs)
    (display-warning 'fosgit (format "\
Fosgit requires Emacs >= %s, you are using %s.

If this comes as a surprise to you, because you do actually have
a newer version installed, then that probably means that the
older version happens to appear earlier on the `$PATH'.  If you
always start Emacs from a shell, then that can be fixed in the
shell's init file.  If you start Emacs by clicking on an icon,
or using some sort of application launcher, then you probably
have to adjust the environment as seen by graphical interface.
For X11 something like ~/.xinitrc should work.\n"
                                    fosgit--minimal-emacs emacs-version)
                     :error))
  (--each '((fosgit-log-edit  . git-commit)
            (git-commit-mode . git-commit)
            (git-rebase-mode . git-rebase))
    (when (or (featurep (car it)) (locate-library (symbol-name (car it))))
      (display-warning 'fosgit (format "%s has to be removed

Fosgit is no longer compatible with the library `%s',
which was used in earlier releases.  Please remove it, so that
Fosgit can use the successor `%s' without the obsolete
library getting in the way.  Then restart Emacs.\n"
                                      (car it)  (car it) (cdr it)) :error))))

(defvar fosgit--remotes-using-recent-git nil)

(defun fosgit-tramp-asserts (directory)
  (-when-let (remote (file-remote-p directory))
    (unless (member remote fosgit--remotes-using-recent-git)
      (-if-let (version (let ((default-directory directory))
                          (fosgit-git-version)))
          (if (version<= fosgit--minimal-git version)
              (push version fosgit--remotes-using-recent-git)
            (display-warning 'fosgit (format "\
Fosgit requires Git >= %s, but on %s the version is %s.

If multiple Git versions are installed on the host then the
problem might be that TRAMP uses the wrong executable.

First check the value of `fosgit-git-executable'.  Its value is
used when running git locally as well as when running it on a
remote host.  The default value is \"git\", except on Windows
where an absolute path is used for performance reasons.

If the value already is just \"git\" but TRAMP never-the-less
doesn't use the correct executable, then consult the info node
`(tramp)Remote programs'.\n" fosgit--minimal-git remote version) :error))
        (display-warning 'fosgit (format "\
Fosgit cannot find Git on %s.

First check the value of `fosgit-git-executable'.  Its value is
used when running git locally as well as when running it on a
remote host.  The default value is \"git\", except on Windows
where an absolute path is used for performance reasons.

If the value already is just \"git\" but TRAMP never-the-less
doesn't find the executable, then consult the info node
`(tramp)Remote programs'.\n" remote) :error)))))

(define-obsolete-function-alias 'global-fosgit-file-buffer-mode
  'global-fosgit-file-mode "Fosgit 2.3.0")

(define-obsolete-function-alias 'fosgit-insert-head-header
  'fosgit-insert-head-branch-header "Fosgit 2.4.0")

(define-obsolete-function-alias 'fosgit-insert-upstream-header
  'fosgit-insert-upstream-branch-header "Fosgit 2.4.0")

(define-obsolete-function-alias 'fosgit-insert-pull-branch-header
  'fosgit-insert-upstream-branch-header "Fosgit 2.4.0")

(provide 'fosgit)

(cl-eval-when (load eval)
  (require 'fosgit-sequence)
  (require 'fosgit-commit)
  (require 'fosgit-remote)
  (require 'fosgit-bisect)
  (require 'fosgit-stash)
  (require 'fosgit-blame)
  (unless (load "fosgit-autoloads" t t)
    (require 'fosgit-submodule)
    (require 'fosgit-ediff)
    (require 'fosgit-extras)
    (require 'git-rebase)))

(if after-init-time
    (progn (fosgit-startup-asserts)
           (fosgit-version))
  (add-hook 'after-init-hook #'fosgit-startup-asserts t)
  (add-hook 'after-init-hook #'fosgit-version t))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; fosgit.el ends here
