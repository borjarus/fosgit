;;; fosgit-diff.el --- inspect Git diffs  -*- lexical-binding: t -*-

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

;; This library implements support for looking at Git diffs and
;; commits.

;;; Code:

(require 'git-commit)
(require 'fosgit-core)

;; For `fosgit-diff-popup'
(declare-function fosgit-stash-show 'fosgit-stash)
;; For `fosgit-diff-visit-file'
(declare-function fosgit-dired-jump 'fosgit)
(declare-function fosgit-find-file-noselect 'fosgit)
(declare-function fosgit-status-internal 'fosgit)
;; For `fosgit-diff-wash-revision'
(declare-function fosgit-insert-tags-header 'fosgit)
;; For `fosgit-diff-while-committing'
(declare-function fosgit-commit-message-buffer 'fosgit)
;; For `fosgit-insert-revision-gravatar'
(defvar gravatar-size)
;; For `fosgit-show-commit' and `fosgit-diff-show-or-scroll'
(declare-function fosgit-blame-chunk-get 'fosgit-blame)
(declare-function fosgit-blame-mode 'fosgit-blame)
(defvar fosgit-blame-mode)
(defvar git-rebase-line)

(require 'diff-mode)
(require 'smerge-mode)

;;; Options
;;;; Diff Mode

(defgroup fosgit-diff nil
  "Inspect and manipulate Git diffs."
  :group 'fosgit-modes)

(custom-add-to-group 'fosgit-diff 'smerge-refine-ignore-whitespace
                     'custom-variable)

(defcustom fosgit-diff-mode-hook nil
  "Hook run after entering Fosgit-Diff mode."
  :group 'fosgit-diff
  :type 'hook)

(defcustom fosgit-diff-sections-hook
  '(fosgit-insert-diff
    fosgit-insert-xref-buttons)
  "Hook run to insert sections into a `fosgit-diff-mode' buffer."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-revision
  :type 'hook)

(defcustom fosgit-diff-expansion-threshold 1.0
  "After how many seconds not to expand anymore diffs.

Except in status buffers, diffs are usually start out fully
expanded.  Because that can take a long time, all diffs that
haven't been fontified during a refresh before the threshold
defined here are instead displayed with their bodies collapsed.

Note that this can cause sections that were previously expanded
to be collapsed.  So you should not pick a very low value here.

The hook function `fosgit-diff-expansion-threshold' has to be a
member of `fosgit-section-set-visibility-hook' for this option
to have any effect"
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-diff
  :type 'float)

(defcustom fosgit-diff-highlight-hunk-body t
  "Whether to highlight bodies of selected hunk sections.
This only has an effect if `fosgit-diff-highlight' is a
member of `fosgit-section-highlight-hook', which see."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-diff
  :type 'boolean)

(defcustom fosgit-diff-show-lines-boundary t
  "Whether to delimit hunk-internal region with thin lines.

When a hunk-internal region (used to stage just the lines that
fall into the region instead of the complete hunk) only covers
context lines, then these lines are the only visual indicator
for the region.  In character-only terminals it's not possible
to draw thin lines."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-diff
  :type 'boolean)

(defcustom fosgit-diff-refine-hunk nil
  "Whether to show word-granularity differences within diff hunks.

nil    never show fine differences.
t      show fine differences for the current diff hunk only.
`all'  show fine differences for all displayed diff hunks."
  :group 'fosgit-diff
  :safe (lambda (val) (memq val '(nil t all)))
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Current" t)
                 (const :tag "All" all)))

(put 'fosgit-diff-refine-hunk 'permanent-local t)

(defcustom fosgit-diff-paint-whitespace t
  "Specify where to highlight whitespace errors.
See `fosgit-highlight-trailing-whitespace',
`fosgit-highlight-indentation'.  The symbol t means in all diffs,
`status' means only in the status buffer, and nil means nowhere."
  :group 'fosgit-diff
  :safe (lambda (val) (memq val '(t nil status)))
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "In status buffer" status)))

(defcustom fosgit-diff-highlight-trailing t
  "Whether to highlight whitespace at the end of a line in diffs.
Used only when `fosgit-diff-paint-whitespace' is non-nil."
  :group 'fosgit-diff
  :safe 'booleanp
  :type 'boolean)

(defcustom fosgit-diff-highlight-indentation nil
  "Highlight the \"wrong\" indentation style.
Used only when `fosgit-diff-paint-whitespace' is non-nil.

The value is a list of cons cells.  The car is a regular
expression, and the cdr is the value that applies to repositories
whose directory matches the regular expression.  If more than one
element matches, then the *last* element in the list applies.
The default value should therefore come first in the list.

If the value is `tabs', highlight indentation with tabs.  If the
value is an integer, highlight indentation with at least that
many spaces.  Otherwise, highlight neither."
  :group 'fosgit-diff
  :type `(repeat (cons (string :tag "Directory regexp")
                       (choice (const :tag "Tabs" tabs)
                               (integer :tag "Spaces" :value ,tab-width)
                               (const :tag "Neither" nil)))))

;;;; Revision Mode

(defgroup fosgit-revision nil
  "Inspect and manipulate Git commits."
  :group 'fosgit-modes)

(defcustom fosgit-revision-mode-hook nil
  "Hook run after entering Fosgit-Revision mode."
  :group 'fosgit-revision
  :type 'hook)

(defcustom fosgit-revision-sections-hook
  '(fosgit-insert-revision-tag
    fosgit-insert-revision-headers
    fosgit-insert-revision-message
    fosgit-insert-revision-notes
    fosgit-insert-revision-diff
    fosgit-insert-xref-buttons)
  "Hook run to insert sections into a `fosgit-revision-mode' buffer."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-revision
  :type 'hook)

(defcustom fosgit-revision-headers-format "\
Author:     %aN <%aE>
AuthorDate: %ad
Commit:     %cN <%cE>
CommitDate: %cd
"
  "Format string used to insert headers in revision buffers.

All headers in revision buffers are inserted by the section
inserter `fosgit-insert-revision-headers'.  Some of the headers
are created by calling `git show --format=FORMAT' where FORMAT
is the format specified here.  Other headers are hard coded or
subject to option `fosgit-revision-insert-related-refs'."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-revision
  :type 'string)

(defcustom fosgit-revision-insert-related-refs t
  "Whether to show related refs in revision buffers."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-revision
  :type 'boolean)

(defcustom fosgit-revision-show-gravatars nil
  "Whether to show gravatar images in revision buffers.

If non-nil, then the value has to be a cons-cell which specifies
where the gravatar images for the author and/or the committer are
inserted inside the text that was previously inserted according
to `fosgit-revision-header-format'.

Both cells are regular expressions.  The car specifies where to
insert the author gravatar image.  The top halve of the image is
inserted right after the matched text, the bottom halve on the
next line at the same offset.  The cdr specifies where to insert
the committer image, accordingly.  Either the car or the cdr may
be nil."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-revision
  :type '(choice (const :tag "Don't show gravatars" nil)
                 (cons  :tag "Show gravatars"
                        (regexp :tag "Author regexp"    "^Author:     ")
                        (regexp :tag "Committer regexp" "^Commit:     "))))

(defcustom fosgit-revision-use-gravatar-kludge nil
  "Whether to work around a bug which affects display of gravatars.

Gravatar images are spliced into two halves which are then
displayed on separate lines.  On OS X the splicing has a bug in
some Emacs builds, which causes the top and bottom halves to be
interchanged.  Enabling this option works around this issue by
interchanging the halves once more, which cancels out the effect
of the bug.

See https://github.com/fosgit/fosgit/issues/2265
and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=7847."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-revision
  :type 'boolean)

;;; Faces

(defface fosgit-diff-file-heading
  '((t :weight bold))
  "Face for diff file headings."
  :group 'fosgit-faces)

(defface fosgit-diff-file-heading-highlight
  '((t :inherit (fosgit-diff-file-heading fosgit-section-highlight)))
  "Face for current diff file headings."
  :group 'fosgit-faces)

(defface fosgit-diff-file-heading-selection
  '((((class color) (background light))
     :inherit fosgit-diff-file-heading-highlight
     :foreground "salmon4")
    (((class color) (background dark))
     :inherit fosgit-diff-file-heading-highlight
     :foreground "LightSalmon3"))
  "Face for selected diff file headings."
  :group 'fosgit-faces)

(defface fosgit-diff-hunk-heading
  '((((class color) (background light))
     :background "grey80"
     :foreground "grey30")
    (((class color) (background dark))
     :background "grey25"
     :foreground "grey70"))
  "Face for diff hunk headings."
  :group 'fosgit-faces)

(defface fosgit-diff-hunk-heading-highlight
  '((((class color) (background light))
     :background "grey75"
     :foreground "grey30")
    (((class color) (background dark))
     :background "grey35"
     :foreground "grey70"))
  "Face for current diff hunk headings."
  :group 'fosgit-faces)

(defface fosgit-diff-hunk-heading-selection
  '((((class color) (background light))
     :inherit fosgit-diff-hunk-heading-highlight
     :foreground "salmon4")
    (((class color) (background dark))
     :inherit fosgit-diff-hunk-heading-highlight
     :foreground "LightSalmon3"))
  "Face for selected diff hunk headings."
  :group 'fosgit-faces)

(defface fosgit-diff-lines-heading
  '((((class color) (background light))
     :inherit fosgit-diff-hunk-heading-highlight
     :background "LightSalmon3")
    (((class color) (background dark))
     :inherit fosgit-diff-hunk-heading-highlight
     :foreground "grey80"
     :background "salmon4"))
  "Face for diff hunk heading when lines are marked."
  :group 'fosgit-faces)

(defface fosgit-diff-lines-boundary
  '((t :inherit fosgit-diff-lines-heading))
  "Face for boundary of marked lines in diff hunk."
  :group 'fosgit-faces)

(defface fosgit-diff-conflict-heading
  '((t :inherit fosgit-diff-hunk-heading))
  "Face for conflict markers."
  :group 'fosgit-faces)

(defface fosgit-diff-added
  '((((class color) (background light))
     :background "#ddffdd"
     :foreground "#22aa22")
    (((class color) (background dark))
     :background "#335533"
     :foreground "#ddffdd"))
  "Face for lines in a diff that have been added."
  :group 'fosgit-faces)

(defface fosgit-diff-removed
 '((((class color) (background light))
     :background "#ffdddd"
     :foreground "#aa2222")
    (((class color) (background dark))
     :background "#553333"
     :foreground "#ffdddd"))
  "Face for lines in a diff that have been removed."
  :group 'fosgit-faces)

(defface fosgit-diff-our
  '((t :inherit fosgit-diff-removed))
  "Face for lines in a diff for our side in a conflict."
  :group 'fosgit-faces)

(defface fosgit-diff-base
  '((((class color) (background light))
     :background "#ffffcc"
     :foreground "#aaaa11")
    (((class color) (background dark))
     :background "#555522"
     :foreground "#ffffcc"))
  "Face for lines in a diff for the base side in a conflict."
  :group 'fosgit-faces)

(defface fosgit-diff-their
  '((t :inherit fosgit-diff-added))
  "Face for lines in a diff for their side in a conflict."
  :group 'fosgit-faces)

(defface fosgit-diff-context
  '((((class color) (background light)) :foreground "grey50")
    (((class color) (background  dark)) :foreground "grey70"))
  "Face for lines in a diff that are unchanged."
  :group 'fosgit-faces)

(defface fosgit-diff-added-highlight
  '((((class color) (background light))
     :background "#cceecc"
     :foreground "#22aa22")
    (((class color) (background dark))
     :background "#336633"
     :foreground "#cceecc"))
  "Face for lines in a diff that have been added."
  :group 'fosgit-faces)

(defface fosgit-diff-removed-highlight
  '((((class color) (background light))
     :background "#eecccc"
     :foreground "#aa2222")
    (((class color) (background dark))
     :background "#663333"
     :foreground "#eecccc"))
  "Face for lines in a diff that have been removed."
  :group 'fosgit-faces)

(defface fosgit-diff-our-highlight
  '((t :inherit fosgit-diff-removed-highlight))
  "Face for lines in a diff for our side in a conflict."
  :group 'fosgit-faces)

(defface fosgit-diff-base-highlight
  '((((class color) (background light))
     :background "#eeeebb"
     :foreground "#aaaa11")
    (((class color) (background dark))
     :background "#666622"
     :foreground "#eeeebb"))
  "Face for lines in a diff for the base side in a conflict."
  :group 'fosgit-faces)

(defface fosgit-diff-their-highlight
  '((t :inherit fosgit-diff-added-highlight))
  "Face for lines in a diff for their side in a conflict."
  :group 'fosgit-faces)

(defface fosgit-diff-context-highlight
  '((((class color) (background light))
     :background "grey95"
     :foreground "grey50")
    (((class color) (background dark))
     :background "grey20"
     :foreground "grey70"))
  "Face for lines in a diff that have been removed."
  :group 'fosgit-faces)

(defface fosgit-diff-whitespace-warning
  '((t :inherit trailing-whitespace))
  "Face for highlighting whitespace errors added lines."
  :group 'fosgit-faces)

(defface fosgit-diffstat-added
  '((((class color) (background light)) :foreground "#22aa22")
    (((class color) (background  dark)) :foreground "#448844"))
  "Face for plus sign in diffstat."
  :group 'fosgit-faces)

(defface fosgit-diffstat-removed
  '((((class color) (background light)) :foreground "#aa2222")
    (((class color) (background  dark)) :foreground "#aa4444"))
  "Face for minus sign in diffstat."
  :group 'fosgit-faces)

;;; Commands

(defconst fosgit-diff-popup-common
  '(:variable fosgit-diff-arguments
    :man-page "git-diff"
    :options  ((?f "Limit to files" "-- " fosgit-read-files)
               (?u "Context lines"  "-U")
               (?m "Detect renames" "-M")
               (?c "Detect copies"  "-C")
               (?a "Diff algorithm" "--diff-algorithm="
                   fosgit-diff-select-algorithm))))

(defvar fosgit-diff-popup
  `(,@fosgit-diff-popup-common
    :switches ((?f "Show surrounding functions"     "--function-context")
               (?b "Ignore whitespace changes"      "--ignore-space-change")
               (?w "Ignore all whitespace"          "--ignore-all-space")
               (?x "Disallow external diff drivers" "--no-ext-diff")
               (?s "Show stats"                     "--stat"))
    :actions  ((?d "Dwim"          fosgit-diff-dwim)
               (?u "Diff unstaged" fosgit-diff-unstaged)
               (?c "Show commit"   fosgit-show-commit)
               (?r "Diff range"    fosgit-diff)
               (?s "Diff staged"   fosgit-diff-staged)
               (?t "Show stash"    fosgit-stash-show)
               (?p "Diff paths"    fosgit-diff-paths)
               (?w "Diff worktree" fosgit-diff-working-tree))
    :default-action fosgit-diff-dwim
    :max-action-columns 3))

(defvar fosgit-diff-refresh-popup
  `(,@fosgit-diff-popup-common
    :switches ((?f "Show surrounding functions"     "--function-context")
               (?b "Ignore whitespace changes"      "--ignore-space-change")
               (?w "Ignore all whitespace"          "--ignore-all-space")
               (?x "Disallow external diff drivers" "--no-ext-diff"))
    :actions  ((?g "Refresh"                fosgit-diff-refresh)
               (?t "Toggle hunk refinement" fosgit-diff-toggle-refine-hunk)
               (?s "Set defaults"           fosgit-diff-set-default-arguments) nil
               (?w "Save defaults"          fosgit-diff-save-default-arguments))
    :max-action-columns 2))

(defvar fosgit-diff-mode-refresh-popup
  `(,@fosgit-diff-popup-common
    :switches ((?f "Show surrounding functions"     "--function-context")
               (?b "Ignore whitespace changes"      "--ignore-space-change")
               (?w "Ignore all whitespace"          "--ignore-all-space")
               (?x "Disallow external diff drivers" "--no-ext-diff")
               (?s "Show stats"                     "--stat"))
    :actions  ((?g "Refresh"                fosgit-diff-refresh)
               (?t "Toggle hunk refinement" fosgit-diff-toggle-refine-hunk)
               (?s "Set defaults"           fosgit-diff-set-default-arguments)
               (?r "Switch range type"      fosgit-diff-switch-range-type)
               (?w "Save defaults"          fosgit-diff-save-default-arguments)
               (?f "Flip revisions"         fosgit-diff-flip-revs))
    :max-action-columns 2))

(defvar fosgit-revision-mode-refresh-popup
  `(,@fosgit-diff-popup-common
    :switches ((?f "Show surrounding functions"     "--function-context")
               (?b "Ignore whitespace changes"      "--ignore-space-change")
               (?w "Ignore all whitespace"          "--ignore-all-space")
               (?x "Disallow external diff drivers" "--no-ext-diff")
               (?s "Show stats"                     "--stat"))
    :actions  ((?g "Refresh"                fosgit-diff-refresh)
               (?t "Toggle hunk refinement" fosgit-diff-toggle-refine-hunk)
               (?s "Set defaults"           fosgit-diff-set-default-arguments) nil
               (?w "Save defaults"          fosgit-diff-save-default-arguments))
    :max-action-columns 2))

(fosgit-define-popup-keys-deferred 'fosgit-diff-popup)
(fosgit-define-popup-keys-deferred 'fosgit-diff-refresh-popup)
(fosgit-define-popup-keys-deferred 'fosgit-diff-mode-refresh-popup)
(fosgit-define-popup-keys-deferred 'fosgit-revision-mode-refresh-popup)

(defcustom fosgit-diff-arguments '("--stat" "--no-ext-diff")
  "The diff arguments used in buffers whose mode derives from `fosgit-diff-mode'."
  :group 'fosgit-diff
  :group 'fosgit-commands
  :type '(repeat (string :tag "Argument")))

(defcustom fosgit-diff-section-arguments '("--no-ext-diff")
  "The diff arguments used in buffers that show other things besides diffs."
  :group 'fosgit-diff
  :group 'fosgit-status
  :type '(repeat (string :tag "Argument")))

(defvar fosgit-diff-section-file-args nil)
(put 'fosgit-diff-section-file-args 'permanent-local t)
(put 'fosgit-diff-section-arguments 'permanent-local t)

(defun fosgit-diff-arguments (&optional refresh)
  (cond ((memq fosgit-current-popup '(fosgit-diff-popup fosgit-diff-refresh-popup))
         (fosgit-popup-export-file-args fosgit-current-popup-args))
        ((derived-mode-p 'fosgit-diff-mode)
         (list (nth 2 fosgit-refresh-args)
               (nth 3 fosgit-refresh-args)))
        (refresh
         (list fosgit-diff-section-arguments
               fosgit-diff-section-file-args))
        (t
         (-if-let (buffer (fosgit-mode-get-buffer 'fosgit-diff-mode))
             (with-current-buffer buffer
               (list (nth 2 fosgit-refresh-args)
                     (nth 3 fosgit-refresh-args)))
           (list (default-value 'fosgit-diff-arguments) nil)))))

(defun fosgit-diff-popup (arg)
  "Popup console for diff commands."
  (interactive "P")
  (let ((fosgit-diff-arguments
         ;; We cannot possibly know what suffix command the user is
         ;; about to invoke, so we also don't know from which buffer
         ;; we should get the current values.  However it is much
         ;; more likely that we will end up updating the diff buffer,
         ;; and we therefore use the value from that buffer.
         (-if-let (buffer (fosgit-mode-get-buffer 'fosgit-diff-mode))
             (with-current-buffer buffer
               (fosgit-popup-import-file-args (nth 2 fosgit-refresh-args)
                                             (nth 3 fosgit-refresh-args)))
           (default-value 'fosgit-diff-arguments))))
    (fosgit-invoke-popup 'fosgit-diff-popup nil arg)))

(defun fosgit-diff-refresh-popup (arg)
  "Popup console for changing diff arguments in the current buffer."
  (interactive "P")
  (let ((fosgit-diff-refresh-popup
         (pcase major-mode
           (`fosgit-revision-mode fosgit-revision-mode-refresh-popup)
           (`fosgit-diff-mode     fosgit-diff-mode-refresh-popup)
           (_                    fosgit-diff-refresh-popup)))
        (fosgit-diff-arguments
         (if (derived-mode-p 'fosgit-diff-mode)
             (fosgit-popup-import-file-args (nth 2 fosgit-refresh-args)
                                           (nth 3 fosgit-refresh-args))
           (fosgit-popup-import-file-args fosgit-diff-section-arguments
                                         fosgit-diff-section-file-args))))
    (fosgit-invoke-popup 'fosgit-diff-refresh-popup nil arg)))

(defun fosgit-diff-select-algorithm (&rest _ignore)
  (fosgit-read-char-case nil t
    (?d "[d]efault"   "default")
    (?m "[m]inimal"   "minimal")
    (?p "[p]atience"  "patience")
    (?h "[h]istogram" "histogram")))

;;;###autoload
(defun fosgit-diff-dwim (&optional args files)
  "Show changes for the thing at point."
  (interactive (fosgit-diff-arguments))
  (pcase (fosgit-diff--dwim)
    (`unstaged (fosgit-diff-unstaged args files))
    (`staged (fosgit-diff-staged nil args files))
    (`(commit . ,value)
     (fosgit-diff (format "%s^..%s" value value) args files))
    (`(stash  . ,value) (fosgit-stash-show value args))
    ((and range (pred stringp))
     (fosgit-diff range args files))
    (_
     (call-interactively #'fosgit-diff))))

(defun fosgit-diff--dwim ()
  "Return information for performing DWIM diff.

The information can be in three forms:
1. TYPE
   A symbol describing a type of diff where no additional information
   is needed to generate the diff.  Currently, this includes `staged'
   and `unstaged'.
2. (TYPE . VALUE)
   Like #1 but the diff requires additional information, which is
   given by VALUE.  Currently, this includes `commit' and `stash',
   where VALUE is the given commit or stash, respectively.
3. RANGE
   A string indicating a diff range.

If no DWIM context is found, nil is returned."
  (cond
   ((--when-let (fosgit-region-values 'commit 'branch)
      (deactivate-mark)
      (concat (car (last it)) ".." (car it))))
   (fosgit-buffer-refname
    (cons 'commit fosgit-buffer-refname))
   ((derived-mode-p 'fosgit-revision-mode)
    (cons 'commit (car fosgit-refresh-args)))
   ((derived-mode-p 'fosgit-diff-mode)
    (nth 0 fosgit-refresh-args))
   (t
    (fosgit-section-case
      ([* unstaged] 'unstaged)
      ([* staged] 'staged)
      (unpushed (fosgit-section-value it))
      (unpulled (fosgit-section-value it))
      (branch (let ((current (fosgit-get-current-branch))
                    (atpoint (fosgit-section-value it)))
                (if (equal atpoint current)
                    (--if-let (fosgit-get-upstream-branch)
                        (format "%s...%s" it current)
                      (if (fosgit-anything-modified-p)
                          current
                        (cons 'commit current)))
                  (format "%s..%s" atpoint current))))
      (commit (cons 'commit (fosgit-section-value it)))
      (stash (cons 'stash (fosgit-section-value it)))))))

(defun fosgit-diff-read-range-or-commit (prompt &optional secondary-default mbase)
  "Read range or revision with special diff range treatment.
If MBASE is non-nil, prompt for which rev to place at the end of
a \"revA...revB\" range.  Otherwise, always construct
\"revA..revB\" range."
  (--if-let (fosgit-region-values 'commit 'branch)
      (let ((revA (car (last it)))
            (revB (car it)))
        (deactivate-mark)
        (if mbase
            (let ((base (fosgit-git-string "merge-base" revA revB)))
              (cond
               ((string= (fosgit-rev-parse revA) base)
                (format "%s..%s" revA revB))
               ((string= (fosgit-rev-parse revB) base)
                (format "%s..%s" revB revA))
               (t
                (let ((main (fosgit-completing-read "View changes along"
                                                   (list revA revB)
                                                   nil t nil nil revB)))
                  (format "%s...%s"
                          (if (string= main revB) revA revB) main)))))
          (format "%s..%s" revA revB)))
    (fosgit-read-range prompt
                      (or (pcase (fosgit-diff--dwim)
                            (`(commit . ,value)
                             (format "%s^..%s" value value))
                            ((and range (pred stringp))
                             range))
                          secondary-default
                          (fosgit-get-current-branch)))))

(defun fosgit-diff-setup (rev-or-range const args files)
  (require 'fosgit)
  (fosgit-mode-setup #'fosgit-diff-mode rev-or-range const args files))

;;;###autoload
(defun fosgit-diff (rev-or-range &optional args files)
  "Show differences between two commits.

REV-OR-RANGE should be a range or a single revision.  If it is a
revision, then show changes in the working tree relative to that
revision.  If it is a range, but one side is omitted, then show
changes relative to `HEAD'.

If the region is active, use the revisions on the first and last
line of the region as the two sides of the range.  With a prefix
argument, instead of diffing the revisions, choose a revision to
view changes along, starting at the common ancestor of both
revisions (i.e., use a \"...\" range)."
  (interactive (cons (fosgit-diff-read-range-or-commit "Diff for range"
                                                      nil current-prefix-arg)
                     (fosgit-diff-arguments)))
  (fosgit-diff-setup rev-or-range nil args files))

;;;###autoload
(defun fosgit-diff-working-tree (&optional rev args files)
  "Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer."
  (interactive
   (cons (and current-prefix-arg
              (fosgit-read-branch-or-commit "Diff working tree and commit"))
         (fosgit-diff-arguments)))
  (fosgit-diff-setup (or rev "HEAD") nil args files))

;;;###autoload
(defun fosgit-diff-staged (&optional rev args files)
  "Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer."
  (interactive
   (cons (and current-prefix-arg
              (fosgit-read-branch-or-commit "Diff index and commit"))
         (fosgit-diff-arguments)))
  (fosgit-diff-setup rev (list "--cached") args files))

;;;###autoload
(defun fosgit-diff-unstaged (&optional args files)
  "Show changes between the working tree and the index."
  (interactive (fosgit-diff-arguments))
  (fosgit-diff-setup nil nil args files))

;;;###autoload
(defun fosgit-diff-while-committing (&optional args files)
  "While committing, show the changes that are about to be committed.
While amending, invoking the command again toggles between
showing just the new changes or all the changes that will
be committed."
  (interactive (fosgit-diff-arguments))
  (let ((toplevel (fosgit-toplevel))
        (diff-buf (fosgit-mode-get-buffer 'fosgit-diff-mode)))
    (if (fosgit-commit-message-buffer)
        (if (and (or ;; most likely an explicit amend
                     (not (fosgit-anything-staged-p))
                     ;; explicitly toggled from within diff
                     (and (eq (current-buffer) diff-buf)))
                 (or (not diff-buf)
                     (with-current-buffer diff-buf
                       (or ;; default to include last commit
                           (not (equal (fosgit-toplevel) toplevel))
                           ;; toggle to include last commit
                           (not (car fosgit-refresh-args))))))
            (fosgit-diff-while-amending args files)
          (fosgit-diff-staged nil args files))
      (user-error "No commit in progress"))))

(define-key git-commit-mode-map
  (kbd "C-c C-d") 'fosgit-diff-while-committing)

(defun fosgit-diff-while-amending (&optional args files)
  (fosgit-diff-setup "HEAD^" (list "--cached") args files))

;;;###autoload
(defun fosgit-diff-paths (a b)
  "Show changes between any two files on disk."
  (interactive (list (read-file-name "First file: " nil nil t)
                     (read-file-name "Second file: " nil nil t)))
  (fosgit-diff-setup nil (list "--no-index")
                    nil (list (expand-file-name a)
                              (expand-file-name b))))

;;;###autoload
(defun fosgit-show-commit (rev &optional args files module)
  "Show the revision at point.
If there is no revision at point or with a prefix argument prompt
for a revision."
  (interactive
   (let* ((mcommit (fosgit-section-when module-commit))
          (atpoint (or (and (bound-and-true-p fosgit-blame-mode)
                            (fosgit-blame-chunk-get :hash))
                       mcommit
                       (fosgit-branch-or-commit-at-point)
                       (fosgit-tag-at-point))))
     (nconc (cons (or (and (not current-prefix-arg) atpoint)
                      (fosgit-read-branch-or-commit "Show commit" atpoint))
                  (fosgit-diff-arguments))
            (and mcommit (list (fosgit-section-parent-value
                                (fosgit-current-section)))))))
  (require 'fosgit)
  (fosgit-with-toplevel
    (when module
      (setq default-directory
            (expand-file-name (file-name-as-directory module))))
    (unless (fosgit-rev-verify-commit rev)
      (user-error "%s is not a commit" rev))
    (-when-let (buffer (fosgit-mode-get-buffer 'fosgit-revision-mode))
      (with-current-buffer buffer
        (let ((prev (car fosgit-refresh-args)))
          (unless (equal rev prev)
            (dolist (child (cdr (fosgit-section-children fosgit-root-section)))
              (when (eq (fosgit-section-type child) 'file)
                (fosgit-section-cache-visibility child)))))))
    (fosgit-mode-setup #'fosgit-revision-mode rev nil args files)))

(defun fosgit-diff-refresh (args files)
  "Set the local diff arguments for the current buffer."
  (interactive (fosgit-diff-arguments t))
  (cond ((derived-mode-p 'fosgit-diff-mode)
         (setcdr (cdr fosgit-refresh-args) (list args files)))
        (t
         (setq-local fosgit-diff-section-arguments args)
         (setq-local fosgit-diff-section-file-args files)))
  (fosgit-refresh))

(defun fosgit-diff-set-default-arguments (args files)
  "Set the global diff arguments for the current buffer."
  (interactive (fosgit-diff-arguments t))
  (cond ((derived-mode-p 'fosgit-diff-mode)
         (customize-set-variable 'fosgit-diff-arguments args)
         (setcdr (cdr fosgit-refresh-args) (list args files)))
        (t
         (customize-set-variable 'fosgit-diff-section-arguments args)
         (kill-local-variable 'fosgit-diff-section-arguments)
         (kill-local-variable 'fosgit-diff-section-file-args)))
  (fosgit-refresh))

(defun fosgit-diff-save-default-arguments (args files)
  "Set and save the global diff arguments for the current buffer."
  (interactive (fosgit-diff-arguments t))
  (cond ((derived-mode-p 'fosgit-diff-mode)
         (customize-save-variable 'fosgit-diff-arguments args)
         (setcdr (cdr fosgit-refresh-args) (list args files)))
        (t
         (customize-save-variable 'fosgit-diff-section-arguments args)
         (kill-local-variable 'fosgit-diff-section-arguments)
         (kill-local-variable 'fosgit-diff-section-file-args)))
  (fosgit-refresh))

(defun fosgit-diff-switch-range-type ()
  "Convert diff range type.
Change \"revA..revB\" to \"revB...revA\", or vice versa."
  (interactive)
  (let ((range (car fosgit-refresh-args)))
    (if (and range
             (derived-mode-p 'fosgit-diff-mode)
             (string-match fosgit-range-re range))
        (progn
          (setcar fosgit-refresh-args
                  (concat (match-string 1 range)
                          (if (string= (match-string 2 range) "..")
                              "..."
                            "..")
                          (match-string 3 range)))
          (fosgit-refresh))
      (user-error "No range to change"))))

(defun fosgit-diff-flip-revs ()
  "Swap revisions in diff range.
Change \"revA..revB\" to \"revB..revA\"."
  (interactive)
  (let ((range (car fosgit-refresh-args)))
    (if (and range
             (derived-mode-p 'fosgit-diff-mode)
             (string-match fosgit-range-re range))
        (progn
          (setcar fosgit-refresh-args
                  (concat (match-string 3 range)
                          (match-string 2 range)
                          (match-string 1 range)))
          (fosgit-refresh))
      (user-error "No range to swap"))))

(defun fosgit-diff-less-context (&optional count)
  "Decrease the context for diff hunks by COUNT lines."
  (interactive "p")
  (fosgit-diff-set-context `(lambda (cur) (max 0 (- (or cur 0) ,count)))))

(defun fosgit-diff-more-context (&optional count)
  "Increase the context for diff hunks by COUNT lines."
  (interactive "p")
  (fosgit-diff-set-context `(lambda (cur) (+ (or cur 0) ,count))))

(defun fosgit-diff-default-context ()
  "Reset context for diff hunks to the default height."
  (interactive)
  (fosgit-diff-set-context #'ignore))

(defun fosgit-diff-set-context (fn)
  (let* ((def (--if-let (fosgit-get "diff.context") (string-to-number it) 3))
         (val (car (fosgit-diff-arguments t)))
         (arg (--first (string-match "^-U\\([0-9]+\\)?$" it) val))
         (num (--if-let (and arg (match-string 1 arg)) (string-to-number it) def))
         (val (delete arg val))
         (num (funcall fn num))
         (arg (and num (not (= num def)) (format "-U%i" num)))
         (val (if arg (cons arg val) val)))
    (if (derived-mode-p 'fosgit-diff-mode)
        (setcar (cddr fosgit-refresh-args) val)
      (setq fosgit-diff-section-arguments val)))
  (fosgit-refresh))

(defun fosgit-diff-context-p ()
  (--if-let (--first (string-match "^-U\\([0-9]+\\)$" it)
                     (car (fosgit-diff-arguments t)))
      (not (equal "-U0" it))
    t))

(defun fosgit-diff-toggle-refine-hunk (&optional style)
  "Turn diff-hunk refining on or off.

If hunk refining is currently on, then hunk refining is turned off.
If hunk refining is off, then hunk refining is turned on, in
`selected' mode (only the currently selected hunk is refined).

With a prefix argument, the \"third choice\" is used instead:
If hunk refining is currently on, then refining is kept on, but
the refining mode (`selected' or `all') is switched.
If hunk refining is off, then hunk refining is turned on, in
`all' mode (all hunks refined).

Customize variable `fosgit-diff-refine-hunk' to change the default mode."
  (interactive "P")
  (setq-local fosgit-diff-refine-hunk
              (if style
                  (if (eq fosgit-diff-refine-hunk 'all) t 'all)
                (not fosgit-diff-refine-hunk)))
  (fosgit-diff-update-hunk-refinement))

(defun fosgit-diff-visit-file (file &optional other-window force-worktree)
  "From a diff, visit the corresponding file at the appropriate position.

When the file is already being displayed in another window of the
same frame, then just select that window and adjust point.  With
a prefix argument also display in another window.

If the diff shows changes in the worktree, the index, or `HEAD',
then visit the actual file.  Otherwise when the diff is about
an older commit, then visit the respective blob using
`fosgit-find-file'.  Also see `fosgit-diff-visit-file-worktree'
which, as the name suggests always visits the actual file."
  (interactive (list (--if-let (fosgit-file-at-point)
                         (expand-file-name it)
                       (user-error "No file at point"))
                     current-prefix-arg))
  (if (file-accessible-directory-p file)
      (fosgit-diff-visit-directory file other-window)
    (let ((current (fosgit-current-section))
          (rev (cond (force-worktree nil)
                     ((derived-mode-p 'fosgit-revision-mode)
                      (car fosgit-refresh-args))
                     ((derived-mode-p 'fosgit-diff-mode)
                      (--when-let (car fosgit-refresh-args)
                        (and (string-match "\\.\\.\\([^.].*\\)?[ \t]*\\'" it)
                             (match-string 1 it))))))
          (unmerged-p (fosgit-anything-unmerged-p file))
          hunk line col buffer)
      (when (and rev (fosgit-rev-head-p rev))
        (setq rev nil))
      (setq hunk
            (pcase (fosgit-diff-scope)
              ((or `hunk `region) current)
              ((or `file `files)  (car (fosgit-section-children current)))
              (`list (car (fosgit-section-children
                           (car (fosgit-section-children current)))))))
      (when (and hunk
                 ;; Currently the `hunk' type is also abused for file
                 ;; mode changes.  Luckily such sections have no value.
                 (fosgit-section-value hunk))
        (setq line (fosgit-diff-hunk-line   hunk)
              col  (fosgit-diff-hunk-column hunk)))
      (setq buffer (if rev
                       (fosgit-find-file-noselect rev file)
                     (or (get-file-buffer file)
                         (find-file-noselect file))))
      (fosgit-display-file-buffer buffer)
      (with-current-buffer buffer
        (when line
          (goto-char (point-min))
          (forward-line (1- line))
          (when col
            (move-to-column col)))
        (when unmerged-p
          (smerge-start-session))
        (run-hooks 'fosgit-diff-visit-file-hook)))))

(defvar fosgit-display-file-buffer-function
  'fosgit-display-file-buffer-traditional
  "The function used by `fosgit-diff-visit-file' to display blob buffers.

Other commands such as `fosgit-find-file' do not use this
function.  Instead they use high-level functions to select the
window to be used to display the buffer.  This variable and the
related functions are an experimental feature and should be
treated as such.")

(defun fosgit-display-file-buffer (buffer)
  (funcall fosgit-display-file-buffer-function buffer))

(defun fosgit-display-file-buffer-traditional (buffer)
  (if (or current-prefix-arg (get-buffer-window buffer))
      (pop-to-buffer buffer)
    (switch-to-buffer buffer)))

(defun fosgit-diff-visit-file-worktree (file &optional other-window)
  "From a diff, visit the corresponding file at the appropriate position.

When the file is already being displayed in another window of the
same frame, then just select that window and adjust point.  With
a prefix argument also display in another window.

The actual file in the worktree is visited. The positions in the
hunk headers get less useful the \"older\" the changes are, and
as a result, jumping to the appropriate position gets less
reliable.

Also see `fosgit-diff-visit-file' which visits the respective
blob, unless the diff shows changes in the worktree, the index,
or `HEAD'."
  (interactive (list (or (fosgit-file-at-point)
                         (user-error "No file at point"))
                     current-prefix-arg))
  (fosgit-diff-visit-file file other-window t))

(defun fosgit-diff-hunk-line (section)
  (let* ((value  (fosgit-section-value section))
         (prefix (- (length value) 2))
         (cpos   (marker-position (fosgit-section-content section)))
         (stop   (line-number-at-pos))
         (cstart (save-excursion (goto-char cpos) (line-number-at-pos)))
         (line   (car (last value))))
    (string-match "^\\+\\([0-9]+\\)" line)
    (setq line (string-to-number (match-string 1 line)))
    (when (> cstart stop)
      (save-excursion
        (goto-char cpos)
        (re-search-forward "^[-+]")
        (setq stop (line-number-at-pos))))
    (save-excursion
      (goto-char cpos)
      (while (< (line-number-at-pos) stop)
        (unless (string-match-p
                 "-" (buffer-substring (point) (+ (point) prefix)))
          (cl-incf line))
        (forward-line)))
    line))

(defun fosgit-diff-hunk-column (section)
  (if (or (< (point) (fosgit-section-content section))
          (save-excursion (beginning-of-line) (looking-at-p "-")))
      0
    (max 0 (- (+ (current-column) 2)
              (length (fosgit-section-value section))))))

(defun fosgit-diff-visit-directory (directory &optional other-window)
  (if (equal (fosgit-toplevel directory)
             (fosgit-toplevel))
      (fosgit-dired-jump other-window)
    (let ((display-buffer-overriding-action
           (if other-window
               '(nil (inhibit-same-window t))
             '(display-buffer-same-window))))
      (fosgit-status-internal directory))))

(defun fosgit-diff-show-or-scroll-up ()
  "Update the commit or diff buffer for the thing at point.

Either show the commit or stash at point in the appropriate
buffer, or if that buffer is already being displayed in the
current frame and contains information about that commit or
stash, then instead scroll the buffer up.  If there is no
commit or stash at point, then prompt for a commit."
  (interactive)
  (fosgit-diff-show-or-scroll 'scroll-up))

(defun fosgit-diff-show-or-scroll-down ()
  "Update the commit or diff buffer for the thing at point.

Either show the commit or stash at point in the appropriate
buffer, or if that buffer is already being displayed in the
current frame and contains information about that commit or
stash, then instead scroll the buffer down.  If there is no
commit or stash at point, then prompt for a commit."
  (interactive)
  (fosgit-diff-show-or-scroll 'scroll-down))

(defun fosgit-diff-show-or-scroll (fn)
  (let (rev cmd buf win)
    (cond
     (fosgit-blame-mode
      (setq rev (fosgit-blame-chunk-get :hash)
            cmd 'fosgit-show-commit
            buf (fosgit-mode-get-buffer 'fosgit-revision-mode)))
     ((derived-mode-p 'git-rebase-mode)
      (save-excursion
        (goto-char (line-beginning-position))
        (--if-let (and (looking-at git-rebase-line)
                       (match-string 2))
            (setq rev it
                  cmd 'fosgit-show-commit
                  buf (fosgit-mode-get-buffer 'fosgit-revision-mode))
          (user-error "No commit on this line"))))
     (t
      (fosgit-section-case
        ((commit branch)
         (setq rev (fosgit-section-value it)
               cmd 'fosgit-show-commit
               buf (fosgit-mode-get-buffer 'fosgit-revision-mode)))
        (stash
         (setq rev (fosgit-section-value it)
               cmd 'fosgit-stash-show
               buf (fosgit-mode-get-buffer 'fosgit-diff-mode))))))
    (if rev
        (if (and buf
                 (setq win (get-buffer-window buf))
                 (with-current-buffer buf
                   (equal (if (eq cmd 'fosgit-stash-show)
                              (concat rev "^2^.." rev)
                            rev)
                          (car fosgit-refresh-args))))
            (with-selected-window win
              (condition-case nil
                  (funcall fn)
                (error
                 (goto-char (pcase fn
                              (`scroll-up   (point-min))
                              (`scroll-down (point-max)))))))
          (let ((fosgit-display-buffer-noselect t))
            (if (eq cmd 'fosgit-show-commit)
                (apply #'fosgit-show-commit rev (fosgit-diff-arguments))
              (funcall cmd rev))))
      (call-interactively #'fosgit-show-commit))))

;;; Diff Mode

(defvar fosgit-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map fosgit-mode-map)
    (define-key map "\C-c\C-d" 'fosgit-diff-while-committing)
    (define-key map "\C-c\C-b" 'fosgit-go-backward)
    (define-key map "\C-c\C-f" 'fosgit-go-forward)
    (define-key map "\s" 'scroll-up)
    (define-key map "\d" 'scroll-down)
    (define-key map "j" 'fosgit-jump-to-diffstat-or-diff)
    map)
  "Keymap for `fosgit-diff-mode'.")

(define-derived-mode fosgit-diff-mode fosgit-mode "Fosgit Diff"
  "Mode for looking at a Git diff.

This mode is documented in info node `(fosgit)Diff buffer'.

\\<fosgit-mode-map>\
Type \\[fosgit-refresh] to refresh the current buffer.
Type \\[fosgit-section-toggle] to expand or hide the section at point.
Type \\[fosgit-visit-thing] to visit the hunk or file at point.

Staging and applying changes is documented in info node
`(fosgit)Staging and unstaging' and info node `(fosgit)Applying'.

\\<fosgit-hunk-section-map>Type \
\\[fosgit-apply] to apply the change at point, \
\\[fosgit-stage] to stage,
\\[fosgit-unstage] to unstage, \
\\[fosgit-discard] to discard, or \
\\[fosgit-reverse] to reverse it.

\\{fosgit-diff-mode-map}"
  :group 'fosgit-diff
  (hack-dir-local-variables-non-file-buffer))

(defun fosgit-diff-refresh-buffer (rev-or-range const _args files)
  "Refresh the current `fosgit-diff-mode' buffer.

In such buffers the buffer-local value of `fosgit-refresh-args'
has the same form as the arguments of this function.  The value
is set in `fosgit-mode-setup'."
  (setq header-line-format
        (propertize
         (if (member "--no-index" const)
             (apply #'format " Differences between %s and %s" files)
           (concat (if rev-or-range
                       (if (string-match-p "\\.\\." rev-or-range)
                           (format " Changes in %s" rev-or-range)
                         (format " Changes from %s to working tree" rev-or-range))
                     (if (member "--cached" const)
                         " Staged changes"
                       " Unstaged changes"))
                   (pcase (length files)
                     (0)
                     (1 (concat " in file " (car files)))
                     (_ (concat " in files "
                                (mapconcat #'identity files ", "))))))
         'face 'fosgit-header-line))
  (fosgit-insert-section (diffbuf)
    (run-hook-with-args 'fosgit-diff-sections-hook rev-or-range)))

(defun fosgit-insert-diff (rev-or-range)
  "Insert the diff into this `fosgit-diff-mode' buffer."
  (fosgit-git-wash #'fosgit-diff-wash-diffs
    "diff" rev-or-range "-p" "--no-prefix"
    (and (member "--stat" (nth 2 fosgit-refresh-args)) "--numstat")
    (nth 1 fosgit-refresh-args)
    (nth 2 fosgit-refresh-args) "--"
    (nth 3 fosgit-refresh-args)))

(defvar fosgit-file-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [C-return] 'fosgit-diff-visit-file-worktree)
    (define-key map "\C-j"     'fosgit-diff-visit-file-worktree)
    (define-key map [remap fosgit-visit-thing]      'fosgit-diff-visit-file)
    (define-key map [remap fosgit-delete-thing]     'fosgit-discard)
    (define-key map [remap fosgit-revert-no-commit] 'fosgit-reverse)
    (define-key map "a"  'fosgit-apply)
    (define-key map "C"  'fosgit-commit-add-log)
    (define-key map "K"  'fosgit-file-untrack)
    (define-key map "R"  'fosgit-file-rename)
    (define-key map "s"  'fosgit-stage)
    (define-key map "u"  'fosgit-unstage)
    map)
  "Keymap for `file' sections.")

(defvar fosgit-hunk-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [C-return] 'fosgit-diff-visit-file-worktree)
    (define-key map "\C-j"     'fosgit-diff-visit-file-worktree)
    (define-key map [remap fosgit-visit-thing]      'fosgit-diff-visit-file)
    (define-key map [remap fosgit-delete-thing]     'fosgit-discard)
    (define-key map [remap fosgit-revert-no-commit] 'fosgit-reverse)
    (define-key map "a"  'fosgit-apply)
    (define-key map "C"  'fosgit-commit-add-log)
    (define-key map "s"  'fosgit-stage)
    (define-key map "u"  'fosgit-unstage)
    map)
  "Keymap for `hunk' sections.")

(defconst fosgit-diff-headline-re
  (concat "^\\(@@@?\\|diff\\|Submodule\\|"
          "\\* Unmerged path\\|merged\\|changed in both\\|"
          "added in remote\\|removed in remote\\)"))

(defconst fosgit-diff-statline-re
  (concat "^ ?"
          "\\(.*\\)"     ; file
          "\\( +| +\\)"  ; separator
          "\\([0-9]+\\|Bin\\(?: +[0-9]+ -> [0-9]+ bytes\\)?$\\) ?"
          "\\(\\+*\\)"   ; add
          "\\(-*\\)$"))  ; del

(defconst fosgit-diff-submodule-re
  (concat "^Submodule \\([^ ]+\\) \\(?:"
          "\\([^ ]+ (new submodule)\\)\\|"
          "\\([^ ]+ (submodule deleted)\\)\\|"
          "\\(contains \\(?:modified\\|untracked\\) content\\)\\|"
          "\\([^ :]+\\)\\( (rewind)\\)?:\\)$"))

(defun fosgit-diff-wash-diffs (args &optional limit)
  (when (member "--stat" args)
    (fosgit-diff-wash-diffstat))
  (when (re-search-forward fosgit-diff-headline-re limit t)
    (goto-char (line-beginning-position))
    (fosgit-wash-sequence (apply-partially 'fosgit-diff-wash-diff args))
    (insert ?\n)))

(defun fosgit-jump-to-diffstat-or-diff ()
  "Jump to the diffstat or diff.
When point is on a file inside the diffstat section, then jump
to the respective diff section, otherwise jump to the diffstat
section or a child thereof."
  (interactive)
  (--if-let (fosgit-get-section
             (append (fosgit-section-case
                       ([file diffstat] `((file . ,(fosgit-section-value it))))
                       (file `((file . ,(fosgit-section-value it)) (diffstat)))
                       (t '((diffstat))))
                     (fosgit-section-ident fosgit-root-section)))
      (fosgit-section-goto it)
    (user-error "No diffstat in this buffer")))

(defun fosgit-diff-wash-diffstat ()
  (let (heading (beg (point)))
    (when (re-search-forward "^ ?\\([0-9]+ +files? change[^\n]*\n\\)" nil t)
      (setq heading (match-string 1))
      (fosgit-delete-match)
      (goto-char beg)
      (fosgit-insert-section it (diffstat)
        (insert (propertize heading 'face 'fosgit-diff-file-heading))
        (fosgit-insert-heading)
        (let (files)
          (while (looking-at "^[-0-9]+\t[-0-9]+\t\\(.+\\)$")
            (push (fosgit-decode-git-path (match-string 1)) files)
            (fosgit-delete-line))
          (setq files (nreverse files))
          (while (looking-at fosgit-diff-statline-re)
            (fosgit-bind-match-strings (file sep cnt add del) nil
              (fosgit-delete-line)
              (when (string-match " +$" file)
                (setq sep (concat (match-string 0 file) sep))
                (setq file (substring file 0 (match-beginning 0))))
              (let ((le (length file)) ld)
                (setq file (fosgit-decode-git-path file))
                (setq ld (length file))
                (when (> le ld)
                  (setq sep (concat (make-string (- le ld) ?\s) sep))))
              (fosgit-insert-section (file (pop files))
                (insert (propertize file 'face 'fosgit-filename) sep cnt " ")
                (when add
                  (insert (propertize add 'face 'fosgit-diffstat-added)))
                (when del
                  (insert (propertize del 'face 'fosgit-diffstat-removed)))
                (insert "\n")))))
        (if (looking-at "^$") (forward-line) (insert "\n"))))))

(defun fosgit-diff-wash-diff (args)
  (cond
   ((looking-at fosgit-diff-submodule-re)
    (fosgit-diff-wash-submodule))
   ((looking-at "^\\* Unmerged path \\(.*\\)")
    (let ((file (fosgit-decode-git-path (match-string 1))))
      (fosgit-delete-line)
      (unless (and (derived-mode-p 'fosgit-status-mode)
                   (not (member "--cached" args)))
        (fosgit-insert-section (file file)
          (insert (propertize
                   (format "unmerged   %s%s" file
                           (pcase (cddr (car (fosgit-file-status file)))
                             (`(?D ?D) " (both deleted)")
                             (`(?D ?U) " (deleted by us)")
                             (`(?U ?D) " (deleted by them)")
                             (`(?A ?A) " (both added)")
                             (`(?A ?U) " (added by us)")
                             (`(?U ?A) " (added by them)")
                             (`(?U ?U) "")))
                   'face 'fosgit-diff-file-heading))
          (insert ?\n))))
    t)
   ((looking-at (concat "^\\(merged\\|changed in both\\|"
                        "added in remote\\|removed in remote\\)"))
    (let ((status (pcase (match-string 1)
                    ("merged" "merged")
                    ("changed in both" "conflict")
                    ("added in remote" "new file")
                    ("removed in remote" "deleted")))
          file orig base modes)
      (fosgit-delete-line)
      (while (looking-at
              "^  \\([^ ]+\\) +[0-9]\\{6\\} \\([a-z0-9]\\{40\\}\\) \\(.+\\)$")
        (fosgit-bind-match-strings (side _blob name) nil
          (pcase side
            ("result" (setq file name))
            ("our" (setq orig name))
            ("their" (setq file name))
            ("base" (setq base name))))
        (fosgit-delete-line))
      (when orig (setq orig (fosgit-decode-git-path orig)))
      (when file (setq file (fosgit-decode-git-path file)))
      (fosgit-diff-insert-file-section (or file base) orig status modes nil)))
   ((looking-at
     "^diff --\\(?:\\(git\\) \\(?:\\(.+?\\) \\2\\)?\\|\\(cc\\|combined\\) \\(.+\\)\\)")
    (let ((status (cond ((equal (match-string 1) "git")        "modified")
                        ((derived-mode-p 'fosgit-revision-mode) "resolved")
                        (t                                     "unmerged")))
          (file (or (match-string 2) (match-string 4)))
          (beg (point))
          orig header modes)
      (save-excursion
        (forward-line 1)
        (setq header (buffer-substring
                      beg (if (re-search-forward fosgit-diff-headline-re nil t)
                              (match-beginning 0)
                            (point-max)))))
      (fosgit-delete-line)
      (while (not (or (eobp) (looking-at fosgit-diff-headline-re)))
        (if (looking-at "^old mode \\([^\n]+\\)\nnew mode \\([^\n]+\\)\n")
            (progn (setq modes (match-string 0))
                   (fosgit-delete-match))
          (cond
           ((looking-at "^--- \\([^/].*?\\)\t?$") ; i.e. not /dev/null
            (setq orig (match-string 1)))
           ((looking-at "^\\+\\+\\+ \\([^/].*?\\)\t?$")
            (setq file (match-string 1)))
           ((looking-at "^\\(copy\\|rename\\) from \\(.+\\)$")
            (setq orig (match-string 2)))
           ((looking-at "^\\(copy\\|rename\\) to \\(.+\\)$")
            (setq file (match-string 2))
            (setq status (if (equal (match-string 1) "copy") "new file" "renamed")))
           ((looking-at "^\\(new file\\|deleted\\)")
            (setq status (match-string 1))))
          (fosgit-delete-line)))
      (when orig
        (setq orig (fosgit-decode-git-path orig)))
      (setq file (fosgit-decode-git-path file))
      ;; KLUDGE `git-log' ignores `--no-prefix' when `-L' is used.
      (when (derived-mode-p 'fosgit-log-mode)
        (setq file (substring file 2))
        (when orig
          (setq orig (substring orig 2))))
      (fosgit-diff-insert-file-section file orig status modes header)))))

(defun fosgit-diff-insert-file-section (file orig status modes header)
  (fosgit-insert-section section
    (file file (or (equal status "deleted")
                   (derived-mode-p 'fosgit-status-mode)))
    (insert (propertize (format "%-10s %s\n" status
                                (if (or (not orig) (equal orig file))
                                    file
                                  (format "%s -> %s" orig file)))
                        'face 'fosgit-diff-file-heading))
    (fosgit-insert-heading)
    (unless (equal orig file)
      (setf (fosgit-section-source section) orig))
    (setf (fosgit-section-diff-header section) header)
    (when modes
      (fosgit-insert-section (hunk)
        (insert modes)))
    (fosgit-wash-sequence #'fosgit-diff-wash-hunk)))

(defun fosgit-diff-wash-submodule ()
  (fosgit-bind-match-strings (module new deleted dirty range rewind) nil
    (fosgit-delete-line)
    (when (and dirty
               (looking-at fosgit-diff-submodule-re)
               (string= (match-string 1) module))
      (setq range (match-string 5))
      (fosgit-delete-line))
    (while (looking-at "^  \\([<>]\\) \\(.+\\)$")
      (fosgit-delete-line))
    (if range
        (let ((default-directory
                (file-name-as-directory
                 (expand-file-name module (fosgit-toplevel)))))
          (setf (fosgit-section-value
                 (fosgit-insert-section (file module t)
                   (fosgit-insert-heading
                     (concat (propertize (concat "modified   " module)
                                         'face 'fosgit-diff-file-heading)
                             " ("
                             (if rewind "rewind" "new commits")
                             (and dirty ", modified content")
                             ")"))
                   (unless rewind
                     (fosgit-git-wash
                         (apply-partially 'fosgit-log-wash-log 'module)
                       "log" "--oneline" "--left-right" range)
                     (delete-char -1))))
                module))
      (fosgit-insert-section (file module)
        (insert (propertize (if new
                                (concat "new module " module)
                              (concat "modified   " module))
                            'face 'fosgit-diff-file-heading))
        (cond (dirty   (insert " (modified content)"))
              (deleted (insert " (deleted submodule)")))
        (insert ?\n)))))

(defun fosgit-diff-wash-hunk ()
  (when (looking-at "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")
    (let ((heading (match-string 0))
          (value (cons (match-string 2) (split-string (match-string 1)))))
      (fosgit-delete-line)
      (fosgit-insert-section it (hunk value)
        (insert (propertize (concat heading "\n") 'face 'fosgit-diff-hunk-heading))
        (fosgit-insert-heading)
        (while (not (or (eobp) (looking-at "^[^-+\s\\]")))
          (forward-line))
        (setf (fosgit-section-end it) (point))
        (setf (fosgit-section-washer it) #'fosgit-diff-paint-hunk)))
    t))

(defun fosgit-diff-expansion-threshold (section)
  "Keep new diff sections collapsed if washing takes too long."
  (and (memq (fosgit-section-type section) '(file))
       (> (float-time (time-subtract (current-time) fosgit-refresh-start-time))
          fosgit-diff-expansion-threshold)
       'hide))

;;; Revision Mode

(define-derived-mode fosgit-revision-mode fosgit-diff-mode "Fosgit Rev"
  "Mode for looking at a Git commit.

This mode is documented in info node `(fosgit)Revision buffer'.

\\<fosgit-mode-map>\
Type \\[fosgit-refresh] to refresh the current buffer.
Type \\[fosgit-section-toggle] to expand or hide the section at point.
Type \\[fosgit-visit-thing] to visit the hunk or file at point.

Staging and applying changes is documented in info node
`(fosgit)Staging and unstaging' and info node `(fosgit)Applying'.

\\<fosgit-hunk-section-map>Type \
\\[fosgit-apply] to apply the change at point, \
\\[fosgit-stage] to stage,
\\[fosgit-unstage] to unstage, \
\\[fosgit-discard] to discard, or \
\\[fosgit-reverse] to reverse it.

\\{fosgit-revision-mode-map}"
  :group 'fosgit-revision
  (hack-dir-local-variables-non-file-buffer))

(defun fosgit-revision-refresh-buffer (rev __const _args _files)
  (setq header-line-format
        (propertize (format " %s %s" (capitalize (fosgit-object-type rev)) rev)
                    'face 'fosgit-header-line))
  (fosgit-insert-section (commitbuf)
    (run-hook-with-args 'fosgit-revision-sections-hook rev)))

(defun fosgit-insert-revision-diff (rev)
  "Insert the diff into this `fosgit-revision-mode' buffer."
  ;; Before v2.2.0, "--format=" did not mean "no output".
  ;; Instead the default format was used.  So use "--format=%n"
  ;; and then delete the empty lines.
  (fosgit-git-wash (lambda (args)
                    (delete-region (point) (progn (forward-line 3) (point)))
                    (fosgit-diff-wash-diffs args))
    "show" "-p" "--cc" "--format=%n" "--no-prefix"
    (and (member "--stat" (nth 2 fosgit-refresh-args)) "--numstat")
    (nth 2 fosgit-refresh-args) (concat rev "^{commit}") "--"
    (nth 3 fosgit-refresh-args)))

(defun fosgit-insert-revision-tag (rev)
  "Insert tag message and headers into a revision buffer.
This function only inserts anything when `fosgit-show-commit' is
called with a tag as argument, when that is called with a commit
or a ref which is not a branch, then it inserts nothing."
  (when (equal (fosgit-object-type rev) "tag")
    (fosgit-insert-section (taginfo)
      (let ((beg (point)))
        (fosgit-git-insert "cat-file" "tag" rev)
        (goto-char beg)
        (forward-line 3)
        (delete-region beg (point)))
      (looking-at "^tagger \\([^<]+\\) <\\([^>]+\\)")
      (let ((heading (format "Tagger: %s <%s>"
                             (match-string 1)
                             (match-string 2))))
        (fosgit-delete-line)
        (insert (propertize heading 'face 'fosgit-section-secondary-heading)))
      (fosgit-insert-heading)
      (goto-char (point-max))
      (insert ?\n))))

(defun fosgit-insert-revision-message (rev)
  "Insert the commit message into a revision buffer."
  (fosgit-insert-section (message)
    (let ((beg (point)))
      (fosgit-rev-insert-format "%B" rev)
      (if (= (point) (+ beg 2))
          (progn (backward-delete-char 2)
                 (insert "(no message)\n"))
        (goto-char beg)
        (forward-line)
        (put-text-property beg (point) 'face 'fosgit-section-secondary-heading)
        (fosgit-insert-heading)
        (goto-char (point-max))))))

(defun fosgit-insert-revision-notes (rev)
  "Insert commit notes into a revision buffer."
  (fosgit-insert-section (notes)
    (let ((beg (point)))
      (fosgit-git-insert "notes" "show" rev)
      (if (= (point) beg)
          (fosgit-cancel-section)
        (goto-char beg)
        (forward-line)
        (put-text-property beg (point) 'face 'fosgit-section-secondary-heading)
        (fosgit-insert-heading)
        (goto-char (point-max))
        (insert ?\n)))))

(defun fosgit-insert-revision-headers (rev)
  "Insert headers about the commit into a revision buffer."
  (fosgit-insert-section (headers)
    ;; Before v2.2.0, "%D" was not supported.
    (--when-let (fosgit-rev-format "%d" rev "--decorate=full")
      (insert (fosgit-format-ref-labels (substring it 2 -1)) ?\s))
    (insert (propertize (fosgit-rev-parse (concat rev "^{commit}"))
                        'face 'fosgit-hash))
    (fosgit-insert-heading)
    (let ((beg (point)))
      (fosgit-rev-insert-format fosgit-revision-headers-format rev)
      (fosgit-insert-revision-gravatars rev beg))
    (when fosgit-revision-insert-related-refs
      (dolist (parent (fosgit-commit-parents rev))
        (fosgit-insert-section (commit parent)
          (let ((line (fosgit-rev-format "%h %s" parent)))
            (string-match "^\\([^ ]+\\) \\(.*\\)" line)
            (fosgit-bind-match-strings (hash msg) line
              (insert "Parent:     ")
              (insert (propertize hash 'face 'fosgit-hash))
              (insert " " msg "\n")))))
      (-when-let (merged (fosgit-list-merged-branches rev))
        (insert "Merged:    ")
        (let (branch)
          (while (and (< (+ (- (point) (line-beginning-position))
                            (length (car merged)) 9)
                         (window-width))
                      (setq branch (pop merged)))
            (insert ?\s)
            (fosgit-insert-section (branch branch)
              (insert (propertize branch 'face 'fosgit-branch-local)))))
        (when merged
          (insert (format " (%s more)" (length merged))))
        (insert ?\n))
      (-when-let (containing (fosgit-list-containing-branches rev))
        (insert "Containing:")
        (let (branch)
          (while (and (< (+ (- (point) (line-beginning-position))
                            (length (car containing)) 9)
                         (window-width))
                      (setq branch (pop containing)))
            (insert ?\s)
            (fosgit-insert-section (branch branch)
              (insert (propertize branch 'face 'fosgit-branch-local)))))
        (when containing
          (insert (format " (%s more)" (length containing))))
        (insert ?\n))
      (-when-let (follows (fosgit-get-current-tag rev t))
        (let ((tag (car  follows))
              (cnt (cadr follows)))
          (fosgit-insert-section (tag tag)
            (insert (format "Follows:    %s (%s)\n"
                            (propertize tag 'face 'fosgit-tag)
                            (propertize (number-to-string cnt)
                                        'face 'fosgit-branch-local))))))
      (-when-let (precedes (fosgit-get-next-tag rev t))
        (let ((tag (car  precedes))
              (cnt (cadr precedes)))
          (fosgit-insert-section (tag tag)
            (insert (format "Precedes:   %s (%s)\n"
                            (propertize tag 'face 'fosgit-tag)
                            (propertize (number-to-string cnt)
                                        'face 'fosgit-tag))))))
      (insert ?\n))))

(defun fosgit-insert-revision-gravatars (rev beg)
  (when (and fosgit-revision-show-gravatars (window-system))
    (require 'gravatar)
    (fosgit-insert-revision-gravatar beg (fosgit-rev-format "%aE" rev)
                                    (car fosgit-revision-show-gravatars))
    (fosgit-insert-revision-gravatar beg (fosgit-rev-format "%cE" rev)
                                    (cdr fosgit-revision-show-gravatars))
    (goto-char (point-max))))

(defun fosgit-insert-revision-gravatar (beg email regexp)
  (when (and email (goto-char beg) (re-search-forward regexp nil t))
    (ignore-errors
      (let* ((offset   (length (match-string 0)))
             (font-obj (query-font (font-at (point) (get-buffer-window))))
             (size     (* 2 (+ (aref font-obj 4) (aref font-obj 5))))
             (align-to (+ offset (ceiling (/ size (aref font-obj 7) 1.0))))
             (gravatar-size (- size 2))
             (slice1  '(slice .0 .0 1.0 0.5))
             (slice2  '(slice .0 .5 1.0 1.0)))
        (gravatar-retrieve
         email
         (lambda (image offset align-to slice1 slice2)
           (unless (eq image 'error)
             (insert (propertize " " 'display `((,@image :ascent center :relief 1)
                                                ,slice1)))
             (insert (propertize " " 'display `((space :align-to ,align-to))))
             (forward-line)
             (forward-char offset)
             (insert (propertize " " 'display `((,@image :ascent center :relief 1)
                                                ,slice2)))
             (insert (propertize " " 'display `((space :align-to ,align-to))))))
         (list offset align-to
               (if fosgit-revision-use-gravatar-kludge slice2 slice1)
               (if fosgit-revision-use-gravatar-kludge slice1 slice2)))))))

;;; Diff Sections

(defvar fosgit-unstaged-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap fosgit-visit-thing]  'fosgit-diff-unstaged)
    (define-key map [remap fosgit-delete-thing] 'fosgit-discard)
    (define-key map "s" 'fosgit-stage)
    (define-key map "u" 'fosgit-unstage)
    map)
  "Keymap for the `unstaged' section.")

(fosgit-define-section-jumper fosgit-jump-to-unstaged "Unstaged changes" unstaged)

(defun fosgit-insert-unstaged-changes ()
  "Insert section showing unstaged changes."
  (fosgit-insert-section (unstaged)
    (fosgit-insert-heading "Unstaged changes:")
    (fosgit-git-wash #'fosgit-diff-wash-diffs
      "diff" fosgit-diff-section-arguments "--no-prefix"
      "--" fosgit-diff-section-file-args)))

(defvar fosgit-staged-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap fosgit-visit-thing]      'fosgit-diff-staged)
    (define-key map [remap fosgit-delete-thing]     'fosgit-discard)
    (define-key map [remap fosgit-revert-no-commit] 'fosgit-reverse)
    (define-key map "s" 'fosgit-stage)
    (define-key map "u" 'fosgit-unstage)
    map)
  "Keymap for the `staged' section.")

(fosgit-define-section-jumper fosgit-jump-to-staged "Staged changes" staged)

(defun fosgit-insert-staged-changes ()
  "Insert section showing staged changes."
  (fosgit-insert-section (staged)
    (fosgit-insert-heading "Staged changes:")
    (fosgit-git-wash #'fosgit-diff-wash-diffs
      "diff" "--cached" fosgit-diff-section-arguments "--no-prefix"
      "--" fosgit-diff-section-file-args)))

;;; Diff Type

(defun fosgit-diff-type (&optional section)
  "Return the diff type of SECTION.

The returned type is one of the symbols `staged', `unstaged',
`committed', or `undefined'.  This type serves a similar purpose
as the general type common to all sections (which is stored in
the `type' slot of the corresponding `fosgit-section' struct) but
takes additional information into account.  When the SECTION
isn't related to diffs and the buffer containing it also isn't
a diff-only buffer, then return nil.

Currently the type can also be one of `tracked' and `untracked'
but these values are not handled explicitly everywhere they
should be and a possible fix could be to just return nil here.

The section has to be a `diff' or `hunk' section, or a section
whose children are of type `diff'.  If optional SECTION is nil,
return the diff type for the current section.  In buffers whose
major mode is `fosgit-diff-mode' SECTION is ignored and the type
is determined using other means.  In `fosgit-revision-mode'
buffers the type is always `committed'.

Do not confuse this with `fosgit-diff-scope' (which see)."
  (--when-let (or section (fosgit-current-section))
    (cond ((derived-mode-p 'fosgit-revision-mode 'fosgit-stash-mode) 'committed)
          ((derived-mode-p 'fosgit-diff-mode)
           (let ((range (nth 0 fosgit-refresh-args))
                 (const (nth 1 fosgit-refresh-args)))
             (cond ((member "--no-index" const) 'undefined)
                   ((not range)
                    (if (member "--cached" const)
                        'staged
                      'unstaged))
                   ((member "--cached" const)
                    (if (fosgit-rev-head-p range)
                        'staged
                      'undefined)) ; i.e. committed and staged
                   (t 'committed))))
          ((derived-mode-p 'fosgit-status-mode)
           (let ((stype (fosgit-section-type it)))
             (if (memq stype '(staged unstaged tracked untracked))
                 stype
               (pcase stype
                 (`file (let* ((parent (fosgit-section-parent it))
                               (type   (fosgit-section-type parent)))
                          (if (eq type 'file)
                              (fosgit-diff-type parent)
                            type)))
                 (`hunk (-> it fosgit-section-parent fosgit-section-parent
                            fosgit-section-type))))))
          ((derived-mode-p 'fosgit-log-mode)
           (if (or (and (fosgit-section-match 'commit section)
                        (fosgit-section-children section))
                   (fosgit-section-match [* file commit] section))
               'committed
           'undefined))
          (t 'undefined))))

(cl-defun fosgit-diff-scope (&optional (section nil ssection) strict)
  "Return the diff scope of SECTION or the selected section(s).

A diff's \"scope\" describes what part of a diff is selected, it is
a symbol, one of `region', `hunk', `hunks', `file', `files', or
`list'.  Do not confuse this with the diff \"type\", as returned by
`fosgit-diff-type'.

If optional SECTION is non-nil, then return the scope of that,
ignoring the sections selected by the region.  Otherwise return
the scope of the current section, or if the region is active and
selects a valid group of diff related sections, the type of these
sections, i.e. `hunks' or `files'.  If SECTION, or if that is nil
the current section, is a `hunk' section; and the region region
starts and ends inside the body of a that section, then the type
is `region'.

If optional STRICT is non-nil then return nil if the diff type of
the section at point is `untracked' or the section at point is not
actually a `diff' but a `diffstat' section."
  (let ((siblings (and (not ssection) (fosgit-region-sections))))
    (setq section (or section (car siblings) (fosgit-current-section)))
    (when (and section
               (or (not strict)
                   (and (not (eq (fosgit-diff-type section) 'untracked))
                        (not (eq (--when-let (fosgit-section-parent section)
                                   (fosgit-section-type it))
                                 'diffstat)))))
      (pcase (list (fosgit-section-type section)
                   (and siblings t)
                   (and (region-active-p) t)
                   ssection)
        (`(hunk nil   t  ,_)
         (if (fosgit-section-internal-region-p section) 'region 'hunk))
        (`(hunk   t   t nil) 'hunks)
        (`(hunk  ,_  ,_  ,_) 'hunk)
        (`(file   t   t nil) 'files)
        (`(file  ,_  ,_  ,_) 'file)
        (`(,(or `staged `unstaged `untracked)
           nil ,_ ,_) 'list)))))

;;; Diff Highlight

(defun fosgit-diff-unhighlight (section selection)
  "Remove the highlighting of the diff-related SECTION."
  (when (eq (fosgit-section-type section) 'hunk)
    (fosgit-diff-paint-hunk section selection nil)
    t))

(defun fosgit-diff-highlight (section selection)
  "Highlight the diff-related SECTION and return t.
If SECTION is not a diff-related section, then do nothing and
return nil.  If SELECTION is non-nil then it is a list of sections
selected by the region, including SECTION.  All of these sections
are highlighted."
  (if (and (fosgit-section-match 'commit section)
           (fosgit-section-children section))
      (progn (if selection
                 (dolist (section selection)
                   (fosgit-diff-highlight-list section selection))
               (fosgit-diff-highlight-list section))
             t)
    (-when-let (scope (fosgit-diff-scope section t))
      (cond ((eq scope 'region)
             (fosgit-diff-paint-hunk section selection t))
            (selection
             (dolist (section selection)
               (fosgit-diff-highlight-recursive section selection)))
            (t
             (fosgit-diff-highlight-recursive section)))
      t)))

(defun fosgit-diff-highlight-recursive (section &optional selection)
  (pcase (fosgit-diff-scope section)
    (`list (fosgit-diff-highlight-list section selection))
    (`file (fosgit-diff-highlight-file section selection))
    (`hunk (fosgit-diff-highlight-heading section selection)
           (fosgit-diff-paint-hunk section selection t))
    (_     (fosgit-section-highlight section nil))))

(defun fosgit-diff-highlight-list (section &optional selection)
  (let ((beg (fosgit-section-start   section))
        (cnt (fosgit-section-content section))
        (end (fosgit-section-end     section)))
    (unless (and (region-active-p)
                 (= end (1+ (region-end))))
      (fosgit-section-make-overlay beg cnt 'fosgit-section-highlight)
      (unless (fosgit-section-hidden section)
        (dolist (child (fosgit-section-children section))
          (fosgit-diff-highlight-recursive child selection))))
    (when fosgit-diff-highlight-hunk-body
      (fosgit-section-make-overlay (1- end) end 'fosgit-section-highlight))))

(defun fosgit-diff-highlight-file (section &optional selection)
  (fosgit-diff-highlight-heading section selection)
  (unless (fosgit-section-hidden section)
    (dolist (child (fosgit-section-children section))
      (fosgit-diff-highlight-recursive child selection))))

(defun fosgit-diff-highlight-heading (section &optional selection)
  (fosgit-section-make-overlay
   (fosgit-section-start section)
   (or (fosgit-section-content section)
       (fosgit-section-end     section))
   (pcase (list (fosgit-section-type section)
                (and (member section selection) t))
     (`(file   t) 'fosgit-diff-file-heading-selection)
     (`(file nil) 'fosgit-diff-file-heading-highlight)
     (`(hunk   t) 'fosgit-diff-hunk-heading-selection)
     (`(hunk nil) 'fosgit-diff-hunk-heading-highlight))))

;;; Hunk Paint

(cl-defun fosgit-diff-paint-hunk
    (section &optional selection
             (highlight (fosgit-section-selected-p section selection)))
  (let (paint)
    (unless fosgit-diff-highlight-hunk-body
      (setq highlight nil))
    (cond (highlight
           (unless (fosgit-section-hidden section)
             (add-to-list 'fosgit-section-highlighted-sections section)
             (cond ((memq section fosgit-section-unhighlight-sections)
                    (setq fosgit-section-unhighlight-sections
                          (delq section fosgit-section-unhighlight-sections)))
                   (fosgit-diff-highlight-hunk-body
                    (setq paint t)))))
          (t
           (cond ((and (fosgit-section-hidden section)
                       (memq section fosgit-section-unhighlight-sections))
                  (add-to-list 'fosgit-section-highlighted-sections section)
                  (setq fosgit-section-unhighlight-sections
                        (delq section fosgit-section-unhighlight-sections)))
                 (t
                  (setq paint t)))))
    (when paint
      (save-excursion
        (goto-char (fosgit-section-start section))
        (let ((end (fosgit-section-end section))
              (merging (looking-at "@@@"))
              (stage nil))
          (forward-line)
          (while (< (point) end)
            (put-text-property
             (point) (1+ (line-end-position)) 'face
             (cond
              ((looking-at "^\\+\\+?\\([<=|>]\\)\\{7\\}")
               (setq stage (pcase (list (match-string 1) highlight)
                             (`("<" nil) 'fosgit-diff-our)
                             (`("<"   t) 'fosgit-diff-our-highlight)
                             (`("|" nil) 'fosgit-diff-base)
                             (`("|"   t) 'fosgit-diff-base-highlight)
                             (`("=" nil) 'fosgit-diff-their)
                             (`("="   t) 'fosgit-diff-their-highlight)
                             (`(">" nil) nil)))
               'fosgit-diff-conflict-heading)
              ((looking-at (if merging  "^\\(\\+\\| \\+\\)" "^\\+"))
               (fosgit-diff-paint-whitespace merging)
               (or stage
                   (if highlight 'fosgit-diff-added-highlight 'fosgit-diff-added)))
              ((looking-at (if merging  "^\\(-\\| -\\)" "^-"))
               (if highlight 'fosgit-diff-removed-highlight 'fosgit-diff-removed))
              (t
               (if highlight 'fosgit-diff-context-highlight 'fosgit-diff-context))))
            (forward-line))))))
  (fosgit-diff-update-hunk-refinement section))

(defun fosgit-diff-paint-whitespace (merging)
  (when (and fosgit-diff-paint-whitespace
             (or (derived-mode-p 'fosgit-status-mode)
                 (not (eq fosgit-diff-paint-whitespace 'status))))
    (let ((prefix (if merging "^[-\\+\s]\\{2\\}" "^[-\\+]"))
          (indent
           (if (local-variable-p 'fosgit-diff-highlight-indentation)
               fosgit-diff-highlight-indentation
             (setq-local
              fosgit-diff-highlight-indentation
              (cdr (--first (string-match-p (car it) default-directory)
                            (nreverse
                             (default-value
                               'fosgit-diff-highlight-indentation))))))))
      (when (and fosgit-diff-highlight-trailing
                 (looking-at (concat prefix ".*?\\([ \t]+\\)$")))
        (let ((ov (make-overlay (match-beginning 1) (match-end 1) nil t)))
          (overlay-put ov 'face 'fosgit-diff-whitespace-warning)
          (overlay-put ov 'evaporate t)))
      (when (or (and (eq indent 'tabs)
                     (looking-at (concat prefix "\\( *\t[ \t]*\\)")))
                (and (integerp indent)
                     (looking-at (format "%s\\([ \t]* \\{%s,\\}[ \t]*\\)"
                                         prefix indent))))
        (let ((ov (make-overlay (match-beginning 1) (match-end 1) nil t)))
          (overlay-put ov 'face 'fosgit-diff-whitespace-warning)
          (overlay-put ov 'evaporate t))))))

(defun fosgit-diff-update-hunk-refinement (&optional section)
  (if section
      (unless (fosgit-section-hidden section)
        (pcase (list fosgit-diff-refine-hunk
                     (fosgit-section-refined section)
                     (eq section (fosgit-current-section)))
          ((or `(all nil ,_) `(t nil t))
           (setf (fosgit-section-refined section) t)
           (save-excursion
             (goto-char (fosgit-section-start section))
             ;; `diff-refine-hunk' does not handle combined diffs.
             (unless (looking-at "@@@")
               (diff-refine-hunk))))
          ((or `(nil t ,_) `(t t nil))
           (setf (fosgit-section-refined section) nil)
           (remove-overlays (fosgit-section-start section)
                            (fosgit-section-end   section)
                            'diff-mode 'fine))))
    (cl-labels ((recurse (section)
                         (if (fosgit-section-match 'hunk section)
                             (fosgit-diff-update-hunk-refinement section)
                           (--each (fosgit-section-children section)
                             (recurse it)))))
      (recurse fosgit-root-section))))


;;; Highlight Region

(defvar fosgit-diff-unmarked-lines-keep-foreground t)

(defun fosgit-diff-update-hunk-region (section)
  (when (and (eq (fosgit-diff-scope section t) 'region)
             (not (and (eq this-command 'mouse-drag-region)
                       (eq (mark) (point)))))
    (let ((sbeg (fosgit-section-start section))
          (cbeg (fosgit-section-content section))
          (rbeg (save-excursion (goto-char (region-beginning))
                                (line-beginning-position)))
          (rend (save-excursion (goto-char (region-end))
                                (line-end-position)))
          (send (fosgit-section-end section))
          (face (if fosgit-diff-highlight-hunk-body
                    'fosgit-diff-context-highlight
                  'fosgit-diff-context)))
      (when fosgit-diff-unmarked-lines-keep-foreground
        (setq face (list :background (face-attribute face :background))))
      (cl-flet ((ov (start end &rest args)
                  (let ((ov (make-overlay start end nil t)))
                    (overlay-put ov 'evaporate t)
                    (while args (overlay-put ov (pop args) (pop args)))
                    (push ov fosgit-region-overlays)
                    ov)))
        (ov sbeg cbeg 'face 'fosgit-diff-lines-heading
            'display (concat (fosgit-diff-hunk-region-header section) "\n"))
        (ov cbeg rbeg 'face face 'priority 2)
        (when (and (window-system) fosgit-diff-show-lines-boundary)
          (ov rbeg (1+ rbeg) 'before-string
              (propertize (concat (propertize "\s" 'display '(space :height (1)))
                                  (propertize "\n" 'line-height t))
                          'face 'fosgit-diff-lines-boundary))
          (ov rend (1+ rend) 'after-string
              (propertize (concat (propertize "\s" 'display '(space :height (1)))
                                  (propertize "\n" 'line-height t))
                          'face 'fosgit-diff-lines-boundary)))
        (ov (1+ rend) send 'face face 'priority 2)))))

;;; Diff Extract

(defun fosgit-diff-file-header (section)
  (when (eq (fosgit-section-type section) 'hunk)
    (setq section (fosgit-section-parent section)))
  (when (eq (fosgit-section-type section) 'file)
    (fosgit-section-diff-header section)))

(defun fosgit-diff-hunk-region-header (section)
  (let ((patch (fosgit-diff-hunk-region-patch section)))
    (string-match "\n" patch)
    (substring patch 0 (1- (match-end 0)))))

(defun fosgit-diff-hunk-region-patch (section &optional args)
  (let ((op (if (member "--reverse" args) "+" "-"))
        (sbeg (fosgit-section-start section))
        (rbeg (save-excursion
                (goto-char (region-beginning))
                (line-beginning-position)))
        (rend (region-end))
        (send (fosgit-section-end section))
        (patch nil))
    (save-excursion
      (goto-char sbeg)
      (while (< (point) send)
        (looking-at "\\(.\\)\\([^\n]*\n\\)")
        (cond ((or (string-match-p "[@ ]" (match-string-no-properties 1))
                   (and (>= (point) rbeg)
                        (<= (point) rend)))
               (push (match-string-no-properties 0) patch))
              ((equal op (match-string-no-properties 1))
               (push (concat " " (match-string-no-properties 2)) patch)))
        (forward-line)))
    (with-temp-buffer
      (insert (mapconcat 'identity (reverse patch) ""))
      (diff-fixup-modifs (point-min) (point-max))
      (setq patch (buffer-string)))
    patch))

;;; fosgit-diff.el ends soon
(provide 'fosgit-diff)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-diff.el ends here
