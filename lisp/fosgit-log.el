;;; fosgit-log.el --- inspect Git history  -*- lexical-binding: t -*-

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

;; This library implements support for looking at Git logs, including
;; special logs like reflogs and cherry-logs, as well as for selecting
;; a commit from a log.

;;; Code:

(require 'fosgit-core)
(require 'fosgit-diff)

(declare-function fosgit-blame-chunk-get 'fosgit-blame)
(declare-function fosgit-blob-visit 'fosgit)
(declare-function fosgit-find-file-noselect 'fosgit)
(declare-function fosgit-insert-head-branch-header 'fosgit)
(declare-function fosgit-insert-upstream-branch-header 'fosgit)
(declare-function fosgit-read-file-from-rev 'fosgit)
(declare-function fosgit-show-commit 'fosgit)
(defvar fosgit-refs-indent-cherry-lines)
(defvar fosgit-refs-show-commit-count)
(defvar fosgit-status-sections-hook)

(require 'ansi-color)
(require 'crm)

;;; Options
;;;; Log Mode

(defgroup fosgit-log nil
  "Inspect and manipulate Git history."
  :group 'fosgit-modes)

(defcustom fosgit-log-mode-hook nil
  "Hook run after entering Fosgit-Log mode."
  :group 'fosgit-log
  :type 'hook)

(defcustom fosgit-log-arguments '("-n256" "--graph" "--decorate")
  "The log arguments used in `fosgit-log-mode' buffers."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-log
  :group 'fosgit-commands
  :type '(repeat (string :tag "Argument")))

(defcustom fosgit-log-remove-graph-args '("--follow" "--grep" "-G" "-S" "-L")
  "The log arguments that cause the `--graph' argument to be dropped."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-log
  :type '(repeat (string :tag "Argument"))
  :options '("--follow" "--grep" "-G" "-S" "-L"))

(defcustom fosgit-log-revision-headers-format "\
%+b
Author:    %aN <%aE>
Committer: %cN <%cE>"
  "Additional format string used with the `++header' argument."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-log
  :type 'string)

(defcustom fosgit-log-auto-more nil
  "Insert more log entries automatically when moving past the last entry.
Only considered when moving past the last entry with
`fosgit-goto-*-section' commands."
  :group 'fosgit-log
  :type 'boolean)

(defcustom fosgit-log-show-margin t
  "Whether to initially show the margin in log buffers.

When non-nil the author name and date are initially displayed in
the margin of log buffers.  The margin can be shown or hidden in
the current buffer using the command `fosgit-toggle-margin'.  In
status buffers this option is ignored but it is possible to show
the margin using the mentioned command."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-log
  :type 'boolean)

(defcustom fosgit-duration-spec
  `((?Y "year"   "years"   ,(round (* 60 60 24 365.2425)))
    (?M "month"  "months"  ,(round (* 60 60 24 30.436875)))
    (?w "week"   "weeks"   ,(* 60 60 24 7))
    (?d "day"    "days"    ,(* 60 60 24))
    (?h "hour"   "hours"   ,(* 60 60))
    (?m "minute" "minutes" 60)
    (?s "second" "seconds" 1))
  "Units used to display durations in a human format.
The value is a list of time units, beginning with the longest.
Each element has the form (CHAR UNIT UNITS SECONDS).  UNIT is the
time unit, UNITS is the plural of that unit.  CHAR is a character
abbreviation.  And SECONDS is the number of seconds in one UNIT.
Also see option `fosgit-log-margin-spec'."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-log
  :type '(repeat (list (character :tag "Unit character")
                       (string    :tag "Unit singular string")
                       (string    :tag "Unit plural string")
                       (integer   :tag "Seconds in unit"))))

(defcustom fosgit-log-margin-spec '(28 7 fosgit-duration-spec)
  "How to format the log margin.

The log margin is used to display each commit's author followed
by the commit's age.  This option controls the total width of the
margin and how time units are formatted, the value has the form:

  (WIDTH UNIT-WIDTH DURATION-SPEC)

WIDTH specifies the total width of the log margin.  UNIT-WIDTH is
either the integer 1, in which case time units are displayed as a
single characters, leaving more room for author names; or it has
to be the width of the longest time unit string in DURATION-SPEC.
DURATION-SPEC has to be a variable, its value controls which time
units, in what language, are being used."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-log
  :set-after '(fosgit-duration-spec)
  :type '(list (integer  :tag "Margin width")
               (choice   :tag "Time unit style"
                         (const   :format "%t\n"
                                  :tag "abbreviate to single character" 1)
                         (integer :format "%t\n"
                                  :tag "show full name" 7))
               (variable :tag "Duration spec variable")))

(defcustom fosgit-log-show-refname-after-summary nil
  "Whether to show refnames after commit summaries.
This is useful if you use really long branch names."
  :package-version '(fosgit . "2.2.0")
  :group 'fosgit-log
  :type 'boolean)

(defface fosgit-log-graph
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
  "Face for the graph part of the log output."
  :group 'fosgit-faces)

(defface fosgit-log-author
  '((((class color) (background light)) :foreground "firebrick")
    (((class color) (background  dark)) :foreground "tomato"))
  "Face for the author part of the log output."
  :group 'fosgit-faces)

(defface fosgit-log-date
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
  "Face for the date part of the log output."
  :group 'fosgit-faces)

;;;; Select Mode

(defcustom fosgit-log-select-arguments '("-n256" "--decorate")
  "The log arguments used in `fosgit-log-select-mode' buffers."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-log
  :type '(repeat (string :tag "Argument")))

(defcustom fosgit-log-select-show-usage 'both
  "Whether to show usage information when selecting a commit from a log.
The message can be shown in the `echo-area' or the `header-line', or in
`both' places.  If the value isn't one of these symbols, then it should
be nil, in which case no usage information is shown."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-log
  :type '(choice (const :tag "in echo-area" echo-area)
                 (const :tag "in header-line" header-line)
                 (const :tag "in both places" both)
                 (const :tag "nowhere")))

;;;; Cherry Mode

(defcustom fosgit-cherry-sections-hook
  '(fosgit-insert-cherry-headers
    fosgit-insert-cherry-commits)
  "Hook run to insert sections into the cherry buffer."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-log
  :type 'hook)

;;;; Reflog Mode

(defcustom fosgit-reflog-arguments '("-n256")
  "The log arguments used in `fosgit-reflog-mode' buffers."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-log
  :group 'fosgit-commands
  :type '(repeat (string :tag "Argument")))

(defcustom fosgit-reflog-show-margin t
  "Whether to initially show the margin in reflog buffers.

When non-nil the author name and date are initially displayed in
the margin of reflog buffers.  The margin can be shown or hidden
in the current buffer using the command `fosgit-toggle-margin'."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-log
  :type 'boolean)

(defface fosgit-reflog-commit '((t :foreground "green"))
  "Face for commit commands in reflogs."
  :group 'fosgit-faces)

(defface fosgit-reflog-amend '((t :foreground "magenta"))
  "Face for amend commands in reflogs."
  :group 'fosgit-faces)

(defface fosgit-reflog-merge '((t :foreground "green"))
  "Face for merge, checkout and branch commands in reflogs."
  :group 'fosgit-faces)

(defface fosgit-reflog-checkout '((t :foreground "blue"))
  "Face for checkout commands in reflogs."
  :group 'fosgit-faces)

(defface fosgit-reflog-reset '((t :foreground "red"))
  "Face for reset commands in reflogs."
  :group 'fosgit-faces)

(defface fosgit-reflog-rebase '((t :foreground "magenta"))
  "Face for rebase commands in reflogs."
  :group 'fosgit-faces)

(defface fosgit-reflog-cherry-pick '((t :foreground "green"))
  "Face for cherry-pick commands in reflogs."
  :group 'fosgit-faces)

(defface fosgit-reflog-remote '((t :foreground "cyan"))
  "Face for pull and clone commands in reflogs."
  :group 'fosgit-faces)

(defface fosgit-reflog-other '((t :foreground "cyan"))
  "Face for other commands in reflogs."
  :group 'fosgit-faces)

;;;; Log Sections

(defcustom fosgit-log-section-commit-count 10
  "How many recent commits to show in certain log sections.
How many recent commits `fosgit-insert-recent-commits' and
`fosgit-insert-unpulled-from-upstream-or-recent' (provided
the upstream isn't ahead of the current branch) show."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-status
  :type 'number)

(defcustom fosgit-log-section-arguments '("-n256" "--decorate")
  "The log arguments used in buffers that show other things besides logs."
  :package-version '(fosgit . "2.4.0")
  :group 'fosgit-log
  :group 'fosgit-status
  :type '(repeat (string :tag "Argument")))

(define-obsolete-variable-alias 'fosgit-log-section-args
  'fosgit-log-section-arguments "2.2.0")

;;; Commands

(defvar fosgit-log-popup
  '(:variable fosgit-log-arguments
    :man-page "git-log"
    :switches ((?g "Show graph"              "--graph")
               (?c "Show graph in color"     "--color")
               (?d "Show refnames"           "--decorate")
               (?S "Show signatures"         "--show-signature")
               (?u "Show diffs"              "--patch")
               (?s "Show diffstats"          "--stat")
               (?h "Show header"             "++header")
               (?D "Simplify by decoration"  "--simplify-by-decoration")
               (?f "Follow renames when showing single-file log" "--follow"))
    :options  ((?n "Limit number of commits" "-n")
               (?f "Limit to files"          "-- " fosgit-read-files)
               (?a "Limit to author"         "--author=")
               (?o "Order commits by"        "++order=" fosgit-log-select-order)
               (?g "Search messages"         "--grep=")
               (?G "Search changes"          "-G")
               (?S "Search occurences"       "-S")
               (?L "Trace line evolution"    "-L" fosgit-read-file-trace))
    :actions  ((?l "Log current"             fosgit-log-current)
               (?L "Log local branches"      fosgit-log-branches)
               (?r "Reflog current"          fosgit-reflog-current)
               (?o "Log other"               fosgit-log)
               (?b "Log all branches"        fosgit-log-all-branches)
               (?O "Reflog other"            fosgit-reflog)
               (?h "Log HEAD"                fosgit-log-head)
               (?a "Log all references"      fosgit-log-all)
               (?H "Reflog HEAD"             fosgit-reflog-head))
    :default-action fosgit-log-current
    :max-action-columns 3))

(defvar fosgit-log-mode-refresh-popup
  '(:variable fosgit-log-arguments
    :man-page "git-log"
    :switches ((?g "Show graph"              "--graph")
               (?c "Show graph in color"     "--color")
               (?d "Show refnames"           "--decorate")
               (?S "Show signatures"         "--show-signature")
               (?u "Show diffs"              "--patch")
               (?s "Show diffstats"          "--stat")
               (?D "Simplify by decoration"  "--simplify-by-decoration")
               (?f "Follow renames when showing single-file log" "--follow"))
    :options  ((?n "Limit number of commits" "-n")
               (?f "Limit to files"          "-- " fosgit-read-files)
               (?a "Limit to author"         "--author=")
               (?o "Order commits by"        "++order=" fosgit-log-select-order)
               (?g "Search messages"         "--grep=")
               (?G "Search changes"          "-G")
               (?S "Search occurences"       "-S")
               (?L "Trace line evolution"    "-L" fosgit-read-file-trace))
    :actions  ((?g "Refresh"       fosgit-log-refresh)
               (?t "Toggle margin" fosgit-toggle-margin)
               (?s "Set defaults"  fosgit-log-set-default-arguments) nil
               (?w "Save defaults" fosgit-log-save-default-arguments))
    :max-action-columns 2))

(defvar fosgit-reflog-mode-refresh-popup
  '(:variable fosgit-reflog-arguments
    :man-page "git-reflog"
    :options  ((?n "Limit number of commits" "-n"))))

(defvar fosgit-log-refresh-popup
  '(:variable fosgit-log-arguments
    :man-page "git-log"
    :switches ((?g "Show graph"          "--graph")
               (?c "Show graph in color" "--color")
               (?d "Show refnames"       "--decorate"))
    :options  ((?n "Limit number of commits" "-n")
               (?o "Order commits by"        "++order=" fosgit-log-select-order))
    :actions  ((?g "Refresh"       fosgit-log-refresh)
               (?t "Toggle margin" fosgit-toggle-margin)
               (?s "Set defaults"  fosgit-log-set-default-arguments) nil
               (?w "Save defaults" fosgit-log-save-default-arguments))
    :max-action-columns 2))

(fosgit-define-popup-keys-deferred 'fosgit-log-popup)
(fosgit-define-popup-keys-deferred 'fosgit-log-mode-refresh-popup)
(fosgit-define-popup-keys-deferred 'fosgit-log-refresh-popup)

(defun fosgit-read-file-trace (&rest _ignored)
  (let ((file  (fosgit-read-file-from-rev "HEAD" "File"))
        (trace (fosgit-read-string "Trace")))
    (if (string-match
         "^\\(/.+/\\|:[^:]+\\|[0-9]+,[-+]?[0-9]+\\)\\(:\\)?$" trace)
        (concat trace (or (match-string 2 trace) ":") file)
      (user-error "Trace is invalid, see man git-log"))))

(defun fosgit-log-select-order (&rest _ignored)
  (fosgit-read-char-case "Order commits by " t
    (?t "[t]opography"     "topo")
    (?a "[a]uthor date"    "author-date")
    (?c "[c]ommitter date" "date")))

(defun fosgit-log-arguments (&optional refresh)
  (cond ((memq fosgit-current-popup
               '(fosgit-log-popup fosgit-log-refresh-popup))
         (fosgit-popup-export-file-args fosgit-current-popup-args))
        ((derived-mode-p 'fosgit-log-mode)
         (list (nth 1 fosgit-refresh-args)
               (nth 2 fosgit-refresh-args)))
        (refresh
         (list fosgit-log-section-arguments nil))
        (t
         (-if-let (buffer (fosgit-mode-get-buffer 'fosgit-log-mode))
             (with-current-buffer buffer
               (list (nth 1 fosgit-refresh-args)
                     (nth 2 fosgit-refresh-args)))
           (list (default-value 'fosgit-log-arguments) nil)))))

(defun fosgit-log-popup (arg)
  "Popup console for log commands."
  (interactive "P")
  (let ((fosgit-log-refresh-popup
         (pcase major-mode
           (`fosgit-log-mode fosgit-log-mode-refresh-popup)
           (_               fosgit-log-refresh-popup)))
        (fosgit-log-arguments
         (-if-let (buffer (fosgit-mode-get-buffer 'fosgit-log-mode))
             (with-current-buffer buffer
               (fosgit-popup-import-file-args (nth 1 fosgit-refresh-args)
                                             (nth 2 fosgit-refresh-args)))
           (default-value 'fosgit-log-arguments))))
    (fosgit-invoke-popup 'fosgit-log-popup nil arg)))

(defun fosgit-log-refresh-popup (arg)
  "Popup console for changing log arguments in the current buffer."
  (interactive "P")
  (fosgit-log-refresh-assert)
  (let ((fosgit-log-refresh-popup
         (cond ((derived-mode-p 'fosgit-log-select-mode)
                fosgit-log-refresh-popup)
               ((derived-mode-p 'fosgit-log-mode)
                (let ((def (copy-sequence fosgit-log-refresh-popup)))
                  (plist-put def :switches (plist-get fosgit-log-popup :switches))
                  (plist-put def :options  (plist-get fosgit-log-popup :options))
                  def))
               (t
                fosgit-log-refresh-popup)))
        (fosgit-log-arguments
         (cond ((derived-mode-p 'fosgit-log-select-mode)
                (cadr fosgit-refresh-args))
               ((derived-mode-p 'fosgit-log-mode)
                (fosgit-popup-import-file-args (nth 1 fosgit-refresh-args)
                                              (nth 2 fosgit-refresh-args)))
               (t
                fosgit-log-section-arguments))))
    (fosgit-invoke-popup 'fosgit-log-refresh-popup nil arg)))

(defun fosgit-log-refresh (args files)
  "Set the local log arguments for the current buffer."
  (interactive (fosgit-log-arguments t))
  (fosgit-log-refresh-assert)
  (cond ((derived-mode-p 'fosgit-log-select-mode)
         (setcar (cdr fosgit-refresh-args) args))
        ((derived-mode-p 'fosgit-log-mode)
         (setcdr fosgit-refresh-args (list args files)))
        (t
         (setq-local fosgit-log-section-arguments args)))
  (fosgit-refresh))

(defun fosgit-log-set-default-arguments (args files)
  "Set the global log arguments for the current buffer."
  (interactive (fosgit-log-arguments t))
  (fosgit-log-refresh-assert)
  (cond ((derived-mode-p 'fosgit-log-select-mode)
         (customize-set-variable 'fosgit-log-select-arguments args)
         (setcar (cdr fosgit-refresh-args) args))
        ((derived-mode-p 'fosgit-log-mode)
         (customize-set-variable 'fosgit-log-arguments args)
         (setcdr fosgit-refresh-args (list args files)))
        (t
         (customize-set-variable 'fosgit-log-section-arguments args)
         (kill-local-variable    'fosgit-log-section-arguments)))
  (fosgit-refresh))

(defun fosgit-log-save-default-arguments (args files)
  "Set and save the global log arguments for the current buffer."
  (interactive (fosgit-log-arguments t))
  (fosgit-log-refresh-assert)
  (cond ((derived-mode-p 'fosgit-log-select-mode)
         (customize-save-variable 'fosgit-log-select-arguments args)
         (setcar (cdr fosgit-refresh-args) args))
        ((derived-mode-p 'fosgit-log-mode)
         (customize-save-variable 'fosgit-log-arguments args)
         (setcdr fosgit-refresh-args (list args files)))
        (t
         (customize-save-variable 'fosgit-log-section-arguments args)
         (kill-local-variable     'fosgit-log-section-arguments)))
  (fosgit-refresh))

(defun fosgit-log-refresh-assert ()
  (cond ((derived-mode-p 'fosgit-reflog-mode)
         (user-error "Cannot change log arguments in reflog buffers"))
        ((derived-mode-p 'fosgit-cherry-mode)
         (user-error "Cannot change log arguments in cherry buffers"))))

(defvar fosgit-log-read-revs-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map crm-local-completion-map)
    (define-key map "\s" 'self-insert-command)
    map))

(defun fosgit-log-read-revs (&optional use-current)
  (or (and use-current (--when-let (fosgit-get-current-branch) (list it)))
      (let* ((choose-completion-string-functions
              '(crm--choose-completion-string))
             (minibuffer-completion-table #'crm--collection-fn)
             (minibuffer-completion-confirm t)
             (crm-completion-table
              `(,@(and (file-exists-p (fosgit-git-dir "FETCH_HEAD"))
                       (list "FETCH_HEAD"))
                ,@(fosgit-list-branch-names)))
             (crm-separator "\\(\\.\\.\\.?\\|[, ]\\)")
             (default (or (fosgit-branch-or-commit-at-point)
                          (unless use-current
                            (fosgit-get-previous-branch))))
             (input (read-from-minibuffer
                     (format "Log rev,s%s: "
                             (if default (format " (%s)" default) ""))
                     nil fosgit-log-read-revs-map
                     nil 'fosgit-revision-history default)))
        (when (string-equal input "")
          (or (setq input default)
              (user-error "Nothing selected")))
        (split-string input "[, ]" t))))

;;;###autoload
(defun fosgit-log-current (revs &optional args files)
  "Show log for the current branch.
When `HEAD' is detached or with a prefix argument show log for
one or more revs read from the minibuffer."
  (interactive (cons (fosgit-log-read-revs t)
                     (fosgit-log-arguments)))
  (fosgit-log revs args files))

;;;###autoload
(defun fosgit-log (revs &optional args files)
  "Show log for one or more revs read from the minibuffer.
The user can input any revision or revisions separated by a
space, or even ranges, but only branches and tags, and a
representation of the commit at point, are available as
completion candidates."
  (interactive (cons (fosgit-log-read-revs)
                     (fosgit-log-arguments)))
  (require 'fosgit)
  (fosgit-mode-setup #'fosgit-log-mode revs args files)
  (fosgit-log-goto-same-commit))

;;;###autoload
(defun fosgit-log-head (&optional args files)
  "Show log for `HEAD'."
  (interactive (fosgit-log-arguments))
  (fosgit-log (list "HEAD") args files))

;;;###autoload
(defun fosgit-log-branches (&optional args files)
  "Show log for all local branches and `HEAD'."
  (interactive (fosgit-log-arguments))
  (fosgit-log (if (fosgit-get-current-branch)
                 (list "--branches")
               (list "HEAD" "--branches"))
             args files))

;;;###autoload
(defun fosgit-log-all-branches (&optional args files)
  "Show log for all local and remote branches and `HEAD'."
  (interactive (fosgit-log-arguments))
  (fosgit-log (if (fosgit-get-current-branch)
                 (list "--branches" "--remotes")
               (list "HEAD" "--branches" "--remotes"))
             args files))

;;;###autoload
(defun fosgit-log-all (&optional args files)
  "Show log for all references and `HEAD'."
  (interactive (fosgit-log-arguments))
  (fosgit-log (if (fosgit-get-current-branch)
                 (list "--all")
               (list "HEAD" "--all"))
             args files))

;;;###autoload
(defun fosgit-log-buffer-file (&optional follow beg end)
  "Show log for the blob or file visited in the current buffer.
With a prefix argument or when `--follow' is part of
`fosgit-log-arguments', then follow renames."
  (interactive (if (region-active-p)
                   (list current-prefix-arg
                         (1- (line-number-at-pos (region-beginning)))
                         (1- (line-number-at-pos (region-end))))
                 (list current-prefix-arg)))
  (-if-let (file (fosgit-file-relative-name))
      (fosgit-mode-setup #'fosgit-log-mode
                        (list (or fosgit-buffer-refname
                                  (fosgit-get-current-branch) "HEAD"))
                        (let ((args (car (fosgit-log-arguments))))
                          (when (and follow (not (member "--follow" args)))
                            (push "--follow" args))
                          (when (and beg end)
                            (setq args (cons (format "-L%s,%s:%s" beg end file)
                                             (cl-delete "-L" args :test
                                                        'string-prefix-p)))
                            (setq file nil))
                          args)
                        (and file (list file)))
    (user-error "Buffer isn't visiting a file"))
  (fosgit-log-goto-same-commit))

;;;###autoload
(defun fosgit-reflog-current ()
  "Display the reflog of the current branch."
  (interactive)
  (fosgit-reflog (fosgit-get-current-branch)))

;;;###autoload
(defun fosgit-reflog (ref)
  "Display the reflog of a branch."
  (interactive (list (fosgit-read-local-branch-or-ref "Show reflog for")))
  (fosgit-mode-setup #'fosgit-reflog-mode ref fosgit-reflog-arguments))

;;;###autoload
(defun fosgit-reflog-head ()
  "Display the `HEAD' reflog."
  (interactive)
  (fosgit-reflog "HEAD"))

(defun fosgit-log-toggle-commit-limit ()
  "Toggle the number of commits the current log buffer is limited to.
If the number of commits is currently limited, then remove that
limit.  Otherwise set it to 256."
  (interactive)
  (fosgit-log-set-commit-limit (lambda (&rest _) nil)))

(defun fosgit-log-double-commit-limit ()
  "Double the number of commits the current log buffer is limited to."
  (interactive)
  (fosgit-log-set-commit-limit '*))

(defun fosgit-log-half-commit-limit ()
  "Half the number of commits the current log buffer is limited to."
  (interactive)
  (fosgit-log-set-commit-limit '/))

(defun fosgit-log-set-commit-limit (fn)
  (let* ((val (car (fosgit-log-arguments t)))
         (arg (--first (string-match "^-n\\([0-9]+\\)?$" it) val))
         (num (and arg (string-to-number (match-string 1 arg))))
         (num (if num (funcall fn num 2) 256)))
    (setq val (delete arg val))
    (setcar (cdr fosgit-refresh-args)
            (if (and num (> num 0))
                (cons (format "-n%i" num) val)
              val)))
  (fosgit-refresh))

(defun fosgit-log-get-commit-limit ()
  (--when-let (--first (string-match "^-n\\([0-9]+\\)?$" it)
                       (car (fosgit-log-arguments t)))
    (string-to-number (match-string 1 it))))

(defun fosgit-log-bury-buffer (&optional arg)
  "Bury the current buffer or the revision buffer in the same frame.
Like `fosgit-mode-bury-buffer' (which see) but with a negative
prefix argument instead bury the revision buffer, provided it
is displayed in the current frame."
  (interactive "p")
  (if (< arg 0)
      (let* ((buf (fosgit-mode-get-buffer 'fosgit-revision-mode))
             (win (and buf (get-buffer-window buf (selected-frame)))))
        (if win
            (with-selected-window win
              (with-current-buffer buf
                (fosgit-mode-bury-buffer (> (abs arg) 1))))
          (user-error "No revision buffer in this frame")))
    (fosgit-mode-bury-buffer (> arg 1))))

;;; Log Mode

(defvar fosgit-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map fosgit-mode-map)
    (define-key map "\C-c\C-b" 'fosgit-go-backward)
    (define-key map "\C-c\C-f" 'fosgit-go-forward)
    (define-key map "=" 'fosgit-log-toggle-commit-limit)
    (define-key map "+" 'fosgit-log-double-commit-limit)
    (define-key map "-" 'fosgit-log-half-commit-limit)
    (define-key map "q" 'fosgit-log-bury-buffer)
    map)
  "Keymap for `fosgit-log-mode'.")

(define-derived-mode fosgit-log-mode fosgit-mode "Fosgit Log"
  "Mode for looking at Git log.

This mode is documented in info node `(fosgit)Log Buffer'.

\\<fosgit-mode-map>\
Type \\[fosgit-refresh] to refresh the current buffer.
Type \\[fosgit-visit-thing] or \\[fosgit-diff-show-or-scroll-up] \
to visit the commit at point.

Type \\[fosgit-branch-popup] to see available branch commands.
Type \\[fosgit-merge-popup] to merge the branch or commit at point.
Type \\[fosgit-cherry-pick-popup] to apply the commit at point.
Type \\[fosgit-reset] to reset HEAD to the commit at point.

\\{fosgit-log-mode-map}"
  :group 'fosgit-log
  (hack-dir-local-variables-non-file-buffer))

(defvar fosgit-log-disable-graph-hack-args
  '("-G" "--grep" "--author")
  "Arguments which disable the graph speedup hack.")

(defun fosgit-log-refresh-buffer (revs args files)
  (setq header-line-format
        (propertize
         (concat " Commits in " (mapconcat 'identity revs  " ")
                 (and files (concat " touching "
                                    (mapconcat 'identity files " "))))
         'face 'fosgit-header-line))
  (unless (= (length files) 1)
    (setq args (remove "--follow" args)))
  (when (--any-p (string-match-p
                  (concat "^" (regexp-opt fosgit-log-remove-graph-args)) it)
                 args)
    (setq args (remove "--graph" args)))
  (unless (member "--graph" args)
    (setq args (remove "--color" args)))
  (-when-let* ((limit (fosgit-log-get-commit-limit))
               (limit (* 2 limit)) ; increase odds for complete graph
               (count (and (= (length revs) 1)
                           (> limit 1024) ; otherwise it's fast enough
                           (setq revs (car revs))
                           (not (string-match-p "\\.\\." revs))
                           (not (member revs '("--all" "--branches")))
                           (-none-p (lambda (arg)
                                      (--any-p (string-prefix-p it arg)
                                               fosgit-log-disable-graph-hack-args))
                                    args)
                           (fosgit-git-string "rev-list" "--count"
                                             "--first-parent" args revs))))
    (setq revs (if (< (string-to-number count) limit)
                   revs
                 (format "%s~%s..%s" revs limit revs))))
  (fosgit-insert-section (logbuf)
    (fosgit-insert-log revs args files)))

(defun fosgit-insert-log (revs &optional args files)
  "Insert a log section.
Do not add this to a hook variable."
  (fosgit-git-wash (apply-partially #'fosgit-log-wash-log 'log)
    "log"
    (format "--format=%%h%s %s[%%aN][%%at]%%s%s"
            (if (member "--decorate" args) "%d" "")
            (if (member "--show-signature" args)
                (progn (setq args (remove "--show-signature" args)) "%G?")
              "")
            (if (member "++header" args)
                (if (member "--graph" (setq args (remove "++header" args)))
                    (concat "\n" fosgit-log-revision-headers-format "\n")
                  (concat "\n" fosgit-log-revision-headers-format "\n"))
              ""))
    (progn
      (--when-let (--first (string-match "^\\+\\+order=\\(.+\\)$" it) args)
        (setq args (cons (format "--%s-order" (match-string 1 it))
                         (remove it args))))
      (if (member "--decorate" args)
          (cons "--decorate=full" (remove "--decorate" args))
        args))
    "--use-mailmap" "--no-prefix" revs "--" files))

(defvar fosgit-commit-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap fosgit-visit-thing] 'fosgit-show-commit)
    (define-key map "a" 'fosgit-cherry-apply)
    map)
  "Keymap for `commit' sections.")

(defvar fosgit-module-commit-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap fosgit-visit-thing] 'fosgit-show-commit)
    map)
  "Keymap for `module-commit' sections.")

(defconst fosgit-log-heading-re
  (concat "^"
          "\\(?4:[-_/|\\*o. ]*\\)"                 ; graph
          "\\(?1:[0-9a-fA-F]+\\) "                 ; sha1
          "\\(?:\\(?3:([^()]+)\\) \\)?"            ; refs
          "\\(?7:[BGUN]\\)?"                       ; gpg
          "\\[\\(?5:[^]]*\\)\\]"                   ; author
          "\\[\\(?6:[^]]*\\)\\]"                   ; date
          "\\(?2:.*\\)$"))                         ; msg

(defconst fosgit-log-cherry-re
  (concat "^"
          "\\(?8:[-+]\\) "                         ; cherry
          "\\(?1:[0-9a-fA-F]+\\) "                 ; sha1
          "\\(?2:.*\\)$"))                         ; msg

(defconst fosgit-log-module-re
  (concat "^"
          "\\(?:\\(?11:[<>]\\) \\)?"               ; side
          "\\(?1:[0-9a-fA-F]+\\) "                 ; sha1
          "\\(?2:.*\\)$"))                         ; msg

(defconst fosgit-log-bisect-vis-re
  (concat "^"
          "\\(?1:[0-9a-fA-F]+\\) "                 ; sha1
          "\\(?:\\(?3:([^()]+)\\) \\)?"            ; refs
          "\\(?2:.*\\)$"))                         ; msg

(defconst fosgit-log-bisect-log-re
  (concat "^# "
          "\\(?3:bad:\\|skip:\\|good:\\) "         ; "refs"
          "\\[\\(?1:[^]]+\\)\\] "                  ; sha1
          "\\(?2:.*\\)$"))                         ; msg

(defconst fosgit-log-reflog-re
  (concat "^"
          "\\(?1:[^ ]+\\) "                        ; sha1
          "\\(?:\\(?:[^@]+@{\\(?6:[^}]+\\)} "      ; date
          "\\(?10:merge \\|autosave \\|restart \\|[^:]+: \\)?" ; refsub
          "\\(?2:.*\\)?\\)\\| \\)$"))              ; msg

(defconst fosgit-reflog-subject-re
  (concat "\\(?1:[^ ]+\\) ?"                       ; command
          "\\(?2:\\(?: ?-[^ ]+\\)+\\)?"            ; option
          "\\(?: ?(\\(?3:[^)]+\\))\\)?"))          ; type

(defconst fosgit-log-stash-re
  (concat "^"
          "\\(?1:[^ ]+\\)"                         ; "sha1"
          "\\(?5: \\)"                             ; "author"
          "\\(?6:[^ ]+\\) "                        ; date
          "\\(?2:.*\\)$"))                         ; msg

(defvar fosgit-log-count nil)

(defun fosgit-log-wash-log (style args)
  (setq args (-flatten args))
  (when (and (member "--graph" args)
             (member "--color" args))
    (let ((ansi-color-apply-face-function
           (lambda (beg end face)
             (put-text-property beg end 'font-lock-face
                                (or face 'fosgit-log-graph)))))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (when (eq style 'cherry)
    (reverse-region (point-min) (point-max)))
  (let ((fosgit-log-count 0)
        (abbrev (fosgit-abbrev-length)))
    (fosgit-wash-sequence (apply-partially 'fosgit-log-wash-rev style abbrev))
    (if (derived-mode-p 'fosgit-log-mode)
        (when (eq fosgit-log-count (fosgit-log-get-commit-limit))
          (fosgit-insert-section (longer)
            (insert-text-button
             (substitute-command-keys
              (format "Type \\<%s>\\[%s] to show more history"
                      'fosgit-log-mode-map
                      'fosgit-log-double-commit-limit))
             'action (lambda (_button)
                       (fosgit-log-double-commit-limit))
             'follow-link t
             'mouse-face 'fosgit-section-highlight)))
      (unless (equal (car args) "cherry")
        (insert ?\n)))))

(defun fosgit-log-wash-rev (style abbrev)
  (when (derived-mode-p 'fosgit-log-mode)
    (cl-incf fosgit-log-count))
  (looking-at (pcase style
                (`log        fosgit-log-heading-re)
                (`cherry     fosgit-log-cherry-re)
                (`module     fosgit-log-module-re)
                (`reflog     fosgit-log-reflog-re)
                (`stash      fosgit-log-stash-re)
                (`bisect-vis fosgit-log-bisect-vis-re)
                (`bisect-log fosgit-log-bisect-log-re)))
  (fosgit-bind-match-strings
      (hash msg refs graph author date gpg cherry _ refsub side) nil
    (let ((align (not (member "--stat" (cadr fosgit-refresh-args)))))
      (fosgit-delete-line)
      (fosgit-insert-section section (commit hash)
        (pcase style
          (`stash      (setf (fosgit-section-type section) 'stash))
          (`module     (setf (fosgit-section-type section) 'module-commit))
          (`bisect-log (setq hash (fosgit-rev-parse "--short" hash))))
        (when cherry
          (when (and (derived-mode-p 'fosgit-refs-mode)
                     fosgit-refs-show-commit-count)
            (insert (make-string fosgit-refs-indent-cherry-lines ?\s)))
          (insert (propertize cherry 'face (if (string= cherry "-")
                                               'fosgit-cherry-equivalent
                                             'fosgit-cherry-unmatched)))
          (insert ?\s))
        (when side
          (insert (propertize side 'face (if (string= side "<")
                                             'fosgit-diff-removed
                                           'fosgit-diff-added)))
          (insert ?\s))
        (when align
          (insert (propertize hash 'face 'fosgit-hash) ?\s))
        (when graph
          (insert graph))
        (unless align
          (insert (propertize hash 'face 'fosgit-hash) ?\s))
        (when (and refs (not fosgit-log-show-refname-after-summary))
          (insert (fosgit-format-ref-labels refs) ?\s))
        (when refsub
          (insert (format "%-2s " (1- fosgit-log-count)))
          (insert (fosgit-reflog-format-subject
                   (substring refsub 0 (if (string-match-p ":" refsub) -2 -1)))))
        (when msg
          (insert (propertize msg 'face
                              (pcase (and gpg (aref gpg 0))
                                (?G 'fosgit-signature-good)
                                (?B 'fosgit-signature-bad)
                                (?U 'fosgit-signature-untrusted)))))
        (when (and refs fosgit-log-show-refname-after-summary)
          (insert ?\s)
          (insert (fosgit-format-ref-labels refs)))
        (insert ?\n)
        (when (memq style '(log reflog stash))
          (goto-char (line-beginning-position))
          (when (and refsub
                     (string-match "\\`\\([^ ]\\) \\+\\(..\\)\\(..\\)" date))
            (setq date (+ (string-to-number (match-string 1 date))
                          (* (string-to-number (match-string 2 date)) 60 60)
                          (* (string-to-number (match-string 3 date)) 60))))
          (save-excursion
            (backward-char)
            (fosgit-format-log-margin author date)))
        (when (and (eq style 'log)
                   (not (or (eobp) (looking-at fosgit-log-heading-re))))
          (when (looking-at "")
            (fosgit-insert-heading)
            (delete-char 1)
            (fosgit-insert-section (commit-header)
              (forward-line)
              (fosgit-insert-heading)
              (re-search-forward "")
              (backward-delete-char 1)
              (forward-char)
              (insert ?\n))
            (delete-char 1))
          (if (looking-at "^\\(---\\|\n\s\\|\ndiff\\)")
              (let ((limit (save-excursion
                             (and (re-search-forward fosgit-log-heading-re nil t)
                                  (match-beginning 0)))))
                (unless (fosgit-section-content fosgit-insert-section--current)
                  (fosgit-insert-heading))
                (delete-char (if (looking-at "\n") 1 4))
                (fosgit-diff-wash-diffs (list "--stat") limit))
            (when align
              (setq align (make-string (1+ abbrev) ? )))
            (when (and (not (eobp)) (not (looking-at fosgit-log-heading-re)))
              (when align
                (setq align (make-string (1+ abbrev) ? )))
              (while (and (not (eobp)) (not (looking-at fosgit-log-heading-re)))
                (when align
                  (save-excursion (insert align)))
                (fosgit-format-log-margin)
                (forward-line))
              ;; When `--format' is used and its value isn't one of the
              ;; predefined formats, then `git-log' does not insert a
              ;; separator line.
              (save-excursion
                (forward-line -1)
                (looking-at "[-_/|\\*o. ]*"))
              (setq graph (match-string 0))
              (unless (string-match-p "[/\\]" graph)
                (insert graph ?\n))))))))
  t)

(defun fosgit-format-log-margin (&optional author date)
  (-let [(width unit-width duration-spec) fosgit-log-margin-spec]
    (when (and date (not author))
      (setq width (+ (if (= unit-width 1) 1 (1+ unit-width))
                     (if (derived-mode-p 'fosgit-log-mode) 1 0))))
    (if date
        (fosgit-make-margin-overlay
         (and author
              (concat (propertize (truncate-string-to-width
                                   (or author "")
                                   (- width 1 3 ; gap, digits
                                      (if (= unit-width 1) 1 (1+ unit-width))
                                      (if (derived-mode-p 'fosgit-log-mode) 1 0))
                                   nil ?\s (make-string 1 fosgit-ellipsis))
                                  'face 'fosgit-log-author)
                      " "))
         (propertize (fosgit-format-duration
                      (abs (truncate (- (float-time)
                                        (string-to-number date))))
                      (symbol-value duration-spec)
                      unit-width)
                     'face 'fosgit-log-date)
         (and (derived-mode-p 'fosgit-log-mode)
              (propertize " " 'face 'fringe)))
      (fosgit-make-margin-overlay
       (propertize (make-string (1- width) ?\s) 'face 'default)
       (propertize " " 'face 'fringe)))))

(defun fosgit-format-duration (duration spec &optional width)
  (-let [(char unit units weight) (car spec)]
    (let ((cnt (round (/ duration weight 1.0))))
      (if (or (not (cdr spec))
              (>= (/ duration weight) 1))
          (if (eq width 1)
              (format "%3i%c" cnt char)
            (format (if width (format "%%3i %%-%is" width) "%i %s")
                    cnt (if (= cnt 1) unit units)))
        (fosgit-format-duration duration (cdr spec) width)))))

(defun fosgit-log-maybe-show-more-commits (section)
  "Automatically insert more commit sections in a log.
Only do so if `point' is on the \"show more\" section,
and `fosgit-log-auto-more' is non-nil."
  (when (and (eq (fosgit-section-type section) 'longer)
             fosgit-log-auto-more)
    (fosgit-log-double-commit-limit)
    (forward-line -1)
    (fosgit-section-forward)))

(defvar fosgit--update-revision-buffer nil)

(defun fosgit-log-maybe-update-revision-buffer (&optional _)
  "When moving in the log buffer, update the revision buffer.
If there is no revision buffer in the same frame, then do nothing."
  (when (derived-mode-p 'fosgit-log-mode)
    (fosgit-log-maybe-update-revision-buffer-1)))

(defun fosgit-log-maybe-update-revision-buffer-1 ()
  (unless fosgit--update-revision-buffer
    (-when-let* ((commit (fosgit-section-when 'commit))
                 (buffer (fosgit-mode-get-buffer 'fosgit-revision-mode nil t)))
      (setq fosgit--update-revision-buffer (list commit buffer))
      (run-with-idle-timer
       fosgit-update-other-window-delay nil
       (lambda ()
         (-let [(rev buf) fosgit--update-revision-buffer]
           (setq fosgit--update-revision-buffer nil)
           (when (buffer-live-p buf)
             (let ((fosgit-display-buffer-noselect t))
               (apply #'fosgit-show-commit rev (fosgit-diff-arguments)))))
         (setq fosgit--update-revision-buffer nil))))))

(defvar fosgit--update-blob-buffer nil)

(defun fosgit-log-maybe-update-blob-buffer (&optional _)
  "When moving in the log buffer, update the blob buffer.
If there is no blob buffer in the same frame, then do nothing."
  (when (derived-mode-p 'fosgit-log-mode)
    (fosgit-log-maybe-update-blob-buffer-1)))

(defun fosgit-log-maybe-update-blob-buffer-1 ()
  (unless fosgit--update-revision-buffer
    (-when-let* ((commit (fosgit-section-when 'commit))
                 (buffer (--first (with-current-buffer it fosgit-buffer-revision)
                                  (-map #'window-buffer (window-list)))))
        (setq fosgit--update-blob-buffer (list commit buffer))
        (run-with-idle-timer
         fosgit-update-other-window-delay nil
         (lambda ()
           (-let [(rev buf) fosgit--update-blob-buffer]
             (setq fosgit--update-blob-buffer nil)
             (when (buffer-live-p buf)
               (save-excursion
                 (with-selected-window (get-buffer-window buf)
                   (with-current-buffer buf
                     (fosgit-blob-visit (list (fosgit-rev-parse rev)
                                             (fosgit-file-relative-name
                                              fosgit-buffer-file-name))
                                       (line-number-at-pos))))))))))))

(defun fosgit-log-goto-same-commit ()
  (-when-let* ((prev fosgit-previous-section)
               (rev  (cond ((fosgit-section-match 'commit prev)
                            (fosgit-section-value prev))
                           ((fosgit-section-match 'branch prev)
                            (fosgit-rev-format
                             "%h" (fosgit-section-value prev)))))
               (same (--first (equal (fosgit-section-value it) rev)
                              (fosgit-section-children fosgit-root-section))))
    (goto-char (fosgit-section-start same))))

;;; Select Mode

(defvar fosgit-log-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map fosgit-log-mode-map)
    (define-key map "\C-c\C-b" 'undefined)
    (define-key map "\C-c\C-f" 'undefined)
    (define-key map "."        'fosgit-log-select-pick)
    (define-key map "e"        'fosgit-log-select-pick)
    (define-key map "\C-c\C-c" 'fosgit-log-select-pick)
    (define-key map "q"        'fosgit-log-select-quit)
    (define-key map "\C-c\C-k" 'fosgit-log-select-quit)
    map)
  "Keymap for `fosgit-log-select-mode'.")

(put 'fosgit-log-select-pick :advertised-binding [?\C-c ?\C-c])
(put 'fosgit-log-select-quit :advertised-binding [?\C-c ?\C-k])

(define-derived-mode fosgit-log-select-mode fosgit-log-mode "Fosgit Select"
  "Mode for selecting a commit from history.

This mode is documented in info node `(fosgit)Select from log'.

\\<fosgit-mode-map>\
Type \\[fosgit-refresh] to refresh the current buffer.
Type \\[fosgit-visit-thing] or \\[fosgit-diff-show-or-scroll-up] \
to visit the commit at point.

\\<fosgit-log-select-mode-map>\
Type \\[fosgit-log-select-pick] to select the commit at point.
Type \\[fosgit-log-select-quit] to abort without selecting a commit."
  :group 'fosgit-log
  (hack-dir-local-variables-non-file-buffer))

(defun fosgit-log-select-refresh-buffer (rev args)
  (fosgit-insert-section (logbuf)
    (fosgit-insert-log rev args)))

(defvar-local fosgit-log-select-pick-function nil)
(defvar-local fosgit-log-select-quit-function nil)

(defun fosgit-log-select (pick &optional msg quit branch)
  (declare (indent defun))
  (fosgit-mode-setup #'fosgit-log-select-mode
                    (or branch (fosgit-get-current-branch) "HEAD")
                    fosgit-log-select-arguments)
  (fosgit-log-goto-same-commit)
  (setq fosgit-log-select-pick-function pick)
  (setq fosgit-log-select-quit-function quit)
  (when fosgit-log-select-show-usage
    (setq msg (substitute-command-keys
               (format-spec
                (if msg
                    (if (string-suffix-p "," msg)
                        (concat msg " or %q to abort")
                      msg)
                  "Type %p to select commit at point, or %q to abort")
                '((?p . "\\[fosgit-log-select-pick]")
                  (?q . "\\[fosgit-log-select-quit]")))))
    (when (memq fosgit-log-select-show-usage '(both header-line))
      (setq header-line-format (propertize (concat " " msg) 'face 'bold)))
    (when (memq fosgit-log-select-show-usage '(both echo-area))
      (message "%s" msg))))

(defun fosgit-log-select-pick ()
  "Select the commit at point and act on it.
Call `fosgit-log-select-pick-function' with the selected
commit as argument."
  (interactive)
  (let ((fun fosgit-log-select-pick-function)
        (rev (fosgit-commit-at-point)))
    (kill-buffer (current-buffer))
    (funcall fun rev)))

(defun fosgit-log-select-quit ()
  "Abort selecting a commit, don't act on any commit."
  (interactive)
  (kill-buffer (current-buffer))
  (when fosgit-log-select-quit-function
    (funcall fosgit-log-select-quit-function)))

;;; Cherry Mode

(defvar fosgit-cherry-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map fosgit-mode-map)
    (define-key map "q" 'fosgit-log-bury-buffer)
    (define-key map "L" 'fosgit-toggle-margin)
    map)
  "Keymap for `fosgit-cherry-mode'.")

(define-derived-mode fosgit-cherry-mode fosgit-mode "Fosgit Cherry"
  "Mode for looking at commits not merged upstream.

\\<fosgit-mode-map>\
Type \\[fosgit-refresh] to refresh the current buffer.
Type \\[fosgit-visit-thing] or \\[fosgit-diff-show-or-scroll-up] \
to visit the commit at point.

Type \\[fosgit-cherry-pick-popup] to apply the commit at point.

\\{fosgit-cherry-mode-map}"
  :group 'fosgit-log
  (hack-dir-local-variables-non-file-buffer))

;;;###autoload
(defun fosgit-cherry (head upstream)
  "Show commits in a branch that are not merged in the upstream branch."
  (interactive
   (let  ((head (fosgit-read-branch "Cherry head")))
     (list head (fosgit-read-other-branch "Cherry upstream" head
                                         (fosgit-get-upstream-branch head)))))
  (fosgit-mode-setup #'fosgit-cherry-mode upstream head))

(defun fosgit-cherry-refresh-buffer (_upstream _head)
  (fosgit-insert-section (cherry)
    (run-hooks 'fosgit-cherry-sections-hook)))

(defun fosgit-insert-cherry-headers ()
  "Insert headers appropriate for `fosgit-cherry-mode' buffers."
  (fosgit-insert-head-branch-header (nth 1 fosgit-refresh-args))
  (fosgit-insert-upstream-branch-header (nth 1 fosgit-refresh-args)
                                       (nth 0 fosgit-refresh-args)
                                       "Upstream: ")
  (insert ?\n))

(defun fosgit-insert-cherry-commits ()
  "Insert commit sections into a `fosgit-cherry-mode' buffer."
  (fosgit-insert-section (cherries)
    (fosgit-insert-heading "Cherry commits:")
    (fosgit-git-wash (apply-partially 'fosgit-log-wash-log 'cherry)
      "cherry" "-v" "--abbrev" fosgit-refresh-args)))

;;; Reflog Mode

(defvar fosgit-reflog-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map fosgit-log-mode-map)
    (define-key map "L" 'fosgit-toggle-margin)
    map)
  "Keymap for `fosgit-reflog-mode'.")

(define-derived-mode fosgit-reflog-mode fosgit-log-mode "Fosgit Reflog"
  "Mode for looking at Git reflog.

This mode is documented in info node `(fosgit)Reflog'.

\\<fosgit-mode-map>\
Type \\[fosgit-refresh] to refresh the current buffer.
Type \\[fosgit-visit-thing] or \\[fosgit-diff-show-or-scroll-up] \
to visit the commit at point.

Type \\[fosgit-cherry-pick-popup] to apply the commit at point.
Type \\[fosgit-reset] to reset HEAD to the commit at point.

\\{fosgit-reflog-mode-map}"
  :group 'fosgit-log
  (hack-dir-local-variables-non-file-buffer))

(defun fosgit-reflog-refresh-buffer (ref args)
  (setq header-line-format
        (propertize (concat " Reflog for " ref) 'face 'fosgit-header-line))
  (fosgit-insert-section (reflogbuf)
    (fosgit-git-wash (apply-partially 'fosgit-log-wash-log 'reflog)
      "reflog" "show" "--format=%h %gd %gs" "--date=raw" args ref)))

(defvar fosgit-reflog-labels
  '(("commit"      . fosgit-reflog-commit)
    ("amend"       . fosgit-reflog-amend)
    ("merge"       . fosgit-reflog-merge)
    ("checkout"    . fosgit-reflog-checkout)
    ("branch"      . fosgit-reflog-checkout)
    ("reset"       . fosgit-reflog-reset)
    ("rebase"      . fosgit-reflog-rebase)
    ("cherry-pick" . fosgit-reflog-cherry-pick)
    ("initial"     . fosgit-reflog-commit)
    ("pull"        . fosgit-reflog-remote)
    ("clone"       . fosgit-reflog-remote)
    ("autosave"    . fosgit-reflog-commit)
    ("restart"     . fosgit-reflog-reset)))

(defun fosgit-reflog-format-subject (subject)
  (let* ((match (string-match fosgit-reflog-subject-re subject))
         (command (and match (match-string 1 subject)))
         (option  (and match (match-string 2 subject)))
         (type    (and match (match-string 3 subject)))
         (label (if (string= command "commit")
                    (or type command)
                  command))
         (text (if (string= command "commit")
                   label
                 (mapconcat #'identity
                            (delq nil (list command option type))
                            " "))))
    (format "%-16s "
            (propertize text 'face
                        (or (cdr (assoc label fosgit-reflog-labels))
                            'fosgit-reflog-other)))))

;;; Log Sections
;;;; Standard Log Sections

(defvar fosgit-unpulled-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap fosgit-visit-thing] 'fosgit-diff-dwim)
    map)
  "Keymap for `unpulled' sections.")

(fosgit-define-section-jumper fosgit-jump-to-unpulled-from-upstream
  "Unpulled from @{upstream}" unpulled "..@{upstream}")

(defun fosgit-insert-unpulled-from-upstream ()
  "Insert commits that haven't been pulled from the upstream yet."
  (when (fosgit-git-success "rev-parse" "@{upstream}")
    (fosgit-insert-section (unpulled "..@{upstream}")
      (fosgit-insert-heading
        (format (propertize "Unpulled from %s:" 'face 'fosgit-section-heading)
                (fosgit-get-upstream-branch)))
      (fosgit-insert-log "..@{upstream}" fosgit-log-section-arguments)
      (fosgit-section-cache-visibility))))

(fosgit-define-section-jumper fosgit-jump-to-unpulled-from-pushremote
  "Unpulled from <push-remote>" unpulled
  (concat ".." (fosgit-get-push-branch)))

(defun fosgit-insert-unpulled-from-pushremote ()
  "Insert commits that haven't been pulled from the push-remote yet."
  (--when-let (fosgit-get-push-branch)
    (unless (and (equal (fosgit-rev-name it)
                        (fosgit-rev-name "@{upstream}"))
                 (or (memq 'fosgit-insert-unpulled-from-upstream
                           fosgit-status-sections-hook)
                     (memq 'fosgit-insert-unpulled-from-upstream-or-recent
                           fosgit-status-sections-hook)))
      (fosgit-insert-section (unpulled (concat ".." it))
        (fosgit-insert-heading
          (format (propertize "Unpulled from %s:" 'face 'fosgit-section-heading)
                  (propertize it 'face 'fosgit-branch-remote)))
        (fosgit-insert-log (concat ".." it) fosgit-log-section-arguments)
        (fosgit-section-cache-visibility)))))

(defvar fosgit-unpushed-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap fosgit-visit-thing] 'fosgit-diff-dwim)
    map)
  "Keymap for `unpushed' sections.")

(fosgit-define-section-jumper fosgit-jump-to-unpushed-to-upstream
  "Unpushed to @{upstream}" unpushed "@{upstream}..")

(defun fosgit-insert-unpushed-to-upstream ()
  "Insert commits that haven't been pushed to the upstream yet."
  (when (fosgit-git-success "rev-parse" "@{upstream}")
    (fosgit-insert-section (unpushed "@{upstream}..")
      (fosgit-insert-heading
        (format (propertize "Unmerged into %s:" 'face 'fosgit-section-heading)
                (fosgit-get-upstream-branch)))
      (fosgit-insert-log "@{upstream}.." fosgit-log-section-arguments)
      (fosgit-section-cache-visibility))))

(fosgit-define-section-jumper fosgit-jump-to-unpushed-to-pushremote
  "Unpushed to <push-remote>" unpushed
  (concat (fosgit-get-push-branch) ".."))

(defun fosgit-insert-unpushed-to-pushremote ()
  "Insert commits that haven't been pushed to the push-remote yet."
  (--when-let (fosgit-get-push-branch)
    (unless (and (equal (fosgit-rev-name it)
                        (fosgit-rev-name "@{upstream}"))
                 (memq 'fosgit-insert-unpushed-to-upstream
                       fosgit-status-sections-hook))
      (fosgit-insert-section (unpushed (concat it ".."))
        (fosgit-insert-heading
          (format (propertize "Unpushed to %s:" 'face 'fosgit-section-heading)
                  (propertize it 'face 'fosgit-branch-remote)))
        (fosgit-insert-log (concat it "..") fosgit-log-section-arguments)
        (fosgit-section-cache-visibility)))))

(defun fosgit-insert-recent-commits (&optional collapse)
  "Insert section showing recent commits.
Show the last `fosgit-log-section-commit-count' commits."
  (let* ((start (format "HEAD~%s" fosgit-log-section-commit-count))
         (range (and (fosgit-rev-verify start)
                     (concat start "..HEAD"))))
    (fosgit-insert-section (recent range collapse)
      (fosgit-insert-heading "Recent commits:")
      (fosgit-insert-log range
                        (cons (format "-%d" fosgit-log-section-commit-count)
                              fosgit-log-section-arguments)))))

(defun fosgit-insert-unpulled-from-upstream-or-recent ()
  "Insert section showing unpulled or recent commits.
If an upstream is configured for the current branch and it is
ahead of the current branch, then show the commits that have
not yet been pulled into the current branch.  If no upstream is
configured or if the upstream is not ahead of the current branch,
then show the last `fosgit-log-section-commit-count' commits."
  (if (equal (fosgit-rev-parse "HEAD")
             (fosgit-rev-parse "@{upstream}"))
      (fosgit-insert-recent-commits t)
    (fosgit-insert-unpulled-from-upstream)))

;;;; Auxiliary Log Sections

(defun fosgit-insert-unpulled-cherries ()
  "Insert section showing unpulled commits.
Like `fosgit-insert-unpulled-to-upstream' but prefix each commit
which has not been applied yet (i.e. a commit with a patch-id
not shared with any local commit) with \"+\", and all others with
\"-\"."
  (when (fosgit-git-success "rev-parse" "@{upstream}")
    (fosgit-insert-section (unpulled "..@{upstream}")
      (fosgit-insert-heading "Unpulled commits:")
      (fosgit-git-wash (apply-partially 'fosgit-log-wash-log 'cherry)
        "cherry" "-v" (fosgit-abbrev-arg)
        (fosgit-get-current-branch) "@{upstream}"))))

(defun fosgit-insert-unpushed-cherries ()
  "Insert section showing unpushed commits.
Like `fosgit-insert-unpushed-to-upstream' but prefix each commit
which has not been applied to upstream yet (i.e. a commit with
a patch-id not shared with any upstream commit) with \"+\", and
all others with \"-\"."
  (when (fosgit-git-success "rev-parse" "@{upstream}")
    (fosgit-insert-section (unpushed "@{upstream}..")
      (fosgit-insert-heading "Unpushed commits:")
      (fosgit-git-wash (apply-partially 'fosgit-log-wash-log 'cherry)
        "cherry" "-v" (fosgit-abbrev-arg) "@{upstream}"))))

;;; Buffer Margins

(defvar-local fosgit-set-buffer-margin-refresh nil)

(defvar-local fosgit-show-margin nil)
(put 'fosgit-show-margin 'permanent-local t)

(defun fosgit-toggle-margin ()
  "Show or hide the Fosgit margin."
  (interactive)
  (unless (derived-mode-p 'fosgit-log-mode 'fosgit-status-mode 'fosgit-refs-mode)
    (user-error "Buffer doesn't contain any logs"))
  (fosgit-set-buffer-margin (not (cdr (window-margins)))))

(defun fosgit-maybe-show-margin ()
  "Maybe show the margin, depending on the major-mode and an option.
Supported modes are `fosgit-log-mode' and `fosgit-reflog-mode',
and the respective options are `fosgit-log-show-margin' and
`fosgit-reflog-show-margin'."
  (if (local-variable-p 'fosgit-show-margin)
      (fosgit-set-buffer-margin fosgit-show-margin)
    (pcase major-mode
      (`fosgit-log-mode    (fosgit-set-buffer-margin fosgit-log-show-margin))
      (`fosgit-reflog-mode (fosgit-set-buffer-margin fosgit-reflog-show-margin)))))

(defun fosgit-set-buffer-margin (enable)
  (let ((width (cond ((not enable) nil)
                     ((derived-mode-p 'fosgit-reflog-mode)
                      (+ (cadr fosgit-log-margin-spec) 5))
                     (t (car fosgit-log-margin-spec)))))
    (setq fosgit-show-margin width)
    (when (and enable fosgit-set-buffer-margin-refresh)
      (fosgit-refresh))
    (-when-let (window (get-buffer-window))
      (with-selected-window window
        (set-window-margins nil (car (window-margins)) width)
        (if enable
            (add-hook  'window-configuration-change-hook
                       'fosgit-set-buffer-margin-1 nil t)
          (remove-hook 'window-configuration-change-hook
                       'fosgit-set-buffer-margin-1 t))))))

(defun fosgit-set-buffer-margin-1 ()
  (-when-let (window (get-buffer-window))
    (with-selected-window window
      (set-window-margins nil (car (window-margins)) fosgit-show-margin))))

(defun fosgit-make-margin-overlay (&rest strings)
  ;; Don't put the overlay on the complete line to work around #1880.
  (let ((o (make-overlay (1+ (line-beginning-position))
                         (line-end-position)
                         nil t)))
    (overlay-put o 'evaporate t)
    (overlay-put o 'before-string
                 (propertize "o" 'display
                             (list '(margin right-margin)
                                   (apply #'concat strings))))))

;;; fosgit-log.el ends soon

(define-obsolete-function-alias 'fosgit-insert-unpulled-or-recent-commits
  'fosgit-insert-unpulled-from-upstream-or-recent "Fosgit 2.4.0")

(provide 'fosgit-log)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; fosgit-log.el ends here
