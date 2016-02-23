;;; fosgit-git.el --- Git functionality  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2016  The Fosgit Project Contributors
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

;; This library implements wrappers for various Git plumbing commands.

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'fosgit-utils)
(require 'fosgit-section)

(declare-function fosgit-process-buffer 'fosgit-process)
(declare-function fosgit-process-file 'fosgit-process)
(declare-function fosgit-process-insert-section 'fosgit-process)
(defvar fosgit-process-error-message-re)
(defvar fosgit-refresh-args) ; from `fosgit-mode' for `fosgit-current-file'
(defvar fosgit-branch-prefer-remote-upstream)

(defvar fosgit-tramp-process-environment nil)

(require 'crm)

;;; Options

;; For now this is shared between `fosgit-process' and `fosgit-git'.
(defgroup fosgit-process nil
  "Git and other external processes used by Fosgit."
  :group 'fosgit)

(defvar fosgit-git-environment nil
  "Prepended to `process-environment' while running git.")

(defcustom fosgit-git-executable
  ;; Git might be installed in a different location on a remote, so
  ;; it is better not to use the full path to the executable, except
  ;; on Window were we would otherwise end up using one one of the
  ;; wrappers "cmd/git.exe" or "cmd/git.cmd", which are much slower
  ;; than using "bin/git.exe" directly.
  (or (and (eq system-type 'windows-nt)
           (--when-let (executable-find "git.exe")
             (or (ignore-errors
                   ;; Git for Windows 2.x provides cygpath so we can
                   ;; ask it for native paths.  Using an upper case
                   ;; alias makes this fail on 1.x (which is good,
                   ;; because we would not want to end up using some
                   ;; other cygpath).
                   (prog1 (car
                           (process-lines
                            it "-c"
                            "alias.X=!x() { which \"$1\" | cygpath -mf -; }; x"
                            "X" "git"))
                     (setq fosgit-git-environment
                           (list (concat "PATH="
                                         (car (process-lines
                                               it "-c"
                                               "alias.P=!cygpath -wp \"$PATH\""
                                               "P")))))))
                 ;; For 1.x, we search for bin/ next to cmd/.
                 (let ((alt (directory-file-name (file-name-directory it))))
                   (if (and (equal (file-name-nondirectory alt) "cmd")
                            (setq alt (expand-file-name
                                       (convert-standard-filename "bin/git.exe")
                                       (file-name-directory alt)))
                            (file-executable-p alt))
                       alt
                     it)))))
      "git")
  "The Git executable used by Fosgit."
  :group 'fosgit-process
  :type 'string)

(defcustom fosgit-git-global-arguments
  '("--no-pager" "--literal-pathspecs" "-c" "core.preloadindex=true")
  "Global git arguments.

The arguments set here are used every time the git executable is
run as a subprocess.  They are placed right after the executable
itself and before the git command - as in `git HERE... COMMAND
REST'.  See the manpage `git(1)' for valid arguments.

Be careful what you add here, especially if you are using Tramp
to connect to servers with ancient Git versions.  Never remove
anything that is part of the default value, unless you really
know what you are doing.  And think very hard before adding
something; it will be used every time Fosgit runs Git for any
purpose."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit
  :type '(repeat string))

(define-obsolete-variable-alias 'fosgit-git-standard-options
  'fosgit-git-global-arguments "2.1.0")

(defcustom fosgit-git-debug nil
  "Whether to enable additional reporting of git errors.

Fosgit basically calls git for one of these two reasons: for
side-effects or to do something with its standard output.

When git is run for side-effects then its output, including error
messages, go into the process buffer which is shown when using \
\\<fosgit-status-mode-map>\\[fosgit-process].

When git's output is consumed in some way, then it would be too
expensive to also insert it into this buffer, but when this
option is non-nil and git returns with a non-zero exit status,
then at least its standard error is inserted into this buffer.

This is only intended for debugging purposes.  Do not enable this
permanently, that would negatively affect performance"
  :group 'fosgit
  :group 'fosgit-process
  :type 'boolean)

(defcustom fosgit-ref-namespaces
  '(("^@$"                       fosgit-head nil)
    ("^refs/tags/\\(.+\\)"       fosgit-tag nil)
    ("^refs/heads/\\(.+\\)"      fosgit-branch-local nil)
    ("^refs/remotes/\\(.+\\)"    fosgit-branch-remote nil)
    ("^refs/bisect/\\(bad\\)"    fosgit-bisect-bad nil)
    ("^refs/bisect/\\(skip.*\\)" fosgit-bisect-skip nil)
    ("^refs/bisect/\\(good.*\\)" fosgit-bisect-good nil)
    ("^refs/stash$"              fosgit-refname-stash nil)
    ("^refs/wip/\\(.+\\)"        fosgit-refname-wip nil)
    ("^\\(bad\\):"               fosgit-bisect-bad nil)
    ("^\\(skip\\):"              fosgit-bisect-skip nil)
    ("^\\(good\\):"              fosgit-bisect-good nil)
    ("\\(.+\\)"                  fosgit-refname nil))
  "How refs are formatted for display.

Each entry controls how a certain type of ref is displayed, and
has the form (REGEXP FACE FORMATTER).  REGEXP is a regular
expression used to match full refs.  The first entry whose REGEXP
matches the reference is used.  The first regexp submatch becomes
the \"label\" that represents the ref and is propertized with
font FONT.  If FORMATTER is non-nil it should be a function that
takes two arguments, the full ref and the face.  It is supposed
to return a propertized label that represents the ref."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-modes
  :type '(repeat
          (list regexp
                face
                (choice (const :tag "first submatch is label" nil)
                        (function :tag "format using function")))))

(defcustom fosgit-prefer-remote-upstream nil
  "Whether to favor remote branches when reading the upstream branch.

This controls whether commands that read a branch from the user
and then set it as the upstream branch, offer a local or a remote
branch as default completion candidate, when they have the choice.

This affects all commands that use `fosgit-read-upstream-branch'
or `fosgit-read-starting-point', which includes all commands that
change the upstream and many which create new branches."
  :package-version '(fosgit . "2.4.2")
  :group 'fosgit-commands
  :type 'boolean)

;;; Git

(defun fosgit-process-git-arguments (args)
  "Prepare ARGS for a function that invokes Git.

Fosgit has many specialized functions for running Git; they all
pass arguments through this function before handing them to Git,
to do the following.

* Flatten ARGS, removing nil arguments.
* Prepend `fosgit-git-global-arguments' to ARGS."
  (append fosgit-git-global-arguments (-flatten args)))

(defun fosgit-git-exit-code (&rest args)
  "Execute Git with ARGS, returning its exit code."
  (apply #'fosgit-process-file fosgit-git-executable nil nil nil
         (fosgit-process-git-arguments args)))

(defun fosgit-git-success (&rest args)
  "Execute Git with ARGS, returning t if its exit code is 0."
  (= (fosgit-git-exit-code args) 0))

(defun fosgit-git-failure (&rest args)
  "Execute Git with ARGS, returning t if its exit code is 1."
  (= (fosgit-git-exit-code args) 1))

(defun fosgit-git-str (&rest args)
  "Execute Git with ARGS, returning the first line of its output.
If there is no output return nil.  If the output begins with a
newline return an empty string.  Like `fosgit-git-string' but
ignore `fosgit-git-debug'."
  (with-temp-buffer
    (apply #'fosgit-process-file fosgit-git-executable nil (list t nil) nil
           (fosgit-process-git-arguments args))
    (unless (bobp)
      (goto-char (point-min))
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun fosgit-git-true (&rest args)
  "Execute Git with ARGS, returning t if it prints \"true\".
Return t if the first (and usually only) output line is the
string \"true\", otherwise return nil."
  (equal (fosgit-git-str args) "true"))

(defun fosgit-git-false (&rest args)
  "Execute Git with ARGS, returning t if it prints \"false\".
Return t if the first (and usually only) output line is the
string \"false\", otherwise return nil."
  (equal (fosgit-git-str args) "false"))

(defun fosgit-git-insert (&rest args)
  "Execute Git with ARGS, inserting its output at point.
If Git exits with a non-zero exit status, then show a message and
add a section in the respective process buffer."
  (setq args (fosgit-process-git-arguments args))
  (if fosgit-git-debug
      (let (log)
        (unwind-protect
            (progn
              (setq log (make-temp-file "fosgit-stderr"))
              (delete-file log)
              (let ((exit (apply #'fosgit-process-file fosgit-git-executable
                                 nil (list t log) nil args)))
                (when (> exit 0)
                  (let ((msg "Git failed"))
                    (when (file-exists-p log)
                      (setq msg (with-temp-buffer
                                  (insert-file-contents log)
                                  (goto-char (point-max))
                                  (and (re-search-backward
                                        fosgit-process-error-message-re nil t)
                                       (match-string 1))))
                      (let ((fosgit-git-debug nil))
                        (with-current-buffer (fosgit-process-buffer t)
                          (fosgit-process-insert-section default-directory
                                                        fosgit-git-executable
                                                        args exit log))))
                    (message "%s" msg)))
                exit))
          (ignore-errors (delete-file log))))
    (apply #'fosgit-process-file fosgit-git-executable
           nil (list t nil) nil args)))

(defun fosgit-git-string (&rest args)
  "Execute Git with ARGS, returning the first line of its output.
If there is no output return nil.  If the output begins with a
newline return an empty string."
  (with-temp-buffer
    (apply #'fosgit-git-insert args)
    (unless (bobp)
      (goto-char (point-min))
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun fosgit-git-lines (&rest args)
  "Execute Git with ARGS, returning its output as a list of lines.
Empty lines anywhere in the output are omitted.

If Git exits with a non-zero exit status, then report show a
message and add a section in the respective process buffer."
  (with-temp-buffer
    (apply #'fosgit-git-insert args)
    (split-string (buffer-string) "\n" t)))

(defun fosgit-git-items (&rest args)
  "Execute Git with ARGS, returning its null-separated output as a list.
Empty items anywhere in the output are omitted.

If Git exits with a non-zero exit status, then report show a
message and add a section in the respective process buffer."
  (with-temp-buffer
    (apply #'fosgit-git-insert args)
    (split-string (buffer-string) "\0" t)))

(defun fosgit-git-wash (washer &rest args)
  "Execute Git with ARGS, inserting washed output at point.
Actually first insert the raw output at point.  If there is no
output call `fosgit-cancel-section'.  Otherwise temporarily narrow
the buffer to the inserted text, move to its beginning, and then
call function WASHER with no argument."
  (declare (indent 1))
  (let ((beg (point)))
    (setq args (-flatten args))
    (fosgit-git-insert args)
    (if (= (point) beg)
        (fosgit-cancel-section)
      (unless (bolp)
        (insert "\n"))
      (save-restriction
        (narrow-to-region beg (point))
        (goto-char beg)
        (funcall washer args))
      (when (or (= (point) beg)
                (= (point) (1+ beg)))
        (fosgit-cancel-section)))))

(defun fosgit-git-version (&optional raw)
  (--when-let (let (fosgit-git-global-arguments)
                (ignore-errors (substring (fosgit-git-string "version") 12)))
    (if raw it (and (string-match "^\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" it)
                    (match-string 1 it)))))

;;; Files

(defun fosgit--safe-default-directory (&optional file)
  (catch 'unsafe-default-dir
    (let ((dir (file-name-as-directory
                (expand-file-name (or file default-directory))))
          (previous nil))
      (while (not (file-accessible-directory-p dir))
        (setq dir (file-name-directory (directory-file-name dir)))
        (when (equal dir previous)
          (throw 'unsafe-default-dir nil))
        (setq previous dir))
      dir)))

(defmacro fosgit--with-safe-default-directory (file &rest body)
  (declare (indent 1) (debug (form body)))
  `(-when-let (default-directory (fosgit--safe-default-directory ,file))
     ,@body))

(defun fosgit-git-dir (&optional path)
  "Return absolute path to the control directory of the current repository.

All symlinks are followed.  If optional PATH is non-nil, then
it has to be a path relative to the control directory and its
absolute path is returned."
  (fosgit--with-safe-default-directory nil
    (--when-let (fosgit-rev-parse-safe "--git-dir")
      (setq it (file-name-as-directory (fosgit-expand-git-file-name it)))
      (if path (expand-file-name (convert-standard-filename path) it) it))))

(defun fosgit-toplevel (&optional directory)
  "Return the absolute path to the toplevel of the current repository.

From within the working tree or control directory of a repository
return the absolute path to the toplevel directory of the working
tree.  As a special case, from within a bare repository return
the control directory instead.  When called outside a repository
then return nil.

When optional DIRECTORY is non-nil then return the toplevel for
that directory instead of the one for `default-directory'.

Try to respect the option `find-file-visit-truename', i.e.  when
the value of that option is nil, then avoid needlessly returning
the truename.  When a symlink to a sub-directory of the working
tree is involved, or when called from within a sub-directory of
the gitdir or from the toplevel of a gitdir, which itself is not
located within the working tree, then it is not possible to avoid
returning the truename."
  (fosgit--with-safe-default-directory directory
    (-if-let (topdir (fosgit-rev-parse-safe "--show-toplevel"))
        (let (updir)
          (setq topdir (fosgit-expand-git-file-name topdir))
          (if (and
               ;; Always honor these settings.
               (not find-file-visit-truename)
               (not (getenv "GIT_WORK_TREE"))
               ;; `--show-cdup' is the relative path to the toplevel
               ;; from `(file-truename default-directory)'.  Here we
               ;; pretend it is relative to `default-directory', and
               ;; go to that directory.  Then we check whether
               ;; `--show-toplevel' still returns the same value and
               ;; whether `--show-cdup' now is the empty string.  If
               ;; both is the case, then we are at the toplevel of
               ;; the same working tree, but also avoided needlessly
               ;; following any symlinks.
               (progn
                 (setq updir (file-name-as-directory
                              (fosgit-rev-parse-safe "--show-cdup")))
                 (setq updir (if (file-name-absolute-p updir)
                                 (concat (file-remote-p default-directory) updir)
                               (expand-file-name updir)))
                 (let ((default-directory updir))
                   (and (string-equal (fosgit-rev-parse-safe "--show-cdup") "")
                        (--when-let (fosgit-rev-parse-safe "--show-toplevel")
                          (string-equal (fosgit-expand-git-file-name it)
                                        topdir))))))
              updir
            (concat (file-remote-p default-directory)
                    (file-name-as-directory topdir))))
      (-when-let (gitdir (fosgit-rev-parse-safe "--git-dir"))
        (setq gitdir (file-name-as-directory
                      (if (file-name-absolute-p gitdir)
                          ;; We might have followed a symlink.
                          (concat (file-remote-p default-directory)
                                  (fosgit-expand-git-file-name gitdir))
                        (expand-file-name gitdir))))
        (if (fosgit-bare-repo-p)
            gitdir
          (let* ((link (expand-file-name "gitdir" gitdir))
                 (wtree (and (file-exists-p link)
                             (fosgit-file-line link))))
            (if (and wtree
                     ;; Ignore .git/gitdir files that result from a
                     ;; Git bug.  See #2364.
                     (not (equal wtree ".git")))
                ;; Return the linked working tree.
                (file-name-directory wtree)
              ;; Step outside the control directory to enter the working tree.
              (file-name-directory (directory-file-name gitdir)))))))))

(defmacro fosgit-with-toplevel (&rest body)
  (declare (indent defun) (debug (body)))
  (let ((toplevel (cl-gensym "toplevel")))
    `(let ((,toplevel (fosgit-toplevel)))
       (if ,toplevel
           (let ((default-directory ,toplevel))
             ,@body)
         (error "Not inside a Git repository: %s" default-directory)))))

(defun fosgit-inside-gitdir-p ()
  "Return t if `default-directory' is below a repository directory."
  (fosgit-rev-parse-p "--is-inside-git-dir"))

(defun fosgit-inside-worktree-p ()
  "Return t if `default-directory' is below the work tree of a repository."
  (fosgit-rev-parse-p "--is-inside-work-tree"))

(defun fosgit-bare-repo-p ()
  "Return t if the current repository is bare."
  (fosgit-rev-parse-p "--is-bare-repository"))

(defun fosgit-git-repo-p (directory &optional non-bare)
  "Return t if DIRECTORY is a Git repository.
When optional NON-BARE is non-nil also return nil if DIRECTORY is
a bare repositories."
  (or (file-regular-p (expand-file-name ".git" directory))
      (file-directory-p (expand-file-name ".git" directory))
      (and (not non-bare)
           (file-regular-p (expand-file-name "HEAD" directory))
           (file-directory-p (expand-file-name "refs" directory))
           (file-directory-p (expand-file-name "objects" directory)))))

(defvar-local fosgit-buffer-revision  nil)
(defvar-local fosgit-buffer-refname   nil)
(defvar-local fosgit-buffer-file-name nil)
(put 'fosgit-buffer-revision  'permanent-local t)
(put 'fosgit-buffer-refname   'permanent-local t)
(put 'fosgit-buffer-file-name 'permanent-local t)

(defun fosgit-file-relative-name (&optional file tracked)
  "Return the path of FILE relative to the repository root.

If optional FILE is nil or omitted return the relative path of
the file being visited in the current buffer, if any, else nil.
If the file is not inside a Git repository then return nil.

If TRACKED is non-nil, return the path only if it matches a
tracked file."
  (unless file
    (with-current-buffer (or (buffer-base-buffer)
                             (current-buffer))
      (setq file (or fosgit-buffer-file-name buffer-file-name))))
  (when (and file (or (not tracked)
                      (fosgit-file-tracked-p (file-relative-name file))))
    (--when-let (fosgit-toplevel
                 (fosgit--safe-default-directory
                  (directory-file-name (file-name-directory file))))
      (file-relative-name file it))))

(defun fosgit-file-tracked-p (file)
  (fosgit-git-success "ls-files" "--error-unmatch" file))

(defun fosgit-list-files (&rest args)
  (apply #'fosgit-git-items "ls-files" "-z" "--full-name" args))

(defun fosgit-tracked-files ()
  (fosgit-list-files "--cached"))

(defun fosgit-untracked-files (&optional all files)
  (fosgit-list-files "--other" (unless all "--exclude-standard") "--" files))

(defun fosgit-modified-files (&optional nomodules)
  (fosgit-git-items "diff-files" "-z" "--name-only"
                   (and nomodules "--ignore-submodules")))

(defun fosgit-staged-files (&optional nomodules files)
  (fosgit-git-items "diff-index" "-z" "--name-only" "--cached"
                   (and nomodules "--ignore-submodules")
                   (fosgit-headish) "--" files))

(defun fosgit-unstaged-files (&optional nomodules files)
  (fosgit-git-items "diff-index" "-z" "--name-only"
                   (and nomodules "--ignore-submodules")
                   (fosgit-headish) "--" files))

(defun fosgit-staged-binary-files ()
  (--mapcat (and (string-match "^-\t-\t\\(.+\\)" it)
                 (list (match-string 1 it)))
            (fosgit-git-items "diff" "-z" "--cached"
                             "--numstat" "--ignore-submodules")))

(defun fosgit-unmerged-files ()
  (fosgit-git-items "diff-files" "-z" "--name-only" "--diff-filter=U"))

(defun fosgit-revision-files (rev)
  (fosgit-with-toplevel
    (fosgit-git-items "ls-tree" "-z" "-r" "--name-only" rev)))

(defun fosgit-changed-files (rev-or-range &optional other-rev)
  "Return list of files the have changed between two revisions.
If OTHER-REV is non-nil, REV-OR-RANGE should be a revision, not a
range.  Otherwise, it can be any revision or range accepted by
\"git diff\" (i.e., <rev>, <revA>..<revB>, or <revA>...<revB>)."
  (fosgit-with-toplevel
    (fosgit-git-items "diff" "-z" "--name-only" rev-or-range other-rev)))

(defun fosgit-renamed-files (revA revB)
  (--map (cons (nth 1 it) (nth 2 it))
         (-partition 3 (fosgit-git-items
                        "diff-tree" "-r" "--diff-filter=R" "-z" "-M"
                        revA revB))))

(defun fosgit-file-status (&rest args)
  (with-temp-buffer
    (save-excursion (fosgit-git-insert "status" "-z" args))
    (let ((pos (point)) status)
      (while (> (skip-chars-forward "[:print:]") 0)
        (let ((x (char-after     pos))
              (y (char-after (1+ pos)))
              (file (buffer-substring (+ pos 3) (point))))
          (forward-char)
          (if (memq x '(?R ?C))
              (progn
                (setq pos (point))
                (skip-chars-forward "[:print:]")
                (push (list file (buffer-substring pos (point)) x y) status)
                (forward-char))
            (push (list file nil x y) status)))
        (setq pos (point)))
      status)))

(defcustom fosgit-cygwin-mount-points
  (when (eq system-type 'windows-nt)
    (cl-sort (--map (if (string-match "^\\(.*\\) on \\(.*\\) type" it)
                        (cons (file-name-as-directory (match-string 2 it))
                              (file-name-as-directory (match-string 1 it)))
                      (lwarn '(fosgit) :error
                             "Failed to parse Cygwin mount: %S" it))
                    ;; If --exec-path is not a native Windows path,
                    ;; then we probably have a cygwin git.
                    (and (not (string-match-p
                               "\\`[a-zA-Z]:"
                               (car (process-lines "git" "--exec-path"))))
                         (ignore-errors (process-lines "mount"))))
             #'> :key (-lambda ((cyg . _win)) (length cyg))))
  "Alist of (CYGWIN . WIN32) directory names.
Sorted from longest to shortest CYGWIN name."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-process
  :type '(alist :key-type string :value-type directory))

(defun fosgit-expand-git-file-name (filename)
  (unless (file-name-absolute-p filename)
    (setq filename (expand-file-name filename)))
  (-if-let ((cyg . win)
            (cl-assoc filename fosgit-cygwin-mount-points
                      :test (lambda (f cyg) (string-prefix-p cyg f))))
      (concat win (substring filename (length cyg)))
    filename))

(defun fosgit-convert-git-filename (filename)
  (-if-let ((cyg . win)
            (cl-rassoc filename fosgit-cygwin-mount-points
                       :test (lambda (f win) (string-prefix-p win f))))
      (concat cyg (substring filename (length win)))
    filename))

(defun fosgit-decode-git-path (path)
  (if (eq (aref path 0) ?\")
      (string-as-multibyte (read path))
    path))

(defun fosgit-file-at-point ()
  (fosgit-section-case
    (file (fosgit-section-value it))
    (hunk (fosgit-section-parent-value it))))

(defun fosgit-current-file ()
  (or (fosgit-file-relative-name)
      (fosgit-file-at-point)
      (and (derived-mode-p 'fosgit-log-mode)
           (nth 3 fosgit-refresh-args))))

;;; Predicates

(defun fosgit-no-commit-p ()
  "Return t if there is no commit in the current Git repository."
  (not (fosgit-rev-verify "HEAD")))

(defun fosgit-anything-staged-p (&optional ignore-submodules &rest files)
  "Return t if there are any staged changes.
If optional FILES is non-nil, then only changes to those files
are considered."
  (fosgit-git-failure "diff" "--quiet" "--cached"
                     (and ignore-submodules "--ignore-submodules")
                     "--" files))

(defun fosgit-anything-unstaged-p (&optional ignore-submodules &rest files)
  "Return t if there are any unstaged changes.
If optional FILES is non-nil, then only changes to those files
are considered."
  (fosgit-git-failure "diff" "--quiet"
                     (and ignore-submodules "--ignore-submodules")
                     "--" files))

(defun fosgit-anything-modified-p (&optional ignore-submodules &rest files)
  "Return t if there are any staged or unstaged changes.
If optional FILES is non-nil, then only changes to those files
are considered."
  (or (apply 'fosgit-anything-staged-p   ignore-submodules files)
      (apply 'fosgit-anything-unstaged-p ignore-submodules files)))

(defun fosgit-anything-unmerged-p (&rest files)
  "Return t if there are any merge conflicts.
If optional FILES is non-nil, then only conflicts in those files
are considered."
  (and (fosgit-git-string "ls-files" "--unmerged" files) t))

;;; Revisions and References

(defun fosgit-rev-parse (&rest args)
  "Execute `git rev-parse ARGS', returning first line of output.
If there is no output return nil."
  (apply #'fosgit-git-string "rev-parse" args))

(defun fosgit-rev-parse-safe (&rest args)
  "Execute `git rev-parse ARGS', returning first line of output.
If there is no output return nil.  Like `fosgit-rev-parse' but
ignore `fosgit-git-debug'."
  (apply #'fosgit-git-str "rev-parse" args))

(defun fosgit-rev-parse-p (&rest args)
  "Execute `git rev-parse ARGS', returning t if it prints \"true\".
Return t if the first (and usually only) output line is the
string \"true\", otherwise return nil."
  (fosgit-git-true "rev-parse" args))

(defun fosgit-rev-verify (rev)
  (fosgit-rev-parse-safe "--verify" rev))

(defun fosgit-rev-verify-commit (rev)
  "Return full hash for REV if it names an existing commit."
  (fosgit-rev-verify (concat rev "^{commit}")))

(defun fosgit-rev-equal (a b)
  (fosgit-git-success "diff" "--quiet" a b))

(defun fosgit-rev-eq (a b)
  (equal (fosgit-rev-verify a)
         (fosgit-rev-verify b)))

(defun fosgit-rev-ancestor-p (a b)
  "Return non-nil if commit A is an ancestor of commit B."
  (fosgit-git-success "merge-base" "--is-ancestor" a b))

(defun fosgit-rev-head-p (rev)
  (or (equal rev "HEAD")
      (and rev
           (not (string-match-p "\\.\\." rev))
           (equal (fosgit-rev-parse rev)
                  (fosgit-rev-parse "HEAD")))))

(defun fosgit-rev-name (rev &optional pattern)
  (fosgit-git-string "name-rev" "--name-only" "--no-undefined"
                    (and pattern (concat "--refs=" pattern))
                    rev))

(defun fosgit-rev-branch (rev)
  (--when-let (fosgit-rev-name rev "refs/heads/*")
    (unless (string-match-p "~" it) it)))

(defun fosgit-get-shortname (rev)
  (let ((fn (apply-partially 'fosgit-git-string "name-rev"
                             "--name-only" "--no-undefined" rev)))
    (setq rev (or (funcall fn "--refs=refs/tags/*")
                  (funcall fn "--refs=refs/heads/*")
                  (funcall fn "--refs=refs/remotes/*" "--always")))
    (if (and (string-match "^\\(?:tags\\|remotes\\)/\\(.+\\)" rev)
             (fosgit-ref-fullname (match-string 1 rev)))
        (match-string 1 rev)
      rev)))

(defun fosgit-name-branch (rev &optional lax)
  (or (fosgit-name-local-branch rev)
      (fosgit-name-remote-branch rev)
      (and lax (or (fosgit-name-local-branch rev t)
                   (fosgit-name-remote-branch rev t)))))

(defun fosgit-name-local-branch (rev &optional lax)
  (--when-let (fosgit-git-string "name-rev" "--name-only" "--no-undefined"
                                "--refs=refs/heads/*" rev)
    (and (or lax (not (string-match-p "~" it))) it)))

(defun fosgit-name-remote-branch (rev &optional lax)
  (--when-let (fosgit-git-string "name-rev" "--name-only" "--no-undefined"
                                "--refs=refs/remotes/*" rev)
    (and (or lax (not (string-match-p "~" it)))
         (substring it 8))))

(defun fosgit-name-tag (rev &optional lax)
  (--when-let (fosgit-git-string "name-rev" "--name-only" "--no-undefined"
                                "--refs=refs/tags/*" rev)
    (and (or lax (not (string-match-p "~" it)))
         (substring it 5))))

(defun fosgit-ref-fullname (name)
  (fosgit-rev-parse "--symbolic-full-name" name))

(defun fosgit-ref-exists-p (ref)
  (fosgit-git-success "show-ref" "--verify" ref))

(defun fosgit-headish ()
  "Return \"HEAD\" or if that doesn't exist the hash of the empty tree."
  (if (fosgit-no-commit-p)
      (fosgit-git-string "mktree")
    "HEAD"))

(defun fosgit-branch-at-point ()
  (fosgit-section-case
    (branch (fosgit-section-value it))
    (commit (fosgit-name-branch (fosgit-section-value it)))))

(defun fosgit-local-branch-at-point ()
  (fosgit-section-case
    (branch (let ((branch (fosgit-section-value it)))
              (when (member branch (fosgit-list-local-branch-names))
                branch)))
    (commit (fosgit-name-local-branch (fosgit-section-value it)))))

(defun fosgit-remote-branch-at-point ()
  (fosgit-section-case
    (branch (let ((branch (fosgit-section-value it)))
              (when (member branch (fosgit-list-remote-branch-names))
                branch)))
    (commit (fosgit-name-remote-branch (fosgit-section-value it)))))

(defun fosgit-commit-at-point ()
  (or (fosgit-section-when commit)
      (and (derived-mode-p 'fosgit-revision-mode)
           (car fosgit-refresh-args))))

(defun fosgit-branch-or-commit-at-point ()
  (or (fosgit-section-case
        (branch (fosgit-section-value it))
        (commit (let ((rev (fosgit-section-value it)))
                  (or (fosgit-get-shortname rev) rev))))
      (and (derived-mode-p 'fosgit-revision-mode)
           (car fosgit-refresh-args))))

(defun fosgit-tag-at-point ()
  (fosgit-section-case
    (tag    (fosgit-section-value it))
    (commit (fosgit-name-tag (fosgit-section-value it)))))

(defun fosgit-stash-at-point ()
  (fosgit-section-when stash))

(defun fosgit-remote-at-point ()
  (fosgit-section-case
    (remote (fosgit-section-value it))
    (branch (fosgit-section-parent-value it))))

(defun fosgit-get-current-branch ()
  "Return the refname of the currently checked out branch.
Return nil if no branch is currently checked out."
  (fosgit-git-string "symbolic-ref" "--short" "HEAD"))

(defun fosgit-get-previous-branch ()
  "Return the refname of the previously checked out branch.
Return nil if no branch can be found in the `HEAD' reflog
which is different from the current branch and still exists."
  (let ((current (fosgit-get-current-branch))
        (i 1) prev)
    (while (and (setq prev (fosgit-rev-verify (format "@{-%i}" i)))
                (or (not (setq prev (fosgit-rev-branch prev)))
                    (equal prev current)))
      (cl-incf i))
    prev))

(cl-defun fosgit-get-upstream-ref
    (&optional (branch (fosgit-get-current-branch)))
  (when branch
    (let ((remote (fosgit-get "branch" branch "remote"))
          (merge  (fosgit-get "branch" branch "merge")))
      (when (and remote merge)
        (cond ((string-equal remote ".") merge)
              ((string-prefix-p "refs/heads/" merge)
               (concat "refs/remotes/" remote "/" (substring merge 11))))))))

(cl-defun fosgit-get-upstream-branch
    (&optional (branch (fosgit-get-current-branch)))
  (when branch
    (let ((remote (fosgit-get "branch" branch "remote"))
          (merge  (fosgit-get "branch" branch "merge")))
      (when (and remote merge (string-prefix-p "refs/heads/" merge))
        (setq merge (substring merge 11))
        (if (string-equal remote ".")
            (propertize merge 'face 'fosgit-branch-local)
          (propertize (concat remote "/" merge) 'face 'fosgit-branch-remote))))))

(defun fosgit-get-indirect-upstream-branch (branch &optional force)
  (let ((remote (fosgit-get "branch" branch "remote")))
    (and remote (not (equal remote "."))
         ;; The user has opted in...
         (or force (member branch fosgit-branch-prefer-remote-upstream))
         ;; and local BRANCH tracks a remote branch...
         (let ((upstream (fosgit-get-upstream-branch branch)))
           ;; whose upstream...
           (and upstream
                ;; has the same name as BRANCH and...
                (equal (substring upstream (1+ (length remote))) branch)
                ;; and can be fast-forwarded to BRANCH.
                (fosgit-rev-ancestor-p upstream branch)
                upstream)))))

(cl-defun fosgit-get-upstream-remote
    (&optional (branch (fosgit-get-current-branch)))
  (when branch
    (fosgit-get "branch" branch "remote")))

(cl-defun fosgit-get-push-remote
    (&optional (branch (fosgit-get-current-branch)))
  (or (and branch (fosgit-get "branch" branch "pushRemote"))
      (fosgit-get "remote.pushDefault")))

(cl-defun fosgit-get-push-branch
    (&optional (branch (fosgit-get-current-branch)))
  (-when-let (remote (and branch (fosgit-get-push-remote branch)))
    (concat remote "/" branch)))

(defun fosgit-get-@{push}-branch (&optional branch)
  (let ((ref (fosgit-rev-parse "--symbolic-full-name"
                              (concat branch "@{push}"))))
    (when (and ref (string-prefix-p "refs/remotes/" ref))
      (substring ref 13))))

(defun fosgit-get-remote (&optional branch)
  (when (or branch (setq branch (fosgit-get-current-branch)))
    (let ((remote (fosgit-get "branch" branch "remote")))
      (unless (equal remote ".")
        remote))))

(defun fosgit-split-branch-name (branch)
  (cond ((member branch (fosgit-list-local-branch-names))
         (cons "." branch))
        ((string-match "/" branch)
         (let ((remote (substring branch 0 (match-beginning 0))))
           (if (save-match-data (member remote (fosgit-list-remotes)))
               (cons remote (substring branch (match-end 0)))
             (error "Invalid branch name %s" branch))))))

(defun fosgit-get-current-tag (&optional rev with-distance)
  "Return the closest tag reachable from REV.

If optional REV is nil then default to \"HEAD\".
If optional WITH-DISTANCE is non-nil then return (TAG COMMITS),
if it is `dirty' return (TAG COMMIT DIRTY). COMMITS is the number
of commits in \"HEAD\" but not in TAG and DIRTY is t if there are
uncommitted changes, nil otherwise."
  (--when-let (fosgit-git-str "describe" "--long" "--tags"
                             (and (eq with-distance 'dirty) "--dirty") rev)
    (save-match-data
      (string-match
       "\\(.+\\)-\\(?:0[0-9]*\\|\\([0-9]+\\)\\)-g[0-9a-z]+\\(-dirty\\)?$" it)
      (if with-distance
          `(,(match-string 1 it)
            ,(string-to-number (or (match-string 2 it) "0"))
            ,@(and (match-string 3 it) (list t)))
        (match-string 1 it)))))

(defun fosgit-get-next-tag (&optional rev with-distance)
  "Return the closest tag from which REV is reachable.

If optional REV is nil then default to \"HEAD\".
If no such tag can be found or if the distance is 0 (in which
case it is the current tag, not the next) return nil instead.
If optional WITH-DISTANCE is non-nil then return (TAG COMMITS)
where COMMITS is the number of commits in TAG but not in REV."
  (--when-let (fosgit-git-str "describe" "--contains" (or rev "HEAD"))
    (save-match-data
      (when (string-match "^[^^~]+" it)
        (setq it (match-string 0 it))
        (unless (equal it (fosgit-get-current-tag rev))
          (if with-distance
              (list it (car (fosgit-rev-diff-count it rev)))
            it))))))

(defvar fosgit-list-refs-namespaces
  '("refs/heads" "refs/remotes" "refs/tags" "refs/pull"))

(defun fosgit-list-refs (&rest args)
  (fosgit-git-lines "for-each-ref" "--format=%(refname)"
                   (or args fosgit-list-refs-namespaces)))

(defun fosgit-list-branches ()
  (fosgit-list-refs "refs/heads" "refs/remotes"))

(defun fosgit-list-local-branches ()
  (fosgit-list-refs "refs/heads"))

(defun fosgit-list-remote-branches (&optional remote)
  (fosgit-list-refs (concat "refs/remotes/" remote)))

(defun fosgit-list-containing-branches (&optional commit)
  (--filter (not (string-match-p "\\`(HEAD" it))
            (--map (substring it 2)
                   (fosgit-git-lines "branch" "--contains" commit))))

(defun fosgit-list-merged-branches (&optional commit)
  (--filter (not (string-match-p "\\`(HEAD" it))
            (--map (substring it 2)
                   (fosgit-git-lines "branch" "--merged" commit))))

(defun fosgit-list-unmerged-branches (&optional commit)
  (--filter (not (string-match-p "\\`(HEAD" it))
            (--map (substring it 2)
                   (fosgit-git-lines "branch" "--no-merged" commit))))

(defun fosgit-list-unmerged-to-upstream-branches ()
  (--filter (-when-let (upstream (fosgit-get-upstream-branch it))
              (member it (fosgit-list-unmerged-branches upstream)))
            (fosgit-list-local-branch-names)))

(defun fosgit-list-refnames (&rest args)
  (fosgit-git-lines "for-each-ref" "--format=%(refname:short)"
                   (or args fosgit-list-refs-namespaces)))

(defun fosgit-list-branch-names ()
  (fosgit-list-refnames "refs/heads" "refs/remotes"))

(defun fosgit-list-local-branch-names ()
  (fosgit-list-refnames "refs/heads"))

(defun fosgit-list-remote-branch-names (&optional remote relative)
  (if (and remote relative)
      (let ((regexp (format "^refs/remotes/%s/\\(.+\\)" remote)))
        (--mapcat (when (string-match regexp it)
                    (list (match-string 1 it)))
                  (fosgit-list-remote-branches remote)))
    (fosgit-list-refnames (concat "refs/remotes/" remote))))

(defun fosgit-format-refs (format &rest args)
  (let ((lines (fosgit-git-lines
                "for-each-ref" (concat "--format=" format)
                (or args (list "refs/heads" "refs/remotes" "refs/tags")))))
    (if (string-match-p "\f" format)
        (--map (split-string it "\f") lines)
      lines)))

(defun fosgit-list-remotes ()
  (fosgit-git-lines "remote"))

(defun fosgit-list-tags ()
  (fosgit-git-lines "tag"))

(defun fosgit-list-notes-refnames ()
  (--map (substring it 6) (fosgit-list-refnames "refs/notes")))

(defun fosgit-remote-list-tags (remote)
  (--map (substring it 51)
         (--filter (not (string-match-p "\\^{}$" it))
                   (fosgit-git-lines "ls-remote" "--tags" remote))))

(defun fosgit-remote-list-branches (remote)
  (--map (substring it 52)
         (--filter (not (string-match-p "\\^{}$" it))
                   (fosgit-git-lines "ls-remote" "--heads" remote))))

(defun fosgit-get-submodules ()
  (--mapcat (and (string-match "^160000 [0-9a-z]\\{40\\} 0\t\\(.+\\)$" it)
                 (list (match-string 1 it)))
            (fosgit-git-items "ls-files" "-z" "--stage")))

(defun fosgit-ref-p (rev)
  (or (car (member rev (fosgit-list-refs)))
      (car (member rev (fosgit-list-refnames)))))

(defun fosgit-branch-p (rev)
  (or (car (member rev (fosgit-list-branches)))
      (car (member rev (fosgit-list-branch-names)))))

(defun fosgit-local-branch-p (rev)
  (or (car (member rev (fosgit-list-local-branches)))
      (car (member rev (fosgit-list-local-branch-names)))))

(defun fosgit-branch-set-face (branch)
  (propertize branch 'face (if (fosgit-local-branch-p branch)
                               'fosgit-branch-local
                             'fosgit-branch-remote)))

(defun fosgit-tag-p (rev)
  (car (member rev (fosgit-list-tags))))

(defun fosgit-remote-p (string)
  (car (member string (fosgit-list-remotes))))

(defun fosgit-rev-diff-count (a b)
  "Return the commits in A but not B and vice versa.
Return a list of two integers: (A>B B>A)."
  (mapcar 'string-to-number
          (split-string (fosgit-git-string "rev-list"
                                          "--count" "--left-right"
                                          (concat a "..." b))
                        "\t")))

(defun fosgit-abbrev-length ()
  (string-to-number (or (fosgit-get "core.abbrev") "7")))

(defun fosgit-abbrev-arg (&optional arg)
  (format "--%s=%d" (or arg "abbrev") (fosgit-abbrev-length)))

(defun fosgit-rev-abbrev (rev)
  (fosgit-rev-parse (fosgit-abbrev-arg "short") rev))

(defun fosgit-commit-children (commit &optional args)
  (-map #'car
        (--filter (member commit (cdr it))
                  (--map (split-string it " ")
                         (fosgit-git-lines
                          "log" "--format=%H %P"
                          (or args (list "--branches" "--tags" "--remotes"))
                          "--not" commit)))))

(defun fosgit-commit-parents (commit)
  (--when-let (fosgit-git-string "rev-list" "-1" "--parents" commit)
    (cdr (split-string it))))

(defun fosgit-assert-one-parent (commit command)
  (when (> (length (fosgit-commit-parents commit)) 1)
    (user-error "Cannot %s a merge commit" command)))

(defun fosgit-patch-id (rev)
  (with-temp-buffer
    (fosgit-process-file
     shell-file-name nil '(t nil) nil shell-command-switch
     (let ((exec (shell-quote-argument fosgit-git-executable)))
       (format "%s diff-tree -u %s | %s patch-id" exec rev exec)))
    (car (split-string (buffer-string)))))

(defun fosgit-rev-format (format &optional rev args)
  (let ((str (fosgit-git-string "show" "--no-patch"
                               (concat "--format=" format) args
                               (if rev (concat rev "^{commit}") "HEAD") "--")))
    (unless (string-equal str "")
      str)))

(defun fosgit-rev-insert-format (format &optional rev args)
  (fosgit-git-insert "show" "--no-patch"
                    (concat "--format=" format) args
                    (if rev (concat rev "^{commit}") "HEAD") "--"))

(defun fosgit-format-rev-summary (rev)
  (--when-let (fosgit-rev-format "%h %s" rev)
    (string-match " " it)
    (put-text-property 0 (match-beginning 0) 'face 'fosgit-hash it)
    it))

(defun fosgit-format-ref-label (ref &optional head)
  (-let [(_re face fn)
         (--first (string-match (car it) ref) fosgit-ref-namespaces)]
    (if fn
        (funcall fn ref face)
      (propertize (or (match-string 1 ref) ref)
                  'face (if (equal ref head) 'fosgit-branch-current face)))))

(defun fosgit-format-ref-labels (string)
  (save-match-data
    (let ((regexp "\\(, \\|tag: \\| -> \\|[()]\\)") head names)
      (if (and (derived-mode-p 'fosgit-log-mode)
               (member "--simplify-by-decoration" (cadr fosgit-refresh-args)))
          (let ((branches (fosgit-list-local-branch-names))
                (re (format "^%s/.+" (regexp-opt (fosgit-list-remotes)))))
            (setq names
                  (--map (cond ((string-equal it "HEAD")     it)
                               ((string-prefix-p "refs/" it) it)
                               ((member it branches) (concat "refs/heads/" it))
                               ((string-match re it) (concat "refs/remotes/" it))
                               (t                    (concat "refs/" it)))
                         (split-string
                          (replace-regexp-in-string "tag: " "refs/tags/" string)
                          regexp t))))
        (setq names (split-string string regexp t)))
      (when (member "HEAD" names)
        (setq head  (fosgit-git-string "symbolic-ref" "HEAD")
              names (cons (or head "@") (delete head (delete "HEAD" names)))))
      (mapconcat (lambda (it) (fosgit-format-ref-label it head)) names " "))))

(defun fosgit-object-type (object)
  (fosgit-git-string "cat-file" "-t" object))

(defmacro fosgit-with-blob (commit file &rest body)
  (declare (indent 2)
           (debug (form form body)))
  `(with-temp-buffer
     (let ((buffer-file-name ,file))
       (save-excursion
         (fosgit-git-insert "cat-file" "-p"
                           (concat ,commit ":" buffer-file-name)))
       (decode-coding-inserted-region
        (point-min) (point-max) buffer-file-name t nil nil t)
       ,@body)))

(defmacro fosgit-with-temp-index (tree arg &rest body)
  (declare (indent 2) (debug (form form body)))
  (let ((file (cl-gensym "file")))
    `(let ((,file (fosgit-convert-git-filename
                   (fosgit-git-dir (make-temp-name "index.fosgit.")))))
       (setq ,file (or (file-remote-p ,file 'localname) ,file))
       (unwind-protect
           (progn (--when-let ,tree
                    (or (fosgit-git-success "read-tree" ,arg it
                                           (concat "--index-output=" ,file))
                        (error "Cannot read tree %s" it)))
                  (if (file-remote-p default-directory)
                      (let ((fosgit-tramp-process-environment
                             (setenv-internal fosgit-tramp-process-environment
                                              "GIT_INDEX_FILE" ,file t)))
                        ,@body)
                    (let ((process-environment process-environment))
                      (setenv "GIT_INDEX_FILE" ,file)
                      ,@body)))
         (ignore-errors
           (delete-file (concat (file-remote-p default-directory) ,file)))))))

(defun fosgit-commit-tree (message &optional tree &rest parents)
  (fosgit-git-string "commit-tree" "-m" message
                    (--mapcat (list "-p" it) (delq nil parents))
                    (or tree (fosgit-git-string "write-tree"))))

(defun fosgit-commit-worktree (message &optional arg &rest other-parents)
  (fosgit-with-temp-index "HEAD" arg
    (and (fosgit-update-files (fosgit-modified-files))
         (apply #'fosgit-commit-tree message nil "HEAD" other-parents))))

(defun fosgit-update-files (files)
  (fosgit-git-success "update-index" "--add" "--remove" "--" files))

(defun fosgit-update-ref (ref message rev &optional stashish)
  (or (if (not (version< (fosgit-git-version) "2.6.0"))
          (fosgit-git-success "update-ref" "--create-reflog"
                             "-m" message ref rev
                             (or (fosgit-rev-verify ref) ""))
        ;; `--create-reflog' didn't exist before v2.6.0
        (let ((oldrev  (fosgit-rev-verify ref))
              (logfile (fosgit-git-dir (concat "logs/" ref))))
          (unless (file-exists-p logfile)
            (when oldrev
              (fosgit-git-success "update-ref" "-d" ref oldrev))
            (make-directory (file-name-directory logfile) t)
            (with-temp-file logfile)
            (when (and oldrev (not stashish))
              (fosgit-git-success "update-ref" "-m" "enable reflog"
                                 ref oldrev ""))))
        (fosgit-git-success "update-ref" "-m" message ref rev
                           (or (fosgit-rev-verify ref) "")))
      (error "Cannot update %s with %s" ref rev)))

(defconst fosgit-range-re
  (concat "\\`\\([^ \t]*[^.]\\)?"       ; revA
          "\\(\\.\\.\\.?\\)"            ; range marker
          "\\([^.][^ \t]*\\)?\\'"))     ; revB

;;; Completion

(defvar fosgit-revision-history nil)

(defun fosgit-read-branch (prompt &optional default)
  (fosgit-completing-read prompt (fosgit-list-branch-names)
                         nil t nil 'fosgit-revision-history
                         (or (fosgit-branch-at-point)
                             default (fosgit-get-current-branch))))

(defun fosgit-read-branch-or-commit (prompt &optional secondary-default)
  (or (fosgit-completing-read prompt (cons "HEAD" (fosgit-list-refnames))
                             nil nil nil 'fosgit-revision-history
                             (or (fosgit-branch-or-commit-at-point)
                                 secondary-default
                                 (fosgit-get-current-branch)))
      (user-error "Nothing selected")))

(defun fosgit-read-range-or-commit (prompt &optional secondary-default)
  (fosgit-read-range
   prompt
   (or (--when-let (fosgit-region-values 'commit 'branch)
         (deactivate-mark)
         (concat (car (last it)) ".." (car it)))
       (fosgit-branch-or-commit-at-point)
       secondary-default
       (fosgit-get-current-branch))))

(defun fosgit-read-range (prompt &optional default)
  (let* ((choose-completion-string-functions
          '(crm--choose-completion-string))
         (minibuffer-completion-table #'crm--collection-fn)
         (minibuffer-completion-confirm t)
         (crm-completion-table (fosgit-list-refnames))
         (crm-separator "\\.\\.\\.?")
         (input (read-from-minibuffer
                 (concat prompt (and default (format " (%s)" default)) ": ")
                 nil crm-local-completion-map
                 nil 'fosgit-revision-history
                 default)))
    (when (string-equal input "")
      (or (setq input default)
          (user-error "Nothing selected")))
    input))

(defun fosgit-read-remote-branch
    (prompt &optional remote default local-branch require-match)
  (let ((choice (fosgit-completing-read
                 prompt
                 (nconc (and local-branch
                             (if remote
                                 (concat remote "/" local-branch)
                               (--map (concat it "/" local-branch)
                                      (fosgit-list-remotes))))
                        (fosgit-list-remote-branch-names remote t))
                 nil require-match nil 'fosgit-revision-history default)))
    (if (or remote (string-match "\\`\\([^/]+\\)/\\(.+\\)" choice))
        choice
      (user-error "`%s' doesn't have the form REMOTE/BRANCH" choice))))

(defun fosgit-read-local-branch (prompt &optional secondary-default)
  (fosgit-completing-read prompt (fosgit-list-local-branch-names)
                         nil t nil 'fosgit-revision-history
                         (or (fosgit-local-branch-at-point)
                             secondary-default
                             (fosgit-get-current-branch))))

(defun fosgit-read-local-branch-or-commit (prompt)
  (let ((branches (fosgit-list-local-branch-names))
        (commit (fosgit-commit-at-point)))
    (or (fosgit-completing-read prompt
                               (if commit (cons commit branches) branches)
                               nil nil nil 'fosgit-revision-history
                               (or (fosgit-local-branch-at-point) commit))
                     (user-error "Nothing selected"))))

(defun fosgit-read-local-branch-or-ref (prompt &optional secondary-default)
  (fosgit-completing-read prompt (nconc (fosgit-list-local-branch-names)
                                       (fosgit-list-refs "refs/"))
                         nil t nil 'fosgit-revision-history
                         (or (fosgit-local-branch-at-point)
                             secondary-default
                             (fosgit-get-current-branch))))

(defun fosgit-read-other-branch
    (prompt &optional exclude secondary-default no-require-match)
  (let* ((current (fosgit-get-current-branch))
         (atpoint (fosgit-branch-at-point))
         (exclude (or exclude current))
         (default (or (and (not (equal atpoint exclude)) atpoint)
                      (and (not (equal current exclude)) current)
                      secondary-default
                      (fosgit-get-previous-branch))))
    (fosgit-completing-read prompt (delete exclude (fosgit-list-branch-names))
                           nil (not no-require-match)
                           nil 'fosgit-revision-history default)))

(defun fosgit-read-other-branch-or-commit
    (prompt &optional exclude secondary-default)
  (let* ((current (fosgit-get-current-branch))
         (atpoint (fosgit-branch-or-commit-at-point))
         (exclude (or exclude current))
         (default (or (and (not (equal atpoint exclude)) atpoint)
                      (and (not (equal current exclude)) current)
                      secondary-default
                      (fosgit-get-previous-branch))))
    (or (fosgit-completing-read prompt (delete exclude (fosgit-list-refnames))
                               nil nil nil 'fosgit-revision-history default)
        (user-error "Nothing selected"))))

(cl-defun fosgit-read-upstream-branch
    (&optional (branch (fosgit-get-current-branch)) prompt)
  (fosgit-completing-read
   (or prompt (format "Change upstream of %s to" branch))
   (nconc (--map (concat it "/" branch)
                 (fosgit-list-remotes))
          (delete branch (fosgit-list-branch-names)))
   nil nil nil 'fosgit-revision-history
   (or (let ((r (fosgit-remote-branch-at-point))
             (l (fosgit-branch-at-point)))
         (when (and l (equal l branch))
           (setq l nil))
         (if fosgit-prefer-remote-upstream (or r l) (or l r)))
       (let ((r (fosgit-branch-p "origin/master"))
             (l (and (not (equal branch "master"))
                     (fosgit-branch-p "master"))))
         (if fosgit-prefer-remote-upstream (or r l) (or l r)))
       (let ((previous (fosgit-get-previous-branch)))
         (and (not (equal previous branch)) previous)))))

(defun fosgit-read-starting-point (prompt)
  (or (fosgit-completing-read
       (concat prompt " starting at")
       (cons "HEAD" (fosgit-list-refnames))
       nil nil nil 'fosgit-revision-history
       (or (let ((r (fosgit-remote-branch-at-point))
                 (l (fosgit-local-branch-at-point)))
             (if fosgit-prefer-remote-upstream (or r l) (or l r)))
           (fosgit-commit-at-point)
           (fosgit-stash-at-point)
           (fosgit-get-current-branch)))
      (user-error "Nothing selected")))

(defun fosgit-read-tag (prompt &optional require-match)
  (fosgit-completing-read prompt (fosgit-list-tags) nil
                         require-match nil 'fosgit-revision-history
                         (fosgit-tag-at-point)))

(defun fosgit-read-stash (prompt &optional use-at-point)
  (let ((atpoint (fosgit-stash-at-point)))
    (or (and use-at-point atpoint)
        (let ((stashes (fosgit-git-lines "stash" "list" "--format=%gd")))
          (fosgit-completing-read prompt stashes nil t nil nil
                                 (or atpoint (car stashes)))))))

(defun fosgit-read-remote (prompt &optional default use-only)
  (let ((remotes (fosgit-list-remotes)))
    (if (and use-only (= (length remotes) 1))
        (car remotes)
      (fosgit-completing-read prompt remotes
                             nil t nil nil
                             (or default
                                 (fosgit-remote-at-point)
                                 (fosgit-get-remote))))))

;;; Variables

(defun fosgit-get (&rest keys)
  "Return the value of Git config entry specified by KEYS."
  (fosgit-git-str "config" (mapconcat 'identity keys ".")))

(defun fosgit-get-all (&rest keys)
  "Return all values of the Git config entry specified by KEYS."
  (let ((fosgit-git-debug nil))
    (fosgit-git-lines "config" "--get-all" (mapconcat 'identity keys "."))))

(defun fosgit-get-boolean (&rest keys)
  "Return the boolean value of Git config entry specified by KEYS."
  (fosgit-git-true "config" "--bool" (mapconcat 'identity keys ".")))

(defun fosgit-set (val &rest keys)
  "Set Git config settings specified by KEYS to VAL."
  (if val
      (fosgit-git-string "config" (mapconcat 'identity keys ".") val)
    (fosgit-git-string "config" "--unset" (mapconcat 'identity keys "."))))

;;; fosgit-git.el ends soon

(define-obsolete-function-alias 'fosgit-get-tracked-ref
  'fosgit-get-upstream-ref "Fosgit 2.4.0")
(define-obsolete-function-alias 'fosgit-get-tracked-branch
  'fosgit-get-upstream-branch "Fosgit 2.4.0")
(define-obsolete-function-alias 'fosgit-get-tracked-remote
  'fosgit-get-upstream-remote "Fosgit 2.4.0")

(provide 'fosgit-git)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-git.el ends here
