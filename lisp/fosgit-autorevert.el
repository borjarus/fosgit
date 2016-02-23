;;; fosgit-autorevert.el --- revert buffers when files in repository change  -*- lexical-binding: t -*-

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

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'fosgit-git)

(require 'autorevert)

(defgroup fosgit-auto-revert nil
  "Revert buffers when files in repository change."
  :group 'auto-revert
  :group 'fosgit-extensions)

(defcustom auto-revert-buffer-list-filter nil
  "Filter that determines which buffers `auto-revert-buffers' reverts.

This option is provided by `fosgit', which also redefines
`auto-revert-buffers' to respect it.  Fosgit users who do not turn
on the local mode `auto-revert-mode' themselves, are best served
by setting the value to `fosgit-auto-revert-repository-buffers-p'.

However the default is nil, to not disturb users who do use the
local mode directly.  If you experience delays when running Fosgit
commands, then you should consider using one of the predicates
provided by Fosgit - especially if you also use Tramp.

Users who do turn on `auto-revert-mode' in buffers in which Fosgit
doesn't do that for them, should likely not use any filter.
Users who turn on `global-auto-revert-mode', do not have to worry
about this option, because it is disregarded if the global mode
is enabled."
  :package-version '(fosgit . "2.4.2")
  :group 'auto-revert
  :group 'fosgit-auto-revert
  :type '(radio (const :tag "no filter" nil)
                (function-item fosgit-auto-revert-buffer-p)
                (function-item fosgit-auto-revert-repository-buffer-p)
                function))

(defcustom fosgit-auto-revert-tracked-only t
  "Whether `fosgit-auto-revert-mode' only reverts tracked files."
  :package-version '(fosgit . "2.4.0")
  :group 'fosgit-auto-revert
  :type 'boolean
  :set (lambda (var val)
         (set var val)
         (when (and (bound-and-true-p fosgit-auto-revert-mode)
                    (featurep 'fosgit-autorevert))
           (fosgit-auto-revert-mode -1)
           (fosgit-auto-revert-mode))))

(defcustom fosgit-auto-revert-immediately t
  "Whether Fosgit reverts buffers immediately.

If this is non-nil and either `global-auto-revert-mode' or
`fosgit-auto-revert-mode' is enabled, then Fosgit immediately
reverts buffers by explicitly calling `auto-revert-buffers'
after running git for side-effects.

If `auto-revert-use-notify' is non-nil (and file notifications
are actually supported), then `fosgit-auto-revert-immediately'
does not have to be non-nil, because the reverts happen
immediately anyway.

If `fosgit-auto-revert-immediately' and `auto-revert-use-notify'
are both nil, then reverts happen after `auto-revert-interval'
seconds of user inactivity.  That is not desirable."
  :package-version '(fosgit . "2.4.0")
  :group 'fosgit-auto-revert
  :type 'boolean)

(defun fosgit-turn-on-auto-revert-mode-if-desired (&optional file)
  (if file
      (--when-let (find-buffer-visiting file)
        (with-current-buffer it
          (fosgit-turn-on-auto-revert-mode-if-desired)))
    (when (and buffer-file-name
               (file-readable-p buffer-file-name)
               (fosgit-toplevel)
               (or (not fosgit-auto-revert-tracked-only)
                   (fosgit-file-tracked-p buffer-file-name)))
      (auto-revert-mode))))

;;;###autoload
(defvar fosgit-revert-buffers t)
(make-obsolete-variable 'fosgit-revert-buffers 'fosgit-auto-revert-mode
                        "Fosgit 2.4.0")

;;;###autoload
(define-globalized-minor-mode fosgit-auto-revert-mode auto-revert-mode
  fosgit-turn-on-auto-revert-mode-if-desired
  :package-version '(fosgit . "2.4.0")
  :group 'fosgit
  :group 'fosgit-auto-revert
  ;; When `global-auto-revert-mode' is enabled, then this mode is
  ;; redundant.  When `fosgit-revert-buffers' is nil, then the user has
  ;; opted out of the automatic reverts while the old implementation
  ;; was still in use.  In all other cases enable the mode because if
  ;; buffers are not automatically reverted that would make many very
  ;; common tasks much more cumbersome.
  :init-value (and (not global-auto-revert-mode) fosgit-revert-buffers))

;; `:init-value t' only sets the value of the mode variable
;; but does not cause the mode function to be called.
(cl-eval-when (load eval)
  (when fosgit-auto-revert-mode
    (fosgit-auto-revert-mode)))

;; If the user has set the obsolete `fosgit-revert-buffers' to nil
;; after loading fosgit, then we should still respect that setting.
(defun fosgit-auto-revert-mode--maybe-turn-off-after-init ()
  (unless fosgit-revert-buffers
    (fosgit-auto-revert-mode -1)))
(unless after-init-time
  (add-hook 'after-init-hook
            #'fosgit-auto-revert-mode--maybe-turn-off-after-init t))

(put 'fosgit-auto-revert-mode 'function-documentation
     "Toggle Fosgit Auto Revert mode.
With a prefix argument ARG, enable Fosgit Auto Revert mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

Fosgit Auto Revert mode is a global minor mode that reverts
buffers associated with a file that is located inside a Git
repository when the file changes on disk.  Use `auto-revert-mode'
to revert a particular buffer.  Or use `global-auto-revert-mode'
to revert all file-visiting buffers, not just those that visit
a file located inside a Git repository.

This global mode works by turning on the buffer-local mode
`auto-revert-mode' at the time a buffer is first created.  The
local mode is turned on if the visited file is being tracked in
a Git repository at the time when the buffer is created.

If `fosgit-auto-revert-tracked-only' is non-nil (the default),
then only tracked files are reverted.  But if you stage a
previously untracked file using `fosgit-stage', then this mode
notices that.

Unlike `global-auto-revert-mode', this mode never reverts any
buffers that are not visiting files.

The behavior of this mode can be customized using the options
in the `autorevert' and `fosgit-autorevert' groups.

This function calls the hook `fosgit-auto-revert-mode-hook'.")

(defun fosgit-auto-revert-buffers ()
  (when (and fosgit-auto-revert-immediately
             (or global-auto-revert-mode
                 (and fosgit-auto-revert-mode auto-revert-buffer-list)))
    (let ((auto-revert-buffer-list-filter
           (or auto-revert-buffer-list-filter
               'fosgit-auto-revert-repository-buffer-p)))
      (auto-revert-buffers))))

(defvar fosgit-auto-revert-toplevel nil)

(when (< emacs-major-version 25)
  (defvar auto-revert-buffers-counter 1
    "Incremented each time `auto-revert-buffers' is called"))

(defun fosgit-auto-revert-buffer-p (buffer)
  "Return t if BUFFER visits a file inside the current repository.
The current repository is the one in which `default-directory' is
located.  If there is no current repository, then return t for
any BUFFER."
  (fosgit-auto-revert-repository-buffer-p buffer t))

(defun fosgit-auto-revert-repository-buffer-p (buffer &optional fallback)
  "Return t if BUFFER visits a file inside the current repository.
The current repository is the one in which `default-directory' is
located.  If there is no current repository, then return FALLBACK
\(which defaults to nil) for any BUFFER."
  ;; Call `fosgit-toplevel' just once per cycle.
  (unless (and fosgit-auto-revert-toplevel
               (= (cdr fosgit-auto-revert-toplevel)
                  auto-revert-buffers-counter))
    (setq fosgit-auto-revert-toplevel
          (cons (or (fosgit-toplevel) 'no-repo)
                auto-revert-buffers-counter)))
  (let ((top (car fosgit-auto-revert-toplevel)))
    (if (eq top 'no-repo)
        fallback
      (let ((dir (with-current-buffer buffer default-directory)))
        (and (equal (file-remote-p dir)
                    (file-remote-p top))
             ;; ^ `tramp-handle-file-in-directory-p' lacks this optimization.
             (file-in-directory-p dir top))))))

(defun auto-revert-buffers--buffer-list-filter ()
  (when (< emacs-major-version 25)
    (cl-incf auto-revert-buffers-counter))
  (when auto-revert-buffer-list-filter
    (setq auto-revert-buffer-list
          (--filter auto-revert-buffer-list-filter
                    auto-revert-buffer-list))))

(advice-add 'auto-revert-buffers :before
            'auto-revert-buffers--buffer-list-filter)

(custom-add-to-group 'fosgit 'auto-revert-check-vc-info 'custom-variable)

;;; fosgit-autorevert.el ends soon
(provide 'fosgit-autorevert)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-autorevert.el ends here
