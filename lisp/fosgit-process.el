;;; fosgit-process.el --- process functionality  -*- lexical-binding: t -*-

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

;; This library implements the tools used to run Git for side-effects.

;; Note that the functions used to run Git and then consume its
;; output, are defined in `fosgit-git.el'.  There's a bit of overlap
;; though.

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'with-editor)
(require 'fosgit-utils)
(require 'fosgit-section)
(require 'fosgit-git)
(require 'fosgit-mode)

(eval-when-compile (require 'dired))
(declare-function dired-uncache 'dired)

;;; Options

(defcustom fosgit-process-connection-type (not (eq system-type 'cygwin))
  "Connection type used for the Git process.

If nil, use pipes: this is usually more efficient, and works on Cygwin.
If t, use ptys: this enables Fosgit to prompt for passphrases when needed."
  :group 'fosgit-process
  :type '(choice (const :tag "pipe" nil)
                 (const :tag "pty" t)))

(defcustom fosgit-need-cygwin-noglob
  (equal "x0\n" (with-temp-buffer
                  (let ((process-environment
                         (append fosgit-git-environment process-environment)))
                    (process-file fosgit-git-executable
                                  nil (current-buffer) nil
                                  "-c" "alias.echo=!echo" "echo" "x{0}"))
                  (buffer-string)))
  "Whether to use a workaround for Cygwin's globbing behavior.

If non-nil, add environment variables to `process-environment' to
prevent the git.exe distributed by Cygwin and MSYS2 from
attempting to perform glob expansion when called from a native
Windows build of Emacs.  See #2246."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-process
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom fosgit-process-popup-time -1
  "Popup the process buffer if a command takes longer than this many seconds."
  :group 'fosgit-process
  :type '(choice (const :tag "Never" -1)
                 (const :tag "Immediately" 0)
                 (integer :tag "After this many seconds")))

(defcustom fosgit-process-log-max 32
  "Maximum number of sections to keep in a process log buffer.
When adding a new section would go beyond the limit set here,
then the older half of the sections are remove.  Sections that
belong to processes that are still running are never removed.
When this is nil, no sections are ever removed."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-process
  :type '(choice (const :tag "Never remove old sections" nil) integer))

(defcustom fosgit-credential-cache-daemon-socket
  (--some (-let [(prog . args) (split-string it)]
            (if (string-match-p
                 "\\`\\(?:\\(?:/.*/\\)?git-credential-\\)?cache\\'" prog)
                (or (cl-loop for (opt val) on args
                             if (string= opt "--socket")
                             return val)
                    (expand-file-name "~/.git-credential-cache/socket"))))
          ;; Note: `fosgit-process-file' is not yet defined when
          ;; evaluating this form, so we use `process-lines'.
          (ignore-errors
            (let ((process-environment
                   (append fosgit-git-environment process-environment)))
              (process-lines fosgit-git-executable
                             "config" "--get-all" "credential.helper"))))
  "If non-nil, start a credential cache daemon using this socket.

When using Git's cache credential helper in the normal way, Emacs
sends a SIGHUP to the credential daemon after the git subprocess
has exited, causing the daemon to also quit.  This can be avoided
by starting the `git-credential-cache--daemon' process directly
from Emacs.

The function `fosgit-maybe-start-credential-cache-daemon' takes
care of starting the daemon if necessary, using the value of this
option as the socket.  If this option is nil, then it does not
start any daemon.  Likewise if another daemon is already running,
then it starts no new daemon.  This function has to be a member
of the hook variable `fosgit-credential-hook' for this to work.
If an error occurs while starting the daemon, most likely because
the necessary executable is missing, then the function removes
itself from the hook, to avoid further futile attempts."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-process
  :type '(choice (file  :tag "Socket")
                 (const :tag "Don't start a cache daemon" nil)))

(defcustom fosgit-process-yes-or-no-prompt-regexp
  " [\[(]\\([Yy]\\(?:es\\)?\\)[/|]\\([Nn]o?\\)[\])] ?[?:] ?$"
  "Regexp matching Yes-or-No prompts of Git and its subprocesses."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-process
  :type 'regexp)

(defcustom fosgit-process-password-prompt-regexps
  '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
    ;; match-group 99 is used to identify a host
    "^\\(Enter \\)?[Pp]assword\\( for '\\(?99:.*\\)'\\)?: ?$"
    "^.*'s password: ?$"
    "^Yubikey for .*: ?$")
  "List of regexps matching password prompts of Git and its subprocesses.
Also see `fosgit-process-find-password-functions'."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-process
  :type '(repeat (regexp)))

(defcustom fosgit-process-find-password-functions nil
  "List of functions to try in sequence to get a password.

These functions may be called when git asks for a password, which
is detected using `fosgit-process-password-prompt-regexps'.  They
are called if and only if matching the prompt resulted in the
value of the 99th submatch to be non-nil.  Therefore users can
control for which prompts these functions should be called by
putting the host name in the 99th submatch, or not.

If the functions are called, then they are called in the order
given, with the host name as only argument, until one of them
returns non-nil.  If they are not called or none of them returns
non-nil, then the password is read from the user instead."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-process
  :type 'hook
  :options '(fosgit-process-password-auth-source))

(defcustom fosgit-process-username-prompt-regexps
  '("^Username for '.*': ?$")
  "List of regexps matching username prompts of Git and its subprocesses."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-process
  :type '(repeat (regexp)))

(defface fosgit-process-ok
  '((t :inherit fosgit-section-heading :foreground "green"))
  "Face for zero exit-status."
  :group 'fosgit-faces)

(defface fosgit-process-ng
  '((t :inherit fosgit-section-heading :foreground "red"))
  "Face for non-zero exit-status."
  :group 'fosgit-faces)

;;; Process Mode

(defvar fosgit-process-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map fosgit-mode-map)
    map)
  "Keymap for `fosgit-process-mode'.")

(define-derived-mode fosgit-process-mode fosgit-mode "Fosgit Process"
  "Mode for looking at Git process output."
  :group 'fosgit-process
  (hack-dir-local-variables-non-file-buffer))

(defun fosgit-process-buffer (&optional nodisplay)
  "Display the current repository's process buffer.

If that buffer doesn't exist yet, then create it.
Non-interactively return the buffer and unless
optional NODISPLAY is non-nil also display it."
  (interactive)
  (let ((topdir (fosgit-toplevel)))
    (unless topdir
      (fosgit--with-safe-default-directory nil
        (setq topdir default-directory)
        (let (prev)
          (while (not (equal topdir prev))
            (setq prev topdir)
            (setq topdir (file-name-directory (directory-file-name topdir)))))))
    (let ((buffer (or (--first (with-current-buffer it
                                 (and (eq major-mode 'fosgit-process-mode)
                                      (equal default-directory topdir)))
                               (buffer-list))
                      (let ((default-directory topdir))
                        (fosgit-generate-new-buffer 'fosgit-process-mode)))))
      (with-current-buffer buffer
        (if fosgit-root-section
            (when fosgit-process-log-max
              (fosgit-process-truncate-log))
          (fosgit-process-mode)
          (let ((inhibit-read-only t))
            (make-local-variable 'text-property-default-nonsticky)
            (fosgit-insert-section (processbuf)
              (insert "\n")))))
      (unless nodisplay
        (fosgit-display-buffer buffer))
      buffer)))

(defun fosgit-process-kill ()
  "Kill the process at point."
  (interactive)
  (fosgit-section-when process
    (let ((process (fosgit-section-value it)))
      (if (eq (process-status process) 'run)
          (when (fosgit-confirm 'kill-process)
            (kill-process process))
        (user-error "Process isn't running")))))

;;; Synchronous Processes

(defvar fosgit-process-raise-error nil)

(defun fosgit-git (&rest args)
  "Call Git synchronously in a separate process, for side-effects.

Option `fosgit-git-executable' specifies the Git executable.
The arguments ARGS specify arguments to Git, they are flattened
before use.

Process output goes into a new section in the buffer returned by
`fosgit-process-buffer'.  If Git exits with a non-zero status,
then raise an error."
  (let ((fosgit-process-raise-error t))
    (fosgit-call-git args)))

(defun fosgit-run-git (&rest args)
  "Call Git synchronously in a separate process, and refresh.

Option `fosgit-git-executable' specifies the Git executable and
option `fosgit-git-global-arguments' specifies constant arguments.
The arguments ARGS specify arguments to Git, they are flattened
before use.

After Git returns, the current buffer (if it is a Fosgit buffer)
as well as the current repository's status buffer are refreshed.

Process output goes into a new section in the buffer returned by
`fosgit-process-buffer'."
  (fosgit-call-git args)
  (fosgit-refresh))

(defvar fosgit-pre-call-git-hook nil)

(defun fosgit-call-git (&rest args)
  "Call Git synchronously in a separate process.

Option `fosgit-git-executable' specifies the Git executable and
option `fosgit-git-global-arguments' specifies constant arguments.
The arguments ARGS specify arguments to Git, they are flattened
before use.

Process output goes into a new section in the buffer returned by
`fosgit-process-buffer'."
  (run-hooks 'fosgit-pre-call-git-hook)
  (apply #'fosgit-call-process fosgit-git-executable
         (fosgit-process-git-arguments args)))

(defun fosgit-call-process (program &rest args)
  "Call PROGRAM synchronously in a separate process.
Process output goes into a new section in the buffer returned by
`fosgit-process-buffer'."
  (-let [(process-buf . section) (fosgit-process-setup program args)]
    (fosgit-process-finish
     (let ((inhibit-read-only t))
       (apply #'fosgit-process-file program nil process-buf nil args))
     process-buf (current-buffer) default-directory section)))

(defun fosgit-process-file (&rest args)
  "Process files synchronously in a separate process.
Identical to `process-file' but temporarily enable Cygwin's
\"noglob\" option during the call."
  (let ((process-environment (append (fosgit-cygwin-env-vars)
                                     process-environment)))
    (apply #'process-file args)))

(defun fosgit-cygwin-env-vars ()
  (append fosgit-git-environment
          (when fosgit-need-cygwin-noglob
            (mapcar (lambda (var)
                      (concat var "=" (--if-let (getenv var)
                                          (concat it " noglob")
                                        "noglob")))
                    '("CYGWIN" "MSYS")))))

(defvar fosgit-this-process nil)

(defun fosgit-run-git-with-input (&rest args)
  "Call Git in a separate process.
ARGS is flattened and then used as arguments to Git.

The current buffer's content is used as the process' standard
input.

Option `fosgit-git-executable' specifies the Git executable and
option `fosgit-git-global-arguments' specifies constant arguments.
The remaining arguments ARGS specify arguments to Git, they are
flattened before use."
  (declare (indent 1))
  (if (file-remote-p default-directory)
      ;; We lack `process-file-region', so fall back to asynch +
      ;; waiting in remote case.
      (progn
        (fosgit-start-git (current-buffer) args)
        (while (and fosgit-this-process
                    (eq (process-status fosgit-this-process) 'run))
          (sleep-for 0.005)))
    (run-hooks 'fosgit-pre-call-git-hook)
    (-let* ((process-environment (append (fosgit-cygwin-env-vars)
                                         process-environment))
            (flat-args (fosgit-process-git-arguments args))
            ((process-buf . section)
             (fosgit-process-setup fosgit-git-executable flat-args))
            (inhibit-read-only t))
      (fosgit-process-finish
       (apply #'call-process-region (point-min) (point-max)
              fosgit-git-executable nil process-buf nil flat-args)
       process-buf nil default-directory section))))

(defun fosgit-run-git-with-logfile (file &rest args)
  "Call Git in a separate process and log its output to FILE.
This function might have a short halflive."
  (apply #'fosgit-process-file fosgit-git-executable nil `(:file ,file) nil
         (fosgit-process-git-arguments args))
  (fosgit-refresh))

;;; Asynchronous Processes

(defun fosgit-run-git-async (&rest args)
  "Start Git, prepare for refresh, and return the process object.
ARGS is flattened and then used as arguments to Git.

Display the command line arguments in the echo area.

After Git returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Fosgit buffer
and still alive), as well as the respective Fosgit status buffer.

See `fosgit-start-process' for more information."
  (message "Running %s %s" fosgit-git-executable
           (let ((m (mapconcat #'identity (-flatten args) " ")))
             (remove-list-of-text-properties 0 (length m) '(face) m)
             m))
  (fosgit-start-git nil args))

(defun fosgit-run-git-with-editor (&rest args)
  "Export GIT_EDITOR and start Git.
Also prepare for refresh and return the process object.
ARGS is flattened and then used as arguments to Git.

Display the command line arguments in the echo area.

After Git returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Fosgit buffer
and still alive), as well as the respective Fosgit status buffer.

See `fosgit-start-process' and `with-editor' for more information."
  (with-editor "GIT_EDITOR"
    (let ((fosgit-process-popup-time -1))
      (fosgit-run-git-async args))))

(defun fosgit-run-git-sequencer (&rest args)
  "Export GIT_EDITOR and start Git.
Also prepare for refresh and return the process object.
ARGS is flattened and then used as arguments to Git.

Display the command line arguments in the echo area.

After Git returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Fosgit buffer
and still alive), as well as the respective Fosgit status buffer.
If the sequence stops at a commit, make the section representing
that commit the current section by moving `point' there.

See `fosgit-start-process' and `with-editor' for more information."
  (with-editor "GIT_EDITOR"
    (let ((fosgit-process-popup-time -1))
      (fosgit-run-git-async args)))
  (set-process-sentinel fosgit-this-process #'fosgit-sequencer-process-sentinel)
  fosgit-this-process)

(defvar fosgit-pre-start-git-hook nil)

(defun fosgit-start-git (input &rest args)
  "Start Git, prepare for refresh, and return the process object.

If INPUT is non-nil, it has to be a buffer or the name of an
existing buffer.  The buffer content becomes the processes
standard input.

Option `fosgit-git-executable' specifies the Git executable and
option `fosgit-git-global-arguments' specifies constant arguments.
The remaining arguments ARGS specify arguments to Git, they are
flattened before use.

After Git returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Fosgit buffer
and still alive), as well as the respective Fosgit status buffer.

See `fosgit-start-process' for more information."
  (run-hooks 'fosgit-pre-start-git-hook)
  (apply #'fosgit-start-process fosgit-git-executable input
         (fosgit-process-git-arguments args)))

(defun fosgit-start-process (program &optional input &rest args)
  "Start PROGRAM, prepare for refresh, and return the process object.

If optional argument INPUT is non-nil, it has to be a buffer or
the name of an existing buffer.  The buffer content becomes the
processes standard input.

The process is started using `start-file-process' and then setup
to use the sentinel `fosgit-process-sentinel' and the filter
`fosgit-process-filter'.  Information required by these functions
is stored in the process object.  When this function returns the
process has not started to run yet so it is possible to override
the sentinel and filter.

After the process returns, `fosgit-process-sentinel' refreshes the
buffer that was current when `fosgit-start-process' was called (if
it is a Fosgit buffer and still alive), as well as the respective
Fosgit status buffer."
  (-let* (((process-buf . section)
           (fosgit-process-setup program args))
          (process
           (let ((process-connection-type
                  ;; Don't use a pty, because it would set icrnl
                  ;; which would modify the input (issue #20).
                  (and (not input) fosgit-process-connection-type))
                 (process-environment (append (fosgit-cygwin-env-vars)
                                              process-environment)))
             (apply #'start-file-process
                    (file-name-nondirectory program)
                    process-buf program args))))
    (with-editor-set-process-filter process #'fosgit-process-filter)
    (set-process-sentinel process #'fosgit-process-sentinel)
    (set-process-buffer   process process-buf)
    (process-put process 'section section)
    (process-put process 'command-buf (current-buffer))
    (process-put process 'default-dir default-directory)
    (when inhibit-fosgit-refresh
      (process-put process 'inhibit-refresh t))
    (setf (fosgit-section-process section) process)
    (with-current-buffer process-buf
      (set-marker (process-mark process) (point)))
    (when input
      (with-current-buffer input
        (process-send-region process (point-min) (point-max))
        (process-send-eof    process)))
    (setq fosgit-this-process process)
    (setf (fosgit-section-value section) process)
    (fosgit-process-display-buffer process)
    process))

;;; Process Internals

(defun fosgit-process-setup (program args)
  (fosgit-process-set-mode-line program args)
  (let ((pwd default-directory)
        (buf (fosgit-process-buffer t)))
    (cons buf (with-current-buffer buf
                (prog1 (fosgit-process-insert-section pwd program args nil nil)
                  (backward-char 1))))))

(defun fosgit-process-insert-section (pwd program args &optional errcode errlog)
  (let ((inhibit-read-only t)
        (fosgit-insert-section--parent fosgit-root-section))
    (goto-char (1- (point-max)))
    (fosgit-insert-section (process)
      (insert (if errcode
                  (format "%3s " (propertize (number-to-string errcode)
                                             'face 'fosgit-process-ng))
                "run "))
      (unless (equal (expand-file-name pwd)
                     (expand-file-name default-directory))
        (insert (file-relative-name pwd default-directory) ?\s))
      (insert (propertize program 'face 'fosgit-section-heading))
      (insert " ")
      (when (and args (equal program fosgit-git-executable))
        (setq args (-split-at (length fosgit-git-global-arguments) args))
        (insert (propertize (char-to-string fosgit-ellipsis)
                            'face 'fosgit-section-heading
                            'help-echo (mapconcat #'identity (car args) " ")))
        (insert " ")
        (setq args (cadr args)))
      (insert (propertize (mapconcat #'identity args " ")
                          'face 'fosgit-section-heading))
      (fosgit-insert-heading)
      (when errlog
        (insert-file-contents errlog)
        (goto-char (1- (point-max))))
      (insert "\n"))))

(defun fosgit-process-truncate-log ()
  (let* ((head nil)
         (tail (fosgit-section-children fosgit-root-section))
         (count (length tail)))
    (when (> (1+ count) fosgit-process-log-max)
      (while (and (cdr tail)
                  (> count (/ fosgit-process-log-max 2)))
        (let* ((inhibit-read-only t)
               (section (car tail))
               (process (fosgit-section-process section)))
          (cond ((not process))
                ((memq (process-status process) '(exit signal))
                 (delete-region (fosgit-section-start section)
                                (1+ (fosgit-section-end section)))
                 (cl-decf count))
                (t
                 (push section head))))
        (pop tail))
      (setf (fosgit-section-children fosgit-root-section)
            (nconc (reverse head) tail)))))

(defun fosgit-process-sentinel (process event)
  "Default sentinel used by `fosgit-start-process'."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (when (string-match "^finished" event)
      (message (concat (capitalize (process-name process)) " finished")))
    (fosgit-process-finish process)
    (when (eq process fosgit-this-process)
      (setq fosgit-this-process nil))
    (unless (process-get process 'inhibit-refresh)
      (let ((command-buf (process-get process 'command-buf)))
        (if (buffer-live-p command-buf)
            (with-current-buffer command-buf
              (fosgit-refresh))
          (with-temp-buffer
            (setq default-directory (process-get process 'default-dir))
            (fosgit-refresh)))))))

(defun fosgit-sequencer-process-sentinel (process event)
  "Special sentinel used by `fosgit-run-git-sequencer'."
  (when (memq (process-status process) '(exit signal))
    (fosgit-process-sentinel process event)
    (--when-let (fosgit-mode-get-buffer 'fosgit-status-mode)
      (with-current-buffer it
        (--when-let
            (fosgit-get-section
             `((commit . ,(fosgit-rev-parse "HEAD"))
               (,(pcase (car (cadr (-split-at
                                    (1+ (length fosgit-git-global-arguments))
                                    (process-command process))))
                   ((or "rebase" "am")   'rebase-sequence)
                   ((or "cherry-pick" "revert") 'sequence)))
               (status)))
          (goto-char (fosgit-section-start it))
          (fosgit-section-update-highlight))))))

(defun fosgit-process-filter (proc string)
  "Default filter used by `fosgit-start-process'."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (fosgit-process-yes-or-no-prompt proc string)
      (fosgit-process-username-prompt  proc string)
      (fosgit-process-password-prompt  proc string)
      (goto-char (process-mark proc))
      (setq string (propertize string 'fosgit-section
                               (process-get proc 'section)))
      ;; Find last ^M in string.  If one was found, ignore
      ;; everything before it and delete the current line.
      (let ((ret-pos (length string)))
        (while (and (>= (cl-decf ret-pos) 0)
                    (/= ?\r (aref string ret-pos))))
        (if (< ret-pos 0)
            (insert string)
          (delete-region (line-beginning-position) (point))
          (insert (substring string (1+ ret-pos)))))
      (set-marker (process-mark proc) (point)))))

(defmacro fosgit-process-kill-on-abort (proc &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((map (cl-gensym)))
    `(let ((,map (make-sparse-keymap)))
       (set-keymap-parent ,map minibuffer-local-map)
       (define-key ,map "\C-g"
         (lambda ()
           (interactive)
           (ignore-errors (kill-process ,proc))
           (abort-recursive-edit)))
       (let ((minibuffer-local-map ,map))
         ,@body))))

(defun fosgit-process-yes-or-no-prompt (process string)
  "Forward Yes-or-No prompts to the user."
  (-when-let (beg (string-match fosgit-process-yes-or-no-prompt-regexp string))
    (let ((max-mini-window-height 30))
      (process-send-string
       process
       (downcase
        (concat
         (match-string
          (if (save-match-data
                (fosgit-process-kill-on-abort process
                  (yes-or-no-p (substring string 0 beg)))) 1 2)
          string)
         "\n"))))))

(defun fosgit-process-password-auth-source (key)
  "Use `auth-source-search' to get a password.
If found, return the password.  Otherwise, return nil."
  (require 'auth-source)
  (let ((secret (plist-get (car (auth-source-search :max 1 :host key
                                                    :require '(:host)))
                           :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun fosgit-process-password-prompt (process string)
  "Find a password based on prompt STRING and send it to git.
First try the functions in `fosgit-process-find-password-functions'.
If none of them returns a password, then read it from the user
instead."
  (--when-let (fosgit-process-match-prompt
               fosgit-process-password-prompt-regexps string)
    (process-send-string
     process (fosgit-process-kill-on-abort process
               (concat (or (--when-let (match-string 99 string)
                             (run-hook-with-args-until-success
                              'fosgit-process-find-password-functions it))
                           (read-passwd it))
                       "\n")))))

(defun fosgit-process-username-prompt (process string)
  "Forward username prompts to the user."
  (--when-let (fosgit-process-match-prompt
               fosgit-process-username-prompt-regexps string)
    (process-send-string
     process (fosgit-process-kill-on-abort process
               (concat (read-string it nil nil (user-login-name)) "\n")))))

(defun fosgit-process-match-prompt (prompts string)
  "Match STRING against PROMPTS and set match data.
Return the matched string suffixed with \": \", if needed."
  (when (--any? (string-match it string) prompts)
    (let ((prompt (match-string 0 string)))
      (cond ((string-suffix-p ": " prompt) prompt)
            ((string-suffix-p ":"  prompt) (concat prompt " "))
            (t                             (concat prompt ": "))))))

(defvar fosgit-credential-hook nil
  "Hook run before Git needs credentials.")

(defvar fosgit-credential-cache-daemon-process nil)

(defun fosgit-maybe-start-credential-cache-daemon ()
  "Maybe start a `git-credential-cache--daemon' process.

If such a process is already running or if the value of option
`fosgit-credential-cache-daemon-socket' is nil, then do nothing.
Otherwise start the process passing the value of that options
as argument."
  (unless (or (not fosgit-credential-cache-daemon-socket)
              (process-live-p fosgit-credential-cache-daemon-process)
              (memq fosgit-credential-cache-daemon-process
                    (list-system-processes)))
    (setq fosgit-credential-cache-daemon-process
          (or (--first (-let (((&alist 'comm comm 'user user)
                               (process-attributes it)))
                         (and (string= comm "git-credential-cache--daemon")
                              (string= user user-login-name)))
                       (list-system-processes))
              (condition-case nil
                  (start-process "git-credential-cache--daemon"
                                 " *git-credential-cache--daemon*"
                                 fosgit-git-executable
                                 "credential-cache--daemon"
                                 fosgit-credential-cache-daemon-socket)
                ;; Some Git implementations (e.g. Windows) won't have
                ;; this program; if we fail the first time, stop trying.
                ((debug error)
                 (remove-hook 'fosgit-credential-hook
                              #'fosgit-maybe-start-credential-cache-daemon)))))))

(add-hook 'fosgit-credential-hook #'fosgit-maybe-start-credential-cache-daemon)

(defun tramp-sh-handle-start-file-process--fosgit-tramp-process-environment
    (fn name buffer program &rest args)
  (if fosgit-tramp-process-environment
      (apply fn name buffer
             (car fosgit-tramp-process-environment)
             (append (cdr fosgit-tramp-process-environment)
                     (cons program args)))
    (apply fn name buffer program args)))

(advice-add 'tramp-sh-handle-start-file-process :around
            'tramp-sh-handle-start-file-process--fosgit-tramp-process-environment)

(defun tramp-sh-handle-process-file--fosgit-tramp-process-environment
    (fn program &optional infile destination display &rest args)
  (if fosgit-tramp-process-environment
      (apply fn "env" infile destination display
             (append fosgit-tramp-process-environment
                     (cons program args)))
    (apply fn program infile destination display args)))

(advice-add 'tramp-sh-handle-process-file :around
            'tramp-sh-handle-process-file--fosgit-tramp-process-environment)

(defun fosgit-process-set-mode-line (program args)
  (when (equal program fosgit-git-executable)
    (setq args (nthcdr (length fosgit-git-global-arguments) args)))
  (let ((str (concat " " program (and args (concat " " (car args))))))
    (dolist (buf (fosgit-mode-get-buffers))
      (with-current-buffer buf (setq mode-line-process str)))))

(defun fosgit-process-unset-mode-line ()
  (dolist (buf (fosgit-mode-get-buffers))
    (with-current-buffer buf (setq mode-line-process nil))))

(defvar fosgit-process-error-message-re
  (concat "^\\(?:error\\|fatal\\|git\\): \\(.*\\)" paragraph-separate))

(define-error 'fosgit-git-error "Git error")

(defvar-local fosgit-this-error nil)

(defun fosgit-process-finish (arg &optional process-buf command-buf
                                 default-dir section)
  (unless (integerp arg)
    (setq process-buf (process-buffer arg)
          command-buf (process-get arg 'command-buf)
          default-dir (process-get arg 'default-dir)
          section     (process-get arg 'section)
          arg         (process-exit-status arg)))
  (fosgit-process-unset-mode-line)
  (when (featurep 'dired)
    (dired-uncache default-dir))
  (when (buffer-live-p process-buf)
    (with-current-buffer process-buf
      (let ((inhibit-read-only t)
            (marker (fosgit-section-start section)))
        (goto-char marker)
        (save-excursion
          (delete-char 3)
          (set-marker-insertion-type marker nil)
          (insert (propertize (format "%3s" arg)
                              'fosgit-section section
                              'face (if (= arg 0)
                                        'fosgit-process-ok
                                      'fosgit-process-ng)))
          (set-marker-insertion-type marker t))
        (if (= (fosgit-section-end section)
               (+ (line-end-position) 2))
            (save-excursion
              (goto-char (1+ (line-end-position)))
              (delete-char -1)
              (setf (fosgit-section-content section) nil))
          (let ((buf (fosgit-process-buffer t)))
            (when (and (= arg 0)
                       (not (--any-p (eq (window-buffer it) buf)
                                     (window-list))))
              (fosgit-section-hide section)))))))
  (unless (= arg 0)
    (let ((msg (or (and (buffer-live-p process-buf)
                        (with-current-buffer process-buf
                          (save-excursion
                            (goto-char (fosgit-section-end section))
                            (--when-let (fosgit-section-content section)
                              (when (re-search-backward
                                     fosgit-process-error-message-re it t)
                                (match-string 1))))))
                   "Git failed")))
      (if fosgit-process-raise-error
          (signal 'fosgit-git-error (format "%s (in %s)" msg default-dir))
        (--when-let (fosgit-mode-get-buffer 'fosgit-status-mode)
          (setq fosgit-this-error msg))
        (message "%s ... [%s buffer %s for details]" msg
                 (-if-let (key (and (buffer-live-p command-buf)
                                    (with-current-buffer command-buf
                                      (car (where-is-internal
                                            'fosgit-process-buffer)))))
                     (format "Hit %s to see" (key-description key))
                   "See")
                 (buffer-name process-buf)))))
  arg)

(defun fosgit-process-display-buffer (process)
  (when (process-live-p process)
    (let ((buf (process-buffer process)))
      (cond ((not (buffer-live-p buf)))
            ((= fosgit-process-popup-time 0)
             (if (minibufferp)
                 (switch-to-buffer-other-window buf)
               (pop-to-buffer buf)))
            ((> fosgit-process-popup-time 0)
             (run-with-timer fosgit-process-popup-time nil
                             (lambda (p)
                               (when (eq (process-status p) 'run)
                                 (let ((buf (process-buffer p)))
                                   (when (buffer-live-p buf)
                                     (if (minibufferp)
                                         (switch-to-buffer-other-window buf)
                                       (pop-to-buffer buf))))))
                             process))))))

;;; fosgit-process.el ends soon
(provide 'fosgit-process)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-process.el ends here
