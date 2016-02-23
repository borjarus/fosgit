;;; fosgit-bisect.el --- bisect support for Magit  -*- lexical-binding: t -*-

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

;; Use a binary search to find the commit that introduced a bug.

;;; Code:

(require 'fosgit)

(defface fosgit-bisect-good
  '((t :foreground "DarkOliveGreen"))
  "Face for good bisect revisions."
  :group 'fosgit-faces)

(defface fosgit-bisect-skip
  '((t :foreground "DarkGoldenrod"))
  "Face for skipped bisect revisions."
  :group 'fosgit-faces)

(defface fosgit-bisect-bad
  '((t :foreground "IndianRed4"))
  "Face for bad bisect revisions."
  :group 'fosgit-faces)

;;;###autoload (autoload 'fosgit-bisect-popup "fosgit-bisect" nil t)
(fosgit-define-popup fosgit-bisect-popup
  "Popup console for bisect commands."
  'fosgit-commands
  :man-page "git-bisect"
  :actions            '((?B "Start"        fosgit-bisect-start)
                        (?s "Start script" fosgit-bisect-run))
  :sequence-actions   '((?b "Bad"          fosgit-bisect-bad)
                        (?g "Good"         fosgit-bisect-good)
                        (?k "Skip"         fosgit-bisect-skip)
                        (?r "Reset"        fosgit-bisect-reset)
                        (?s "Run script"   fosgit-bisect-run))
  :sequence-predicate 'fosgit-bisect-in-progress-p)

;;;###autoload
(defun fosgit-bisect-start (bad good)
  "Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a know
good and a bad commit.  To move the session forward use the
other actions from the bisect popup (\
\\<fosgit-status-mode-map>\\[fosgit-bisect-popup])."
  (interactive (if (fosgit-bisect-in-progress-p)
                   (user-error "Already bisecting")
                 (fosgit-bisect-start-read-args)))
  (fosgit-git-bisect "start" (list bad good) t))

(defun fosgit-bisect-start-read-args ()
  (let  ((b (fosgit-read-branch-or-commit "Start bisect with bad revision")))
    (list b (fosgit-read-other-branch-or-commit "Good revision" b))))

;;;###autoload
(defun fosgit-bisect-reset ()
  "After bisecting, cleanup bisection state and return to original `HEAD'."
  (interactive)
  (when (fosgit-confirm 'reset-bisect)
    (fosgit-run-git "bisect" "reset")
    (ignore-errors (delete-file (fosgit-git-dir "BISECT_CMD_OUTPUT")))))

;;;###autoload
(defun fosgit-bisect-good ()
  "While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question."
  (interactive)
  (fosgit-git-bisect "good"))

;;;###autoload
(defun fosgit-bisect-bad ()
  "While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question."
  (interactive)
  (fosgit-git-bisect "bad"))

;;;###autoload
(defun fosgit-bisect-skip ()
  "While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one."
  (interactive)
  (fosgit-git-bisect "skip"))

;;;###autoload
(defun fosgit-bisect-run (cmdline &optional bad good)
  "Bisect automatically by running commands after each step.

Unlike `git bisect run' this can be used before bisecting has
begun.  In that case it behaves like `git bisect start; git
bisect run'."
  (interactive (let ((args (and (not (fosgit-bisect-in-progress-p))
                                (fosgit-bisect-start-read-args))))
                 (cons (read-shell-command "Bisect shell command: ") args)))
  (when (and bad good)
    (fosgit-bisect-start bad good))
  (fosgit-git-bisect "run" (list cmdline)))

(defun fosgit-git-bisect (subcommand &optional args no-assert)
  (unless (or no-assert (fosgit-bisect-in-progress-p))
    (user-error "Not bisecting"))
  (fosgit-with-toplevel
    (fosgit-run-git-with-logfile
     (fosgit-git-dir "BISECT_CMD_OUTPUT") "bisect" subcommand args)))

(defun fosgit-bisect-in-progress-p ()
  (file-exists-p (fosgit-git-dir "BISECT_LOG")))

(defun fosgit-insert-bisect-output ()
  "While bisecting, insert section with output from `git bisect'."
  (when (fosgit-bisect-in-progress-p)
    (let ((lines
           (or (fosgit-file-lines (fosgit-git-dir "BISECT_CMD_OUTPUT"))
               (list "Bisecting: (no saved bisect output)"
                     "It appears you have invoked `git bisect' from a shell."
                     "There is nothing wrong with that, we just cannot display"
                     "anything useful here.  Consult the shell output instead.")))
          (done-re "^[a-z0-9]\\{40\\} is the first bad commit$"))
      (fosgit-insert-section (bisect-output t)
        (fosgit-insert-heading
          (propertize (or (and (string-match done-re (car lines)) (pop lines))
                          (--first (string-match done-re it) lines)
                          (pop lines))
                      'face 'fosgit-section-heading))
        (dolist (line lines)
          (insert line "\n"))))
    (insert "\n")))

(defun fosgit-insert-bisect-rest ()
  "While bisecting, insert section visualizing the bisect state."
  (when (fosgit-bisect-in-progress-p)
    (fosgit-insert-section (bisect-view)
      (fosgit-insert-heading "Bisect Rest:")
      (fosgit-git-wash (apply-partially 'fosgit-log-wash-log 'bisect-vis)
        "bisect" "visualize" "git" "log"
        "--format=%h%d %s" "--decorate=full"))))

(defun fosgit-insert-bisect-log ()
  "While bisecting, insert section logging bisect progress."
  (when (fosgit-bisect-in-progress-p)
    (fosgit-insert-section (bisect-log)
      (fosgit-insert-heading "Bisect Log:")
      (fosgit-git-wash #'fosgit-wash-bisect-log "bisect" "log")
      (insert ?\n))))

(defun fosgit-wash-bisect-log (_args)
  (let (beg)
    (while (progn (setq beg (point-marker))
                  (re-search-forward "^\\(git bisect [^\n]+\n\\)" nil t))
      (fosgit-bind-match-strings (heading) nil
        (fosgit-delete-match)
        (save-restriction
          (narrow-to-region beg (point))
          (goto-char (point-min))
          (fosgit-insert-section (bisect-log heading t)
            (insert (propertize heading 'face 'fosgit-section-secondary-heading))
            (fosgit-insert-heading)
            (fosgit-wash-sequence
             (apply-partially 'fosgit-log-wash-rev 'bisect-log
                              (fosgit-abbrev-length)))
            (insert ?\n)))))
    (when (re-search-forward
           "# first bad commit: \\[\\([a-z0-9]\\{40\\}\\)\\] [^\n]+\n" nil t)
      (fosgit-bind-match-strings (hash) nil
        (fosgit-delete-match)
        (fosgit-insert-section (bisect-log)
          (insert hash " is the first bad commit\n"))))))

;;; fosgit-bisect.el ends soon
(provide 'fosgit-bisect)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-bisect.el ends here
