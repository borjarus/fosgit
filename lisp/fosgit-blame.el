;;; fosgit-blame.el --- blame support for Magit  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2016  The Magit Project Contributors
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

;; Annotates each line in file-visiting buffer with information from
;; the revision which last modified the line.

;;; Code:

(require 'fosgit)

;;; Options

(defgroup fosgit-blame nil
  "Blame support for Fosgit."
  :group 'fosgit-extensions)

(defcustom fosgit-blame-heading-format "%-20a %C %s"
  "Format string used for blame headings.

The following placeholders are recognized:

  %H    hash
  %s    summary
  %a    author
  %A    author time
  %c    committer
  %C    committer time

The author and committer time formats can be specified with
`fosgit-blame-time-format'."
  :group 'fosgit-blame
  :type 'string)

(defcustom fosgit-blame-time-format "%F %H:%M"
  "Format for time strings in blame headings."
  :group 'fosgit-blame
  :type 'string)

(defcustom fosgit-blame-show-headings t
  "Whether to initially show blame block headings.
The headings can also be toggled locally using command
`fosgit-blame-toggle-headings'."
  :group 'fosgit-blame
  :type 'boolean)

(defcustom fosgit-blame-disable-modes '(fci-mode yascroll-bar-mode)
  "List of modes not compatible with Fosgit-Blame mode.
This modes are turned off when Fosgit-Blame mode is turned on,
and then turned on again when turning off the latter."
  :group 'fosgit-blame
  :type '(repeat (symbol :tag "Mode")))

(make-variable-buffer-local 'fosgit-blame-disabled-modes)

(defcustom fosgit-blame-mode-lighter " Blame"
  "The mode-line lighter of the Fosgit-Blame mode."
  :group 'fosgit-blame
  :type '(choice (const :tag "No lighter" "") string))

(unless (find-lisp-object-file-name 'fosgit-blame-goto-chunk-hook 'defvar)
  (add-hook 'fosgit-blame-goto-chunk-hook 'fosgit-blame-maybe-update-revision-buffer))
(defcustom fosgit-blame-goto-chunk-hook '(fosgit-blame-maybe-update-revision-buffer)
  "Hook run by `fosgit-blame-next-chunk' and `fosgit-blame-previous-chunk'."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-blame
  :type 'hook
  :options '(fosgit-blame-maybe-update-revision-buffer))

(defface fosgit-blame-heading
  '((((class color) (background light))
     :background "grey80"
     :foreground "black")
    (((class color) (background dark))
     :background "grey25"
     :foreground "black"))
  "Face for blame headings."
  :group 'fosgit-faces)

(defface fosgit-blame-summary
  '((t :inherit fosgit-blame-heading))
  "Face used for commit summary in blame headings."
  :group 'fosgit-faces)

(defface fosgit-blame-hash
  '((t :inherit fosgit-blame-heading))
  "Face used for commit hash in blame headings."
  :group 'fosgit-faces)

(defface fosgit-blame-name
  '((t :inherit fosgit-blame-heading))
  "Face used for author and committer names in blame headings."
  :group 'fosgit-faces)

(defface fosgit-blame-date
  '((t :inherit fosgit-blame-heading))
  "Face used for dates in blame headings."
  :group 'fosgit-faces)

;;; Code

(defvar fosgit-blame-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'fosgit-show-commit)
    (define-key map "\s" 'fosgit-diff-show-or-scroll-up)
    (define-key map "\d" 'fosgit-diff-show-or-scroll-down)
    (define-key map "b"  'fosgit-blame-popup)
    (define-key map "n"  'fosgit-blame-next-chunk)
    (define-key map "N"  'fosgit-blame-next-chunk-same-commit)
    (define-key map "p"  'fosgit-blame-previous-chunk)
    (define-key map "P"  'fosgit-blame-previous-chunk-same-commit)
    (define-key map "q"  'fosgit-blame-quit)
    (define-key map "t"  'fosgit-blame-toggle-headings)
    (define-key map "\M-w" 'fosgit-blame-copy-hash)
    map)
  "Keymap for `fosgit-blame-mode'.")

(defun fosgit-blame-put-keymap-before-view-mode ()
  "Put `fosgit-blame-mode' ahead of `view-mode' in `minor-mode-map-alist'."
  (--when-let (assq 'fosgit-blame-mode
                    (cl-member 'view-mode minor-mode-map-alist :key #'car))
    (setq minor-mode-map-alist
          (cons it (delq it minor-mode-map-alist))))
  (remove-hook 'view-mode-hook #'fosgit-blame-put-keymap-before-view-mode))

(add-hook 'view-mode-hook #'fosgit-blame-put-keymap-before-view-mode)

(defvar-local fosgit-blame-buffer-read-only nil)
(defvar-local fosgit-blame-cache nil)
(defvar-local fosgit-blame-process nil)
(defvar-local fosgit-blame-recursive-p nil)
(defvar-local fosgit-blame-separator nil)

(define-minor-mode fosgit-blame-mode
  "Display blame information inline.
\n\\{fosgit-blame-mode-map}"
  :lighter fosgit-blame-mode-lighter
  (cond (fosgit-blame-mode
         (when (called-interactively-p 'any)
           (setq fosgit-blame-mode nil)
           (user-error
            (concat "Don't call `fosgit-blame-mode' directly; "
                    "instead use `fosgit-blame' or `fosgit-blame-popup'")))
         (setq fosgit-blame-buffer-read-only buffer-read-only)
         (read-only-mode 1)
         (dolist (mode fosgit-blame-disable-modes)
           (when (and (boundp mode) (symbol-value mode))
             (funcall mode -1)
             (push mode fosgit-blame-disabled-modes)))
         (setq fosgit-blame-separator (fosgit-blame-format-separator)))
        (t
         (unless fosgit-blame-buffer-read-only
           (read-only-mode -1))
         (dolist (mode fosgit-blame-disabled-modes)
           (funcall mode 1))
         (when (process-live-p fosgit-blame-process)
           (kill-process fosgit-blame-process))
         (save-excursion
           (save-restriction
             (widen)
             (dolist (ov (overlays-in (point-min) (point-max)))
               (when (overlay-get ov 'fosgit-blame)
                 (delete-overlay ov))))))))

(defun auto-revert-handler--unless-fosgit-blame-mode ()
  "If Fosgit-Blame mode is on, then do nothing.  See #1731."
  fosgit-blame-mode)

(advice-add 'auto-revert-handler :before-until
            'auto-revert-handler--unless-fosgit-blame-mode)

;;;###autoload (autoload 'fosgit-blame-popup "fosgit-blame" nil t)
(fosgit-define-popup fosgit-blame-popup
  "Popup console for blame commands."
  'fosgit-commands
  :man-page "git-blame"
  :switches '((?w "Ignore whitespace" "-w")
              (?r "Do not treat root commits as boundaries" "--root"))
  :options  '((?C "Detect lines moved or copied within a file" "-C")
              (?M "Detect lines moved or copied between files" "-M"))
  :actions  '((?b "Blame" fosgit-blame))
  :default-arguments '("-w")
  :default-action 'fosgit-blame)

;;;###autoload
(defun fosgit-blame (revision file &optional args line)
  "Display edit history of FILE up to REVISION.

Interactively blame the file being visited in the current buffer.
If the buffer visits a revision of that file, then blame up to
that revision, otherwise blame the file's full history, including
uncommitted changes.

If Fosgit-Blame mode is already turned on then blame recursively, by
visiting REVISION:FILE (using `fosgit-find-file'), where revision
is the revision before the revision that added the lines at
point.

ARGS is a list of additional arguments to pass to `git blame';
only arguments available from `fosgit-blame-popup' should be used.
\n(fn REVISION FILE &optional ARGS)" ; LINE is for internal use
  (interactive
   (let ((args (fosgit-blame-arguments)))
     (if fosgit-blame-mode
         (--if-let (fosgit-blame-chunk-get :previous-hash)
             (list it (fosgit-blame-chunk-get :previous-file)
                   args (fosgit-blame-chunk-get :previous-start))
           (user-error "Block has no further history"))
       (--if-let (fosgit-file-relative-name nil 'tracked)
           (list (or fosgit-buffer-refname fosgit-buffer-revision) it args)
         (if buffer-file-name
             (user-error "Buffer isn't visiting a tracked file")
           (user-error "Buffer isn't visiting a file"))))))
  (fosgit-with-toplevel
    (if revision
        (fosgit-find-file revision file)
      (let ((default-directory default-directory))
        (--if-let (find-buffer-visiting file)
            (progn (switch-to-buffer it)
                   (save-buffer))
          (find-file file))))
    ;; ^ Make sure this doesn't affect the value used below.  b640c6f
    (widen)
    (when line
      (setq fosgit-blame-recursive-p t)
      (goto-char (point-min))
      (forward-line (1- line)))
    (unless fosgit-blame-mode
      (setq fosgit-blame-cache (make-hash-table :test 'equal))
      (let ((show-headings fosgit-blame-show-headings))
        (fosgit-blame-mode 1)
        (setq-local fosgit-blame-show-headings show-headings))
      (message "Blaming...")
      (let ((fosgit-process-popup-time -1)
            (inhibit-fosgit-refresh t))
        (fosgit-run-git-async
         "blame" "--incremental" args
         "-L" (format "%s,%s"
                      (line-number-at-pos (window-start))
                      (line-number-at-pos (1- (window-end nil t))))
         revision "--" file))
      (setq fosgit-blame-process fosgit-this-process)
      (set-process-filter fosgit-this-process 'fosgit-blame-process-filter)
      (set-process-sentinel
       fosgit-this-process
       `(lambda (process event)
          (when (memq (process-status process) '(exit signal))
            (fosgit-process-sentinel process event)
            (fosgit-blame-assert-buffer process)
            (with-current-buffer (process-get process 'command-buf)
              (when fosgit-blame-mode
                (let ((fosgit-process-popup-time -1)
                      (inhibit-fosgit-refresh t)
                      (default-directory ,default-directory))
                  (fosgit-run-git-async "blame" "--incremental" ,@args
                                       ,revision "--" ,file))
                (setq fosgit-blame-process fosgit-this-process)
                (set-process-filter
                 fosgit-this-process 'fosgit-blame-process-filter)
                (set-process-sentinel
                 fosgit-this-process 'fosgit-blame-process-sentinel)))))))))

(defun fosgit-blame-process-sentinel (process event)
  (let ((status (process-status process)))
    (when (memq status '(exit signal))
      (fosgit-process-sentinel process event)
      (if (eq status 'exit)
          (message "Blaming...done")
        (fosgit-blame-assert-buffer process)
        (with-current-buffer (process-get process 'command-buf)
          (fosgit-blame-mode -1))
        (message "Blaming...failed")))))

(defvar fosgit-blame-log nil
  "Whether to log blame output to the process buffer.
This is intended for debugging purposes.")

(defun fosgit-blame-process-filter (process string)
  (when fosgit-blame-log
    (fosgit-process-filter process string))
  (--when-let (process-get process 'partial-line)
    (setq string (concat it string))
    (setf (process-get process 'partial-line) nil))
  (fosgit-blame-assert-buffer process)
  (with-current-buffer (process-get process 'command-buf)
    (when fosgit-blame-mode
      (let ((chunk (process-get process 'chunk))
            (lines (split-string string "\n" t)))
        (unless (string-match-p "\n\\'" string)
          (process-put process 'chunk chunk)
          (process-put process 'partial-line (car (last lines)))
          (setq lines (butlast lines)))
        (dolist (line lines)
          (cond
           ((equal line ""))
           ((not chunk)
            (string-match
             "^\\(.\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)" line)
            (setq chunk
                  (list :hash (let ((hash (match-string 1 line)))
                                (unless (equal hash (make-string 40 ?0))
                                  hash))
                        :previous-start (string-to-number (match-string 2 line))
                        :start (string-to-number (match-string 3 line))
                        :lines (string-to-number (match-string 4 line)))))
           ((string-match "^filename \\(.+\\)" line)
            (let* ((hash (plist-get chunk :hash))
                   (file (match-string 1 line)))
              (--if-let (gethash hash fosgit-blame-cache)
                  (setq chunk (nconc chunk it))
                (plist-put chunk :filename file)
                (puthash hash chunk fosgit-blame-cache)))
            (fosgit-blame-make-overlay chunk)
            (setq chunk nil))
           ((string-match "^previous \\(.\\{40\\}\\) \\(.+\\)" line)
            (plist-put chunk :previous-hash (match-string 1 line))
            (plist-put chunk :previous-file (match-string 2 line)))
           ((string-match "^\\([^ ]+?-mail\\) <\\([^>]+\\)>" line)
            (plist-put chunk (intern (concat ":" (match-string 1 line)))
                       (string-to-number (match-string 2 line))))
           ((string-match "^\\([^ ]+?-\\(?:time\\|tz\\)\\) \\(.+\\)" line)
            (plist-put chunk (intern (concat ":" (match-string 1 line)))
                       (string-to-number (match-string 2 line))))
           ((string-match "^\\([^ ]+\\) \\(.+\\)" line)
            (plist-put chunk (intern (concat ":" (match-string 1 line)))
                       (match-string 2 line))))
          (process-put process 'chunk chunk))))))

(defun fosgit-blame-assert-buffer (process)
  (unless (buffer-live-p (process-get process 'command-buf))
    (kill-process process)
    (user-error "Buffer being blamed has been killed")))

(defun fosgit-blame-make-overlay (chunk)
  (let ((ov (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (forward-line (1- (plist-get chunk :start)))
                (--when-let (--first (overlay-get it 'fosgit-blame)
                                     (overlays-at (point)))
                  (delete-overlay it))
                (make-overlay (point)
                              (progn (forward-line
                                      (plist-get chunk :lines))
                                     (point))))))
        (heading (fosgit-blame-format-heading chunk)))
    (overlay-put ov 'fosgit-blame chunk)
    (overlay-put ov 'fosgit-blame-heading heading)
    (overlay-put ov 'before-string
                 (if fosgit-blame-show-headings
                     heading
                   fosgit-blame-separator))))

(defun fosgit-blame-format-separator ()
  (propertize
   (concat (propertize " "  'display '(space :height (2)))
           (propertize "\n" 'line-height t))
   'face (list :background (face-attribute 'fosgit-blame-heading :background))))

(defun fosgit-blame-format-heading (chunk)
  (with-temp-buffer
    (insert (format-spec
             (concat fosgit-blame-heading-format "\n")
             `((?H . ,(propertize (or (plist-get chunk :hash) "")
                                  'face 'fosgit-blame-hash))
               (?s . ,(propertize (or (plist-get chunk :summary) "")
                                  'face 'fosgit-blame-summary))
               (?a . ,(propertize (or (plist-get chunk :author) "")
                                  'face 'fosgit-blame-name))
               (?A . ,(propertize (fosgit-blame-format-time-string
                                   fosgit-blame-time-format
                                   (plist-get chunk :author-time)
                                   (plist-get chunk :author-tz))
                                  'face 'fosgit-blame-date))
               (?c . ,(propertize (or (plist-get chunk :committer) "")
                                  'face 'fosgit-blame-name))
               (?C . ,(propertize (fosgit-blame-format-time-string
                                   fosgit-blame-time-format
                                   (plist-get chunk :committer-time)
                                   (plist-get chunk :committer-tz))
                                  'face 'fosgit-blame-date)))))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((face (get-text-property (point) 'face))
            (next (or (next-single-property-change (point) 'face)
                      (point-max))))
        (unless face
          (put-text-property (point) next 'face 'fosgit-blame-heading))
        (goto-char next)))
    (buffer-string)))

(defun fosgit-blame-format-time-string (format time tz)
  (format-time-string
   format (seconds-to-time (+ time (* (/ tz 100) 60 60) (* (% tz 100) 60)))))

(defun fosgit-blame-quit ()
  "Turn off Fosgit-Blame mode.
If the buffer was created during a recursive blame,
then also kill the buffer."
  (interactive)
  (if fosgit-blame-recursive-p
      (kill-buffer)
    (fosgit-blame-mode -1)))

(defun fosgit-blame-next-chunk ()
  "Move to the next chunk."
  (interactive)
  (--if-let (next-single-char-property-change (point) 'fosgit-blame)
      (progn (goto-char it)
             (run-hooks 'fosgit-blame-goto-chunk-hook))
    (user-error "No more chunks")))

(defun fosgit-blame-previous-chunk ()
  "Move to the previous chunk."
  (interactive)
  (--if-let (previous-single-char-property-change (point) 'fosgit-blame)
      (progn (goto-char it)
             (run-hooks 'fosgit-blame-goto-chunk-hook))
    (user-error "No more chunks")))

(defun fosgit-blame-next-chunk-same-commit (&optional previous)
  "Move to the next chunk from the same commit.\n\n(fn)"
  (interactive)
  (-if-let (hash (fosgit-blame-chunk-get :hash))
      (let ((pos (point)) ov)
        (save-excursion
          (while (and (not ov)
                      (not (= pos (if previous (point-min) (point-max))))
                      (setq pos (funcall
                                 (if previous
                                     'previous-single-char-property-change
                                   'next-single-char-property-change)
                                 pos 'fosgit-blame)))
            (--when-let (fosgit-blame-overlay-at pos)
              (when (equal (fosgit-blame-chunk-get :hash pos) hash)
                (setq ov it)))))
        (if ov
            (goto-char (overlay-start ov))
          (user-error "No more chunks from same commit")))
    (user-error "This chunk hasn't been blamed yet")))

(defun fosgit-blame-previous-chunk-same-commit ()
  "Move to the previous chunk from the same commit."
  (interactive)
  (fosgit-blame-next-chunk-same-commit 'previous-single-char-property-change))

(defun fosgit-blame-toggle-headings ()
  "Show or hide blame chunk headings."
  (interactive)
  (setq-local fosgit-blame-show-headings (not fosgit-blame-show-headings))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((next (next-single-char-property-change (point) 'fosgit-blame)))
          (--when-let (fosgit-blame-overlay-at (point))
            (overlay-put it 'before-string
                         (if fosgit-blame-show-headings
                             (overlay-get it 'fosgit-blame-heading)
                           fosgit-blame-separator)))
          (goto-char (or next (point-max))))))))

(defun fosgit-blame-copy-hash ()
  "Save hash of the current chunk's commit to the kill ring."
  (interactive)
  (kill-new (message "%s" (fosgit-blame-chunk-get :hash))))

(defun fosgit-blame-chunk-get (key &optional pos)
  (--when-let (fosgit-blame-overlay-at pos)
    (plist-get (overlay-get it 'fosgit-blame) key)))

(defun fosgit-blame-overlay-at (&optional pos)
  (--first (overlay-get it 'fosgit-blame)
           (overlays-at (or pos (point)))))

(defun fosgit-blame-maybe-update-revision-buffer ()
  (unless fosgit--update-revision-buffer
    (setq fosgit--update-revision-buffer nil)
    (-when-let* ((commit (fosgit-blame-chunk-get :hash))
                 (buffer (fosgit-mode-get-buffer 'fosgit-revision-mode nil t)))
      (setq fosgit--update-revision-buffer (list commit buffer))
      (run-with-idle-timer
       fosgit-update-other-window-delay nil
       (lambda ()
         (-let [(rev buf) fosgit--update-revision-buffer]
           (setq fosgit--update-revision-buffer nil)
           (when (buffer-live-p buf)
             (let ((fosgit-display-buffer-noselect t))
               (apply #'fosgit-show-commit rev (fosgit-diff-arguments))))))))))

;;; fosgit-blame.el ends soon
(provide 'fosgit-blame)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-blame.el ends here
