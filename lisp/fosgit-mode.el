;;; fosgit-mode.el --- create and refresh Magit buffers  -*- lexical-binding: t -*-

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

;; This library implements the abstract major-mode `fosgit-mode' from
;; which almost all other Fosgit major-modes derive.  The code in here
;; is mostly concerned with creating and refreshing Fosgit buffers.

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'fosgit-section)
(require 'fosgit-git)

;; For `fosgit-xref-insert-buttons' from `fosgit'
(defvar fosgit-diff-show-xref-buttons)
(defvar fosgit-revision-show-xref-buttons)
;; For `fosgit-refresh' and `fosgit-refresh-all'
(declare-function fosgit-auto-revert-buffers 'fosgit-autorevert)

(require 'format-spec)
(require 'help-mode)

;;; Options

(defcustom fosgit-mode-hook
  '(fosgit-load-config-extensions
    fosgit-xref-setup)
  "Hook run when entering a mode derived from Fosgit mode."
  :group 'fosgit-modes
  :type 'hook
  :options '(fosgit-load-config-extensions
             fosgit-xref-setup
             bug-reference-mode))

(defcustom fosgit-mode-setup-hook
  '(fosgit-maybe-save-repository-buffers
    fosgit-maybe-show-margin)
  "Hook run by `fosgit-mode-setup'."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-modes
  :type 'hook
  :options '(fosgit-maybe-save-repository-buffers
             fosgit-maybe-show-margin))

(defcustom fosgit-pre-refresh-hook '(fosgit-maybe-save-repository-buffers)
  "Hook run before refreshing in `fosgit-refresh'.

This hook, or `fosgit-post-refresh-hook', should be used
for functions that are not tied to a particular buffer.

To run a function with a particular buffer current, use
`fosgit-refresh-buffer-hook' and use `derived-mode-p'
inside your function."
  :package-version '(fosgit . "2.4.0")
  :group 'fosgit-modes
  :type 'hook
  :options '(fosgit-maybe-save-repository-buffers))

(defcustom fosgit-post-refresh-hook nil
  "Hook run after refreshing in `fosgit-refresh'.

This hook, or `fosgit-pre-refresh-hook', should be used
for functions that are not tied to a particular buffer.

To run a function with a particular buffer current, use
`fosgit-refresh-buffer-hook' and use `derived-mode-p'
inside your function."
  :package-version '(fosgit . "2.4.0")
  :group 'fosgit-modes
  :type 'hook)

(defcustom fosgit-display-buffer-function 'fosgit-display-buffer-traditional
  "The function used display a Fosgit buffer.

All Fosgit buffers (buffers whose major-modes derive from
`fosgit-mode') are displayed using `fosgit-display-buffer',
which in turn uses the function specified here."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-modes
  :type '(radio (function-item fosgit-display-buffer-traditional)
                (function-item display-buffer)
                (function :tag "Function")))

(unless (find-lisp-object-file-name 'fosgit-pre-display-buffer-hook 'defvar)
  (add-hook 'fosgit-pre-display-buffer-hook 'fosgit-save-window-configuration))
(defcustom fosgit-pre-display-buffer-hook '(fosgit-save-window-configuration)
  "Hook run by `fosgit-display-buffer' before displaying the buffer."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-modes
  :type 'hook
  :options '(fosgit-save-window-configuration))

(unless (find-lisp-object-file-name 'fosgit-post-display-buffer-hook 'defvar)
  (add-hook 'fosgit-post-display-buffer-hook 'fosgit-maybe-set-dedicated))
(defcustom fosgit-post-display-buffer-hook '(fosgit-maybe-set-dedicated)
  "Hook run by `fosgit-display-buffer' after displaying the buffer."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-modes
  :type 'hook
  :options '(fosgit-maybe-set-dedicated))

(defcustom fosgit-generate-buffer-name-function
  'fosgit-generate-buffer-name-default-function
  "The function used to generate the name for a Fosgit buffer."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-modes
  :type '(radio (function-item fosgit-generate-buffer-name-default-function)
                (function :tag "Function")))

(defcustom fosgit-buffer-name-format "*%M%v: %t"
  "The format string used to name Fosgit buffers.

The following %-sequences are supported:

`%m' The name of the major-mode, but with the `-mode' suffix
     removed.

`%M' Like \"%m\" but abbreviate `fosgit-status-mode' as `fosgit'.

`%v' The value the buffer is locked to, in parentheses, or an empty
     string if the buffer is not locked to a value.

`%V' Like \"%v\", but the string is prefixed with a space, unless it
     is an empty string.

`%t' The top-level directory of the working tree of the
     repository, or if `fosgit-uniquify-buffer-names' is non-nil
     an abbreviation of that.

The value should always contain either \"%m\" or \"%M\" as well as
\"%t\".  If `fosgit-uniquify-buffer-names' is non-nil, then the
value must end with \"%t\".

This is used by `fosgit-generate-buffer-name-default-function'.
If another `fosgit-generate-buffer-name-function' is used, then
it may not respect this option, or on the contrary it may
support additional %-sequences."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-modes
  :type 'string)

(defcustom fosgit-uniquify-buffer-names t
  "Whether to uniquify the names of Fosgit buffers."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-modes
  :type 'boolean)

(defcustom fosgit-bury-buffer-function 'fosgit-restore-window-configuration
  "The function used to bury or kill the current Fosgit buffer."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-modes
  :type '(radio (function-item quit-window)
                (function-item fosgit-mode-quit-window)
                (function-item fosgit-restore-window-configuration)
                (function :tag "Function")))

(defcustom fosgit-region-highlight-hook
  '(fosgit-section-update-region fosgit-diff-update-hunk-region)
  "Functions used to highlight the region.
Each function is run with the current section as only argument
until one of them returns non-nil.  When multiple sections are
selected, then this hook does not run and the region is not
displayed.  Otherwise fall back to regular region highlighting."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-modes
  :type 'hook
  :options '(fosgit-section-update-region fosgit-diff-update-hunk-region))

(defcustom fosgit-refresh-verbose nil
  "Whether to revert Fosgit buffers verbosely."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-modes
  :type 'boolean)

(defcustom fosgit-refresh-buffer-hook nil
  "Normal hook for `fosgit-refresh-buffer' to run after refreshing."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-modes
  :type 'hook)

(defcustom fosgit-refresh-status-buffer t
  "Whether the status buffer is refreshed after running git.

When this is non-nil, then the status buffer is automatically
refreshed after running git for side-effects, in addition to the
current Fosgit buffer, which is always refreshed automatically.

Only set this to nil after exhausting all other options to
improve performance."
  :package-version '(fosgit . "2.4.0")
  :group 'fosgit-status
  :type 'boolean)

(defcustom fosgit-save-repository-buffers t
  "Whether to save file-visiting buffers when appropriate.

If this is non-nil then all modified file-visiting buffers
belonging to the current repository may be saved before running
commands, before creating new Fosgit buffers, and before
explicitly refreshing such buffers.  If this is `dontask' then
this is done without user intervention, if it is t then the user
has to confirm each save.  `dontask' is the recommended setting."
  :group 'fosgit
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask" t)
                 (const :tag "Save without asking" dontask)))

(defcustom fosgit-keep-region-overlay nil
  "Whether to keep the region overlay when there is a valid selection.

By default Fosgit removes the regular region overlay if, and only
if, that region constitutes a valid selection as understood by
Fosgit commands.  Otherwise it does not remove that overlay, and
the region looks like it would in other buffers.

There are two types of such valid selections: hunk-internal
regions and regions that select two or more sibling sections.
In such cases Fosgit removes the region overlay and instead
highlights a slightly larger range.  All text (for hunk-internal
regions) or the headings of all sections (for sibling selections)
that are inside that range (not just inside the region) are acted
on by commands such as the staging command.  This buffer range
begins at the beginning of the line on which the region begins
and ends at the end of the line on which the region ends.

Because Fosgit acts on this larger range and not the region, it is
actually quite important to visualize that larger range.  If we
don't do that, then one might think that these commands act on
the region instead.  If you want to *also* visualize the region,
then set this option to t.  But please note that when the region
does *not* constitute a valid selection, then the region is
*always* visualized as usual, and that it is usually under such
circumstances that you want to use a non-fosgit command to act on
the region.

Besides keeping the region overlay, setting this option to t also
causes all face properties, except for `:foreground', to be
ignored for the faces used to highlight headings of selected
sections.  This avoids the worst conflicts that result from
displaying the region and the selection overlays at the same
time.  We are not interested in dealing with other conflicts.
In fact we *already* provide a way to avoid all of these
conflicts: *not* changing the value of this option.

It should be clear by now that we consider it a mistake to set
this to display the region when the Fosgit selection is also
visualized, but since it has been requested a few times and
because it doesn't cost much to offer this option we do so.
However that might change.  If the existence of this option
starts complicating other things, then it will be removed."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-modes
  :type 'boolean)

;;; Fosgit Mode

(defvar fosgit-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "\t"    'fosgit-section-toggle)
    (define-key map [C-tab] 'fosgit-section-cycle)
    (define-key map [M-tab] 'fosgit-section-cycle-diffs)
    (define-key map [s-tab] 'fosgit-section-cycle-global)
    (define-key map [backtab] 'fosgit-section-cycle-global)
    (define-key map "^"    'fosgit-section-up)
    (define-key map "n"    'fosgit-section-forward)
    (define-key map "p"    'fosgit-section-backward)
    (define-key map "\M-n" 'fosgit-section-forward-sibling)
    (define-key map "\M-p" 'fosgit-section-backward-sibling)
    (define-key map "+"    'fosgit-diff-more-context)
    (define-key map "-"    'fosgit-diff-less-context)
    (define-key map "0"    'fosgit-diff-default-context)
    (define-key map "1"    'fosgit-section-show-level-1)
    (define-key map "2"    'fosgit-section-show-level-2)
    (define-key map "3"    'fosgit-section-show-level-3)
    (define-key map "4"    'fosgit-section-show-level-4)
    (define-key map "\M-1" 'fosgit-section-show-level-1-all)
    (define-key map "\M-2" 'fosgit-section-show-level-2-all)
    (define-key map "\M-3" 'fosgit-section-show-level-3-all)
    (define-key map "\M-4" 'fosgit-section-show-level-4-all)
    (define-key map "g" 'fosgit-refresh)
    (define-key map "G" 'fosgit-refresh-all)
    (define-key map "q" 'fosgit-mode-bury-buffer)
    (define-key map "$" 'fosgit-process-buffer)
    (define-key map "a" 'fosgit-cherry-apply)
    (define-key map "A" 'fosgit-cherry-pick-popup)
    (define-key map "b" 'fosgit-branch-popup)
    (define-key map "B" 'fosgit-bisect-popup)
    (define-key map "c" 'fosgit-commit-popup)
    (define-key map "d" 'fosgit-diff-popup)
    (define-key map "D" 'fosgit-diff-refresh-popup)
    (define-key map "h" 'fosgit-dispatch-popup)
    (define-key map "?" 'fosgit-dispatch-popup)
    (define-key map "\C-c\C-c" 'fosgit-dispatch-popup)
    (define-key map "\C-c\C-e" 'fosgit-dispatch-popup)
    (define-key map "e" 'fosgit-ediff-dwim)
    (define-key map "E" 'fosgit-ediff-popup)
    (define-key map "f" 'fosgit-fetch-popup)
    (define-key map "F" 'fosgit-pull-popup)
    (define-key map "i" 'fosgit-gitignore)
    (define-key map "I" 'fosgit-gitignore-locally)
    (define-key map "k" 'fosgit-delete-thing)
    (define-key map "K" 'fosgit-file-untrack)
    (define-key map "l" 'fosgit-log-popup)
    (define-key map "L" 'fosgit-log-refresh-popup)
    (define-key map "m" 'fosgit-merge-popup)
    (define-key map "M" 'fosgit-remote-popup)
    (define-key map "o" 'fosgit-submodule-popup)
    (define-key map "P" 'fosgit-push-popup)
    (define-key map "r" 'fosgit-rebase-popup)
    (define-key map "R" 'fosgit-file-rename)
    (define-key map "t" 'fosgit-tag-popup)
    (define-key map "T" 'fosgit-notes-popup)
    (define-key map "\r"       'fosgit-visit-thing)
    (define-key map [C-return] 'fosgit-visit-thing)
    (define-key map [M-return] 'fosgit-dired-jump)
    (define-key map "\s"       'fosgit-diff-show-or-scroll-up)
    (define-key map "\d"       'fosgit-diff-show-or-scroll-down)
    (define-key map "s" 'fosgit-stage-file)
    (define-key map "S" 'fosgit-stage-modified)
    (define-key map "u" 'fosgit-unstage-file)
    (define-key map "U" 'fosgit-unstage-all)
    (define-key map "v" 'fosgit-revert-no-commit)
    (define-key map "V" 'fosgit-revert-popup)
    (define-key map "w" 'fosgit-am-popup)
    (define-key map "W" 'fosgit-patch-popup)
    (define-key map "x" 'fosgit-reset)
    (define-key map "y" 'fosgit-show-refs-popup)
    (define-key map "Y" 'fosgit-cherry)
    (define-key map "z" 'fosgit-stash-popup)
    (define-key map "Z" 'fosgit-stash-popup)
    (define-key map ":" 'fosgit-git-command)
    (define-key map "!" 'fosgit-run-popup)
    (define-key map "\C-xa"  'fosgit-add-change-log-entry)
    (define-key map "\C-x4a" 'fosgit-add-change-log-entry-other-window)
    (define-key map "\C-w"   'fosgit-copy-section-value)
    (define-key map "\M-w"   'fosgit-copy-buffer-revision)
    (define-key map [remap evil-previous-line] 'evil-previous-visual-line)
    (define-key map [remap evil-next-line] 'evil-next-visual-line)
    map)
  "Parent keymap for all keymaps of modes derived from `fosgit-mode'.")

(defun fosgit-delete-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which deletes the thing at point."
  (interactive)
  (user-error "There is no thing at point that could be deleted"))

(defun fosgit-visit-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which visits the thing at point."
  (interactive)
  (user-error "There is no thing at point that could be visited"))

(easy-menu-define fosgit-mode-menu fosgit-mode-map
  "Fosgit menu"
  '("Fosgit"
    ["Refresh" fosgit-refresh t]
    ["Refresh all" fosgit-refresh-all t]
    "---"
    ["Stage" fosgit-stage t]
    ["Stage modified" fosgit-stage-modified t]
    ["Unstage" fosgit-unstage t]
    ["Reset index" fosgit-reset-index t]
    ["Commit" fosgit-commit-popup t]
    ["Add log entry" fosgit-commit-add-log t]
    ["Tag" fosgit-tag t]
    "---"
    ["Diff working tree" fosgit-diff-working-tree t]
    ["Diff" fosgit-diff t]
    ("Log"
     ["Log" fosgit-log t]
     ["Reflog" fosgit-reflog t]
     ["Extended..." fosgit-log-popup t])
    "---"
    ["Cherry pick" fosgit-cherry-pick t]
    ["Revert commit" fosgit-revert-popup t]
    "---"
    ["Ignore" fosgit-gitignore t]
    ["Ignore locally" fosgit-gitignore-locally t]
    ["Discard" fosgit-discard t]
    ["Reset head" fosgit-reset-head t]
    ["Stash" fosgit-stash t]
    ["Snapshot" fosgit-snapshot t]
    "---"
    ["Branch..." fosgit-checkout t]
    ["Merge" fosgit-merge t]
    ["Ediff resolve" fosgit-ediff-resolve t]
    ["Rebase..." fosgit-rebase-popup t]
    "---"
    ["Push" fosgit-push t]
    ["Pull" fosgit-pull t]
    ["Remote update" fosgit-fetch-all t]
    ("Submodule"
     ["Submodule update" fosgit-submodule-update t]
     ["Submodule update and init" fosgit-submodule-setup t]
     ["Submodule init" fosgit-submodule-init t]
     ["Submodule sync" fosgit-submodule-sync t])
    "---"
    ("Extensions")
    "---"
    ["Display Git output" fosgit-process-buffer t]
    ["Quit Fosgit" fosgit-mode-bury-buffer t]))

(defun fosgit-load-config-extensions ()
  "Load Fosgit extensions that are defined at the Git config layer."
  (dolist (ext (fosgit-get-all "fosgit.extension"))
    (let ((sym (intern (format "fosgit-%s-mode" ext))))
      (when (fboundp sym)
        (funcall sym 1)))))

(define-derived-mode fosgit-mode special-mode "Fosgit"
  "Parent major mode from which Fosgit major modes inherit.

Fosgit is documented in info node `(fosgit)'."
  :group 'fosgit-modes
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t) ; see #1771
  (setq show-trailing-whitespace nil)
  (setq list-buffers-directory default-directory)
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (add-hook 'post-command-hook #'fosgit-section-update-highlight t t)
  (setq-local redisplay-highlight-region-function 'fosgit-highlight-region)
  (setq-local redisplay-unhighlight-region-function 'fosgit-unhighlight-region)
  (when (fboundp 'linum-mode)
    (linum-mode -1)))

(defvar-local fosgit-region-overlays nil)

(defun fosgit-highlight-region (start end window rol)
  (mapc #'delete-overlay fosgit-region-overlays)
  (if (and (run-hook-with-args-until-success 'fosgit-region-highlight-hook
                                             (fosgit-current-section))
           (not fosgit-keep-region-overlay))
      (funcall (default-value 'redisplay-unhighlight-region-function) rol)
    (funcall (default-value 'redisplay-highlight-region-function)
             start end window rol)))

(defun fosgit-unhighlight-region (rol)
  (setq fosgit-section-highlighted-section nil)
  (mapc #'delete-overlay fosgit-region-overlays)
  (funcall (default-value 'redisplay-unhighlight-region-function) rol))

(defvar-local fosgit-refresh-args nil
  "The arguments used to refresh the current buffer.")
(put 'fosgit-refresh-args 'permanent-local t)

(defvar-local fosgit-previous-section nil)
(put 'fosgit-previous-section 'permanent-local t)

(defun fosgit-mode-setup (mode &rest args)
  "Setup up a MODE buffer using ARGS to generate its content."
  (let ((buffer (fosgit-mode-get-buffer mode t))
        (section (fosgit-current-section)))
    (with-current-buffer buffer
      (setq fosgit-previous-section section)
      (setq fosgit-refresh-args args)
      (funcall mode))
    (fosgit-display-buffer buffer)
    (with-current-buffer buffer
      (run-hooks 'fosgit-mode-setup-hook)
      (fosgit-refresh-buffer))))

(defvar fosgit-display-buffer-noselect nil
  "If non-nil, then `fosgit-display-buffer' doesn't call `select-window'.")

(defun fosgit-display-buffer (buffer)
  "Display BUFFER in some window and maybe select it.

Display the buffer using `fosgit-display-buffer-function' and
then, unless `fosgit-display-buffer-noselect' is non-nil, select
the window which was used to display the buffer.

Also run the hooks `fosgit-pre-display-buffer-hook'
and `fosgit-post-display-buffer-hook'."
  (with-current-buffer buffer
    (run-hooks 'fosgit-pre-display-buffer-hook))
  (let ((window (funcall fosgit-display-buffer-function buffer)))
    (unless fosgit-display-buffer-noselect
      (select-window window)))
  (with-current-buffer buffer
    (run-hooks 'fosgit-post-display-buffer-hook)))

(defun fosgit-display-buffer-traditional (buffer)
  "Display BUFFER the way this has traditionally been done."
  (display-buffer
   buffer (if (and (derived-mode-p 'fosgit-mode)
                   (not (memq (with-current-buffer buffer major-mode)
                              '(fosgit-process-mode
                                fosgit-revision-mode
                                fosgit-diff-mode
                                fosgit-stash-mode
                                fosgit-status-mode))))
              '(display-buffer-same-window)
            nil))) ; display in another window

(defun fosgit-maybe-set-dedicated ()
  "Mark the selected window as dedicated if appropriate.

If a new window was created to display the buffer, then remember
that fact.  That information is used by `fosgit-mode-quit-window',
to determine whether the window should be deleted when its last
Fosgit buffer is buried."
  (let ((window (get-buffer-window (current-buffer))))
    (when (and (window-live-p window)
               (not (window-prev-buffers window)))
      (set-window-parameter window 'fosgit-dedicated t))))

(defvar-local fosgit--default-directory nil
  "Value of `default-directory' when buffer is generated.
This exists to prevent a let-bound `default-directory' from
tricking `fosgit-mode-get-buffer' or `fosgit-mode-get-buffers' into
thinking a buffer belongs to a repo that it doesn't.")
(put 'fosgit--default-directory 'permanent-local t)

(defun fosgit-mode-get-buffers ()
  (let ((topdir (fosgit-toplevel)))
    (--filter (with-current-buffer it
                (and (derived-mode-p 'fosgit-mode)
                     (equal fosgit--default-directory topdir)))
              (buffer-list))))

(defvar-local fosgit-buffer-locked-p nil)
(put 'fosgit-buffer-locked-p 'permanent-local t)

(defun fosgit-mode-get-buffer (mode &optional create frame)
  (-if-let (topdir (fosgit-toplevel))
      (or (--first (with-current-buffer it
                     (and (eq major-mode mode)
                          (equal fosgit--default-directory topdir)
                          (not fosgit-buffer-locked-p)))
                   (if frame
                       (-map #'window-buffer
                             (window-list (unless (eq frame t) frame)))
                     (buffer-list)))
          (and create
               (let ((default-directory topdir))
                 (fosgit-generate-new-buffer mode))))
    (user-error "Not inside a Git repository")))

(defun fosgit-generate-new-buffer (mode)
  (let* ((name (funcall fosgit-generate-buffer-name-function mode))
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq fosgit--default-directory default-directory))
    (when fosgit-uniquify-buffer-names
      (add-to-list 'uniquify-list-buffers-directory-modes mode)
      (with-current-buffer buffer
        (setq list-buffers-directory default-directory))
      (let ((uniquify-buffer-name-style
             (if (memq uniquify-buffer-name-style '(nil forward))
                 'post-forward-angle-brackets
               uniquify-buffer-name-style)))
        (uniquify-rationalize-file-buffer-names
         name (file-name-directory (directory-file-name default-directory))
         buffer)))
    buffer))

(defun fosgit-generate-buffer-name-default-function (mode &optional value)
  (let ((m (substring (symbol-name mode) 0 -5))
        (v (and value (format "%s" (if (listp value) value (list value))))))
    (format-spec
     fosgit-buffer-name-format
     `((?m . ,m)
       (?M . ,(if (eq mode 'fosgit-status-mode) "fosgit" m))
       (?v . ,(or v ""))
       (?V . ,(if v (concat " " v) ""))
       (?t . ,(if fosgit-uniquify-buffer-names
                  (file-name-nondirectory
                   (directory-file-name default-directory))
                default-directory))))))

(defun fosgit-toggle-buffer-lock ()
  "Lock the current buffer to its value or unlock it.

Locking a buffer to its value, prevents it from being reused to
display another value.  The name of a locked buffer contains its
value, which allows telling it apart from other locked buffers
and the unlocked buffer.

Not all Fosgit buffers can be locked to their values, for example
it wouldn't make sense to lock a status buffer.

There can only be a single unlocked buffer using a certain
major-mode per repository.  So when a buffer is being unlocked
and another unlocked buffer already exists for that mode and
repository, then the former buffer is instead deleted and the
latter is displayed in its place."
  (interactive)
  (if fosgit-buffer-locked-p
      (-if-let (unlocked (fosgit-mode-get-buffer major-mode))
          (let ((locked (current-buffer)))
            (set-buffer unlocked)
            (kill-buffer locked))
        (setq fosgit-buffer-locked-p nil)
        (rename-buffer (funcall fosgit-generate-buffer-name-function
                                major-mode)))
    (setq fosgit-buffer-locked-p
          (cond ((memq major-mode '(fosgit-cherry-mode
                                    fosgit-log-mode
                                    fosgit-reflog-mode
                                    fosgit-refs-mode
                                    fosgit-revision-mode
                                    fosgit-stash-mode
                                    fosgit-stashes-mode))
                 (car fosgit-refresh-args))
                ((eq major-mode 'fosgit-diff-mode)
                 (let ((rev  (nth 0 fosgit-refresh-args))
                       (args (nth 1 fosgit-refresh-args)))
                   (cond
                    ((member "--no-index" args)
                     (nth 3 fosgit-refresh-args))
                    (rev (if args (cons rev args) rev))
                    (t   (if (member "--cached" args) "staged" "unstaged")))))))
    (if fosgit-buffer-locked-p
        (rename-buffer (funcall fosgit-generate-buffer-name-function
                                major-mode fosgit-buffer-locked-p))
      (user-error "Buffer has no value it could be locked to"))))

(defun fosgit-mode-bury-buffer (&optional kill-buffer)
  "Bury the current buffer.
With a prefix argument, kill the buffer instead.
This is done using `fosgit-bury-buffer-function'."
  (interactive "P")
  (funcall fosgit-bury-buffer-function kill-buffer))

(defun fosgit-mode-quit-window (kill-buffer)
  "Quit the selected window and bury its buffer.

This behaves similar to `quit-window', but when the window
was originally created to display a Fosgit buffer and the
current buffer is the last remaining Fosgit buffer that was
ever displayed in the selected window, then delete that
window."
  (if (or (one-window-p)
          (--first (let ((buffer (car it)))
                     (and (not (eq buffer (current-buffer)))
                          (buffer-live-p buffer)
                          (or (not (window-parameter nil 'fosgit-dedicated))
                              (with-current-buffer buffer
                                (derived-mode-p 'fosgit-mode
                                                'fosgit-process-mode)))))
                   (window-prev-buffers)))
      (quit-window kill-buffer)
    (let ((window (selected-window)))
      (quit-window kill-buffer)
      (when (window-live-p window)
        (delete-window window)))))

;;; Refresh Fosgit Buffers

(defvar inhibit-fosgit-refresh nil)

(defun fosgit-refresh ()
  "Refresh some buffers belonging to the current repository.

Refresh the current buffer if its major mode derives from
`fosgit-mode', and refresh the corresponding status buffer.

Run hooks `fosgit-pre-refresh-hook' and `fosgit-post-refresh-hook'."
  (interactive)
  (unless inhibit-fosgit-refresh
    (fosgit-run-hook-with-benchmark 'fosgit-pre-refresh-hook)
    (when (derived-mode-p 'fosgit-mode)
      (fosgit-refresh-buffer))
    (--when-let (and fosgit-refresh-status-buffer
                     (not (derived-mode-p 'fosgit-status-mode))
                     (fosgit-mode-get-buffer 'fosgit-status-mode))
      (with-current-buffer it
        (fosgit-refresh-buffer)))
    (fosgit-auto-revert-buffers)
    (fosgit-run-hook-with-benchmark 'fosgit-post-refresh-hook)))

(defun fosgit-refresh-all ()
  "Refresh all buffers belonging to the current repository.

Refresh all Fosgit buffers belonging to the current repository,
and revert buffers that visit files located inside the current
repository.

Run hooks `fosgit-pre-refresh-hook' and `fosgit-post-refresh-hook'."
  (interactive)
  (fosgit-run-hook-with-benchmark 'fosgit-pre-refresh-hook)
  (dolist (buffer (fosgit-mode-get-buffers))
    (with-current-buffer buffer (fosgit-refresh-buffer)))
  (fosgit-auto-revert-buffers)
  (fosgit-run-hook-with-benchmark 'fosgit-post-refresh-hook))

(defvar-local fosgit-refresh-start-time nil)

(defun fosgit-refresh-buffer ()
  "Refresh the current Fosgit buffer."
  (setq fosgit-refresh-start-time (current-time))
  (let ((refresh (intern (format "%s-refresh-buffer"
                                 (substring (symbol-name major-mode) 0 -5)))))
    (when (functionp refresh)
      (when fosgit-refresh-verbose
        (message "Refreshing buffer `%s'..." (buffer-name)))
      (let* ((buffer (current-buffer))
             (windows
              (--mapcat (with-selected-window it
                          (with-current-buffer buffer
                            (-when-let (section (fosgit-current-section))
                              (list
                               (nconc (list it section)
                                      (fosgit-refresh-get-relative-position))))))
                        (or (get-buffer-window-list buffer nil t)
                            (list (selected-window))))))
        (deactivate-mark)
        (setq fosgit-section-highlight-overlays nil
              fosgit-section-highlighted-section nil
              fosgit-section-highlighted-sections nil
              fosgit-section-unhighlight-sections nil)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (save-excursion
            (apply refresh fosgit-refresh-args)))
        (dolist (window windows)
          (with-selected-window (car window)
            (with-current-buffer buffer
              (apply #'fosgit-section-goto-successor (cdr window)))))
        (run-hooks 'fosgit-refresh-buffer-hook)
        (fosgit-section-update-highlight)
        (set-buffer-modified-p nil))
      (when fosgit-refresh-verbose
        (message "Refreshing buffer `%s'...done (%.3fs)" (buffer-name)
                 (float-time (time-subtract (current-time)
                                            fosgit-refresh-start-time)))))))

(defun fosgit-refresh-get-relative-position ()
  (-when-let (section (fosgit-current-section))
    (let ((start (fosgit-section-start section)))
      (list (count-lines start (point))
            (- (point) (line-beginning-position))
            (and (eq (fosgit-section-type section) 'hunk)
                 (region-active-p)
                 (progn (goto-char (line-beginning-position))
                        (when  (looking-at "^[-+]") (forward-line))
                        (while (looking-at "^[ @]") (forward-line))
                        (let ((beg (point)))
                          (cond ((looking-at "^[-+]")
                                 (forward-line)
                                 (while (looking-at "^[-+]") (forward-line))
                                 (while (looking-at "^ ")    (forward-line))
                                 (forward-line -1)
                                 (regexp-quote (buffer-substring-no-properties
                                                beg (line-end-position))))
                                (t t)))))))))

;;; Save File-Visiting Buffers

(defvar disable-fosgit-save-buffers nil)

(defun fosgit-pre-command-hook ()
  (setq disable-fosgit-save-buffers nil))
(add-hook 'pre-command-hook #'fosgit-pre-command-hook)

(defvar fosgit-after-save-refresh-buffers nil)

(defun fosgit-after-save-refresh-buffers ()
  (dolist (buffer fosgit-after-save-refresh-buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (fosgit-refresh-buffer))))
  (setq fosgit-after-save-refresh-buffers nil)
  (remove-hook 'post-command-hook 'fosgit-after-save-refresh-buffers))

(defun fosgit-after-save-refresh-status ()
  "Refresh the status buffer of the current repository.

This function is intended to be added to `after-save-hook'.

If the status buffer does not exist or the file being visited in
the current buffer isn't inside a repository, then do nothing.

Note that refreshing a Fosgit buffer is done by re-creating its
contents from scratch, which can be slow in large repositories.
If you are not satisfied with Fosgit's performance, then you
should obviously not add this function to that hook."
  (unless disable-fosgit-save-buffers
    (--when-let (ignore-errors (fosgit-mode-get-buffer 'fosgit-status-mode))
      (add-to-list 'fosgit-after-save-refresh-buffers it)
      (add-hook 'post-command-hook 'fosgit-after-save-refresh-buffers))))

(defun fosgit-maybe-save-repository-buffers ()
  "Maybe save file-visiting buffers belonging to the current repository.
Do so if `fosgit-save-repository-buffers' is non-nil.  You should
not remove this from any hooks, instead set that variable to nil
if you so desire."
  (when (and fosgit-save-repository-buffers
             (not disable-fosgit-save-buffers))
    (setq disable-fosgit-save-buffers t)
    (let ((msg (current-message)))
      (fosgit-save-repository-buffers
       (eq fosgit-save-repository-buffers 'dontask))
      (when (and msg (not (equal msg (current-message))))
        (message "%s" msg)))))

(add-hook 'fosgit-pre-refresh-hook #'fosgit-maybe-save-repository-buffers)
(add-hook 'fosgit-pre-call-git-hook #'fosgit-maybe-save-repository-buffers)
(add-hook 'fosgit-pre-start-git-hook #'fosgit-maybe-save-repository-buffers)

(defun fosgit-save-repository-buffers (&optional arg)
  "Save file-visiting buffers belonging to the current repository.
After any buffer where `buffer-save-without-query' is non-nil
is saved without asking, the user is asked about each modified
buffer which visits a file in the current repository.  Optional
argument (the prefix) non-nil means save all with no questions."
  (interactive "P")
  (-when-let (topdir (fosgit-rev-parse-safe "--show-toplevel"))
    (save-some-buffers
     arg (-partial (lambda (topdir)
                     (and buffer-file-name
                          ;; Avoid needlessly connecting to unrelated remotes.
                          (string-prefix-p topdir buffer-file-name)
                          (equal (fosgit-rev-parse-safe "--show-toplevel")
                                 topdir)))
                   topdir))))

;;; Restore Window Configuration

(defvar fosgit-inhibit-save-previous-winconf nil)

(defvar-local fosgit-previous-window-configuration nil)
(put 'fosgit-previous-window-configuration 'permanent-local t)

(defun fosgit-save-window-configuration ()
  "Save the current window configuration.

Later, when the buffer is buried, it may be restored by
`fosgit-restore-window-configuration'."
  (if fosgit-inhibit-save-previous-winconf
      (when (eq fosgit-inhibit-save-previous-winconf 'unset)
        (setq fosgit-previous-window-configuration nil))
    (unless (get-buffer-window (current-buffer) (selected-frame))
      (setq fosgit-previous-window-configuration
            (current-window-configuration)))))

(defun fosgit-restore-window-configuration (&optional kill-buffer)
  "Bury or kill the current buffer and restore previous window configuration."
  (let ((winconf fosgit-previous-window-configuration)
        (buffer (current-buffer))
        (frame (selected-frame)))
    (quit-window kill-buffer (selected-window))
    (when (and winconf (equal frame (window-configuration-frame winconf)))
      (set-window-configuration winconf)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq fosgit-previous-window-configuration nil))))))

;;; Buffer History

(defun fosgit-go-backward ()
  "Move backward in current buffer's history."
  (interactive)
  (if help-xref-stack
      (help-xref-go-back (current-buffer))
    (user-error "No previous entry in buffer's history")))

(defun fosgit-go-forward ()
  "Move forward in current buffer's history."
  (interactive)
  (if help-xref-forward-stack
      (help-xref-go-forward (current-buffer))
    (user-error "No next entry in buffer's history")))

(defun fosgit-insert-xref-buttons (&optional _)
  "Insert xref buttons."
  (when (or help-xref-stack help-xref-forward-stack)
    (when help-xref-stack
      (fosgit-xref-insert-button help-back-label 'fosgit-xref-backward))
    (when help-xref-forward-stack
      (when help-xref-stack
        (insert " "))
      (fosgit-xref-insert-button help-forward-label 'fosgit-xref-forward))))

(defun fosgit-xref-insert-button (label type)
  (fosgit-insert-section (button label)
    (insert-text-button label 'type type
                        'help-args (list (current-buffer)))))

(define-button-type 'fosgit-xref-backward
  :supertype 'help-back
  'mouse-face 'fosgit-section-highlight
  'help-echo (purecopy "mouse-2, RET: go back to previous history entry"))

(define-button-type 'fosgit-xref-forward
  :supertype 'help-forward
  'mouse-face 'fosgit-section-highlight
  'help-echo (purecopy "mouse-2, RET: go back to next history entry"))

(defun fosgit-xref-setup ()
  "Insert backward/forward buttons if the major-mode supports it.
Currently `fosgit-log-mode', `fosgit-reflog-mode',
`fosgit-diff-mode', and `fosgit-revision-mode' support it"
  (when (memq major-mode '(fosgit-log-mode
                           fosgit-reflog-mode
                           fosgit-diff-mode
                           fosgit-revision-mode))
    (when help-xref-stack-item
      (push (cons (point) help-xref-stack-item) help-xref-stack)
      (setq help-xref-forward-stack nil))
    (when (called-interactively-p 'interactive)
      (--when-let (nthcdr 10 help-xref-stack)
        (setcdr it nil)))
    (setq help-xref-stack-item
          `(fosgit-xref-restore ,default-directory ,@fosgit-refresh-args))))

(defun fosgit-xref-restore (&rest args)
  (fosgit-xref-setup)
  (setq default-directory  (car args))
  (setq fosgit-refresh-args (cdr args))
  (fosgit-refresh-buffer))

;;; Utilities

(defun fosgit-run-hook-with-benchmark (hook)
  (when hook
    (if fosgit-refresh-verbose
        (let ((start (current-time)))
          (message "Running %s..." hook)
          (run-hooks hook)
          (message "Running %s...done (%.3fs)" hook
                   (float-time (time-subtract (current-time) start))))
      (run-hooks hook))))

;;; fosgit-mode.el ends soon
(provide 'fosgit-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-mode.el ends here
