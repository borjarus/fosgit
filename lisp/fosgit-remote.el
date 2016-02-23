;;; fosgit-remote.el --- transfer Git commits  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2016  The Magit Project Contributors
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

;; This library implements support for interacting with remote
;; repositories.  Commands for cloning, fetching, pulling, and
;; pushing are defined here.

;;; Code:

(require 'fosgit)

;;; Clone

(defcustom fosgit-clone-set-remote-head nil
  "Whether cloning creates the symbolic-ref `<remote>/HEAD'."
  :package-version '(fosgit . "2.4.2")
  :group 'fosgit-commands
  :type 'boolean)

(defcustom fosgit-clone-set-remote.pushDefault 'ask
  "Whether to set the value of `remote.pushDefault' after cloning.

If t, then set without asking.  If nil, then don't set.  If
`ask', then ask."
  :package-version '(fosgit . "2.4.0")
  :group 'fosgit-commands
  :type '(choice (const :tag "set" t)
                 (const :tag "ask" ask)
                 (const :tag "don't set" nil)))

;;;###autoload
(defun fosgit-clone (repository directory)
  "Clone the REPOSITORY to DIRECTORY.
Then show the status buffer for the new repository."
  (interactive
   (let  ((url (fosgit-read-string-ns "Clone repository")))
     (list url (read-directory-name
                "Clone to: " nil nil nil
                (and (string-match "\\([^./]+\\)\\(\\.git\\)?$" url)
                     (match-string 1 url))))))
  (setq directory (file-name-as-directory (expand-file-name directory)))
  (message "Cloning %s..." repository)
  (when (= (fosgit-call-git "clone" repository
                           ;; Stop cygwin git making a "c:" directory.
                           (fosgit-convert-git-filename directory))
           0)
    (let ((default-directory directory))
      (when (or (eq  fosgit-clone-set-remote.pushDefault t)
                (and fosgit-clone-set-remote.pushDefault
                     (y-or-n-p "Set `remote.pushDefault' to \"origin\"? ")))
        (fosgit-call-git "config" "remote.pushDefault" "origin"))
      (unless fosgit-clone-set-remote-head
        (fosgit-remote-unset-head "origin")))
    (message "Cloning %s...done" repository)
    (fosgit-status-internal directory)))

;;; Setup

(defcustom fosgit-remote-add-set-remote.pushDefault 'ask-if-unset
  "Whether to set the value of `remote.pushDefault' after adding a remote.

If `ask', then always ask.  If `ask-if-unset', then ask, but only
if the variable isn't set already.  If nil, then don't ever set.
If the value is a string, then set without asking, provided the
name of the name of the added remote is equal to that string and
the variable isn't already set."
  :package-version '(fosgit . "2.4.0")
  :group 'fosgit-commands
  :type '(choice (const  :tag "ask if unset" ask-if-unset)
                 (const  :tag "always ask" ask)
                 (string :tag "set if named")
                 (const  :tag "don't set")))

;;;###autoload (autoload 'fosgit-remote-popup "fosgit-remote" nil t)
(fosgit-define-popup fosgit-remote-popup
  "Popup console for remote commands."
  'fosgit-commands nil nil
  :man-page "git-remote"
  :actions  '((?a "Add"     fosgit-remote-add)
              (?r "Rename"  fosgit-remote-rename)
              (?k "Remove"  fosgit-remote-remove)
              (?u "Set url" fosgit-remote-set-url)))

(defun fosgit-read-url (prompt &optional initial-input)
  (let ((url (fosgit-read-string-ns prompt initial-input)))
    (if (string-prefix-p "~" url)
        (expand-file-name url)
      url)))

;;;###autoload
(defun fosgit-remote-add (remote url)
  "Add a remote named REMOTE and fetch it."
  (interactive (list (fosgit-read-string-ns "Remote name")
                     (fosgit-read-url "Remote url")))
  (if (pcase (list fosgit-remote-add-set-remote.pushDefault
                   (fosgit-get "remote.defaultPush"))
        (`(,(pred stringp) ,_) t)
        ((or `(ask ,_) `(ask-if-unset nil))
         (y-or-n-p (format "Set `remote.pushDefault' to \"%s\"? " remote))))
      (progn (fosgit-call-git "remote" "add" "-f" remote url)
             (fosgit-call-git "config" "remote.pushDefault" remote)
             (fosgit-refresh))
    (fosgit-run-git-async "remote" "add" "-f" remote url)))

;;;###autoload
(defun fosgit-remote-rename (old new)
  "Rename the remote named OLD to NEW."
  (interactive
   (let  ((remote (fosgit-read-remote "Rename remote")))
     (list remote (fosgit-read-string-ns (format "Rename %s to" remote)))))
  (unless (string= old new)
    (fosgit-run-git "remote" "rename" old new)))

;;;###autoload
(defun fosgit-remote-set-url (remote url)
  "Change the url of the remote named REMOTE to URL."
  (interactive
   (let  ((remote (fosgit-read-remote "Set url of remote")))
     (list remote (fosgit-read-url
                   "Url" (fosgit-get "remote" remote "url")))))
  (fosgit-run-git "remote" "set-url" remote url))

;;;###autoload
(defun fosgit-remote-remove (remote)
  "Delete the remote named REMOTE."
  (interactive (list (fosgit-read-remote "Delete remote")))
  (fosgit-run-git "remote" "rm" remote))

;;;###autoload
(defun fosgit-remote-set-head (remote &optional branch)
  "Set the local representation of REMOTE's default branch.
Query REMOTE and set the symbolic-ref refs/remotes/<remote>/HEAD
accordingly.  With a prefix argument query for the branch to be
used, which allows you to select an incorrect value if you fancy
doing that."
  (interactive
   (let  ((remote (fosgit-read-remote "Set HEAD for remote")))
     (list remote
           (and current-prefix-arg
                (fosgit-read-remote-branch (format "Set %s/HEAD to" remote)
                                          remote nil nil t)))))
  (fosgit-run-git "remote" "set-head" remote (or branch "--auto")))

;;;###autoload
(defun fosgit-remote-unset-head (remote)
  "Unset the local representation of REMOTE's default branch.
Delete the symbolic-ref \"refs/remotes/<remote>/HEAD\"."
  (interactive (list (fosgit-read-remote "Unset HEAD for remote")))
  (fosgit-run-git "remote" "set-head" remote "--delete"))

;;; Fetch

;;;###autoload (autoload 'fosgit-fetch-popup "fosgit-remote" nil t)
(fosgit-define-popup fosgit-fetch-popup
  "Popup console for fetch commands."
  'fosgit-commands
  :man-page "git-fetch"
  :switches '((?p "Prune deleted branches" "--prune"))
  :actions  '("Fetch from"
              (?p fosgit-get-push-remote    fosgit-fetch-from-pushremote)
              (?u fosgit-get-remote         fosgit-fetch-from-upstream)
              (?e "elsewhere"              fosgit-fetch)
              (?a "all remotes"            fosgit-fetch-all)
              "Fetch"
              (?m "submodules"             fosgit-submodule-fetch))
  :default-action 'fosgit-fetch
  :max-action-columns 1)

(defun fosgit-git-fetch (remote args)
  (run-hooks 'fosgit-credential-hook)
  (fosgit-run-git-async "fetch" remote args))

;;;###autoload
(defun fosgit-fetch-from-pushremote (args)
  "Fetch from the push-remote of the current branch."
  (interactive (list (fosgit-fetch-arguments)))
  (--if-let (fosgit-get-push-remote)
      (fosgit-git-fetch it args)
    (--if-let (fosgit-get-current-branch)
        (user-error "No push-remote is configured for %s" it)
      (user-error "No branch is checked out"))))

;;;###autoload
(defun fosgit-fetch-from-upstream (args)
  "Fetch from the upstream repository of the current branch."
  (interactive (list (fosgit-fetch-arguments)))
  (--if-let (fosgit-get-remote)
      (fosgit-git-fetch it args)
    (--if-let (fosgit-get-current-branch)
        (user-error "No upstream is configured for %s" it)
      (user-error "No branch is checked out"))))

;;;###autoload
(defun fosgit-fetch (remote args)
  "Fetch from another repository."
  (interactive (list (fosgit-read-remote "Fetch remote")
                     (fosgit-fetch-arguments)))
  (fosgit-git-fetch remote args))

;;;###autoload
(defun fosgit-fetch-all (args)
  "Fetch from all remotes."
  (interactive (list (fosgit-fetch-arguments)))
  (run-hooks 'fosgit-credential-hook)
  (fosgit-run-git-async "remote" "update" args))

;;;###autoload
(defun fosgit-fetch-all-prune ()
  "Fetch from all remotes, and prune.
Prune remote tracking branches for branches that have been
removed on the respective remote."
  (interactive)
  (run-hooks 'fosgit-credential-hook)
  (fosgit-run-git-async "remote" "update" "--prune"))

;;;###autoload
(defun fosgit-fetch-all-no-prune ()
  "Fetch from all remotes."
  (interactive)
  (run-hooks 'fosgit-credential-hook)
  (fosgit-run-git-async "remote" "update"))

;;; Pull

;;;###autoload (autoload 'fosgit-pull-popup "fosgit-remote" nil t)
(fosgit-define-popup fosgit-pull-popup
  "Popup console for pull commands."
  'fosgit-commands
  :man-page "git-pull"
  :variables '("Variables"
               (?r "branch.%s.rebase"
                   fosgit-cycle-branch*rebase
                   fosgit-pull-format-branch*rebase))
  :actions '((lambda ()
               (--if-let (fosgit-get-current-branch)
                   (concat
                    (propertize "Pull into " 'face 'fosgit-popup-heading)
                    (propertize it           'face 'fosgit-branch-local)
                    (propertize " from"      'face 'fosgit-popup-heading))
                 (propertize "Pull from" 'face 'fosgit-popup-heading)))
             (?p fosgit-get-push-branch     fosgit-pull-from-pushremote)
             (?u fosgit-get-upstream-branch fosgit-pull-from-upstream)
             (?e "elsewhere"               fosgit-pull))
  :default-action 'fosgit-pull
  :max-action-columns 1)

;;;###autoload (autoload 'fosgit-pull-and-fetch-popup "fosgit-remote" nil t)
(fosgit-define-popup fosgit-pull-and-fetch-popup
  "Popup console for pull and fetch commands.

This popup is intended as a replacement for the separate popups
`fosgit-pull-popup' and `fosgit-fetch-popup'.  To use it, add this
to your init file:

  (with-eval-after-load \\='fosgit-remote
    (define-key fosgit-mode-map \"f\" \\='fosgit-pull-and-fetch-popup)
    (define-key fosgit-mode-map \"F\" nil))

The combined popup does not offer all commands and arguments
available from the individual popups.  Instead of the argument
`--prune' and the command `fosgit-fetch-all' it uses two commands
`fosgit-fetch-prune' and `fosgit-fetch-no-prune'.  And the commands
`fosgit-fetch-from-pushremote' and `fosgit-fetch-from-upstream' are
missing.  To add them use something like:

  (with-eval-after-load \\='fosgit-remote
    (fosgit-define-popup-action \\='fosgit-pull-and-fetch-popup ?U
      \\='fosgit-get-upstream-branch
      \\='fosgit-fetch-from-upstream-remote ?F)
    (fosgit-define-popup-action \\='fosgit-pull-and-fetch-popup ?P
      \\='fosgit-get-push-branch
      \\='fosgit-fetch-from-push-remote ?F))"
  'fosgit-commands
  :man-page "git-pull"
  :variables '("Pull variables"
               (?r "branch.%s.rebase"
                   fosgit-cycle-branch*rebase
                   fosgit-pull-format-branch*rebase))
  :actions '((lambda ()
               (--if-let (fosgit-get-current-branch)
                   (concat
                    (propertize "Pull into " 'face 'fosgit-popup-heading)
                    (propertize it           'face 'fosgit-branch-local)
                    (propertize " from"      'face 'fosgit-popup-heading))
                 (propertize "Pull from" 'face 'fosgit-popup-heading)))
             (?p fosgit-get-push-branch     fosgit-pull-from-pushremote)
             (?u fosgit-get-upstream-branch fosgit-pull-from-upstream)
             (?e "elsewhere"               fosgit-pull)
             "Fetch from"
             (?f "remotes"           fosgit-fetch-all-no-prune)
             (?F "remotes and prune" fosgit-fetch-all-prune)
             "Fetch"
             (?m "submodules"        fosgit-submodule-fetch))
  :default-action 'fosgit-fetch
  :max-action-columns 1)

(defun fosgit-pull-format-branch*rebase ()
  (fosgit-popup-format-variable (format "branch.%s.rebase"
                                       (or (fosgit-get-current-branch) "<name>"))
                               '("true" "false")
                               "false" "pull.rebase"))

(defun fosgit-git-pull (source args)
  (run-hooks 'fosgit-credential-hook)
  (-let [(remote . branch)
         (fosgit-split-branch-name source)]
    (fosgit-run-git-with-editor "pull" args remote branch)))

;;;###autoload
(defun fosgit-pull-from-pushremote (args)
  "Pull from the push-remote of the current branch."
  (interactive (list (fosgit-pull-arguments)))
  (--if-let (fosgit-get-push-branch)
      (fosgit-git-pull it args)
    (--if-let (fosgit-get-current-branch)
        (user-error "No push-remote is configured for %s" it)
      (user-error "No branch is checked out"))))

;;;###autoload
(defun fosgit-pull-from-upstream (args)
  "Pull from the upstream of the current branch."
  (interactive (list (fosgit-pull-arguments)))
  (--if-let (fosgit-get-upstream-branch)
      (fosgit-git-pull it args)
    (--if-let (fosgit-get-current-branch)
        (user-error "No upstream is configured for %s" it)
      (user-error "No branch is checked out"))))

;;;###autoload
(defun fosgit-pull (source args)
  "Pull from a branch read in the minibuffer."
  (interactive (list (fosgit-read-remote-branch "Pull" nil nil nil t)
                     (fosgit-pull-arguments)))
  (fosgit-git-pull source args))

;;; Push

(defcustom fosgit-push-current-set-remote-if-missing t
  "Whether to configure missing remotes before pushing.

When nil, then the command `fosgit-push-current-to-pushremote' and
`fosgit-push-current-to-upstream' do not appear in the push popup
if the push-remote resp. upstream is not configured.  If the user
invokes one of these commands anyway, then it raises an error.

When non-nil, then these commands always appear in the push
popup.  But if the required configuration is missing, then they
do appear in a way that indicates that this is the case.  If the
user invokes one of them, then it asks for the necessary
configuration, stores the configuration, and then uses it to push
a first time.

This option also affects whether the argument `--set-upstream' is
available in the popup.  If the value is t, then that argument is
redundant.  But note that changing the value of this option does
not take affect immediately, the argument will only be added or
removed after restarting Emacs."
  :package-version '(fosgit . "2.4.0")
  :group 'fosgit-commands
  :type 'boolean)

;;;###autoload (autoload 'fosgit-push-popup "fosgit-remote" nil t)
(fosgit-define-popup fosgit-push-popup
  "Popup console for push commands."
  'fosgit-commands
  :man-page "git-push"
  :switches `((?f "Force"         "--force-with-lease")
              (?h "Disable hooks" "--no-verify")
              (?d "Dry run"       "--dry-run")
              ,@(and (not fosgit-push-current-set-remote-if-missing)
                     '((?u "Set upstream"  "--set-upstream"))))
  :actions '((lambda ()
               (--when-let (fosgit-get-current-branch)
                 (concat (propertize "Push " 'face 'fosgit-popup-heading)
                         (propertize it      'face 'fosgit-branch-local)
                         (propertize " to"   'face 'fosgit-popup-heading))))
             (?p fosgit--push-current-to-pushremote-desc
                 fosgit-push-current-to-pushremote)
             (?u fosgit--push-current-to-upstream-desc
                 fosgit-push-current-to-upstream)
             (?e "elsewhere\n"       fosgit-push-current)
             "Push"
             (?o "another branch"    fosgit-push)
             (?T "a tag"             fosgit-push-tag)
             (?m "matching branches" fosgit-push-matching)
             (?t "all tags"          fosgit-push-tags))
  :max-action-columns 2)

(defun fosgit-git-push (branch target args)
  (run-hooks 'fosgit-credential-hook)
  (-let [(remote . target)
         (fosgit-split-branch-name target)]
    (fosgit-run-git-async "push" "-v" args remote
                         (format "%s:refs/heads/%s" branch target))))

;;;###autoload
(defun fosgit-push-current-to-pushremote (args &optional push-remote)
  "Push the current branch to `branch.<name>.pushRemote'.
If that variable is unset, then push to `remote.pushDefault'.

When `fosgit-push-current-set-remote-if-missing' is non-nil and
the push-remote is not configured, then read the push-remote from
the user, set it, and then push to it.  With a prefix argument
the push-remote can be changed before pushed to it."
  (interactive
   (list (fosgit-push-arguments)
         (and (fosgit--push-current-set-pushremote-p current-prefix-arg)
              (fosgit-read-remote (format "Set push-remote of %s and push there"
                                         (fosgit-get-current-branch))))))
  (--if-let (fosgit-get-current-branch)
      (progn (when push-remote
               (fosgit-call-git "config"
                               (format "branch.%s.pushRemote"
                                       (fosgit-get-current-branch))
                               push-remote))
             (-if-let (remote (fosgit-get-push-remote it))
                 (if (member remote (fosgit-list-remotes))
                     (fosgit-git-push it (concat remote "/" it) args)
                   (user-error "Remote `%s' doesn't exist" remote))
               (user-error "No push-remote is configured for %s" it)))
    (user-error "No branch is checked out")))

(defun fosgit--push-current-set-pushremote-p (&optional change)
  (and (or change
           (and fosgit-push-current-set-remote-if-missing
                (not (fosgit-get-push-remote))))
       (fosgit-get-current-branch)))

(defun fosgit--push-current-to-pushremote-desc ()
  (--if-let (fosgit-get-push-branch)
      (concat (fosgit-branch-set-face it) "\n")
    (and (fosgit--push-current-set-pushremote-p)
         (concat (propertize "pushRemote" 'face 'bold)
                 ", after setting that\n"))))

;;;###autoload
(defun fosgit-push-current-to-upstream (args &optional upstream)
  "Push the current branch to its upstream branch.

When `fosgit-push-current-set-remote-if-missing' is non-nil and
the upstream is not configured, then read the upstream from the
user, set it, and then push to it.  With a prefix argument the
upstream can be changed before pushed to it."
  (interactive
   (list (fosgit-push-arguments)
         (and (fosgit--push-current-set-upstream-p current-prefix-arg)
              (fosgit-read-upstream-branch))))
  (--if-let (fosgit-get-current-branch)
      (progn
        (when upstream
          (fosgit-set-branch*merge/remote it upstream))
        (-if-let (target (fosgit-get-upstream-branch it))
            (fosgit-git-push it target args)
          (user-error "No upstream is configured for %s" it)))
    (user-error "No branch is checked out")))

(defun fosgit--push-current-set-upstream-p (&optional change)
  (and (or change
           (and fosgit-push-current-set-remote-if-missing
                (not (fosgit-get-upstream-branch))))
       (fosgit-get-current-branch)))

(defun fosgit--push-current-to-upstream-desc ()
  (--if-let (fosgit-get-upstream-branch)
      (concat (fosgit-branch-set-face it) "\n")
    (and (fosgit--push-current-set-upstream-p)
         (concat (propertize "@{upstream}" 'face 'bold)
                 ", after setting that\n"))))

;;;###autoload
(defun fosgit-push-current (target args)
  "Push the current branch to a branch read in the minibuffer."
  (interactive
   (--if-let (fosgit-get-current-branch)
       (list (fosgit-read-remote-branch (format "Push %s to" it)
                                       nil nil it 'confirm)
             (fosgit-push-arguments))
     (user-error "No branch is checked out")))
  (fosgit-git-push (fosgit-get-current-branch) target args))

;;;###autoload
(defun fosgit-push (source target args)
  "Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer."
  (interactive
   (let ((source (fosgit-read-local-branch-or-commit "Push")))
     (list source
           (fosgit-read-remote-branch (format "Push %s to" source) nil
                                     (fosgit-get-upstream-branch source)
                                     source 'confirm)
           (fosgit-push-arguments))))
  (fosgit-git-push source target args))

;;;###autoload
(defun fosgit-push-matching (remote &optional args)
  "Push all matching branches to another repository.
If multiple remotes exist, then read one from the user.
If just one exists, use that without requiring confirmation."
  (interactive (list (fosgit-read-remote "Push matching branches to" nil t)
                     (fosgit-push-arguments)))
  (run-hooks 'fosgit-credential-hook)
  (fosgit-run-git-async "push" "-v" args remote ":"))

;;;###autoload
(defun fosgit-push-tags (remote &optional args)
  "Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default."
  (interactive (list (fosgit-read-remote "Push tags to remote" nil t)
                     (fosgit-push-arguments)))
  (run-hooks 'fosgit-credential-hook)
  (fosgit-run-git-async "push" remote "--tags" args))

;;;###autoload
(defun fosgit-push-tag (tag remote &optional args)
  "Push a tag to another repository."
  (interactive
   (let  ((tag (fosgit-read-tag "Push tag")))
     (list tag (fosgit-read-remote (format "Push %s to remote" tag) nil t)
           (fosgit-push-arguments))))
  (run-hooks 'fosgit-credential-hook)
  (fosgit-run-git-async "push" remote tag args))

;;;###autoload
(defun fosgit-push-implicitly (args)
  "Push somewhere without using an explicit refspec.

This command simply runs \"git push -v [ARGS]\".  ARGS are the
arguments specified in the popup buffer.  No explicit refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

To add this command to the push popup add this to your init file:

  (with-eval-after-load \\='fosgit-remote
    (fosgit-define-popup-action \\='fosgit-push-popup ?P
      'fosgit-push-implicitly--desc
      'fosgit-push-implicitly ?p t))

The function `fosgit-push-implicitly--desc' attempts to predict
what this command will do, the value it returns is displayed in
the popup buffer."
  (interactive (list (fosgit-push-arguments)))
  (run-hooks 'fosgit-credential-hook)
  (fosgit-run-git-async "push" "-v" args))

(defun fosgit-push-implicitly--desc ()
  (let ((default (fosgit-get "push.default")))
    (unless (equal default "nothing")
      (or (-when-let* ((remote (or (fosgit-get-remote)
                                   (fosgit-remote-p "origin")))
                       (refspec (fosgit-get "remote" remote "push")))
            (format "%s using %s"
                    (propertize remote  'face 'fosgit-branch-remote)
                    (propertize refspec 'face 'bold)))
          (--when-let (and (not (fosgit-get-push-branch))
                           (fosgit-get-upstream-branch))
            (format "%s aka %s\n"
                    (fosgit-branch-set-face it)
                    (propertize "@{upstream}" 'face 'bold)))
          (--when-let (fosgit-get-push-branch)
            (format "%s aka %s\n"
                    (fosgit-branch-set-face it)
                    (propertize "pushRemote" 'face 'bold)))
          (--when-let (fosgit-get-@{push}-branch)
            (format "%s aka %s\n"
                    (fosgit-branch-set-face it)
                    (propertize "@{push}" 'face 'bold)))
          (format "using %s (%s is %s)\n"
                  (propertize "git push"     'face 'bold)
                  (propertize "push.default" 'face 'bold)
                  (propertize default        'face 'bold))))))

;;;###autoload
(defun fosgit-push-to-remote (remote args)
  "Push to REMOTE without using an explicit refspec.
The REMOTE is read in the minibuffer.

This command simply runs \"git push -v [ARGS] REMOTE\".  ARGS
are the arguments specified in the popup buffer.  No refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

To add this command to the push popup add this to your init file:

  (with-eval-after-load \\='fosgit-remote
    (fosgit-define-popup-action \\='fosgit-push-popup ?r
      'fosgit-push-to-remote--desc
      'fosgit-push-to-remote ?p t))"
  (interactive (list (fosgit-read-remote "Push to remote")
                     (fosgit-push-arguments)))
  (run-hooks 'fosgit-credential-hook)
  (fosgit-run-git-async "push" "-v" args remote))

(defun fosgit-push-to-remote--desc ()
  (format "using %s\n" (propertize "git push <remote>" 'face 'bold)))

;;; Email

;;;###autoload (autoload 'fosgit-patch-popup "fosgit-remote" nil t)
(fosgit-define-popup fosgit-patch-popup
  "Popup console for patch commands."
  'fosgit-commands
  :man-page "git-format-patch"
  :switches '("Switches for formatting patches"
              (?l "Add cover letter" "--cover-letter"))
  :options  '("Options for formatting patches"
              (?f "From"             "--from=")
              (?t "To"               "--to=")
              (?c "CC"               "--cc=")
              (?r "In reply to"      "--in-reply-to=")
              (?v "Reroll count"     "--reroll-count=")
              (?s "Thread style"     "--thread=")
              (?U "Context lines"    "-U")
              (?M "Detect renames"   "-M")
              (?C "Detect copies"    "-C")
              (?A "Diff algorithm"   "--diff-algorithm="
                  fosgit-diff-select-algorithm)
              (?o "Output directory" "--output-directory="))
  :actions  '((?p "Format patches"   fosgit-format-patch)
              (?r "Request pull"     fosgit-request-pull))
  :default-action 'fosgit-format-patch)

;;;###autoload
(defun fosgit-format-patch (range args)
  "Create patches for the commits in RANGE.
When a single commit is given for RANGE, create a patch for the
changes introduced by that commit (unlike 'git format-patch'
which creates patches for all commits that are reachable from
HEAD but not from the specified commit)."
  (interactive
   (list (-if-let (revs (fosgit-region-values 'commit))
             (concat (car (last revs)) "^.." (car revs))
           (let ((range (fosgit-read-range-or-commit "Format range or commit")))
             (if (string-match-p "\\.\\." range)
                 range
               (format "%s~..%s" range range))))
         (fosgit-patch-arguments)))
  (fosgit-call-git "format-patch" range args)
  (when (member "--cover-letter" args)
    (find-file
     (expand-file-name
      "0000-cover-letter.patch"
      (let ((topdir (fosgit-toplevel)))
        (or (--some (and (string-match "--output-directory=\\(.+\\)" it)
                         (expand-file-name (match-string 1 it) topdir))
                    args)
            topdir))))))

;;;###autoload
(defun fosgit-request-pull (url start end)
  "Request upstream to pull from you public repository.

URL is the url of your publically accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit."
  (interactive
   (list (fosgit-get "remote" (fosgit-read-remote "Remote") "url")
         (fosgit-read-branch-or-commit "Start" (fosgit-get-upstream-branch))
         (fosgit-read-branch-or-commit "End")))
  (let ((dir default-directory))
    ;; mu4e changes default-directory
    (compose-mail)
    (setq default-directory dir))
  (message-goto-body)
  (fosgit-git-insert "request-pull" start url end)
  (set-buffer-modified-p nil))

;;; fosgit-remote.el ends soon
(provide 'fosgit-remote)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-remote.el ends here
