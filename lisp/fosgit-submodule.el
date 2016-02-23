;;; fosgit-submodule.el --- submodule support for Fosgit  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2015  The Magit Project Contributors
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

(require 'fosgit)

;;; Commands

;;;###autoload (autoload 'fosgit-submodule-popup "fosgit-submodule" nil t)
(fosgit-define-popup fosgit-submodule-popup
  "Popup console for submodule commands."
  'fosgit-commands nil nil
  :man-page "git-submodule"
  :actions  '((?a "Add"    fosgit-submodule-add)
              (?b "Setup"  fosgit-submodule-setup)
              (?i "Init"   fosgit-submodule-init)
              (?u "Update" fosgit-submodule-update)
              (?s "Sync"   fosgit-submodule-sync)
              (?f "Fetch"  fosgit-submodule-fetch)
              (?d "Deinit" fosgit-submodule-deinit)))

;;;###autoload
(defun fosgit-submodule-add (url &optional path)
  "Add the repository at URL as a submodule.
Optional PATH is the path to the submodule relative to the root
of the superproject. If it is nil then the path is determined
based on URL."
  (interactive
   (fosgit-with-toplevel
     (let ((path (read-file-name
                  "Add submodule: " nil nil nil
                  (fosgit-section-when [file untracked]
                    (directory-file-name (fosgit-section-value it))))))
       (when path
         (setq path (file-name-as-directory (expand-file-name path)))
         (when (member path (list "" default-directory))
           (setq path nil)))
       (list (fosgit-read-string-ns
              "Remote url"
              (and path (fosgit-git-repo-p path t)
                   (let ((default-directory path))
                     (fosgit-get "remote" (or (fosgit-get-remote) "origin")
                                "url"))))
             (and path (directory-file-name (file-relative-name path)))))))
  (fosgit-run-git "submodule" "add" url path))

;;;###autoload
(defun fosgit-submodule-setup ()
  "Clone and register missing submodules and checkout appropriate commits."
  (interactive)
  (fosgit-submodule-update t))

;;;###autoload
(defun fosgit-submodule-init ()
  "Register submodules listed in \".gitmodules\" into \".git/config\"."
  (interactive)
  (fosgit-with-toplevel
    (fosgit-run-git-async "submodule" "init")))

;;;###autoload
(defun fosgit-submodule-update (&optional init)
  "Clone missing submodules and checkout appropriate commits.
With a prefix argument also register submodules in \".git/config\"."
  (interactive "P")
  (fosgit-with-toplevel
    (fosgit-run-git-async "submodule" "update" (and init "--init"))))

;;;###autoload
(defun fosgit-submodule-sync ()
  "Update each submodule's remote URL according to \".gitmodules\"."
  (interactive)
  (fosgit-with-toplevel
    (fosgit-run-git-async "submodule" "sync")))

;;;###autoload
(defun fosgit-submodule-fetch (&optional all)
  "Fetch all submodules.
With a prefix argument fetch all remotes."
  (interactive "P")
  (fosgit-with-toplevel
    (fosgit-run-git-async "submodule" "foreach"
                         (format "git fetch %s || true" (if all "--all" "")))))

;;;###autoload
(defun fosgit-submodule-deinit (path)
  "Unregister the submodule at PATH."
  (interactive
   (list (fosgit-completing-read "Deinit module" (fosgit-get-submodules)
                                nil t nil nil (fosgit-section-when module))))
  (fosgit-with-toplevel
    (fosgit-run-git-async "submodule" "deinit" path)))

;;; Sections

;;;###autoload
(defun fosgit-insert-submodule-commits (section range)
  "For internal use, don't add to a hook."
  (if (fosgit-section-hidden section)
      (setf (fosgit-section-washer section)
            (apply-partially #'fosgit-insert-submodule-commits section range))
    (fosgit-git-wash (apply-partially 'fosgit-log-wash-log 'module)
      "log" "--oneline" range)
    (when (> (point) (fosgit-section-content section))
      (delete-char -1))))

;;;###autoload
(defun fosgit-insert-unpulled-module-commits ()
  "Insert sections for all submodules with unpulled commits.
These sections can be expanded to show the respective commits."
  (-when-let (modules (fosgit-get-submodules))
    (fosgit-insert-section section (unpulled-modules)
      (fosgit-insert-heading "Unpulled modules:")
      (fosgit-with-toplevel
        (dolist (module modules)
          (let ((default-directory
                  (expand-file-name (file-name-as-directory module))))
            (-when-let (tracked (fosgit-get-upstream-ref))
              (fosgit-insert-section sec (file module t)
                (fosgit-insert-heading
                  (concat (propertize module 'face 'fosgit-diff-file-heading) ":"))
                (fosgit-insert-submodule-commits
                 section (concat "HEAD.." tracked)))))))
      (if (> (point) (fosgit-section-content section))
          (insert ?\n)
        (fosgit-cancel-section)))))

;;;###autoload
(defun fosgit-insert-unpushed-module-commits ()
  "Insert sections for all submodules with unpushed commits.
These sections can be expanded to show the respective commits."
  (-when-let (modules (fosgit-get-submodules))
    (fosgit-insert-section section (unpushed-modules)
      (fosgit-insert-heading "Unpushed modules:")
      (fosgit-with-toplevel
        (dolist (module modules)
          (let ((default-directory
                  (expand-file-name (file-name-as-directory module))))
            (-when-let (tracked (fosgit-get-upstream-ref))
              (fosgit-insert-section sec (file module t)
                (fosgit-insert-heading
                  (concat (propertize module 'face 'fosgit-diff-file-heading) ":"))
                (fosgit-insert-submodule-commits
                 section (concat tracked "..HEAD")))))))
      (if (> (point) (fosgit-section-content section))
          (insert ?\n)
        (fosgit-cancel-section)))))

;;; fosgit-submodule.el ends soon
(provide 'fosgit-submodule)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-submodule.el ends here
