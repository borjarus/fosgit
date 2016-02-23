;;; fosgit-core.el --- core functionality  -*- lexical-binding: t -*-

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

;; This library requires several other libraries, so that yet other
;; libraries can just require this one, instead of having to require
;; all the other ones.  In other words this separates the low-level
;; stuff from the rest.  It also defines some Custom groups.

;;; Code:

(require 'fosgit-utils)
(require 'fosgit-section)
(require 'fosgit-git)
(require 'fosgit-mode)
(require 'fosgit-popup)
(require 'fosgit-process)
(require 'fosgit-autorevert)

(defgroup fosgit nil
  "Controlling Git from Emacs."
  :group 'tools)

(defgroup fosgit-commands nil
  "Options controlling behavior of certain commands."
  :group 'fosgit)

(defgroup fosgit-modes nil
  "Modes used or provided by Fosgit."
  :group 'fosgit)

(defgroup fosgit-extensions nil
  "Extensions to Fosgit."
  :group 'fosgit)

(defgroup fosgit-faces nil
  "Faces used by Fosgit."
  :group 'fosgit
  :group 'faces)

(custom-add-to-group 'fosgit-modes   'fosgit-popup       'custom-group)
(custom-add-to-group 'fosgit-faces   'fosgit-popup-faces 'custom-group)
(custom-add-to-group 'fosgit-modes   'git-commit        'custom-group)
(custom-add-to-group 'fosgit-faces   'git-commit-faces  'custom-group)
(custom-add-to-group 'fosgit-modes   'git-rebase        'custom-group)
(custom-add-to-group 'fosgit-faces   'git-rebase-faces  'custom-group)
(custom-add-to-group 'fosgit-process 'with-editor       'custom-group)

(custom-add-to-group 'fosgit 'vc-follow-symlinks 'custom-variable)

;;; fosgit-core.el ends soon
(provide 'fosgit-core)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-core.el ends here
