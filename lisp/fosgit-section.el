;;; fosgit-section.el --- section functionality  -*- lexical-binding: t -*-

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

;; This library implements "sections" as used in all Fosgit buffers.
;; If you have used Fosgit before then you probably know what that
;; means, otherwise think "read-only Org-Mode for Git", kinda.

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'fosgit-utils)

(defvar fosgit-keep-region-overlay)

;;; Options

(defgroup fosgit-section nil
  "Expandable sections."
  :group 'fosgit)

(defcustom fosgit-section-show-child-count t
  "Whether to append the number of children to section headings."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-section
  :type 'boolean)

(defcustom fosgit-section-movement-hook
  '(fosgit-hunk-set-window-start
    fosgit-log-maybe-update-revision-buffer
    fosgit-log-maybe-show-more-commits)
  "Hook run by `fosgit-section-goto'.
That function in turn is used by all section movement commands."
  :package-version '(fosgit . "2.3.0")
  :group 'fosgit-section
  :type 'hook
  :options '(fosgit-hunk-set-window-start
             fosgit-status-maybe-update-revision-buffer
             fosgit-status-maybe-update-blob-buffer
             fosgit-log-maybe-update-revision-buffer
             fosgit-log-maybe-update-blob-buffer
             fosgit-log-maybe-show-more-commits))

(defcustom fosgit-section-highlight-hook
  '(fosgit-diff-highlight
    fosgit-section-highlight
    fosgit-section-highlight-selection)
  "Functions used to highlight the current section.
Each function is run with the current section as only argument
until one of them returns non-nil."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-section
  :type 'hook
  :options '(fosgit-diff-highlight
             fosgit-section-highlight
             fosgit-section-highlight-selection))

(defcustom fosgit-section-unhighlight-hook
  '(fosgit-diff-unhighlight)
  "Functions used to unhighlight the previously current section.
Each function is run with the current section as only argument
until one of them returns non-nil.  Most sections are properly
unhighlighted without requiring a specialized unhighlighter,
diff-related sections being the only exception."
  :package-version '(fosgit . "2.1.0")
  :group 'fosgit-section
  :type 'hook
  :options '(fosgit-diff-unhighlight))

(defcustom fosgit-section-set-visibility-hook
  '(fosgit-diff-expansion-threshold
    fosgit-section-set-visibility-from-cache)
  "Hook used to set the initial visibility of a section.
Stop at the first function that returns non-nil.  The value
should be `show' or `hide'.  If no function returns non-nil
determine the visibility as usual, i.e. use the hardcoded
section specific default (see `fosgit-insert-section')."
  :package-version '(fosgit . "2.4.0")
  :group 'fosgit-section
  :type 'hook
  :options '(fosgit-diff-expansion-threshold
             fosgit-section-set-visibility-from-cache))

(defface fosgit-section-highlight
  '((((class color) (background light)) :background "grey95")
    (((class color) (background  dark)) :background "grey20"))
  "Face for highlighting the current section."
  :group 'fosgit-faces)

(defface fosgit-section-heading
  '((((class color) (background light)) :foreground "DarkGoldenrod4" :weight bold)
    (((class color) (background  dark)) :foreground "LightGoldenrod2" :weight bold))
  "Face for section headings."
  :group 'fosgit-faces)

(defface fosgit-section-secondary-heading '((t :weight bold))
  "Face for section headings of some secondary headings."
  :group 'fosgit-faces)

(defface fosgit-section-heading-selection
  '((((class color) (background light)) :foreground "salmon4")
    (((class color) (background  dark)) :foreground "LightSalmon3"))
  "Face for selected section headings."
  :group 'fosgit-faces)

;;; Core

(cl-defstruct fosgit-section
  type value start content end hidden washer refined
  source diff-header process parent children)

(defvar-local fosgit-root-section nil
  "The root section in the current buffer.
All other sections are descendants of this section.  The value
of this variable is set by `fosgit-insert-section' and you should
never modify it.")
(put 'fosgit-root-section 'permanent-local t)

(defun fosgit-current-section ()
  "Return the section at point."
  (or (get-text-property (point) 'fosgit-section) fosgit-root-section))

(defun fosgit-section-ident (section)
  "Return an unique identifier for SECTION.
The return value has the form ((TYPE . VALUE)...)."
  (cons (cons (fosgit-section-type section)
              (fosgit-section-value section))
        (--when-let (fosgit-section-parent section)
          (fosgit-section-ident it))))

(defun fosgit-get-section (ident &optional root)
  "Return the section identified by IDENT.
IDENT has to be a list as returned by `fosgit-section-ident'."
  (setq ident (reverse ident))
  (let ((section (or root fosgit-root-section)))
    (when (eq (car (pop ident)) (fosgit-section-type section))
      (while (and ident
                  (setq section
                        (--first
                         (and (eq    (caar ident) (fosgit-section-type it))
                              (equal (cdar ident) (fosgit-section-value it)))
                         (fosgit-section-children section))))
        (pop ident))
      section)))

(defvar fosgit-insert-section--current nil "For internal use only.")
(defvar fosgit-insert-section--parent  nil "For internal use only.")
(defvar fosgit-insert-section--oldroot nil "For internal use only.")

;;; Commands
;;;; Movement

(defun fosgit-section-forward ()
  "Move to the beginning of the next visible section."
  (interactive)
  (if (eobp)
      (user-error "No next section")
    (let ((section (fosgit-current-section)))
      (if (fosgit-section-parent section)
          (let ((next (and (not (fosgit-section-hidden section))
                           (not (= (fosgit-section-end section) (1+ (point))))
                           (car (fosgit-section-children section)))))
            (while (and section (not next))
              (unless (setq next (car (fosgit-section-siblings section 'next)))
                (setq section (fosgit-section-parent section))))
            (if next
                (fosgit-section-goto next)
              (user-error "No next section")))
        (fosgit-section-goto 1)))))

(defun fosgit-section-backward ()
  "Move to the beginning of the current or the previous visible section.
When point is at the beginning of a section then move to the
beginning of the previous visible section.  Otherwise move to
the beginning of the current section."
  (interactive)
  (if (bobp)
      (user-error "No previous section")
    (let ((section (fosgit-current-section)) children)
      (cond
       ((and (= (point) (1- (fosgit-section-end section)))
             (setq children (fosgit-section-children section)))
        (fosgit-section-goto (car (last children))))
       ((and (fosgit-section-parent section)
             (not (= (point) (fosgit-section-start section))))
        (fosgit-section-goto section))
       (t
        (let ((prev (car (fosgit-section-siblings section 'prev))))
          (if prev
              (while (and (not (fosgit-section-hidden prev))
                          (setq children (fosgit-section-children prev)))
                (setq prev (car (last children))))
            (setq prev (fosgit-section-parent section)))
          (cond (prev
                 (fosgit-section-goto prev))
                ((fosgit-section-parent section)
                 (user-error "No previous section"))
                ;; Eob special cases.
                ((not (get-text-property (1- (point)) 'invisible))
                 (fosgit-section-goto -1))
                (t
                 (goto-char (previous-single-property-change
                             (1- (point)) 'invisible))
                 (forward-line -1)
                 (fosgit-section-goto (fosgit-current-section))))))))))

(defun fosgit-section-up ()
  "Move to the beginning of the parent section."
  (interactive)
  (--if-let (fosgit-section-parent (fosgit-current-section))
      (fosgit-section-goto it)
    (user-error "No parent section")))

(defun fosgit-section-forward-sibling ()
  "Move to the beginning of the next sibling section.
If there is no next sibling section, then move to the parent."
  (interactive)
  (let ((current (fosgit-current-section)))
    (if (fosgit-section-parent current)
        (--if-let (car (fosgit-section-siblings current 'next))
            (fosgit-section-goto it)
          (fosgit-section-forward))
      (fosgit-section-goto 1))))

(defun fosgit-section-backward-sibling ()
  "Move to the beginning of the previous sibling section.
If there is no previous sibling section, then move to the parent."
  (interactive)
  (let ((current (fosgit-current-section)))
    (if (fosgit-section-parent current)
        (--if-let (car (fosgit-section-siblings current 'prev))
            (fosgit-section-goto it)
          (fosgit-section-backward))
      (fosgit-section-goto -1))))

(defun fosgit-section-goto (arg)
  (if (integerp arg)
      (progn (forward-line arg)
             (setq arg (fosgit-current-section)))
    (goto-char (fosgit-section-start arg)))
  (run-hook-with-args 'fosgit-section-movement-hook arg))

(defun fosgit-section-set-window-start (section)
  "Ensure the beginning of SECTION is visible."
  (unless (pos-visible-in-window-p (fosgit-section-end section))
    (set-window-start (selected-window) (fosgit-section-start section))))

(defun fosgit-hunk-set-window-start (section)
  "Ensure the beginning of the `hunk' SECTION is visible.
It the SECTION has a different type, then do nothing."
  (when (eq (fosgit-section-type section) 'hunk)
    (fosgit-section-set-window-start section)))

(defmacro fosgit-define-section-jumper (name heading type &optional value)
  "Define an interactive function to go some section.
Together TYPE and VALUE identify the section.
HEADING is the displayed heading of the section."
  (declare (indent defun))
  `(defun ,name (&optional expand) ,(format "\
Jump to the section \"%s\".
With a prefix argument also expand it." heading)
     (interactive "P")
     (--if-let (fosgit-get-section
                (cons (cons ',type ,value)
                      (fosgit-section-ident fosgit-root-section)))
         (progn (goto-char (fosgit-section-start it))
                (when expand
                  (with-local-quit (fosgit-section-show it))
                  (recenter 0)))
       (message ,(format "Section \"%s\" wasn't found" heading)))))

;;;; Visibility

(defun fosgit-section-show (section)
  "Show the body of the current section."
  (interactive (list (fosgit-current-section)))
  (setf (fosgit-section-hidden section) nil)
  (-when-let (washer (fosgit-section-washer section))
    (setf (fosgit-section-washer section) nil)
    (let ((inhibit-read-only t)
          (fosgit-insert-section--parent section)
          (content (fosgit-section-content section)))
      (save-excursion
        (if (and content (< content (fosgit-section-end section)))
            (funcall washer section) ; already partially washed (hunk)
          (goto-char (fosgit-section-end section))
          (setf (fosgit-section-content section) (point-marker))
          (funcall washer)
          (setf (fosgit-section-end section) (point-marker)))))
    (fosgit-section-update-highlight))
  (-when-let (beg (fosgit-section-content section))
    (remove-overlays beg (fosgit-section-end section) 'invisible t))
  (fosgit-section-update-visibility-cache section)
  (dolist (child (fosgit-section-children section))
    (if (fosgit-section-hidden child)
        (fosgit-section-hide child)
      (fosgit-section-show child))))

(defun fosgit-section-hide (section)
  "Hide the body of the current section."
  (interactive (list (fosgit-current-section)))
  (if (eq section fosgit-root-section)
      (user-error "Cannot hide root section")
    (setf (fosgit-section-hidden section) t)
    (-when-let (beg (fosgit-section-content section))
      (let ((end (fosgit-section-end section)))
        (remove-overlays beg end 'invisible t)
        (let ((o (make-overlay beg end)))
          (overlay-put o 'evaporate t)
          (overlay-put o 'invisible t))))))

(defun fosgit-section-toggle (section)
  "Toggle visibility of the body of the current section."
  (interactive (list (fosgit-current-section)))
  (if (eq section fosgit-root-section)
      (user-error "Cannot hide root section")
    (goto-char (fosgit-section-start section))
    (if (fosgit-section-hidden section)
        (fosgit-section-show section)
      (fosgit-section-hide section))))

(defun fosgit-section-toggle-children (section)
  "Toggle visibility of bodies of children of the current section."
  (interactive (list (fosgit-current-section)))
  (goto-char (fosgit-section-start section))
  (let* ((children (fosgit-section-children section))
         (show (-any? 'fosgit-section-hidden children)))
    (dolist (c children)
      (setf (fosgit-section-hidden c) show)))
  (fosgit-section-show section))

(defun fosgit-section-show-children (section &optional depth)
  "Recursively show the bodies of children of the current section.
With a prefix argument show children that deep and hide deeper
children."
  (interactive (list (fosgit-current-section)))
  (fosgit-section-show-children-1 section depth)
  (fosgit-section-show section))

(defun fosgit-section-show-children-1 (section &optional depth)
  (dolist (s (fosgit-section-children section))
    (setf (fosgit-section-hidden s) nil)
    (if depth
        (if (> depth 0)
            (fosgit-section-show-children-1 s (1- depth))
          (fosgit-section-hide s))
      (fosgit-section-show-children-1 s))))

(defun fosgit-section-hide-children (section)
  "Recursively hide the bodies of children of the current section."
  (interactive (list (fosgit-current-section)))
  (mapc 'fosgit-section-hide (fosgit-section-children section)))

(defun fosgit-section-show-headings (section)
  "Recursively show headings of children of the current section.
Only show the headings, previously shown text-only bodies are
hidden."
  (interactive (list (fosgit-current-section)))
  (fosgit-section-show-headings-1 section)
  (fosgit-section-show section))

(defun fosgit-section-show-headings-1 (section)
  (dolist (s (fosgit-section-children section))
    (setf (fosgit-section-hidden s) nil)
    (when (or (fosgit-section-children s)
              (not (fosgit-section-content s)))
      (fosgit-section-show-headings-1 s))))

(defun fosgit-section-cycle (section)
  "Cycle visibility of current section and its children."
  (interactive (list (fosgit-current-section)))
  (goto-char (fosgit-section-start section))
  (if (fosgit-section-hidden section)
      (progn (fosgit-section-show section)
             (fosgit-section-hide-children section))
    (let ((children (fosgit-section-children section)))
      (cond ((and (-any? 'fosgit-section-hidden   children)
                  (-any? 'fosgit-section-children children))
             (fosgit-section-show-headings section))
            ((-any? 'fosgit-section-hidden-body children)
             (fosgit-section-show-children section))
            (t
             (fosgit-section-hide section))))))

(defun fosgit-section-cycle-global ()
  "Cycle visibility of all sections in the current buffer."
  (interactive)
  (let ((children (fosgit-section-children fosgit-root-section)))
    (cond ((and (-any? 'fosgit-section-hidden   children)
                (-any? 'fosgit-section-children children))
           (fosgit-section-show-headings fosgit-root-section))
          ((-any? 'fosgit-section-hidden-body children)
           (fosgit-section-show-children fosgit-root-section))
          (t
           (mapc 'fosgit-section-hide children)))))

(defun fosgit-section-cycle-diffs ()
  "Cycle visibility of diff-related sections in the current buffer."
  (interactive)
  (-when-let (sections
              (cond ((derived-mode-p 'fosgit-status-mode)
                     (--mapcat
                      (when it
                        (when (fosgit-section-hidden it)
                          (fosgit-section-show it))
                        (fosgit-section-children it))
                      (list (fosgit-get-section '((staged)   (status)))
                            (fosgit-get-section '((unstaged) (status))))))
                    ((derived-mode-p 'fosgit-diff-mode)
                     (--filter (eq (fosgit-section-type it) 'file)
                               (fosgit-section-children fosgit-root-section)))))
    (if (-any? 'fosgit-section-hidden sections)
        (dolist (s sections)
          (fosgit-section-show s)
          (fosgit-section-hide-children s))
      (let ((children (cl-mapcan 'fosgit-section-children sections)))
        (cond ((and (-any? 'fosgit-section-hidden   children)
                    (-any? 'fosgit-section-children children))
               (mapc 'fosgit-section-show-headings sections))
              ((-any? 'fosgit-section-hidden-body children)
               (mapc 'fosgit-section-show-children sections))
              (t
               (mapc 'fosgit-section-hide sections)))))))

(defun fosgit-section-hidden-body (section &optional pred)
  (--if-let (fosgit-section-children section)
      (funcall (or pred '-any?) 'fosgit-section-hidden-body it)
    (and (fosgit-section-content section)
         (fosgit-section-hidden  section))))

(defun fosgit-section-invisible-p (section)
  "Return t if the SECTION's body is invisible.
When the body of an ancestor of SECTION is collapsed then
SECTION's body (and heading) obviously cannot be visible."
  (or (fosgit-section-hidden section)
      (--when-let (fosgit-section-parent section)
        (fosgit-section-invisible-p it))))

(defun fosgit-section-show-level (level)
  "Show surrounding sections up to LEVEL.
If LEVEL is negative show up to the absolute value.
Sections at higher levels are hidden."
  (if (< level 0)
      (let ((s (fosgit-current-section)))
        (setq level (- level))
        (while (> (1- (length (fosgit-section-ident s))) level)
          (setq s (fosgit-section-parent s))
          (goto-char (fosgit-section-start s)))
        (fosgit-section-show-children fosgit-root-section (1- level)))
    (cl-do* ((s (fosgit-current-section) (fosgit-section-parent s))
             (i (1- (length (fosgit-section-ident s))) (cl-decf i)))
        ((cond ((< i level) (fosgit-section-show-children s (- level i 1)) t)
               ((= i level) (fosgit-section-hide s) t))
         (fosgit-section-goto s)))))

(defun fosgit-section-show-level-1 ()
  "Show surrounding sections on first level."
  (interactive)
  (fosgit-section-show-level 1))

(defun fosgit-section-show-level-1-all ()
  "Show all sections on first level."
  (interactive)
  (fosgit-section-show-level -1))

(defun fosgit-section-show-level-2 ()
  "Show surrounding sections up to second level."
  (interactive)
  (fosgit-section-show-level 2))

(defun fosgit-section-show-level-2-all ()
  "Show all sections up to second level."
  (interactive)
  (fosgit-section-show-level -2))

(defun fosgit-section-show-level-3 ()
  "Show surrounding sections up to third level."
  (interactive)
  (fosgit-section-show-level 3))

(defun fosgit-section-show-level-3-all ()
  "Show all sections up to third level."
  (interactive)
  (fosgit-section-show-level -3))

(defun fosgit-section-show-level-4 ()
  "Show surrounding sections up to fourth level."
  (interactive)
  (fosgit-section-show-level 4))

(defun fosgit-section-show-level-4-all ()
  "Show all sections up to fourth level."
  (interactive)
  (fosgit-section-show-level -4))

;;;; Auxiliary

(defun fosgit-describe-section ()
  "Show information about the section at point.
This command is intended for debugging purposes."
  (interactive)
  (let ((section (fosgit-current-section)))
    (message "%S %S %s-%s"
             (fosgit-section-value section)
             (apply 'vector (mapcar 'car (fosgit-section-ident section)))
             (marker-position (fosgit-section-start section))
             (marker-position (fosgit-section-end section)))))

;;; Match

(defun fosgit-section-match (condition &optional section)
  "Return t if SECTION matches CONDITION.
SECTION defaults to the section at point.

Conditions can take the following forms:
  (CONDITION...)  matches if any of the CONDITIONs matches.
  [TYPE...]       matches if the first TYPE matches the type
                  of the section at point, the second matches
                  that of its parent, and so on.
  [* TYPE...]     matches sections that match [TYPE...] and
                  also recursively all their child sections.
  TYPE            matches TYPE regardless of its parents.

Each TYPE is a symbol.  Note that is not necessary to specify all
TYPEs up to the root section as printed by `fosgit-describe-type',
unless of course your want to be that precise."
  ;; When recursing SECTION actually is a type list.  Matching
  ;; macros also pass such a list instead of a section struct.
  (let ((types (if (fosgit-section-p section)
                   (mapcar 'car (fosgit-section-ident section))
                 section)))
    (when (or types section (fosgit-current-section))
      (if (listp condition)
          (--first (fosgit-section-match it types) condition)
        (fosgit-section-match-1 (if (symbolp condition)
                                   (list condition)
                                 (append condition nil))
                               types)))))

(defun fosgit-section-match-1 (l1 l2)
  (or (null l1)
      (if (eq (car l1) '*)
          (or (fosgit-section-match-1 (cdr l1) l2)
              (and l2
                   (fosgit-section-match-1 l1 (cdr l2))))
        (and l2
             (equal (car l1) (car l2))
             (fosgit-section-match-1 (cdr l1) (cdr l2))))))

(defmacro fosgit-section-when (condition &rest body)
  "If the section at point matches CONDITION evaluate BODY.

If the section matches evaluate BODY forms sequentially and
return the value of the last one, or if there are no BODY forms
return the value of the section.  If the section does not match
return nil.

See `fosgit-section-match' for the forms CONDITION can take."
  (declare (indent 1)
           (debug (sexp body)))
  `(--when-let (fosgit-current-section)
     (when (fosgit-section-match ',condition
                                (mapcar 'car (fosgit-section-ident it)))
       ,@(or body '((fosgit-section-value it))))))

(defmacro fosgit-section-case (&rest clauses)
  "Choose among clauses on the type of the section at point.

Each clause looks like (CONDITION BODY...).  The type of the
section is compared against each CONDITION; the BODY forms of the
first match are evaluated sequentially and the value of the last
form is returned.  Inside BODY the symbol `it' is bound to the
section at point.  If no clause succeeds or if there is no
section at point return nil.

See `fosgit-section-match' for the forms CONDITION can take.
Additionally a CONDITION of t is allowed in the final clause, and
matches if no other CONDITION match, even if there is no section
at point."
  (declare (indent 0)
           (debug (&rest (sexp body))))
  (let ((ident (cl-gensym "id")))
    `(let* ((it (fosgit-current-section))
            (,ident (and it (mapcar 'car (fosgit-section-ident it)))))
       (cond ,@(mapcar (lambda (clause)
                         `(,(or (eq (car clause) t)
                                `(and it (fosgit-section-match
                                          ',(car clause) ,ident)))
                           ,@(cdr clause)))
                       clauses)))))
;;; Create

(defvar fosgit-insert-section-hook nil
  "Hook run after `fosgit-insert-section's BODY.
Avoid using this hook and only ever do so if you know
what you are doing and are sure there is no other way.")

(defmacro fosgit-insert-section (&rest args)
  "Insert a section at point.

TYPE is the section type, a symbol.  Many commands that act on
the current section behave differently depending on that type.
Also if a variable `fosgit-TYPE-section-map' exists, then use
that as the text-property `keymap' of all text belonging to the
section (but this may be overwritten in subsections).

Optional VALUE is the value of the section, usually a string
that is required when acting on the section.

When optional HIDE is non-nil collapse the section body by
default, i.e. when first creating the section, but not when
refreshing the buffer.  Else expand it by default.  This can be
overwritten using `fosgit-section-set-visibility-hook'.  When a
section is recreated during a refresh, then the visibility of
predecessor is inherited and HIDE is ignored (but the hook is
still honored).

BODY is any number of forms that actually insert the section's
heading and body.  Optional NAME, if specified, has to be a
symbol, which is then bound to the struct of the section being
inserted.

Before BODY is evaluated the `start' of the section object is set
to the value of `point' and after BODY was evaluated its `end' is
set to the new value of `point'; BODY is responsible for moving
`point' forward.

If it turns out inside BODY that the section is empty, then
`fosgit-cancel-section' can be used to abort and remove all traces
of the partially inserted section.  This can happen when creating
a section by washing Git's output and Git didn't actually output
anything this time around.

\(fn [NAME] (TYPE &optional VALUE HIDE) &rest BODY)"
  (declare (indent defun)
           (debug ([&optional symbolp] (symbolp &optional form form) body)))
  (let ((s (if (symbolp (car args))
               (pop args)
             (cl-gensym "section"))))
    `(let* ((,s (make-fosgit-section
                 :type ',(nth 0 (car args))
                 :value ,(nth 1 (car args))
                 :start (point-marker)
                 :parent fosgit-insert-section--parent)))
       (setf (fosgit-section-hidden ,s)
             (-if-let (value (run-hook-with-args-until-success
                              'fosgit-section-set-visibility-hook ,s))
                 (eq value 'hide)
               (--if-let (and fosgit-insert-section--oldroot
                              (fosgit-get-section
                               (fosgit-section-ident ,s)
                               fosgit-insert-section--oldroot))
                   (fosgit-section-hidden it)
                 ,(nth 2 (car args)))))
       (let ((fosgit-insert-section--current ,s)
             (fosgit-insert-section--parent  ,s)
             (fosgit-insert-section--oldroot
              (or fosgit-insert-section--oldroot
                  (unless fosgit-insert-section--parent
                    (prog1 fosgit-root-section
                      (setq fosgit-root-section ,s))))))
         (catch 'cancel-section
           ,@(cdr args)
           (run-hooks 'fosgit-insert-section-hook)
           (fosgit-insert-child-count ,s)
           (set-marker-insertion-type (fosgit-section-start ,s) t)
           (let* ((end (setf (fosgit-section-end ,s) (point-marker)))
                  (map (intern (format "fosgit-%s-section-map"
                                       (fosgit-section-type ,s))))
                  (map (and (boundp map) (symbol-value map))))
             (save-excursion
               (goto-char (fosgit-section-start ,s))
               (while (< (point) end)
                 (let ((next (or (next-single-property-change
                                  (point) 'fosgit-section)
                                 end)))
                   (unless (get-text-property (point) 'fosgit-section)
                     (put-text-property (point) next 'fosgit-section ,s)
                     (when map
                       (put-text-property (point) next 'keymap map)))
                   (goto-char next)))))
           (if (eq ,s fosgit-root-section)
               (fosgit-section-show ,s)
             (setf (fosgit-section-children (fosgit-section-parent ,s))
                   (nconc (fosgit-section-children (fosgit-section-parent ,s))
                          (list ,s)))))
         ,s))))

(defun fosgit-cancel-section ()
  (when fosgit-insert-section--current
    (if (not (fosgit-section-parent fosgit-insert-section--current))
        (insert "(empty)\n")
      (delete-region (fosgit-section-start fosgit-insert-section--current)
                     (point))
      (setq fosgit-insert-section--current nil)
      (throw 'cancel-section nil))))

(defun fosgit-insert-heading (&rest args)
  "Insert the heading for the section currently being inserted.

This function should only be used inside `fosgit-insert-section'.

When called without any arguments, then just set the `content'
slot of the object representing the section being inserted to
a marker at `point'.  The section should only contain a single
line when this function is used like this.

When called with arguments ARGS, which have to be strings, then
insert those strings at point.  The section should not contain
any text before this happens and afterwards it should again only
contain a single line.  If the `face' property is set anywhere
inside any of these strings, then insert all of them unchanged.
Otherwise use the `fosgit-section-heading' face for all inserted
text.

The `content' property of the section struct is the end of the
heading (which lasts from `start' to `content') and the beginning
of the the body (which lasts from `content' to `end').  If the
value of `content' is nil, then the section has no heading and
its body cannot be collapsed.  If a section does have a heading
then its height must be exactly one line, including a trailing
newline character.  This isn't enforced, you are responsible for
getting it right.  The only exception is that this function does
insert a newline character if necessary."
  (declare (indent defun))
  (when args
    (let ((heading (apply #'concat args)))
      (insert (if (next-single-property-change 0 'face (concat "0" heading))
                  heading
                (propertize heading 'face 'fosgit-section-heading)))))
  (unless (bolp)
    (insert ?\n))
  (setf (fosgit-section-content fosgit-insert-section--current) (point-marker)))

(defvar fosgit-insert-headers-hook nil "For internal use only.")

(defun fosgit-insert-headers (hooks)
  (let ((fosgit-insert-section-hook
         (cons 'fosgit-insert-remaining-headers
               (if (listp fosgit-insert-section-hook)
                   fosgit-insert-section-hook
                 (list fosgit-insert-section-hook))))
        (fosgit-insert-headers-hook hooks)
        wrapper)
    (while (and (setq wrapper (pop fosgit-insert-headers-hook))
                (= (point) (point-min)))
      (funcall wrapper))))

(defun fosgit-insert-remaining-headers ()
  (if (= (point) (point-min))
      (fosgit-cancel-section)
    (fosgit-insert-heading)
    (remove-hook 'fosgit-insert-section-hook 'fosgit-insert-remaining-headers)
    (mapc #'funcall fosgit-insert-headers-hook)
    (insert "\n")))

(defun fosgit-insert-child-count (section)
  "Modify SECTION's heading to contain number of child sections.

If `fosgit-section-show-child-count' is non-nil and the SECTION
has children and its heading ends with \":\", then replace that
with \" (N)\", where N is the number of child sections.

This function is called by `fosgit-insert-section' after that has
evaluated its BODY.  Admittedly that's a bit of a hack."
  ;; This has to be fast, not pretty!
  (let (content count)
    (when (and fosgit-section-show-child-count
               (setq count (length (fosgit-section-children section)))
               (> count 0)
               (setq content (fosgit-section-content section))
               (eq (char-before (1- content)) ?:))
      (save-excursion
        (goto-char (- content 2))
        (insert (format " (%s)" count))
        (delete-char 1)))))

;;; Update

(defvar-local fosgit-section-highlight-overlays nil)
(defvar-local fosgit-section-highlighted-section nil)
(defvar-local fosgit-section-highlighted-sections nil)
(defvar-local fosgit-section-unhighlight-sections nil)

(defun fosgit-section-update-region (_)
  ;; Don't show complete region.  Highlighting emphasizes headings.
  (fosgit-region-sections))

(defun fosgit-section-update-highlight ()
  (let ((section (fosgit-current-section)))
    (unless (eq section fosgit-section-highlighted-section)
      (let ((inhibit-read-only t)
            (deactivate-mark nil)
            (selection (fosgit-region-sections)))
        (mapc #'delete-overlay fosgit-section-highlight-overlays)
        (setq fosgit-section-unhighlight-sections
              fosgit-section-highlighted-sections
              fosgit-section-highlighted-sections nil)
        (unless (eq section fosgit-root-section)
          (run-hook-with-args-until-success
           'fosgit-section-highlight-hook section selection))
        (--each fosgit-section-unhighlight-sections
          (run-hook-with-args-until-success
           'fosgit-section-unhighlight-hook it selection))
        (restore-buffer-modified-p nil)
        (unless (eq fosgit-section-highlighted-section section)
          (setq fosgit-section-highlighted-section
                (unless (fosgit-section-hidden section) section))))
      (setq deactivate-mark nil))))

(defun fosgit-section-highlight (section selection)
  "Highlight SECTION and if non-nil all SELECTION.
This function works for any section but produces undesirable
effects for diff related sections, which by default are
highlighted using `fosgit-diff-highlight'.  Return t."
  (cond (selection
         (fosgit-section-make-overlay (fosgit-section-start     (car selection))
                                     (fosgit-section-end (car (last selection)))
                                     'fosgit-section-highlight)
         (fosgit-section-highlight-selection nil selection))
        (t
         (fosgit-section-make-overlay (fosgit-section-start section)
                                     (fosgit-section-end   section)
                                     'fosgit-section-highlight)))
  t)

(defun fosgit-section-highlight-selection (_ selection)
  "Highlight the section selection region.
If SELECTION is non-nil then it is a list of sections selected by
the region.  The headings of these sections are then highlighted.

This is a fallback for people who don't want to highlight the
current section and therefore removed `fosgit-section-highlight'
from `fosgit-section-highlight-hook'.

This function is necessary to ensure that a representation of
such a region is visible.  If neither of these functions were
part of the hook variable, then such a region would be
invisible."
  (when selection
    (--each selection
      (fosgit-section-make-overlay (fosgit-section-start it)
                                  (or (fosgit-section-content it)
                                      (fosgit-section-end it))
                                  'fosgit-section-heading-selection))
    t))

(defun fosgit-section-make-overlay (start end face)
  ;; Yes, this doesn't belong here.  But the alternative of
  ;; spreading this hack across the code base is even worse.
  (when (and fosgit-keep-region-overlay
             (memq face '(fosgit-section-heading-selection
                          fosgit-diff-file-heading-selection
                          fosgit-diff-hunk-heading-selection)))
    (setq face (list :foreground (face-foreground face))))
  (let ((ov (make-overlay start end nil t)))
    (overlay-put ov 'face face)
    (overlay-put ov 'evaporate t)
    (push ov fosgit-section-highlight-overlays)
    ov))

(defun fosgit-section-goto-successor (section line char arg)
  (let ((ident (fosgit-section-ident section)))
    (--if-let (fosgit-get-section ident)
        (let ((start (fosgit-section-start it)))
          (goto-char start)
          (unless (eq it fosgit-root-section)
            (ignore-errors
              (forward-line line)
              (forward-char char))
            (unless (eq (fosgit-current-section) it)
              (goto-char start))))
      (or (and (eq (fosgit-section-type section) 'hunk)
               (-when-let (parent (fosgit-get-section
                                   (fosgit-section-ident
                                    (fosgit-section-parent section))))
                 (let* ((children (fosgit-section-children parent))
                        (siblings (fosgit-section-siblings section 'prev))
                        (previous (nth (length siblings) children)))
                   (if (not arg)
                       (--when-let (or previous (car (last children)))
                         (goto-char (fosgit-section-start it)))
                     (when previous
                       (goto-char (fosgit-section-start previous)))
                     (if (and (stringp arg)
                              (re-search-forward
                               arg (fosgit-section-end parent) t))
                         (goto-char (match-beginning 0))
                       (goto-char (fosgit-section-end (car (last children))))
                       (forward-line -1)
                       (while (looking-at "^ ")    (forward-line -1))
                       (while (looking-at "^[-+]") (forward-line -1))
                       (forward-line))))))
          (goto-char (--if-let (fosgit-section-goto-successor-1 section)
                         (if (eq (fosgit-section-type it) 'button)
                             (point-min)
                           (fosgit-section-start it))
                       (point-min)))))))

(defun fosgit-section-goto-successor-1 (section)
  (or (--when-let (pcase (fosgit-section-type section)
                    (`staged 'unstaged)
                    (`unstaged 'staged)
                    (`unpushed 'unpulled)
                    (`unpulled 'unpushed))
        (fosgit-get-section `((,it) (status))))
      (--when-let (car (fosgit-section-siblings section 'next))
        (fosgit-get-section (fosgit-section-ident it)))
      (--when-let (car (fosgit-section-siblings section 'prev))
        (fosgit-get-section (fosgit-section-ident it)))
      (--when-let (fosgit-section-parent section)
        (or (fosgit-get-section (fosgit-section-ident it))
            (fosgit-section-goto-successor-1 it)))))

;;; Visibility

(defvar-local fosgit-section-visibility-cache nil)
(put 'fosgit-section-visibility-cache 'permanent-local t)

(defun fosgit-section-set-visibility-from-cache (section)
  "Set SECTION's visibility to the cached value.
Currently the cache can only be used to remember that a section's
body should be collapsed, not that it should be expanded.  Return
either `hide' or nil."
  (and (member (fosgit-section-visibility-ident section)
               fosgit-section-visibility-cache)
       'hide))

(cl-defun fosgit-section-cache-visibility
    (&optional (section fosgit-insert-section--current))
  (let ((ident (fosgit-section-visibility-ident section)))
    (if (fosgit-section-hidden section)
        (cl-pushnew ident fosgit-section-visibility-cache :test #'equal)
      (setq fosgit-section-visibility-cache
            (delete ident fosgit-section-visibility-cache)))))

(defun fosgit-section-update-visibility-cache (section)
  (setq fosgit-section-visibility-cache
        (delete (fosgit-section-visibility-ident section)
                fosgit-section-visibility-cache)))

(defun fosgit-section-visibility-ident (section)
  (let ((type  (fosgit-section-type  section))
        (value (fosgit-section-value section)))
    (cons type
          (cond ((not (memq type '(unpulled unpushed))) value)
                ((string-match-p "@{upstream}" value) value)
                ;; Unfortunately Git chokes on "@{push}" when the
                ;; value of `push.default' does not allow a 1:1
                ;; mapping.  But collapsed logs of unpushed and
                ;; unpulled commits in the status buffer should
                ;; remain invisible after changing branches.
                ;; So we have to pretend the value is constant.
                ((string-match-p "\\`\\.\\." value) "..@{push}")
                (t "@{push}..")))))

;;; Utilities

(cl-defun fosgit-section-selected-p (section &optional (selection nil sselection))
  (and (not (eq section fosgit-root-section))
       (or  (eq section (fosgit-current-section))
            (memq section (if sselection
                              selection
                            (setq selection (fosgit-region-sections))))
            (--when-let (fosgit-section-parent section)
              (fosgit-section-selected-p it selection)))))

(defun fosgit-section-parent-value (section)
  (setq section (fosgit-section-parent section))
  (when section (fosgit-section-value  section)))

(defun fosgit-section-siblings (section &optional direction)
  "Return a list of the sibling sections of SECTION.

If optional DIRECTION is `prev' then return siblings that come
before SECTION, if it is `next' then return siblings that come
after SECTION.  For all other values return all siblings
excluding SECTION itself."
  (-when-let (parent (fosgit-section-parent section))
    (let ((siblings  (fosgit-section-children parent)))
      (pcase direction
        (`prev  (cdr (member section (reverse siblings))))
        (`next  (cdr (member section siblings)))
        (_      (remq section siblings))))))

(defun fosgit-region-values (&rest types)
  "Return a list of the values of the selected sections.

Also see `fosgit-region-sections' whose doc-string explains when a
region is a valid section selection.  If the region is not active
or is not a valid section selection, then return nil.  If optional
TYPES is non-nil then the selection not only has to be valid; the
types of all selected sections additionally have to match one of
TYPES, or nil is returned."
  (mapcar 'fosgit-section-value (apply 'fosgit-region-sections types)))

(defun fosgit-region-sections (&rest types)
  "Return a list of the selected sections.

When the region is active and constitutes a valid section
selection, then return a list of all selected sections.  This is
the case when the region begins in the heading of a section and
ends in the heading of a sibling of that first section.  When
the selection is not valid then return nil.  Most commands that
can act on the selected sections, then instead just act on the
current section, the one point is in.

When the region looks like it would in any other buffer then
the selection is invalid.  When the selection is valid then the
region uses the `fosgit-section-highlight'.  This does not apply
to diffs were things get a bit more complicated, but even here
if the region looks like it usually does, then that's not a
valid selection as far as this function is concerned.

If optional TYPES is non-nil then the selection not only has to
be valid; the types of all selected sections additionally have to
match one of TYPES, or nil is returned."
  (when (use-region-p)
    (let* ((rbeg (region-beginning))
           (rend (region-end))
           (sbeg (get-text-property rbeg 'fosgit-section))
           (send (get-text-property rend 'fosgit-section)))
      (unless (memq send (list sbeg fosgit-root-section nil))
        (let ((siblings (fosgit-section-siblings sbeg 'next)) sections)
          (when (and (memq send siblings)
                     (fosgit-section-position-in-heading-p sbeg rbeg)
                     (fosgit-section-position-in-heading-p send rend))
            (while siblings
              (push (car siblings) sections)
              (when (eq (pop siblings) send)
                (setq siblings nil)))
            (setq sections (cons sbeg (nreverse sections)))
            (when (or (not types)
                      (--all-p (memq (fosgit-section-type it) types) sections))
              sections)))))))

(defun fosgit-section-position-in-heading-p (section pos)
  "Return t if POSITION is inside the heading of SECTION."
  (and (>= pos (fosgit-section-start section))
       (<  pos (or (fosgit-section-content section)
                   (fosgit-section-end section)))
       t))

(defun fosgit-section-internal-region-p (&optional section)
  "Return t if the region is active and inside SECTION's body.
If optional SECTION is nil, use the current section."
  (and (region-active-p)
       (or section (setq section (fosgit-current-section)))
       (let ((beg (get-text-property (region-beginning) 'fosgit-section)))
         (and (eq beg (get-text-property   (region-end) 'fosgit-section))
              (eq beg section)))
       (not (or (fosgit-section-position-in-heading-p section (region-beginning))
                (fosgit-section-position-in-heading-p section (region-end))))
       t))

(defun fosgit-wash-sequence (function)
  "Repeatedly call FUNCTION until it returns nil or eob is reached.
FUNCTION has to move point forward or return nil."
  (while (and (not (eobp)) (funcall function))))

(defun fosgit-add-section-hook (hook function &optional at append local)
  "Add to the value of section hook HOOK the function FUNCTION.

Add FUNCTION at the beginning of the hook list unless optional
APPEND is non-nil, in which case FUNCTION is added at the end.
If FUNCTION already is a member then move it to the new location.

If optional AT is non-nil and a member of the hook list, then
add FUNCTION next to that instead.  Add before or after AT, or
replace AT with FUNCTION depending on APPEND.  If APPEND is the
symbol `replace', then replace AT with FUNCTION.  For any other
non-nil value place FUNCTION right after AT.  If nil, then place
FUNCTION right before AT.  If FUNCTION already is a member of the
list but AT is not, then leave FUNCTION where ever it already is.

If optional LOCAL is non-nil, then modify the hook's buffer-local
value rather than its global value.  This makes the hook local by
copying the default value.  That copy is then modified.

HOOK should be a symbol.  If HOOK is void, it is first set to nil.
HOOK's value must not be a single hook function.  FUNCTION should
be a function that takes no arguments and inserts one or multiple
sections at point, moving point forward.  FUNCTION may choose not
to insert its section(s), when doing so would not make sense.  It
should not be abused for other side-effects.  To remove FUNCTION
again use `remove-hook'."
  (unless (boundp hook)
    (error "Cannot add function to undefined hook variable %s" hook))
  (or (default-boundp hook) (set-default hook nil))
  (let ((value (if local
                   (if (local-variable-p hook)
                       (symbol-value hook)
                     (unless (local-variable-if-set-p hook)
                       (make-local-variable hook))
                     (copy-sequence (default-value hook)))
                 (default-value hook))))
    (if at
        (when (setq at (member at value))
          (setq value (delq function value))
          (cond ((eq append 'replace)
                 (setcar at function))
                (append
                 (push function (cdr at)))
                (t
                 (push (car at) (cdr at))
                 (setcar at function))))
      (setq value (delq function value)))
    (unless (member function value)
      (setq value (if append
                      (append value (list function))
                    (cons function value))))
    (when (eq append 'replace)
      (setq value (delq at value)))
    (if local
        (set hook value)
      (set-default hook value))))

;;; fosgit-section.el ends soon
(provide 'fosgit-section)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-section.el ends here
