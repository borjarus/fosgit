;;; fosgit-popup.el --- Define prefix-infix-suffix command combos  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2016  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; This library was inspired by and replaces library `fosgit-key-mode',
;; which was written by Phil Jackson <phil@shellarchive.co.uk> and is
;; distributed under the GNU General Public License version 3 or later.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Package-Requires: ((emacs "24.4") (async "20150909.2257") (dash "20151021.113"))
;; Keywords: bindings
;; Homepage: https://github.com/magit/magit

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

;; This library implements a generic interface for toggling switches
;; and setting options and then invoking an Emacs command which does
;; something with these arguments.  The prototypical use is for the
;; command to call an external process, passing on the arguments as
;; command line arguments.  But this is only one of many possible
;; uses (though the one this library is optimized for).

;; With the Emacs concept of "prefix arguments" in mind this could be
;; described as "infix arguments with feedback in a buffer".

;; Commands that set the prefix argument for the subsequent command do
;; not limit what that next command could be.  But entering a command
;; console popup does limit the selection to the commands defined for
;; that popup, and so we use the term "infix" instead of "prefix".

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'dash)
(require 'format-spec)

(and (require 'async-bytecomp nil t)
     (memq 'fosgit (bound-and-true-p async-bytecomp-allowed-packages))
     (fboundp 'async-bytecomp-package-mode)
     (async-bytecomp-package-mode 1))

(declare-function info 'info)
(declare-function Man-find-section 'man)
(declare-function Man-next-section 'man)

;; For the `:variable' event type.
(declare-function fosgit-call-git 'fosgit-process)
(declare-function fosgit-git-string 'fosgit-git)
(declare-function fosgit-refresh 'fosgit-mode)
(declare-function fosgit-set 'fosgit-git)

;; For branch actions.
(declare-function fosgit-branch-set-face 'fosgit-git)
(declare-function fosgit-local-branch-p 'fosgit-git)

;;; Settings
;;;; Custom Groups

(defgroup fosgit-popup nil
  "Infix arguments with a popup as feedback."
  :group 'bindings)

(defgroup fosgit-popup-faces nil
  "Faces used by Fosgit-Popup."
  :group 'fosgit-popup)

;;;; Custom Options

(defcustom fosgit-popup-display-buffer-action '((display-buffer-below-selected))
  "The action used to display a popup buffer.

Popup buffers are displayed using `display-buffer' with the value
of this option as ACTION argument.  You can also set this to nil
and instead add an entry to `display-buffer-alist'."
  :package-version '(fosgit-popup . "2.4.0")
  :group 'fosgit-popup
  :type 'sexp)

(defcustom fosgit-popup-manpage-package
  (if (memq system-type '(windows-nt ms-dos)) 'woman 'man)
  "The package used to display manpages.
One of `man' or `woman'."
  :group 'fosgit-popup
  :type '(choice (const man) (const woman)))

(defcustom fosgit-popup-show-help-echo t
  "Show usage information in the echo area."
  :group 'fosgit-popup
  :type 'boolean)

(defcustom fosgit-popup-show-common-commands t
  "Initially show section with commands common to all popups.
This section can also be toggled temporarily using \
\\<fosgit-popup-mode-map>\\[fosgit-popup-toggle-show-common-commands]."
  :group 'fosgit-popup
  :type 'boolean)

(defcustom fosgit-popup-use-prefix-argument 'disabled
  "Control how prefix arguments affect infix argument popups.

This option controls the effect that the use of a prefix argument
before entering a popup has.  The *intended* default is `default',
but the *actual* default is `disabled'.  This is necessary because
the old popup implementation did simply forward such a pre-popup
prefix argument to the action invoked from the popup, and changing
that without users being aware of it could lead to tears.

`disabled' Bring up a Custom option buffer so that the user reads
           this and then makes an informed choice.

`default'  With a prefix argument directly invoke the popup's
           default action (an Emacs command), instead of bringing
           up the popup.

`popup'    With a prefix argument bring up the popup, otherwise
           directly invoke the popup's default action.

`nil'      Ignore prefix arguments."
  :group 'fosgit-popup
  :type '(choice
          (const :tag "Call default action instead of showing popup" default)
          (const :tag "Show popup instead of calling default action" popup)
          (const :tag "Ignore prefix argument" nil)
          (const :tag "Abort and show usage information" disabled)))

;;;; Custom Faces

(defface fosgit-popup-heading
  '((t :inherit font-lock-keyword-face))
  "Face for key mode header lines."
  :group 'fosgit-popup-faces)

(defface fosgit-popup-key
  '((t :inherit font-lock-builtin-face))
  "Face for key mode buttons."
  :group 'fosgit-popup-faces)

(defface fosgit-popup-argument
  '((t :inherit font-lock-warning-face))
  "Face used to display enabled arguments in popups."
  :group 'fosgit-popup-faces)

(defface fosgit-popup-disabled-argument
  '((t :inherit shadow))
  "Face used to display disabled arguments in popups."
  :group 'fosgit-popup-faces)

(defface fosgit-popup-option-value
  '((t :inherit font-lock-string-face))
  "Face used to display option values in popups."
  :group 'fosgit-popup-faces)

;;;; Keymap

(defvar fosgit-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'fosgit-invoke-popup-action)
    (define-key map [?- t]        'fosgit-invoke-popup-switch)
    (define-key map [?= t]        'fosgit-invoke-popup-option)
    (define-key map [?\C-c ?\C-c] 'fosgit-popup-set-default-arguments)
    (define-key map [?\C-x ?\C-s] 'fosgit-popup-save-default-arguments)
    (define-key map [?\C-g]       'fosgit-popup-quit)
    (define-key map [??]          'fosgit-popup-help)
    (define-key map [?\C-h ?i]    'fosgit-popup-info)
    (define-key map [?\C-t]       'fosgit-popup-toggle-show-common-commands)
    (define-key map [?\d]         'backward-button)
    (define-key map [?\C-p]       'backward-button)
    (define-key map [?\t]         'forward-button)
    (define-key map [?\C-n]       'forward-button)
    (define-key map [?\r]         'push-button)
    map)
  "Keymap for `fosgit-popup-mode'.

\\<fosgit-popup-mode-map>\
This keymap contains bindings common to all popups.  A section
listing these commands can be shown or hidden using \
\\[fosgit-popup-toggle-show-common-commands].

The prefix used to toggle any switch can be changed by binding
another key to `fosgit-invoke-popup-switch'.  Likewise binding
another key to `fosgit-invoke-popup-option' changes the prefixed
used to set any option.  The two prefixes have to be different.
If you change these bindings you should also change the `prefix'
property of the button types `fosgit-popup-switch-button' and
`fosgit-popup-option-button'.

If you change any other binding, then you might have to also edit
`fosgit-popup-common-commands' for things to align correctly in
the section listing these commands.

Never bind an alphabetic character in this keymap or you might
make it impossible to invoke certain actions.")

(defvar fosgit-popup-common-commands
  '(("Set defaults"          fosgit-popup-set-default-arguments)
    ("View popup manual"     fosgit-popup-info)
    ("Toggle this section"   fosgit-popup-toggle-show-common-commands)
    ("Save defaults"         fosgit-popup-save-default-arguments)
    ("    Popup help prefix" fosgit-popup-help)
    ("Abort"                 fosgit-popup-quit)))

;;;; Buttons

(define-button-type 'fosgit-popup-button
  'face nil
  'action (lambda (button)
            (funcall (button-get button 'function)
                     (button-get button 'event))))

(define-button-type 'fosgit-popup-switch-button
  'supertype 'fosgit-popup-button
  'function  'fosgit-invoke-popup-switch
  'property  :switches
  'heading   "Switches\n"
  'formatter 'fosgit-popup-format-argument-button
  'format    " %k %d (%a)"
  'prefix    ?-
  'maxcols   1)

(define-button-type 'fosgit-popup-option-button
  'supertype 'fosgit-popup-button
  'function  'fosgit-invoke-popup-option
  'property  :options
  'heading   "Options\n"
  'formatter 'fosgit-popup-format-argument-button
  'format    " %k %d (%a%v)"
  'prefix    ?=
  'maxcols   1)

(define-button-type 'fosgit-popup-variable-button
  'supertype 'fosgit-popup-button
  'function  'fosgit-invoke-popup-action
  'property  :variables
  'heading   "Variables\n"
  'formatter 'fosgit-popup-format-variable-button
  'format    " %k %d"
  'prefix    nil
  'maxcols   1)

(define-button-type 'fosgit-popup-action-button
  'supertype 'fosgit-popup-button
  'function  'fosgit-invoke-popup-action
  'property  :actions
  'heading   "Actions\n"
  'formatter 'fosgit-popup-format-action-button
  'format    " %k %d"
  'prefix    nil
  'maxcols   :max-action-columns)

(define-button-type 'fosgit-popup-command-button
  'supertype 'fosgit-popup-action-button
  'formatter 'fosgit-popup-format-command-button
  'action    (lambda (button)
               (let ((command (button-get button 'function)))
                 (unless (eq command 'push-button)
                   (call-interactively command)))))

(define-button-type 'fosgit-popup-internal-command-button
  'supertype 'fosgit-popup-command-button
  'heading   "Common Commands\n"
  'maxcols   3)

;;; Events

(defvar-local fosgit-this-popup nil
  "The popup which is currently active.
This is intended for internal use only.
Don't confuse this with `fosgit-current-popup'.")

(defvar-local fosgit-this-popup-events nil
  "The events known to the active popup.
This is intended for internal use only.
Don't confuse this with `fosgit-current-popup-args'.")

(defun fosgit-popup-get (prop)
  "While a popup is active, get the value of PROP."
  (if (memq prop '(:switches :options :variables :actions))
      (plist-get fosgit-this-popup-events prop)
    (plist-get (symbol-value fosgit-this-popup) prop)))

(defun fosgit-popup-put (prop val)
  "While a popup is active, set the value of PROP to VAL."
  (if (memq prop '(:switches :options :variables :actions))
      (setq fosgit-this-popup-events
            (plist-put fosgit-this-popup-events prop val))
    (error "Property %s isn't supported" prop)))

(defvar fosgit-current-popup nil
  "The popup from which this editing command was invoked.

Use this inside the `interactive' form of a popup aware command
to determine whether it was invoked from a popup and if so from
which popup.  If the current command was invoked without the use
of a popup then this is nil.")

(defvar fosgit-current-popup-args nil
  "The value of the popup arguments for this editing command.

If the current command was invoked from a popup, then this is
a list of strings of all the set switches and options.  This
includes arguments which are set by default not only those
explicitly set during this invocation.

When the value is nil, then that can be because no argument is
set, or because the current command wasn't invoked from a popup;
consult `fosgit-current-popup' to tell the difference.

Generally it is better to use `NAME-arguments', which is created
by `fosgit-define-popup', instead of this variable or the function
by the same name, because `NAME-argument' uses the default value
for the arguments when the editing command is invoked directly
instead of from a popup.  When the command is bound in several
popups that might not be feasible though.")

(defun fosgit-current-popup-args (&rest filter)
  "Return the value of the popup arguments for this editing command.

The value is the same as that of the variable by the same name
\(which see), except that FILTER is applied.  FILTER is a list
of regexps; only arguments that match one of them are returned.
The first element of FILTER may also be `:not' in which case
only arguments that don't match any of the regexps are returned,
or `:only' which doesn't change the behaviour."
  (let ((-compare-fn (lambda (a b) (fosgit-popup-arg-match b a))))
    (-filter (if (eq (car filter) :not)
                 (lambda (arg) (not (-contains? (cdr filter) arg)))
               (when (eq (car filter) :only)
                 (pop filter))
               (lambda (arg) (-contains? filter arg)))
             fosgit-current-popup-args)))

(defun fosgit-popup-arg-match (pattern string)
  (if (or (string-match-p "=$" pattern)
          (string-match-p "^-[A-Z]$" pattern))
      (string-match (format "^%s\\(.*\\)$" pattern) string)
    (string-equal string pattern)))

(cl-defstruct fosgit-popup-event key dsc arg fun use val)

(defun fosgit-popup-event-keydsc (ev)
  (let ((key (fosgit-popup-event-key ev)))
    (key-description (if (vectorp key) key (vector key)))))

(defun fosgit-popup-lookup (event type)
  (--first (equal (fosgit-popup-event-key it) event)
           (-filter 'fosgit-popup-event-p (fosgit-popup-get type))))

(defun fosgit-popup-get-args ()
  (--mapcat (when (and (fosgit-popup-event-p it)
                       (fosgit-popup-event-use it))
              (list (format "%s%s"
                            (fosgit-popup-event-arg it)
                            (or (fosgit-popup-event-val it) ""))))
            (append (fosgit-popup-get :switches)
                    (fosgit-popup-get :options))))

(defmacro fosgit-popup-convert-events (def form)
  (declare (indent 1) (debug (form form)))
  `(--map (if (or (null it) (stringp it) (functionp it)) it ,form) ,def))

(defun fosgit-popup-convert-switches (val def)
  (fosgit-popup-convert-events def
    (let ((a (nth 2 it)))
      (make-fosgit-popup-event
       :key (car it) :dsc (cadr it) :arg a
       :use (and (member a val) t)))))

(defun fosgit-popup-convert-options (val def)
  (fosgit-popup-convert-events def
    (let* ((a (nth 2 it))
           (r (format "^%s\\(.*\\)" a))
           (v (--first (string-match r it) val)))
      (make-fosgit-popup-event
       :key (car it)  :dsc (cadr it) :arg a
       :use (and v t) :val (and v (match-string 1 v))
       :fun (or (nth 3 it) 'read-from-minibuffer)))))

(defun fosgit-popup-convert-variables (_val def)
  (fosgit-popup-convert-events def
    (make-fosgit-popup-event
     :key (car it) :dsc (cadr it) :fun (nth 2 it) :arg (nth 3 it))))

(defun fosgit-popup-convert-actions (_val def)
  (fosgit-popup-convert-events def
    (make-fosgit-popup-event
     :key (car it) :dsc (cadr it) :fun (nth 2 it))))

;;; Define

(defmacro fosgit-define-popup (name doc &rest args)
  "Define a popup command named NAME.

NAME should begin with the package prefix and by convention end
with `-popup'.  That name is used for the actual command as well
as for a variable used internally.  DOC is used as the doc-string
of that command.

Also define an option and a function named `SHORTNAME-arguments',
where SHORTNAME is NAME with the trailing `-popup' removed.  The
name of this option and this function can be overwritten using
the optional argument OPTION, but that is rarely advisable. As a
special case if OPTION is specified but nil, do not define this
option and this function at all.

The option `SHORTNAME-arguments' holds the default value for the
popup arguments.  It can be customized from within the popup or
using the Custom interface.

The function `SHORTNAME-arguments' is a wrapper around the
variable `fosgit-current-popup-args', both of which are intended
to be used inside the `interactive' form of commands commonly
invoked from the popup `NAME'.  When such a command is invoked
from that popup, then the function `SHORTNAME-arguments' returns
the value of the variable `fosgit-current-popup-args'; however
when the command is invoked directly, then it returns the default
value of the variable `SHORTNAME-arguments'.

Optional argument GROUP specifies the Custom group in which the
option is placed.  If omitted then the option is placed in some
group the same way it is done when directly using `defcustom'.

Optional argument MODE is deprecated, instead use the keyword
arguments `:setup-function' and/or `:refresh-function'.  If MODE
is non-nil, then it specifies the mode used by the popup buffer,
instead of the default, which is `fosgit-popup-mode'.

The remaining arguments should have the form

    [KEYWORD VALUE]...

The following keywords are meaningful (and by convention are
usually specified in that order):

`:actions'
  The actions which can be invoked from the popup.  VALUE is a
  list whose members have the form (KEY DESC COMMAND), see
  `fosgit-define-popup-action' for details.

  Actions are regular Emacs commands, which usually have an
  `interactive' form setup to consume the values of the popup
  `:switches' and `:options' when invoked from the corresponding
  popup, else when invoked as the default action or directly
  without using the popup, the default value of the variable
  `SHORTNAME-arguments'.  This is usually done by calling the
  function `SHORTNAME-arguments'.

  Members of VALUE may also be strings, assuming the first member
  is also a string.  Instead of just one action section with the
  heading \"Actions\", multiple sections are then inserted into
  the popup buffer, using these strings as headings.

  Members of VALUE may also be nil.  This should only be used
  together with `:max-action-columns' and allows having gaps in
  the action grit, which can help arranging actions sensibly.

`:default-action'
  The default action of the popup which is used directly instead
  of displaying the popup buffer, when the popup is invoked with
  a prefix argument.  Also see `fosgit-popup-use-prefix-argument'
  and `:use-prefix', which can be used to inverse the meaning of
  the prefix argument.

`:use-prefix'
  Controls when to display the popup buffer and when to invoke
  the default action (if any) directly.  This overrides the
  global default set using `fosgit-popup-use-prefix-argument'.
  The value, if specified, should be one of `default' or `popup'.

`:max-action-columns'
  The maximum number of actions to display on a single line.
  This helps arranging actions more sensibly.  If there is not
  enough room to display that many actions on one line, then
  this is ignored.

`:switches'
  The popup arguments which can be toggled on and off.  VALUE
  is a list whose members have the form (KEY DESC SWITCH), see
  `fosgit-define-popup-switch' for details.

`:options'
  The popup arguments which take a value, as in \"--opt=OPTVAL\".
  VALUE is a list whose members have the form (KEY DESC OPTION
  READER), see `fosgit-define-popup-option' for details.

`:default-arguments'
  The default arguments, a list of switches (which are then
  enabled by default) and options with there default values, as
  in \"--OPT=OPTVAL\".

`:sequence-predicate'
  When this function returns non-nil, then the popup uses
  `:sequence-actions' instead of `:actions', and does not show
  the `:switches' and `:options'.

`:sequence-actions'
  The actions which can be invoked from the popup, when
  `:sequence-predicate' returns non-nil.

`:setup-function'
  When this function is specified, then it is used instead of
  `fosgit-popup-default-setup'.

`:refresh-function'
  When this function is specified, then it is used instead of
  calling `fosgit-popup-insert-section' three times with symbols
  `fosgit-popup-switch-button', `fosgit-popup-option-button', and
  finally `fosgit-popup-action-button' as argument.

`:man-page'
  The name of the manpage to be displayed when the user requests
  help for a switch or argument.

\(fn NAME DOC [GROUP [MODE [OPTION]]] :KEYWORD VALUE...)"
  (declare (indent defun) (doc-string 2))
  (let* ((grp  (unless (keywordp (car args)) (pop args)))
         (mode (unless (keywordp (car args)) (pop args)))
         (opt  (symbol-name name))
         (opt  (if (keywordp (car args))
                   (intern (concat (if (string-suffix-p "-popup" opt)
                                       (substring opt 0 -6)
                                     opt)
                                   "-arguments"))
                 (eval (pop args)))))
    `(progn
       (defun ,name (&optional arg) ,doc
         (interactive "P")
         (fosgit-invoke-popup ',name ,mode arg))
       (defvar ,name
         (list :variable ',opt ,@args))
       (fosgit-define-popup-keys-deferred ',name)
       ,@(when opt
           `((defcustom ,opt (plist-get ,name :default-arguments)
               ""
               ,@(and grp (list :group grp))
               :type '(repeat (string :tag "Argument")))
             (defun ,opt ()
               (if (eq fosgit-current-popup ',name)
                   fosgit-current-popup-args
                 ,opt))
             (put ',opt 'definition-name ',name))))))

(defun fosgit-define-popup-switch (popup key desc switch
                                        &optional enable at prepend)
  "In POPUP, define KEY as SWITCH.

POPUP is a popup command defined using `fosgit-define-popup'.
SWITCH is a string representing an argument that takes no value.
KEY is a character representing the second event in the sequence
of keystrokes used to toggle the argument.  (The first event, the
prefix, is shared among all switches, defaults to -, and can be
changed in `fosgit-popup-mode-keymap').

DESC is a string describing the purpose of the argument, it is
displayed in the popup.

If optional ENABLE is non-nil then the switch is on by default.

SWITCH is inserted after all other switches already defined for
POPUP, unless optional PREPEND is non-nil, in which case it is
placed first.  If optional AT is non-nil then it should be the
KEY of another switch already defined for POPUP, the argument
is then placed before or after AT, depending on PREPEND."
  (declare (indent defun))
  (fosgit-define-popup-key popup :switches key
    (list desc switch enable) at prepend))

(defun fosgit-define-popup-option (popup key desc option
                                        &optional reader value at prepend)
  "In POPUP, define KEY as OPTION.

POPUP is a popup command defined using `fosgit-define-popup'.
OPTION is a string representing an argument that takes a value.
KEY is a character representing the second event in the sequence
of keystrokes used to set the argument's value.  (The first
event, the prefix, is shared among all options, defaults to =,
and can be changed in `fosgit-popup-mode-keymap').

DESC is a string describing the purpose of the argument, it is
displayed in the popup.

If optional VALUE is non-nil then the option is on by default,
and VALUE is its default value.

OPTION is inserted after all other options already defined for
POPUP, unless optional PREPEND is non-nil, in which case it is
placed first.  If optional AT is non-nil then it should be the
KEY of another option already defined for POPUP, the argument
is then placed before or after AT, depending on PREPEND."
  (declare (indent defun))
  (fosgit-define-popup-key popup :options key
    (list desc option reader value) at prepend))

(defun fosgit-define-popup-variable (popup key desc command formatter
                                          &optional at prepend)
  "In POPUP, define KEY as COMMAND.

POPUP is a popup command defined using `fosgit-define-popup'.
COMMAND is a command which calls `fosgit-popup-set-variable'.
FORMATTER is a function which calls `fosgit-popup-format-variable'.
These two functions have to be called with the same arguments.

KEY is a character representing the event used interactively call
the COMMAND.

DESC is the variable or a representation thereof.  It's not
actually used for anything.

COMMAND is inserted after all other commands already defined for
POPUP, unless optional PREPEND is non-nil, in which case it is
placed first.  If optional AT is non-nil then it should be the
KEY of another command already defined for POPUP, the command
is then placed before or after AT, depending on PREPEND."
  (declare (indent defun))
  (fosgit-define-popup-key popup :variables key
    (list desc command formatter) at prepend))

(defun fosgit-define-popup-action (popup key desc command
                                        &optional at prepend)
  "In POPUP, define KEY as COMMAND.

POPUP is a popup command defined using `fosgit-define-popup'.
COMMAND can be any command but should usually consume the popup
arguments in its `interactive' form.
KEY is a character representing the event used invoke the action,
i.e. to interactively call the COMMAND.

DESC is a string describing the purpose of the action, it is
displayed in the popup.

COMMAND is inserted after all other commands already defined for
POPUP, unless optional PREPEND is non-nil, in which case it is
placed first.  If optional AT is non-nil then it should be the
KEY of another command already defined for POPUP, the command
is then placed before or after AT, depending on PREPEND."
  (declare (indent defun))
  (fosgit-define-popup-key popup :actions key
    (list desc command) at prepend))

(defun fosgit-define-popup-sequence-action
    (popup key desc command &optional at prepend)
  "Like `fosgit-define-popup-action' but for `:sequence-action'."
  (declare (indent defun))
  (fosgit-define-popup-key popup :sequence-actions key
    (list desc command) at prepend))

(defconst fosgit-popup-type-plural-alist
  '((:switch . :switches)
    (:option . :options)
    (:variable . :variables)
    (:action . :actions)
    (:sequence-action . :sequence-actions)))

(defun fosgit-popup-pluralize-type (type)
  (or (cdr (assq type fosgit-popup-type-plural-alist))
      type))

(defun fosgit-define-popup-key
    (popup type key def &optional at prepend)
  "In POPUP, define KEY as an action, switch, or option.
It's better to use one of the specialized functions
  `fosgit-define-popup-action',
  `fosgit-define-popup-sequence-action',
  `fosgit-define-popup-switch',
  `fosgit-define-popup-option', or
  `fosgit-define-popup-variable'."
  (declare (indent defun))
  (setq type (fosgit-popup-pluralize-type type))
  (if (memq type '(:switches :options :variables :actions :sequence-actions))
      (if (boundp popup)
          (let* ((plist (symbol-value popup))
                 (value (plist-get plist type))
                 (elt   (assoc key value)))
            (if elt
                (setcdr elt def)
              (setq elt (cons key def)))
            (if at
                (when (setq at (cl-member at value :key 'car-safe :test 'equal))
                  (setq value (cl-delete key value :key 'car-safe :test 'equal))
                  (if prepend
                      (progn (push (car at) (cdr at))
                             (setcar at elt))
                    (push elt (cdr at))))
              (setq value (cl-delete key value :key 'car-safe :test 'equal)))
            (unless (assoc key value)
              (setq value (if prepend
                              (cons elt value)
                            (append value (list elt)))))
            (set popup (plist-put plist type value)))
        (push (list type key def at prepend)
              (get popup 'fosgit-popup-deferred)))
    (error "Unknown popup event type: %s" type)))

(defun fosgit-define-popup-keys-deferred (popup)
  (dolist (args (get popup 'fosgit-popup-deferred))
    (condition-case err
        (apply #'fosgit-define-popup-key popup args)
      ((debug error)
       (display-warning 'fosgit (error-message-string err) :error))))
  (put popup 'fosgit-popup-deferred nil))

(defun fosgit-change-popup-key (popup type from to)
  "In POPUP, bind TO to what FROM was bound to.
TYPE is one of `:action', `:sequence-action', `:switch', or
`:option'.  Bind TO and unbind FROM, both are characters."
  (--if-let (assoc from (plist-get (symbol-value popup)
                                   (fosgit-popup-pluralize-type type)))
      (setcar it to)
    (message "fosgit-change-popup-key: FROM key %c is unbound" from)))

(defun fosgit-remove-popup-key (popup type key)
  "In POPUP, remove KEY's binding of TYPE.
POPUP is a popup command defined using `fosgit-define-popup'.
TYPE is one of `:action', `:sequence-action', `:switch', or
`:option'.  KEY is the character which is to be unbound."
  (setq type (fosgit-popup-pluralize-type type))
  (let* ((plist (symbol-value popup))
         (alist (plist-get plist type))
         (value (assoc key alist)))
    (set popup (plist-put plist type (delete value alist)))))

;;; Invoke

(defvar-local fosgit-popup-previous-winconf nil)

(defun fosgit-invoke-popup (popup mode arg)
  (let* ((def     (symbol-value popup))
         (val     (symbol-value (plist-get def :variable)))
         (default (plist-get def :default-action))
         (local   (plist-get def :use-prefix))
         (use-prefix (or local fosgit-popup-use-prefix-argument)))
    (cond
     ((and arg (eq fosgit-popup-use-prefix-argument 'disabled))
      (customize-option-other-window 'fosgit-popup-use-prefix-argument)
      (error (concat "The meaning of prefix arguments has changed.  "
                     "Please explicitly enable their use again.")))
     ((or (and (eq use-prefix 'default) arg)
          (and (eq use-prefix 'popup) (not arg)))
      (if default
          (let ((fosgit-current-popup (list popup 'default))
                (fosgit-current-popup-args
                 (let ((fosgit-this-popup popup)
                       (fosgit-this-popup-events nil))
                   (fosgit-popup-default-setup val def)
                   (fosgit-popup-get-args))))
            (when (and arg (listp arg))
              (setq current-prefix-arg (and (not (= (car arg) 4))
                                            (list (/ (car arg) 4)))))
            (call-interactively default))
        (message "%s has no default action; showing popup instead." popup)
        (fosgit-popup-mode-setup popup mode)))
     ((memq use-prefix '(disabled default popup nil))
      (fosgit-popup-mode-setup popup mode)
      (when fosgit-popup-show-help-echo
        (message (concat "Type C-h i to view popup manual, "
                         "? to describe an argument or action."))))
     (local
      (error "Invalid :use-prefix popup property value: %s" use-prefix))
     (t
      (error "Invalid fosgit-popup-use-prefix-argument value: %s" use-prefix)))))

(defun fosgit-invoke-popup-switch (event)
  (interactive (list last-command-event))
  (--if-let (fosgit-popup-lookup event :switches)
      (progn
        (setf (fosgit-popup-event-use it)
              (not (fosgit-popup-event-use it)))
        (fosgit-refresh-popup-buffer))
    (user-error "%c isn't bound to any switch" event)))

(defun fosgit-invoke-popup-option (event)
  (interactive (list last-command-event))
  (--if-let (fosgit-popup-lookup event :options)
      (progn
        (if (fosgit-popup-event-use it)
            (setf (fosgit-popup-event-use it) nil)
          (let* ((arg (fosgit-popup-event-arg it))
                 (val (funcall
                       (fosgit-popup-event-fun it)
                       (concat arg (unless (string-match-p "=$" arg) ": "))
                       (fosgit-popup-event-val it))))
            (setf (fosgit-popup-event-use it) t)
            (setf (fosgit-popup-event-val it) val)))
        (fosgit-refresh-popup-buffer))
    (user-error "%c isn't bound to any option" event)))

(defun fosgit-invoke-popup-action (event)
  (interactive (list last-command-event))
  (let ((action   (fosgit-popup-lookup event :actions))
        (variable (fosgit-popup-lookup event :variables)))
    (if (or action variable)
        (let ((fosgit-current-popup fosgit-this-popup)
              (fosgit-current-popup-args (fosgit-popup-get-args))
              (command (fosgit-popup-event-fun (or action variable))))
          (when action
            (fosgit-popup-quit))
          (call-interactively command)
          (setq this-command command)
          (unless action
            (fosgit-refresh-popup-buffer)))
      (if (eq event ?q)
          (fosgit-popup-quit)
        (user-error "%c isn't bound to any action" event)))))

(defun fosgit-popup-set-variable
    (variable choices &optional default other)
  (--if-let (--if-let (fosgit-git-string "config" "--local" variable)
                (cadr (member it choices))
              (car choices))
      (fosgit-set it variable)
    (fosgit-call-git "config" "--unset" variable))
  (fosgit-refresh)
  (message "%s %s" variable
           (fosgit-popup-format-variable-1 variable choices default other)))

(defun fosgit-popup-quit ()
  "Quit the current popup command without invoking an action."
  (interactive)
  (let ((winconf fosgit-popup-previous-winconf))
    (if (derived-mode-p 'fosgit-popup-mode)
        (kill-buffer)
      (fosgit-popup-help-mode -1)
      (kill-local-variable 'fosgit-popup-previous-winconf))
    (when winconf
      (set-window-configuration winconf))))

(defun fosgit-popup-read-number (prompt &optional default)
  "Like `read-number' but DEFAULT may be a numeric string."
  (read-number prompt (if (stringp default)
                          (string-to-number default)
                        default)))

;;; Save

(defun fosgit-popup-set-default-arguments (arg)
  "Set default value for the arguments for the current popup.
Then close the popup without invoking an action; unless a prefix
argument is used in which case the popup remains open.

For a popup named `NAME-popup' that usually means setting the
value of the custom option `NAME-arguments'."
  (interactive "P")
  (customize-set-variable (fosgit-popup-get :variable)
                          (fosgit-popup-get-args))
  (unless arg (fosgit-popup-quit)))

(defun fosgit-popup-save-default-arguments (arg)
  "Save default value for the arguments for the current popup.
Then close the popup without invoking an action; unless a prefix
argument is used in which case the popup remains open.

For a popup named `NAME-popup' that usually means saving the
value of the custom option `NAME-arguments'."
  (interactive "P")
  (customize-save-variable (fosgit-popup-get :variable)
                           (fosgit-popup-get-args))
  (unless arg (fosgit-popup-quit)))

;;; Help

(defun fosgit-popup-toggle-show-common-commands ()
  "Show or hide an additional section with common commands.
The commands listed in this section are common to all popups
and are defined in `fosgit-popup-mode-map' (which see)."
  (interactive)
  (setq fosgit-popup-show-common-commands
        (not fosgit-popup-show-common-commands))
  (fosgit-refresh-popup-buffer)
  (fit-window-to-buffer))

(defun fosgit-popup-help ()
  "Show help for the argument or action at point."
  (interactive)
  (let* ((man (fosgit-popup-get :man-page))
         (key (read-key-sequence
               (concat "Describe key" (and man " (? for manpage)") ": ")))
         (int (aref key (1- (length key))))
         (def (or (lookup-key (current-local-map)  key t)
                  (lookup-key (current-global-map) key))))
    (pcase def
      (`fosgit-invoke-popup-switch
       (fosgit-popup-manpage man (fosgit-popup-lookup int :switches)))
      (`fosgit-invoke-popup-option
       (fosgit-popup-manpage man (fosgit-popup-lookup int :options)))
      (`fosgit-popup-help
       (fosgit-popup-manpage man nil))
      ((or `self-insert-command
           `fosgit-invoke-popup-action)
       (setq def (or (fosgit-popup-lookup int :actions)
                     (fosgit-popup-lookup int :variables)))
       (if def
           (fosgit-popup-describe-function (fosgit-popup-event-fun def))
         (ding)
         (message nil)))
      (`nil (ding)
            (message nil))
      (_    (fosgit-popup-describe-function def)))))

(defun fosgit-popup-manpage (topic arg)
  (unless topic
    (user-error "No man page associated with %s"
                (fosgit-popup-get :man-page)))
  (when arg
    (setq arg (fosgit-popup-event-arg arg)))
  (let ((winconf (current-window-configuration)) buffer)
    (pcase fosgit-popup-manpage-package
      (`woman (delete-other-windows)
              (split-window-below)
              (with-no-warnings ; display-buffer-function is obsolete
                (let ((display-buffer-alist nil)
                      (display-buffer-function nil))
                  (woman topic)))
              (setq buffer (current-buffer)))
      (`man   (cl-letf (((symbol-function #'fboundp) (lambda (_) nil)))
                (setq buffer (man topic)))
              (delete-other-windows)
              (split-window-below)
              (set-window-buffer (selected-window) buffer)))
    (with-current-buffer buffer
      (setq fosgit-popup-previous-winconf winconf)
      (fosgit-popup-help-mode)
      (fit-window-to-buffer (next-window))
      (if (and arg
               (Man-find-section "OPTIONS")
               (re-search-forward (format "^[\t\s]+\\(-., \\)*?%s[=\n]" arg)
                                  (save-excursion
                                    (Man-next-section 1)
                                    (point))
                                  t))
          (goto-char (1+ (match-beginning 0)))
        (goto-char (point-min))))))

(defun fosgit-popup-describe-function (function)
  (let ((winconf (current-window-configuration)))
    (delete-other-windows)
    (split-window-below)
    (other-window 1)
    (with-no-warnings ; display-buffer-function is obsolete
      (let ((display-buffer-alist nil)
            (display-buffer-function nil))
        (describe-function function)))
    (fit-window-to-buffer)
    (other-window 1)
    (setq fosgit-popup-previous-winconf winconf)
    (fosgit-popup-help-mode)))

(defun fosgit-popup-info ()
  "Show the popup manual."
  (interactive)
  (let ((winconf (current-window-configuration)))
    (delete-other-windows)
    (split-window-below)
    (info "(fosgit-popup.info)Usage")
    (fosgit-popup-help-mode)
    (setq fosgit-popup-previous-winconf winconf))
  (fosgit-popup-help-mode)
  (fit-window-to-buffer (next-window)))

(define-minor-mode fosgit-popup-help-mode
  "Auxiliary minor mode used to restore previous window configuration.
When some sort of help buffer is created from within a popup,
then this minor mode is turned on in that buffer, so that when
the user quits it, the previous window configuration is also
restored."
  :keymap '(([remap Man-quit]    . fosgit-popup-quit)
            ([remap Info-exit]   . fosgit-popup-quit)
            ([remap quit-window] . fosgit-popup-quit)))

;;; Modes

(define-derived-mode fosgit-popup-mode fundamental-mode "FosgitPopup"
  "Major mode for infix argument popups."
  :mode 'fosgit-popup
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local scroll-margin 0)
  (setq-local fosgit-popup-show-common-commands fosgit-popup-show-common-commands)
  (hack-dir-local-variables-non-file-buffer))

(put 'fosgit-popup-mode 'mode-class 'special)

(defun fosgit-popup-default-setup (val def)
  (if (--when-let (fosgit-popup-get :sequence-predicate)
        (funcall it))
      (fosgit-popup-put :actions (fosgit-popup-convert-actions
                                 val (fosgit-popup-get :sequence-actions)))
    (fosgit-popup-put :variables (fosgit-popup-convert-variables
                                 val (plist-get def :variables)))
    (fosgit-popup-put :switches  (fosgit-popup-convert-switches
                                 val (plist-get def :switches)))
    (fosgit-popup-put :options   (fosgit-popup-convert-options
                                 val (plist-get def :options)))
    (fosgit-popup-put :actions   (fosgit-popup-convert-actions
                                 val (plist-get def :actions)))))

(defun fosgit-popup-mode-setup (popup mode)
  (let ((val (symbol-value (plist-get (symbol-value popup) :variable)))
        (def (symbol-value popup)))
    (fosgit-popup-mode-display-buffer (get-buffer-create
                                      (format "*%s*" popup))
                                     (or mode 'fosgit-popup-mode))
    (setq fosgit-this-popup popup)
    (if (bound-and-true-p fosgit-popup-setup-hook) ; obsolete
        (run-hook-with-args 'fosgit-popup-setup-hook val def)
      (funcall (or (fosgit-popup-get :setup-function)
                   'fosgit-popup-default-setup)
               val def)))
  (fosgit-refresh-popup-buffer)
  (fit-window-to-buffer nil nil (line-number-at-pos (point-max))))

(defun fosgit-popup-mode-display-buffer (buffer mode)
  (let ((winconf (current-window-configuration)))
    (select-window (display-buffer buffer fosgit-popup-display-buffer-action))
    (funcall mode)
    (setq fosgit-popup-previous-winconf winconf)))

(defvar fosgit-refresh-popup-buffer-hook nil
  "Hook run by `fosgit-refresh-popup-buffer'.

The hook is run right after inserting the representation of the
popup events but before optionally inserting the representation
of events shared by all popups and before point is adjusted.")

(defun fosgit-refresh-popup-buffer ()
  (let* ((inhibit-read-only t)
         (button (button-at (point)))
         (prefix (and button (button-get button 'prefix)))
         (event  (and button (button-get button 'event))))
    (erase-buffer)
    (save-excursion
      (--if-let (fosgit-popup-get :refresh-function)
          (funcall it)
        (fosgit-popup-insert-section 'fosgit-popup-switch-button)
        (fosgit-popup-insert-section 'fosgit-popup-option-button)
        (fosgit-popup-insert-section 'fosgit-popup-variable-button)
        (fosgit-popup-insert-section 'fosgit-popup-action-button))
      (run-hooks 'fosgit-refresh-popup-buffer-hook)
      (when fosgit-popup-show-common-commands
        (fosgit-popup-insert-command-section
         'fosgit-popup-internal-command-button
         fosgit-popup-common-commands)))
    (set-buffer-modified-p nil)
    (when event
      (while (and (ignore-errors (forward-button 1))
                  (let ((b (button-at (point))))
                    (or (not (equal (button-get b 'prefix) prefix))
                        (not (equal (button-get b 'event)  event)))))))))

;;; Draw

(defvar fosgit-popup-min-padding 3
  "Minimal amount of whitespace between columns in popup buffers.")

(defun fosgit-popup-insert-section (type &optional spec heading)
  (if (not spec)
      (progn (setq spec (fosgit-popup-get (button-type-get type 'property)))
             (when spec
               (if (or (stringp (car spec))
                       (functionp (car spec)))
                   (--each (--partition-by-header
                            (or (stringp it) (functionp it))
                            spec)
                     (fosgit-popup-insert-section type (cdr it) (car it)))
                 (fosgit-popup-insert-section type spec))))
    (let* ((formatter (button-type-get type 'formatter))
           (items (mapcar (lambda (ev)
                            (and ev (or (funcall formatter type ev) '(""))))
                          (or spec (fosgit-popup-get
                                    (button-type-get type 'property)))))
           (maxcols (button-type-get type 'maxcols))
           (pred (fosgit-popup-get :sequence-predicate)))
      (if (and pred (funcall pred))
          (setq maxcols nil)
        (cl-typecase maxcols
          (keyword (setq maxcols (fosgit-popup-get maxcols)))
          (symbol  (setq maxcols (symbol-value maxcols)))))
      (when items
        (if (functionp heading)
            (when (setq heading (funcall heading))
              (insert heading ?\n))
          (unless heading
            (setq heading (button-type-get type 'heading)))
          (insert (propertize heading 'face 'fosgit-popup-heading))
          (unless (string-match "\n$" heading)
            (insert "\n")))
        (when heading
          (let ((colwidth
                 (+ (apply 'max (mapcar (lambda (e) (length (car e))) items))
                    fosgit-popup-min-padding)))
            (dolist (item items)
              (unless (bolp)
                (let ((padding (- colwidth (% (current-column) colwidth))))
                  (if (and (< (+ (current-column) padding colwidth)
                              (window-width))
                           (< (ceiling (/ (current-column) (* colwidth 1.0)))
                              (or maxcols 1000)))
                      (insert (make-string padding ?\s))
                    (insert "\n"))))
              (unless (equal item '(""))
                (if item
                    (apply 'insert-button item)
                  (insert ?\s)))))
          (insert (if (= (char-before) ?\n) "\n" "\n\n")))))))

(defun fosgit-popup-format-argument-button (type ev)
  (list (format-spec
         (button-type-get type 'format)
         `((?k . ,(propertize (concat
                               (--when-let (button-type-get type 'prefix)
                                 (char-to-string it))
                               (fosgit-popup-event-keydsc ev))
                              'face 'fosgit-popup-key))
           (?d . ,(fosgit-popup-event-dsc ev))
           (?a . ,(propertize (fosgit-popup-event-arg ev)
                              'face (if (fosgit-popup-event-use ev)
                                        'fosgit-popup-argument
                                      'fosgit-popup-disabled-argument)))
           (?v . ,(let ((val (fosgit-popup-event-val ev)))
                    (if (and (fosgit-popup-event-use ev)
                             (not (equal val "")))
                        (propertize (format "\"%s\"" val)
                                    'face 'fosgit-popup-option-value)
                      "")))))
        'type type 'event (fosgit-popup-event-key ev)))

(defun fosgit-popup-format-variable-button (type ev)
  (list (format-spec
         (button-type-get type 'format)
         `((?k . ,(propertize (fosgit-popup-event-keydsc ev)
                              'face 'fosgit-popup-key))
           (?d . ,(funcall (fosgit-popup-event-arg ev)))))
        'type type 'event (fosgit-popup-event-key ev)))

(defun fosgit-popup-format-variable
    (variable choices &optional default other width)
  (concat variable
          (if width (make-string (- width (length variable)) ?\s) " ")
          (fosgit-popup-format-variable-1 variable choices default other)))

(defun fosgit-popup-format-variable-1
    (variable choices &optional default other)
  (let ((local  (fosgit-git-string "config" "--local"  variable))
        (global (fosgit-git-string "config" "--global" variable)))
    (when other
      (--if-let (fosgit-git-string "config" other)
          (setq other (concat other ":" it))
        (setq other nil)))
    (concat
     (propertize "[" 'face 'fosgit-popup-disabled-argument)
     (mapconcat
      (lambda (choice)
        (propertize choice 'face (if (equal choice local)
                                     'fosgit-popup-option-value
                                   'fosgit-popup-disabled-argument)))
      choices
      (propertize "|" 'face 'fosgit-popup-disabled-argument))
     (when (or global other default)
       (concat
        (propertize "|" 'face 'fosgit-popup-disabled-argument)
        (cond (global
               (propertize (concat "global:" global)
                           'face (cond (local
                                        'fosgit-popup-disabled-argument)
                                       ((member global choices)
                                        'fosgit-popup-option-value)
                                       (t
                                        'font-lock-warning-face))))
              (other
               (propertize other
                           'face (if local
                                     'fosgit-popup-disabled-argument
                                   'fosgit-popup-option-value)))
              (default
               (propertize (concat "default:" default)
                           'face (if local
                                     'fosgit-popup-disabled-argument
                                   'fosgit-popup-option-value))))))
     (propertize "]" 'face 'fosgit-popup-disabled-argument))))

(defun fosgit-popup-format-action-button (type ev)
  (let* ((dsc (fosgit-popup-event-dsc ev))
         (fun (and (functionp dsc) dsc)))
    (when fun
      (setq dsc
            (-when-let (branch (funcall fun))
              (if (next-single-property-change 0 'face (concat "0" branch))
                  branch
                (fosgit-branch-set-face branch)))))
    (when dsc
      (list (format-spec
             (button-type-get type 'format)
             `((?k . ,(propertize (fosgit-popup-event-keydsc ev)
                                  'face 'fosgit-popup-key))
               (?d . ,dsc)
               (?D . ,(if (and (not fun)
                               (eq (fosgit-popup-event-fun ev)
                                   (fosgit-popup-get :default-action)))
                          (propertize dsc 'face 'bold)
                        dsc))))
            'type type 'event (fosgit-popup-event-key ev)))))

(defun fosgit-popup-insert-command-section (type spec)
  (fosgit-popup-insert-section
   type (mapcar (lambda (elt)
                  (list (car (where-is-internal (cadr elt)
                                                (current-local-map)))
                        (car elt)))
                spec)))

(defun fosgit-popup-format-command-button (type elt)
  (nconc (fosgit-popup-format-action-button
          type (make-fosgit-popup-event :key (car  elt)
                                       :dsc (cadr elt)))
         (list 'function (lookup-key (current-local-map) (car elt)))))

;;; Utilities

(defun fosgit-popup-import-file-args (args files)
  (if files
      (cons (concat "-- " (mapconcat #'identity files ",")) args)
    args))

(defun fosgit-popup-export-file-args (args)
  (let ((files (--first (string-prefix-p "-- " it) args)))
    (when files
      (setq args  (remove files args)
            files (split-string (substring files 3) ",")))
    (list args files)))

;;; fosgit-popup.el ends soon

(defconst fosgit-popup-font-lock-keywords
  (eval-when-compile
    `((,(concat "(\\(fosgit-define-popup\\)\\_>"
                "[ \t'\(]*"
                "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
       (1 'font-lock-keyword-face)
       (2 'font-lock-function-name-face nil t)))))

(font-lock-add-keywords 'emacs-lisp-mode fosgit-popup-font-lock-keywords)

(provide 'fosgit-popup)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fosgit-popup.el ends here
