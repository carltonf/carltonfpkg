;;; carltonfpkg.el --- Misc collection of personal elisp code

;; This file is NOT part of Emacs.

;; Copyright (C) 2013 Carl Xiong <xiongc05@gmail.com>
;; Filename: carltonfpkg.el
;; Version: $Revision: 0.1 $
;; Author: Carl Xiong <xiongc05@gmail.com>
;; Created: 2013-12-03

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more
;; details. http://www.gnu.org/copyleft/gpl.html
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If you did not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:
;; Personal collection of Elisp utilities.
;; Everything that is not configuration or a complete package goes here.
;;
;; All should live under `myi-' namespace. (my Intermediate)


;;; History:
;; 2013/12/03 Carl Xiong
;;     A brutal inclusion of all of my elisp bits from imit.el

;;; Code:
(eval-when-compile (require 'cl))
(require 'dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables Definitions, put here at the very beginnings to avoid
;;; compilation warnings
;;;
(defvar common-current-time-formats
  '(("Date and time" . "%b-%d-%Y %H:%M:%S %Z")
    ("Only date" . "%b-%d-%Y")
    ("Date with weekday" . " %a %b-%d-%Y")
    ("Only time" . "%H:%M:%S")
    ("Time with timezone" . "%H:%M:%S %Z"))
  "An association list. Several commonly used format of date and
time to be used with `insert-current-date-time'. See `format-time-string' for other
choices.

NOTE: The first format is hard-coded as default.")

(defvar Info-node-boundary-min nil
  "Minimum buffer position of current node content.")
(defvar Info-node-boundary-max nil
  "Maximum buffer position of current node content.")

;;;;;;;;;;;;;;;;
;;; Generic functionality
(defmacro with-trivial-minor-mode (keymap msg
                                   &optional confirm-sexp cancel-sexp exit-sexp)
  "Macro that defines a fake minor mode that temporarily enable KEYMAP globally.
Any other key event would quit this special mode.

WARNING: for anything slightly more complex than several hotkeys,
use minor mode instead.

KEYMAP is an association list in the forms of ((key . sexp) ...),
you might want to add a helper here for more complicated
mappings.

MSG is a message string in the mini-buffer that indicates the
current state.

CONFIRM-SEXP, a sexp gets runs at confirmation, confirm key is
hard-coded to Enter.

CANCEL-SEXP, a sexp gets run at cancellation, cancel key is
hard-coded to C-g.

UNWIND-SEXP, a sexp always gets run as in UNWINDFORMS in
`unwind-protect'."
  (let ((key-mapped-sexp (make-symbol "key-mapped-sexp")))
    `(unwind-protect
         (let ((,key-mapped-sexp t))
           (while ,key-mapped-sexp
             (eval ,key-mapped-sexp)
             (message ,msg)
             (setq ,key-mapped-sexp (cdr (assoc (read-key) ,keymap))))
           (case last-input-event
             (?\r                       ; confirmation
              (eval ,confirm-sexp))
             (?\C-G                     ; cancel
              (eval ,cancel-sexp))
             (otherwise                 ;replay whatever other keys
              (setq unread-command-events (list last-input-event)))))
       ,exit-sexp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: ICRepos Manager
;;;###autoload
(defun myi-icrepos-manager (log-path)
  (with-current-buffer (find-file-noselect log-path)
    (setq default-directory "~/local")))
;; TODO a major mode for this?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhanced Buffer Management
;;;
(defun switch-to-last-buffer ()
  "Switch to previously visited buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

;;;; pop up previous buffer in the other window without selection
(defun pop-up-previous-buffer-other-window ()
  "Switch to previously visited buffer."
  (interactive)
  (display-buffer (other-buffer)))

(defun my-purge-useless-buffers ()
  "Clean up some useless buffers. Currently only 'ecpiped-*' buffers.
TODO This function is very basic, some parts are reinventing of
the wheel and some are too primitive. We'd better utilize/enhance
existing frameworks. e.g. `clean-buffer-list' or marks/filters in
`ibuffer'."
  (interactive)
  (let ((result-list-buffer-name "*Buffers-To-Be-Purged*")
        result-list-buffer
        buf-name
        buf-list-to-kill
        old-window-conf)
    (setq old-window-conf (current-window-configuration))
    (dolist (buf (buffer-list))
      (setq buf-name (buffer-name buf))
      ;; to-be-purged buffer list construct
      (if (or (string/begins-with buf-name "ecpiped-")
              nil)
          (add-to-list 'buf-list-to-kill buf-name)))
    (if (not (null buf-list-to-kill))
        (progn
          (setq result-list-buffer (get-buffer-create result-list-buffer-name))
          (with-help-window result-list-buffer-name
            (mapc (function (lambda (str)
                              (princ (format "%s    " str)))) buf-list-to-kill))
          (if (y-or-n-p "Kill all these buffers [y/n]: ")
              (progn (mapc 'kill-buffer buf-list-to-kill)
                     (message "All buffers deleted."))
            (message "Nothing to done."))
          (set-window-configuration old-window-conf)
          (kill-buffer result-list-buffer))
      (message "No buffer need purging."))))

;;; clone-any buffer.
(defun clone-any-buffer (&optional newname)
  "Clone any clonable buffer. For file-related buffer, create a
temporary buffer and insert content their, while for other
buffers it will call `clone-buffer'. A crude implementation."
  (interactive
   (list (if current-prefix-arg
             (read-buffer "Name of new cloned buffer: " (current-buffer)))))
  (if (buffer-file-name)
      (let* ((orig-buf (current-buffer))
             (orig-buf-beg (point-min))
             (orig-buf-end (point-max))
             (orig-buf-pos (point))
             (mode major-mode)
             (cloned-buf (generate-new-buffer
                          (or newname (format "[cloned]%s" (buffer-name orig-buf))))))
        (with-current-buffer cloned-buf
          (insert-buffer-substring orig-buf orig-buf-beg orig-buf-end)
          ;; setup major modes
          (funcall mode)
          ;; not changed
          (set-buffer-modified-p nil)
          ;; clear undo history
          (setq buffer-undo-list nil)
          ;; goto the same position
          (goto-char orig-buf-pos))
        (pop-to-buffer cloned-buf))
    (clone-buffer newname t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhanced Register Facilities
;;;
(require 'register)

(defun buffer-file-to-register (register &optional arg)
  "Store current buffer and its associated file in register REGISTER.
If the buffer has no file associated, nil will be stored instead.
Use \\[jump-to-register] to go to that buffer. Argument is a
character, naming the register. "
  (interactive "cBuffer to register: \nP")
  (let ((buff (current-buffer))
        (file (buffer-file-name (current-buffer))))
    (set-register register (list 'buffer-file buff file))))

(defun func-to-register (register func)
  "A non-interactive function for Elisp code only. When you call
\\[jump-to-register], func will be inovked. Basically this is a
register-based key binding mechanism. Compared to `defun', this
is only for temporary function key binding and can be used for
any functions rather than only interactive ones."
  (set-register register (list 'function func)))

(defadvice jump-to-register (around my-extended-jump-to-register
                                    (register &optional delete))
  "extended register functionality support. Currently we can save
  buffer-name (not location) and functions into register. "
  (interactive "cJump to register: \nP")
  (let ((val (get-register register)))
    ;; add buffer switching support to register jump
    (cond
     ((and (consp val) (eq (car val) 'buffer-file))
      (let ((buff (nth 1 val))
            (file (nth 2 val)))
        (cond
         (file
          (find-file file))
         ((bufferp buff)
          (switch-to-buffer buff))
         (t
          (error "The buffer can't be visited. (Killed and no file associated)")))))

     ;; add func support
     ((and (consp val) (eq (car val) 'function))
      ;; (fset 'func-internal (car (cdr val)))
      ;; (func-internal)
      (funcall (car (cdr val))))

     ;; call to original function
     (t
      ad-do-it))))

(ad-activate #'jump-to-register)

;;; TODO: rewrite list-registers to have better display (at least listing order
;;; is sorted according to content not register name)
(defadvice describe-register-1 (around my-exteneded-register-description
                                       (register &optional verbose))
  "Extended register description support."
  (princ "Register ")
  (princ (single-key-description register))
  (princ " contains ")
  (let ((val (get-register register)))
    (cond
     ;; add buffer-file support
     ((and (consp val) (eq (car val) 'buffer-file))
      (princ "a buffer-file reference:\n    buffer: ")
      (princ (nth 1 val))
      (princ ",\n    file: ")
      (princ (nth 2 val))
      (princ ".\n"))

     ;; add func support
     ((and (consp val) (eq (car val) 'function))
      (princ "a function:\n    func: ")
      (princ (cdr val)))

     ;; run the original function
     (t
      ad-do-it))))

(ad-activate 'describe-register-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Enhanced Dired
(defun dired-up-directory-in-place ()
  "Move up directory tree in current dired buffer"
  (interactive)
  (find-alternate-file ".."))

;;; visit the default directory (usually just the buffer file's directory)
(defun visit-deffault-directory (&optional arg)
  "visit the default directory (usually just the buffer file's
directory), with prefix ARG, kill current buffer"
  (interactive "P")
  (if arg
      (let ((this-buffer (current-buffer)))
        (dired-jump)
        (kill-buffer this-buffer))
    (dired-jump)))

;;: Enhance "renaming/moving" experience
(defun myi-dired-rename-file-here (fpath)
  "Rename/move a file to the current =default-directory=.

TODO integrate with dired. What about a temporary dired buffer?"
  (interactive (list (ido-read-file-name "Files to be moved HERE: "
                                         "~/Downloads")))
  (rename-file fpath default-directory)
  (message "Files moved here."))

(defun diredp-rename-this/other-file (&optional arg)
  "Without RAG, it's `diredp-rename-this-file'. Otherwise it pops
up a window for selecting files to be moved here."
  (interactive "P")
  (if arg
      (call-interactively #'myi-dired-rename-file-here)
    (call-interactively #'diredp-rename-this-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhanced Movement
;; scroll down and up without moving cursor (useful for looking code)
(defun scroll-down-1-keep-cursor ()
  "scroll down the screen 1 line without moving the cursor"
  (interactive)
  (scroll-down 1))

(defun scroll-up-1-keep-cursor ()
  "scroll down the screen 1 line without moving the cursor"
  (interactive)
  (scroll-up 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: recentloc
;;;
;;; If we need to jump around two places often, use marks `C-x C-x' and etc
;;;
;;; A much more powerful register-based location jumping mechanism.
;;;
;;; TODO:
;;; 1. nice listing of recorded locations with context
;;; 2. maybe we could enhance f10 like this too
;;; 3. Move out as a separate library
(require 'ring)

(defconst my-recentloc-ring-size 13
  "The maximum size of the `my-recentloc-ring'.")

(defvar my-recentloc-ring (make-ring my-recentloc-ring-size)
  "The location ring. `my-recentloc-ring-size' store the maximum size of the ring.")

(defvar my-cycling-start-marker nil
  "Store the position where the jumping started.")

(defvar my-recentloc-cycling-key 'f12
  "Symbol represent the cycling key.")

(require 'hl-line)
(defun my-recentloc-push-ring ()
  "Push current point onto the top of `my-recentloc-ring'"
  (interactive)
  (let ((cur-marker (point-marker)))
    (ring-insert my-recentloc-ring cur-marker)
    (message "Push %s onto recentloc ring" cur-marker)))

(defun my-recentloc-cycle-ring (&optional arg)
  "Cycle through `my-recentloc-ring'. Repeated
`my-recentloc-cycling-key' jumps to older recorded locations. Any
key other than `my-recentloc-cycling-key' will abort cycling. C-g
will take you back to where you starts. Enter will confirm the
current location. With a prefix ARG, it jumps back to where you
start cycling.

`hl-line-mode' is temporarily enabled when you are cycling."
  (interactive "P")
  (let ((goto-marker-func-with-hl
         (lambda (marker)
           (when (markerp marker)
             (hl-line-unhighlight)
             (switch-to-buffer (marker-buffer marker))
             (goto-char marker)
             (let ((hl-line-mode t)) (hl-line-highlight))))))
    (cond (arg
           (unless (funcall goto-marker-func-with-hl my-cycling-start-marker)
             (hl-line-unhighlight)
             (message "Abort: No start location of cycling.")))
          ((ring-empty-p my-recentloc-ring)
           (message "Abort: Recentloc has no records."))
          (t
           (setq my-cycling-start-marker (point-marker)) ;save start marker
           ;; A macro to enable hl-line mode only while cycling. It is used only
           ;; after you have switched to the desired buffer and should wrap
           ;; around time-consuming operations within that buffer.
           (let* ((recentloc-ring-len (ring-length my-recentloc-ring))
                  (ring-idx 0)
                  (cur-marker (ring-ref my-recentloc-ring ring-idx)))
             (funcall goto-marker-func-with-hl cur-marker)
             (with-trivial-minor-mode
              '((f12
                 . (progn
                     (recenter)
                     (setq ring-idx (1+ ring-idx))
                     (setq cur-marker (ring-ref my-recentloc-ring ring-idx))
                     (if (= ring-idx recentloc-ring-len)
                         ;; stay in the same marker twice for wrapping
                         (progn (message "Recentloc cycle wrapping...")
                                (setq ring-idx -1))
                       (funcall goto-marker-func-with-hl cur-marker)))))
              (format "Recentloc cycling to %s..." cur-marker)
              nil
              (progn
                (switch-to-buffer (marker-buffer my-cycling-start-marker))
                (goto-char my-cycling-start-marker))
              (hl-line-unhighlight)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; enhanced shell/consoles/repls
;;;
(defun myi-extended-shell-resync-dirs ()
  "If there is other windows in current frame, then change
shell's current directory to the `default-directory' of the
buffer of the `previous-window'. This serves as a replacement of
the original `shell-resync-dirs'."
  (interactive)
  (let (new-curdir)
    (when (> (length (window-list)) 1)
      (setq new-curdir (with-selected-window (previous-window)
                         default-directory))
      (process-send-string (current-buffer)
                           ;; lined with `dirs'
                           (concat "cd " new-curdir " ; \\"))))
  (shell-resync-dirs))

;;; myi-consoles BEG
(require 'eieio)
(defvar myi-consoles-mode-list '(comint-mode eshell-mode)
  "A list of modes whose buffers are considered as
'consoles' and can be switched using `myi-consoles'.")

(defvar myi-consoles-autostarts
  (list
   ;; Workhorse in-Emacs shell.
   (cons "*debug-shell*" #'shell)
   ;; A secondary in-Emacs shell
   (cons "*extra-shell*" #'shell)
   ;; `eshell' ARG works different from `shell's
   (cons "*eshell*" (lambda (ignore) (eshell))))
  "An alist of autostart consoles. The elements of
  the list should be a CONS of format (BUFNAME . FUNC), of which
  FUNC will be passed with BUFNAME only. FUNC can be any lisp
  functions, but normally it should create a buffer and set up
  its major mode. The buffer is setup only when it gets chosen,
  see `buffer-mru-list-update'.")

(defun myi-consoles-class-buffer-mru-alist-valid? (alist)
  "Validate the value of `buffer-mru-alist' slot in `myi-consoles-class'.

Each element of `buffer-mru-alist' should be a con cell in the
form (buffer-name . timestamp). And the whole list should be
sorted with timestamp ascendingly."
  (let ((last-concell (car alist)))
    (-all? (lambda (concell)
             ;; TODO we should have `time-more-p'
             (unless (time-less-p (cdr last-concell)
                                  (cdr concell))
               (setq last-concell concell)
               t))
           (cdr alist))))
(defclass myi-consoles-class ()
   ;; No initarg, this variable is exposed to outside as `buffer-mru-list'.
   ;; User will think of this only as buffer list
  ((buffer-mru-alist :initform nil
                     :type (satisfies myi-consoles-class-buffer-mru-alist-valid?)
                     :protection :private
                     :reader buffer-mru-list
                     :writer buffer-mru-list-add
                     :documentation
                     "`myi-consoles' console buffer alist in MRU
  order. To preserve the order, use designated writer.")))

(defmethod derived-member-of-mode-list? ((this myi-consoles-class) mm)
  "Test whether major MM is a derived mode from one of the
`myi-consoles-mode-list'"
  (let ((major-mode mm))
    (apply #'derived-mode-p myi-consoles-mode-list)))

(defmethod buffer-mru-list-update ((this myi-consoles-class) &optional chosen-console)
  "Update `buffer-mru-list'.

By default, it checks if there are new buffers conforming
`myi-consoles-mode-list' or new additions of
`myi-consoles-autostarts'. And thus this method should be called
before using `buffer-mru-list' to make sure the list is
up-to-date.

Or, if CHOSEN-CONSOLE is non-nil, this method will only update
CHOSEN-CONSOLE in `buffer-mru-list'. And thus should be called
when a new console is chosen. While updating list, if the
CHOSEN-CONSOLE is one of autostart, this method also makes sure
it gets started.

TODO some hooks to do auto-updates?"
  (let ((autostarts myi-consoles-autostarts))
    (with-slots (buffer-mru-alist) this
      (if chosen-console
          ;; resorting only when CHOSEN-CONSOLE is not at head
          (progn
            (unless (string-equal (caar buffer-mru-alist) chosen-console)
              (setq buffer-mru-alist
                    (remove-if #'(lambda (bt-pair)
                                   (string-equal (car bt-pair)
                                                 chosen-console))
                               buffer-mru-alist))
              (buffer-mru-list-add this chosen-console))
            ;; make sure the buffer exists
            (when (and (assoc chosen-console autostarts)
                       (not (buffer-live-p (get-buffer chosen-console))))
              (save-window-excursion
                (funcall (cdr (assoc chosen-console autostarts))
                         chosen-console))))
        ;; global update
        (setq buffer-mru-alist
              (remove-if (lambda (bt-pair)
                           (let ((bufname (car bt-pair)))
                             (and (not (assoc bufname autostarts))
                                  (not (buffer-live-p (get-buffer
                                                       bufname))))))
                         buffer-mru-alist))
        (loop for buf in (buffer-list)
              when (derived-member-of-mode-list? this
                                                 (buffer-local-value 'major-mode buf))
              do (buffer-mru-list-add this
                                      (buffer-name buf)))
        (loop for bufn in (assoc-keys autostarts)
              do (buffer-mru-list-add this bufn))))))

(defmethod buffer-mru-list-add ((this myi-consoles-class) bufname)
  "Add bufname to `buffer-mru-list' if bufname is not in the list yet"
  (unless (member bufname (buffer-mru-list this))
    (with-slots (buffer-mru-alist) this
      (setq buffer-mru-alist
            (-sort (lambda (c1 c2)
                     ;; MRU order
                     (not (time-less-p (cdr c1) (cdr c2))))
                   (apply #'list
                          (cons bufname (current-time))
                          buffer-mru-alist))))))

(defmethod buffer-mru-list ((this myi-consoles-class))
  "Return `buffer-mru-list'."
  (assoc-keys (oref this buffer-mru-alist)))

(defmethod buffer-mru-list-contains? ((this myi-consoles-class) buf)
  "Test whether BUF in contained in `buffer-mru-list'."
  (member (buffer-name buf) (buffer-mru-list this)))

(defmacro eieio-clear-method-definitions (symbal &optional class)
  "Convenient macros. Clear all method definitions for SYMBAL.
Optionally only the definitions of CLASS.

NOTE if all method definitions of SYMBAL is cleared,
`fmakunbound' the SYMBAL as well, so later we can define it with
new methods. "
  (error "Not implemented."))

(defvar myi-consoles-data (myi-consoles-class "myi-consoles")
  "Object of `myi-consoles-class' mainly holds data for
`myi-consoles', some are state data while some are customization
and etc.")

(defun myi-consoles (&optional prompt-choices-p)
  "An all-in-one command for all interactive buffers, i.e.
various REPL buffers, `eshell', `comint'-derived buffers. With no
argument, switch to last visited console. If the current buffer
is already one of the consoles, prompt to switch to another
console in current window.

Related data include stateful ones are stored in
`myi-consoles-data'."
  (interactive "P")
  (buffer-mru-list-update myi-consoles-data)
  (unless (buffer-mru-list myi-consoles-data)
    (error "Myi-Consoles: No consoles are available."))
  (let* ((console-mru-list (buffer-mru-list myi-consoles-data))
         ;; Choices of consoles are displayed when
         ;; 1. User explicitly requires.
         ;; 2. `myi-consoles' is issued while current buffer is a console.
         ;; 3. There are more than one choice in `console-mru-list'.
         ;; 4. The first candidate is an non-existent autostart buffer.
         (in-console-p (when (buffer-mru-list-contains?
                              myi-consoles-data (current-buffer))
                         (setq console-mru-list
                               (remove (buffer-name) console-mru-list))
                         t))
         (prompt-choices-p (or prompt-choices-p
                               in-console-p
                               (= (length console-mru-list) 1)
                               (not (buffer-live-p (get-buffer
                                                    (car console-mru-list))))))
         (chosen-console (if prompt-choices-p
                             (ido-completing-read
                              "Console: " console-mru-list nil t)
                           (car console-mru-list))))
    (buffer-mru-list-update myi-consoles-data chosen-console)
    ;; switching console
    ;; reuse current window if already in a console
    (if in-console-p
        ;; also bury last visited console buffer, since a switch within a
        ;; console usually means a mistake in the first, so don't mangle the
        ;; buffer list.
        (progn (bury-buffer (current-buffer))
               (switch-to-buffer chosen-console))
      (switch-to-buffer-other-window chosen-console))))
;;; myi-consoles END


;;; custom shells
;;; A template, which can be used for almost every kind of shells.
;;! make the function below can define buffers like standard `shell'
;;! maybe we can write a function to ask what shell to execute similar
;; to the `term' but with predefined list to do auto-complete
(require 'shell)

(defvar explicit-pdl-args nil
  "Used by `ipdl'")

(defvar explicit-cpan-args nil
  "Used by `icpan'")

(defun ipdl (&optional buffer)
  "Internal PDL shell in emacs, directly use the Emacs Shell mode."
  (interactive)
 ; make use of the "shell-mode"
  (let '(old-explicit-shell-file-name explicit-shell-file-name)
    (setq explicit-shell-file-name "/usr/bin/pdl")
    (setq explicit-pdl-args '("-glut"))
    (setq buffer (or buffer "Infernal PDL Shell"))
    (shell buffer)
 ; restore the all the values
    (setq explicit-shell-file-name old-explicit-shell-file-name)))

(defun icpan (&optional buffer)
  "Internal PDL shell in emacs, directly use the Emacs Shell mode."
  (interactive)

 ; make use of the "shell-mode"
  (let '(old-explicit-shell-file-name explicit-shell-file-name)
    (setq explicit-shell-file-name "/usr/bin/perlbin/core/cpan")
    (setq explicit-cpan-args nil)
    (setq buffer (or buffer "Infernal CPAN Shell"))
    (shell buffer)
 ; restore the all the values
    (setq explicit-shell-file-name old-explicit-shell-file-name)))


;;; Disabled at Tue Dec 03 14:39:23 CST 2013
; quick settings for access terms and shells
;; (defun named-term-shell (term-buffer-name)
;;   "An assistant func to run a bash-terminal emulator with a name"
;;   (if (string= (buffer-name) term-buffer-name)
;;       (bury-buffer)
;;     (if (get-buffer term-buffer-name)
;; 	(switch-to-buffer term-buffer-name)
;;       (term "/bin/bash")
;;       (rename-buffer term-buffer-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Emacs Lisp organization
;;: IMenu support for self-organized source format
;; (defun myi-emacs-lisp-setup-organization-imenu ()
;;   "Add extra organization items to imenus."
;;   (add-to-list 'imenu-generic-expression
;;                '("Section" "^;;;: \\([- +_[:alnum:]]+\\)" 1)))
;; (add-hook 'emacs-lisp-mode-hook #'myi-emacs-lisp-setup-organization-imenu)
;;
;; Superseded by `outshine' and `outline-minor-mode'
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Man Page
(require 'man)

(defun myi-list-opened-man-buffers ()
  "Return a list of opened man page buffers."
  (loop for curbuf in (buffer-list)
        if (eq (buffer-local-value 'major-mode curbuf)
               'Man-mode)
        collect (buffer-name curbuf)))

(defun my-go-to-man ()
  "Go to man page by using ido to select entries in `Man-topic-history' first.

TODO currently no match is not allowed, amend this by passing
users' input to man command. (currently mitigate through putting
input into `Man-topic-history'.)"
  (interactive)
  ;; With asynchronous call, we have to wait for a bit before we can purge the
  ;; list. `lexical-let' has to be used here, o/w the callback function will
  ;; lose the definition of `man-buf'
  ;;
  ;; TODO eliminate the needs of timer.
  (lexical-let* ((man-def (Man-default-man-entry))
                 (man-args
                  (ido-completing-read "Man Entry: " Man-topic-history
                                       nil nil nil nil
                                       (when (member man-def Man-topic-history)
                                         man-def)))
                 (man-buf (cond ((or (null man-args)
                                     (not (member man-args Man-topic-history)))
                                 ;; add to history for `M-p' completion, will
                                 ;; be purged if invalid.
                                 (add-to-history 'Man-topic-history man-args)
                                 (command-execute #'man))
                                (t
                                 (man man-args)))))
    (run-at-time "2 sec" nil
                 (lambda ()
                   (unless (buffer-name man-buf)
                     (pop Man-topic-history)
                     (delete "" Man-topic-history)
                     (delete-dups Man-topic-history)
                     (message (format "Man: purging from history invalid man entry '%s'"
                                      man-args)))))))

(defun my-man-imenu-regex-expressions ()
  "Add to `Man-mode-hook' for use, to create more index.

TODO not very smart as the current pattern matching doesn't
consider text properties."
  (setq imenu-generic-expression
        (list (list Man-arguments "^\\([A-Z][A-Z0-9 /-]+\\)$" 0)
              ;; (list (concat "CO-" Man-arguments)
              ;;       "^       \\([A-Za-z/-]+\\).*" 1)
              (list (concat Man-arguments "-CO") ;commands or operation
                    (concat "^" (make-string 7 32)
                           "\\([A-Za-z/-]+\\)"
                           ".*\n+"
                           (make-string 9 32)) 1))))

;;; Info Mode Enhancement
(eval-when-compile
  (require 'info))

(defun myi-info-mode-create-imenu-index ()
  "Create an `Imenu' index, for _current node only_."
  (goto-char Info-node-boundary-max)
  (let (current-node-imenu-index
        (idx-pattern "^ -- \\([_[:alnum:]]+\\): \\([-_[:alnum:]]+\\)")
        idx-type
        idx-name
        idx-marker-entry
        type-list)
    (while (re-search-backward idx-pattern Info-node-boundary-min t)
      (setq idx-type (match-string-no-properties 1)
            idx-name (match-string-no-properties 2)
            idx-marker-entry (cons idx-name (point-marker)))
      (if (setq type-list (assoc idx-type current-node-imenu-index))
          (setf (cdr type-list)
                (cons idx-marker-entry (cdr type-list)))
        (add-to-list 'current-node-imenu-index
                     (list idx-type idx-marker-entry))))
    current-node-imenu-index))

(defun myi-info-mode-setup-imenu ()
  "Setup a simple Imenu for info mode"
  (interactive)
  (setq imenu-create-index-function
        #'myi-info-mode-create-imenu-index))

(add-hook 'Info-mode-hook #'myi-info-mode-setup-imenu)

(defadvice Info-goto-node (after myi-info-mode-record-node-boundaries)
  (setq Info-node-boundary-min (point-min)
        Info-node-boundary-max (point-max)))
(ad-activate #'Info-goto-node)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: myi-auxiliary-buffers
;;;
;; This leads to default *scratch* buffer, used for non-important scratch
(eval-when-compile
  (require 'ido))

(defvar myi-auxiliary-buffer-alist '(("*scratch*")
                                     ("*scratch-text*"
                                      . (progn
                                          (text-mode)
                                          (visual-line-mode 1)
                                          (flyspell-mode-on))))
  "List of auxiliary buffers, which can be easily accessed
through `myi-auxiliary-buffer-switch'. New buffers can also be
added by using `myi-auxiliary-buffer-add/remove-the-current',
only added buffers can be removed though.

Each element of the list is of the format: (BUF-NAME . FORM),
the FORM is to be evaluated when this buffer is created.")

(defvar myi-auxiliary-buffer-history nil
  "Auxiliary buffer selection history.")

(defun myi-auxiliary-buffer-switch (&optional toggle-add-remove)
  "Fast switch to one buffer in `myi-auxiliary-buffer-alist'.
If the current buffer is one of the buffers, try to switch to
another one with `ido' prompt.

TOGGLE-ADD-REMOVE - if the current buffer is in auxiliary
buffers, remove the current buffer from auxiliary list.
Otherwise, add the current buffer into auxiliary list."
  (interactive "P")
  (if toggle-add-remove
      (myi-auxiliary-buffer-add/remove-the-current)
    ;; switching
    (let* ((in-aux-buffer-p (assoc (buffer-name (current-buffer)) myi-auxiliary-buffer-alist))
           ;; TODO `ido-completing-read' will damage CHOICES list in
           ;; `ido-make-choice-list' if DEF is not nil. I think this is a bug, but
           ;; here is a workaround.
           ;; Here we use `myi-auxiliary-buffer-history' to create an MRU buffer list.
           (mru-buf-candidates (let ((candidates (-uniq
                                                  (append myi-auxiliary-buffer-history
                                                          (assoc-keys myi-auxiliary-buffer-alist)))))
                                 (setq myi-auxiliary-buffer-history (copy-tree candidates))
                                 candidates))
           ;; default to last one
           (chosen-buffer (progn (when in-aux-buffer-p
                                   (setq mru-buf-candidates
                                         (cdr mru-buf-candidates)))
                                 (car mru-buf-candidates))))
      ;; prompting choices under these conditions
      ;; 1. Currently in an aux buffer AND there are more than 1 candidates available
      ;; 2. Next candidate doesn't exit yet.
      (when (or (and in-aux-buffer-p
                     (> (length mru-buf-candidates) 1))
                (not (buffer-live-p
                      (get-buffer chosen-buffer))))
        (setq chosen-buffer
              (ido-completing-read
               "Auxiliary Buffer: " mru-buf-candidates
               ;; simply `'hist' wouldn't work.
               nil t nil nil
               chosen-buffer)))
      ;; manually manage history
      (push chosen-buffer myi-auxiliary-buffer-history)
      ;; two consecutive `myi-auxiliary-buffer-switch' shows that the user's
      ;; intention is to switch to an aux buffer other than last one
      (funcall (if in-aux-buffer-p
                   #'switch-to-buffer
                 #'switch-to-buffer-other-window)
               (get-buffer-create chosen-buffer))
      ;; run the associated hook
      (when (not (buffer-live-p
                  (get-buffer chosen-buffer)))
        (eval (cdr (assoc chosen-buffer myi-auxiliary-buffer-alist))))
      ;; don't fill the buffer list with auxiliary buffers.
      (when in-aux-buffer-p
        (bury-buffer (other-buffer))))))

(defun myi-auxiliary-buffer-add/remove-the-current ()
  "Add/remove current buffer to/from
`myi-auxiliary-buffer-history'. Used in
`myi-auxiliary-buffer-switch'.

Newly added and removable buffers are in the form '(BUF-NAME)."
  (let* ((current-buf-name (buffer-name (current-buffer)))
         (is-aux-buf (assoc current-buf-name myi-auxiliary-buffer-alist)))
    (if is-aux-buf
        (cond
         ((y-or-n-p (format "Remove %s from auxiliary buffers"
                            current-buf-name))
          (setq myi-auxiliary-buffer-alist
                (delete (list current-buf-name) myi-auxiliary-buffer-alist))
          (setq myi-auxiliary-buffer-history
                (delete current-buf-name myi-auxiliary-buffer-history))
          (message "%s removed from auxiliaries" current-buf-name))
         (t
          (message "Abort. No buffers removed.")))
      (cond
       ((y-or-n-p (format "Add %s to auxiliary buffers" current-buf-name))
        (add-to-list 'myi-auxiliary-buffer-alist (list current-buf-name))
        (let ((history-delete-duplicates t)) ;remove duplications
          (add-to-history 'myi-auxiliary-buffer-history current-buf-name))
        (message "Add %s to auxiliaries" current-buf-name))
       (t
        (message "Abort. No buffers added."))))))

;; (defun my-quickly-take-notes  (beg end)
;;   "Quicly save text in the region to *scratch-text*"
;;   (interactive "r")
;;   (let ((str (filter-buffer-substring beg end)))
;;     (with-current-buffer (get-buffer-create "*scratch-text*")
;;       (goto-char (point-max))
;;       ;; insert header
;;       (insert "\n\n;;;; ---------------- "
;;               (format-time-string (cdar common-current-time-formats)
;;                                   (current-time))
;;               " ----------------\n")
;;       (insert str)
;;       (message "Quick Note saved."))))

;;; In light of `help-go-back' and `help-go-forward', I found it's super
;;; convenient to pop up help window. Also `with-help-window' macro is good for
;;; us as well.
(defun myh-switch-to-help-window (&rest extras)
  "Pop up the help window, EXTRAS are to be ignored."
  (switch-to-buffer-other-window (get-buffer (help-buffer))
                                 nil))

(defun myi-help-jump ()
  (interactive)
  (let ((help-topics-list
         (with-current-buffer (help-buffer)
           (append (list (list 0 #'myh-switch-to-help-window
                               (cadr help-xref-stack-item))) ;fake position 0
                   help-xref-stack
                   help-xref-forward-stack)))
        help-topics-alist
        topic-choice)
    (setq help-topics-alist
          (loop for item in help-topics-list
                collect (cons
                         (let ((position (car item))
                               (help-type (cadr item))
                               (topic (caddr item))
                               (extras (cdddr item)))
                           (format "%-30s %-15s %-5s %s" 
                                   (propertize (pp-to-string topic) 'face 'bold)
                                   (pp-to-string help-type)
                                   (pp-to-string position)
                                   (s-trim (pp-to-string extras))))
                         item)))
    (setq topic-choice
          (helm-comp-read "Pick a help topic: "
                          (assoc-keys help-topics-alist)
                          :must-match t))
    (let* ((item (assoc-default topic-choice help-topics-alist
                                nil nil))
           (method (cadr item))
           (args (cddr item)))
      (apply method args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Extra Clipboard
(defvar myi-extra-clipboard ""
  "A variable that holds extra copied content. It contains a
`kill-ring' element.")
(defun myi-extra-clipboard-kill/yank (&optional arg)
  "A simple extra clipboard that can holds a copied content
independently from `kill-ring'.

If ARG is non-nill, put the latest kill in `myi-extra-clipboard'
_and_ insert the content of `myi-extra-clipboard' at point like
when ARG is nil."
  (interactive "P")
  (when arg
    (setq myi-extra-clipboard
          (current-kill 0)))
  (insert-for-yank myi-extra-clipboard))

(defun insert-last-n-killed (num)
  "Insert the last NUM killed text at current point. Each text
snippet is separated by exactly one line break."
  (interactive "p")
  (loop for i from 0 to num
        do (insert (nth i kill-ring) "\n")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhanced Time/Date
;;; A pure elisp alternative to  "C-u M-! date"
(defun insert-current-time (&optional select)
  "Insert the current time at the cursor point.
With SELECT, prompt user to select a date format from
`common-current-time-formats'. Otherwise, the first format in
`common-current-time-formats' is used."
  (interactive "P")
  (let* ((format-names (assoc-keys common-current-time-formats))
         (formats (assoc-values common-current-time-formats))
         (default-choice (car format-names))
         (default-format (car formats))
         (format
          (if select
              (cdr (assoc (ido-completing-read "Format: "
                                               format-names
                                               nil t nil nil
                                               (car format-names))
                          common-current-time-formats))
            default-format)))
    (insert (format-time-string format))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; enhanced `recentf'
(require 'recentf)

;;; enable recentf support for dired directory.
(defun recentf-add-dired-directory ()
  "Add dired directory into recentf history. Note that this will
not record multiple-directory dired buffer case."
  (when (and (stringp dired-directory)
             (equal "" (file-name-nondirectory dired-directory)))
    (recentf-add-file dired-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhanced Editing
;; The following is adopted from emacs-fu {{{
;; NOTE: use 'sudo -e' or 'sudoedit' if you are at terminal
(defun djcb-find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable
by user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhanced interaction with external world
(defun pwd-to-X-clipboard ()
  "Show the current default directory."
  (interactive)
  (message "Directory %s" default-directory)
  (x-set-selection "CLIPBOARD" default-directory)
  (x-set-selection "PRIMARY" default-directory))

;;; TODO perfect this one, it only occasionally work. There is also a charset
;;; encoding issue.
(defun yank-from-x-primary ()
  "Yank from X primary. This helps works with other program in
copying/pasting."
  (interactive)
  (let ((primary (x-get-selection 'PRIMARY)))
    (if primary
        (insert (x-get-selection 'PRIMARY))
      (error "No primary selection"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc utilities
;;;
(defun my-find-quoted-string-in-buffer (criterion &optional description)
  "Find quoted strings (quoted with \" or \') that satisfies
  CRITERION, which is a function that accepts a single string as
  argument and returns `nil' or `t'. Optional DESCRIPTION is used
  to display info about the criterion.

Searching happens within the _whole_ buffer, use narrowing to
restrict to the part you are interested.

Result will be displayed within a temp buffer within
compilation-mode enabled (for easy navigation).

TODO the current quoted string regexp is far from complete."
  (let ((beg (point-min))               ;TODO support region
        (end (point-max))
        (file-name (or buffer-file-name ""))
        (result-buf (get-buffer-create "*Find-Strings*")))
    ;; use relative path instead
    (setq file-name (file-relative-name file-name))
    (with-current-buffer result-buf
      (read-only-mode 1)
      (erase-buffer)
      (insert (format "Find strings satisfy: %s\n" description)
              "---------------------------------------------\n"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(\"[^\"]*\"\\|\'[^\']*\'\\)" end t)
        (let ((matched-str (substring (match-string-no-properties 1) 1 -1))
              (line-number (line-number-at-pos)))
          (if (funcall criterion matched-str)
              (with-current-buffer result-buf
                (insert (format "%s:%d: len %d: %s\n"
                                file-name line-number
                                (length matched-str) matched-str)))))))
    (pop-to-buffer result-buf)
    (compilation-mode)
    (goto-char (point-min))))

;;; example usage of `my-find-quoted-string-in-buffer'
(defun my-fqs-demo ()
  (interactive)
  (my-find-quoted-string-in-buffer
   (function
    (lambda (str) (>= (length str) 32)))
   "len >= 32"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhanced Searching&Replacing
;;;
(defun isearch-forward-word-lax (&optional whole-word no-recursive-edit)
  "`isearch-forward-word' that searches word prefix by default.
With a prefix argument, do a incremental search forward for a
sequence of words."
  (interactive "P")
  ;; `isearch-search-fun-function' variable stores a function that returns a
  ;; function. It turns out that "-lax" suits our needs.
  (let ((isearch-search-fun-function
         (unless whole-word
           (lambda () (if isearch-forward
                          'word-search-forward-lax
                        'word-search-backward-lax))))
        (lax-flag (if whole-word t 'lax)))
    ;; pass symbol `lax' for `word-p' argument, which will specially treated by
    ;; `word-search-regexp''s advices.
    (isearch-mode t nil nil (not no-recursive-edit) lax-flag)))

;;; make `isearch-occur' work
;;;
;;; Though advising primitive functions is not recommended, I think it's OK
;;; here.
;;;
;;; 3 things to check:
;;;
;;; 1. Whether `isearch-forward-word-lax' works with `isearch-occur'
;;; 2. Whether ordinary `isearch-forward-word' work as usual.
(defadvice word-search-regexp (around word-search-regexp-lax-aware
                                      (string &optional lax))
  (if (eq isearch-word 'lax) (progn (message "yes word lax") (setq lax t)))
  ad-do-it)

(ad-activate 'word-search-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhanced Org
(defun myi-org-capture-source-snippet-with-block (BEG END)
  "Surround the marked region with org inline source tag, the
  major mode used is the one where the region is in."
  (interactive "r")
  (let ((cur-mode-name
         (substring (format "%s" major-mode) 0 -5)) ; remove the ending "-mode"
        (raw-grabbed-snippet (buffer-substring-no-properties BEG END))
        (snippet-buf "*Org Grabbed Source Snippet*")
        captured-snippet)
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create snippet-buf)
            (org-mode)
            (insert "#+BEGIN_SRC " cur-mode-name "\n"
                    raw-grabbed-snippet "\n"
                    "#+END_SRC\n")
            (setq captured-snippet (buffer-string)))
          ;; `save-window-excursion' mixes current buffers.
          (save-window-excursion
            (display-buffer snippet-buf nil)
            (if (y-or-n-p "Org Capture Snippet: Is this what you want (Y/N)? ")
                (kill-new captured-snippet))))
      (kill-buffer snippet-buf))))

(defvar myi-org-source-block-common-list
  '("emacs-lisp"
    "shell-script"
    "python"
    "ruby"
    "javascript" "html"
    ;; non-code mode
    "text"
    "diff")
  "A common list of types of org source block.")

(defun org-insert-source-block-and-edit (language)
  "A helper function to insert a source block."
  (interactive (list (ido-completing-read "Source Type: "
                                          myi-org-source-block-common-list)))
  ;; (indent-according-to-mode)
  (let (org-src-block-template-macro)
    (fset 'org-src-block-template-macro
          (lambda (&optional arg)
            "Insert a source code block in Org, utilizing Org's
             own template system."
            (interactive "p")
            (kmacro-exec-ring-item (quote ("<s	" 0 "%d")) arg)))
    (org-src-block-template-macro)
    (insert language)
    (forward-line)
    (org-edit-src-code)))

(defun myi-calibre-path-to-org-link (query-str)
  "This function returns an Org-link by invoking 'calibredb
--formats -s title:<QUERY-STR>'.

NOTE: This is really primitive function. You should make sure the
query string is valid and yield one result by using external
command line tool 'calibredb'. Otherwise, this function will not
get correct Org link."
  (interactive "sCalibre Query String: ")
  (let ((info-buffer "*Calibre-Org-Link Result*")
        book-id
        raw-info
        book-path
        book-description
        ;; reset default directory temporarily if the current one doesn't exists
        (default-directory (if (not (file-exists-p default-directory))
                               "~/"
                             default-directory)))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create info-buffer)
            (let (completion-collection)
              (call-process "/usr/bin/calibredb" nil t nil
                            "list" "--search" query-str)
              ;; collect numeric ids
              (save-excursion
                (goto-char (point-min))
                (while (search-forward-regexp
                        "^\\<[[:digit:]]+\\>" nil t)
                  (push (match-string 0) completion-collection))
                (setq completion-collection
                      (nreverse completion-collection)))
              (unless completion-collection
                (error (format "Empty result: Abort! Your query is (%s)"
                               query-str)))
              (save-window-excursion
                (display-buffer info-buffer nil)
                (setq book-id
                      (string-to-number
                       (ido-completing-read "Input the ID of the book: "
                                            completion-collection
                                            nil t))))
              (erase-buffer))

            (call-process "/usr/bin/calibredb" nil t nil
                          "list" "-f" "formats" "--search"
                          (format "id:\"%d\"" book-id))
            ;; parsing
            (goto-char (point-min))
            ;; (search-forward-regexp "\\[\\(/.+[^],]+\\)/\\([^],]+\\)[],]")
            (search-forward-regexp "\\[\\(/.+[^],]+\\)[],]")
            (setq raw-info (buffer-substring (point-min) (point-max))
                  book-path (file-name-directory (match-string 1))
                  book-description (file-name-base (match-string 1)))
            ;; sanitize path&description strings
            (cl-flet ((sanitize-path-and-descrip
                       (str)
                       ;; trim the start of the string
                       (setq str (replace-regexp-in-string "^[ \t]*" "" str))
                       ;; trim the end of the string
                       (setq str (replace-regexp-in-string "[ \t]*$" "" str))
                       ;; replace line breaking with single white space
                       (setq str (replace-regexp-in-string "[\n\r]" " " str))
                       ;; collapse whitespace
                       (setq str (replace-regexp-in-string "[ \t][ \t]+" " " str))))
              (setq book-path (sanitize-path-and-descrip book-path))
              (setq book-description (sanitize-path-and-descrip book-description)))

            ;; append parsing result to this buffer
            (goto-char (point-max))
            (insert (format "%s\n" (make-string 31 ?#))
                    (format "Query Title: %s\n" query-str)
                    (format "Path: %s\n" book-path)
                    (format "Description: %s\n" book-description)))
          ;; wait for user confirmation
          (save-window-excursion
            (display-buffer info-buffer nil)
              (org-insert-link nil book-path book-description)))
      ;; always clean up
      (kill-buffer info-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhanced Emacs Configuration Helper
;;;
;; helper functions to locate key Emacs directory/files
(defvar myi-emacs-key-pos
  '("~/.icrepos/emacs-custom-lisp-repos/"
    "~/etc/emacs"
    "~/etc/emacs/custom-lisp/"))

(defun myi-goto-key-pos ()
  "Go to key pos."
  (interactive)
  (let ((key-pos (ido-completing-read "Key Pos: "
                                      myi-emacs-key-pos)))
    (dired key-pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sdcv enhancer, to interact with outside world.
;;;
;;; TODO integrate into sdcv

;;; TODO make the following variables part of a library
(defvar sdcv-lookup-frame-name "*sdcv-lookup-frame*"
  "The name of a separate frame for sdcv looking up.")

(defvar sdcv-lookup-frame nil
  "The real frame object for `sdcv-lookup-frame-name'")

;;; 1600x900
;; `sdcv-started-externally-from' is defined in `sdcv-mode'
(defvar sdcv-started-externally-from)
(defun myi-sdcv-called-externally ()
  (let ()
    (select-frame-set-input-focus
     (if (frame-live-p sdcv-lookup-frame)
         sdcv-lookup-frame
       (setq sdcv-lookup-frame
             (make-frame
              ;; `user-position'/`user-size' flags are needed
              ;; width are exaggerated, the effect is to be maximum
              ;; height is char size
              `((name . ,sdcv-lookup-frame-name)
                (top . (- 1)) (left . 0) (width . 180) (height . 10)
                (user-position t) (user-size t)
                (auto-raise . t))))))
    ;; WM fiddling
    (call-process "/usr/bin/wmctrl" nil nil nil
                    "-r" sdcv-lookup-frame-name "-b" "add,above")
    (call-process "/usr/bin/wmctrl" nil nil nil
                  "-a" sdcv-lookup-frame-name)

    ;; do sdcv query in the selected frame
    (setq sdcv-started-externally-from t)
    ;; (sdcv-goto-sdcv)
    ;; (sticky-window-delete-other-windows)
    (sdcv-search)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs/Elisp development helper
;;;

(defun myi-add-this-to-load-path (&optional path)
  "Invoke this command to add CWD of the current buffer to
`load-path', ease the development."
  (interactive)
  (let ((cwd-path default-directory))
    (if cwd-path
        (when (y-or-n-p (format "Add [%s] to 'load-path' ?"
                                cwd-path))
          (add-to-list 'load-path cwd-path)
          (message (format "Add [%s] to 'load-path'." cwd-path)))
        (error "No CWD set for the buffer."))))

;;; Tue Mar 25 11:03:23 CST 2014: deprecated in favor of simple symlink
(defun myi-custom-lisp-dev-prep ()
  "Helper function to develop custom-lisp packages. Can only be
used in `dired-mode'. Exchange the name of local custom lisp
directory and link to custom lisp repo."
  (interactive)
  (unless (eq (buffer-local-value 'major-mode (current-buffer))
              'dired-mode)
    (error "Can only be used in dired buffer."))
  (let ((pkg-name (dired-get-filename 'no-dir))
        (pkg-link-name)
        (pkg-tmp-name)
        (pkg-repo-path))
    (if (string-match "_\\(.+\\)\\.iclink"
                      pkg-name)
        (progn (setq pkg-link-name pkg-name)
               (setq pkg-name (match-string-no-properties 1 pkg-link-name)))
      (setq pkg-link-name
            (concat "_" pkg-name ".iclink")))
    (message (format "Creating/Switching custom lisp local directory and repo link for '%s' and '%s'."
                     pkg-name pkg-link-name))
    ;; create symbolic link to repo
    ;; assuming that original name as file always exists
    ;; the following runs only for the first time.
    (setq pkg-repo-path (concat "~/.icrepos/emacs-custom-lisp-repos/"
                                pkg-name))
    ;; `file-exists-p' follows the link
    (unless (file-exists-p pkg-link-name)
      (if (file-symlink-p pkg-link-name)
          (warn "Emacs Custom Lisp repo '%s' not found!!" pkg-name)
        (make-symbolic-link pkg-repo-path pkg-link-name)))
    ;; exchange names
    (setq pkg-tmp-name (concat pkg-name (emacs-uptime "%s")))
    (rename-file pkg-name pkg-tmp-name)
    (rename-file pkg-link-name pkg-name)
    (rename-file pkg-tmp-name pkg-link-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; indirect narrowing
;;;

;;; narrow-to-region-indirect-buffer
(defun narrow-to-region-indirect-buffer (&optional name start end)
  "Create a cloned indirect buffer and narrow the current region
in the newly created buffer.

With prefix argument, ask for a name otherwise automatically use
the region point for name."
  (interactive (list (generate-new-buffer-name
                      (let ((auto-name
                             (concat (buffer-name)
                                     (format "<-%d:%d->"
                                             (region-beginning) (region-end)))))
                        (if current-prefix-arg
                            (read-string "Name for the indirected narrowed buffer: ")
                          auto-name)))
                     (region-beginning) (region-end)))
  (with-current-buffer (clone-indirect-buffer
                        name
                        'display)
    (narrow-to-region start end)
    (deactivate-mark)
    (goto-char (point-min))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general functionality extension
(defun princ-to-string (arg)
  "Return a string containing the printed representation of
OBJECT, like princ."
  (with-temp-buffer
    (princ arg (current-buffer))
    (buffer-string)))

;; (with-ert-expectations
;;  (expect "3.1415"
;;    (princ-to-string 3.1415))
;;  (expect "hello"
;;    (princ-to-string "hello")))

(defun s-concat* (&rest args)
  "An JavaScript-like string addition that automatically converts
  non-string into string."
  (mapconcat #'princ-to-string args ""))
(defalias 's-+ #'s-concat*)

;; (with-ert-expectations
;;  (expect "I love you."
;;      (s-concat* "I " "love " "you."))
;;  (expect "is 42"
;;    (s-concat* "is " 42))
;;  (expect "is 42"
;;    (s-+ "is " 42)))

(defun s-mapconcat* (func sequence &optional separator)
  (setq separator (or separator ""))
  (mapconcat (lambda (arg)
               (princ-to-string
                (funcall func arg)))
             sequence
             separator))

;; (with-ert-expectations
;;  (expect "2014-6-6"
;;    (s-mapconcat* #'identity '(2014 6 6) "-"))
;;  (expect "19:9:14:3434"
;;    (s-mapconcat* #'identity '(19 9 14 3434) ":")))

(defun make-string* (length init)
  "An enhanced version of `make-string', INIT can be anything
  that can be converted to string. The result is LENGTH copies of
  converted INIT concatenated together."
  ;; (let ((i 0)
  ;;       (init-str (princ-to-string init))
  ;;       (result))
  ;;   (while (< i length)
  ;;     (setq result (concat init-str result))
  ;;     (setq i (1+ i)))
  ;;   result)
  (mapconcat #'identity
             (loop repeat length
                   collect (princ-to-string init))
             nil))

;; (with-ert-expectations
;;  (expect "%5s%5s%5s%5s%5s"
;;    (make-string* 5 "%5s"))
;;  (expect ""
;;    (make-string* 0 "lll")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhance paredit
(defun my-comment-or-uncomment-sexp-dwim ()
  "Comment/Uncomment the sexp at point. Should be part of the
`paredit-comment-dwim'

Also note, this function uses changed `er/mark-comment-block'"
  (interactive)
  (save-excursion
    (setq transient-mark-mode t)
    (if (or (eq 'comment (syntax-ppss-context (syntax-ppss (point))))
            (looking-at "\\s *\\s<"))
        ;; use `mark-comment' from ER package, o/w we need to duplicate this
        ;; functionality.
        (er/mark-comment)
      (mark-sexp))
    (paredit-comment-dwim)
    (setq transient-mark-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhance window movement
(defun myi-other-window ()
  "A replacement for `other-window'.

WARNING: This command should be bound with 'C-x o' and 'C-x O'
and it acts differently for these two bindings. DON'T rebind this
command to other keys, it WON'T work.

'C-x o' moves forward as (other-window 1)
'C-x O' moves backward as (other-window -1)

For windows less than 3, it calls `other-window' directly.
Otherwise, enters a fake minor mode, with ?o moving forward
windows and ?O moving backward windows."
  (interactive)
  (let ((oldwindow (selected-window))
        (window-num (length (window-list)))
        (step-other-window (lambda ()
                             (hl-line-unhighlight)
                             (other-window (if (eq last-input-event ?o) 1 -1))
                             (let ((hl-line-mode t)) (hl-line-highlight)))))
    (other-window (if (eq last-input-event ?o) 1 -1))
    ;; in case of 3+ windows
    (when (> window-num 2)
      (with-trivial-minor-mode
       '((?o . (funcall step-other-window))
         (?O . (funcall step-other-window)))
       "Switching forward(o)/backward(O) windowd..."
       nil
       (select-window oldwindow t)
       (hl-line-unhighlight)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhanced Calc
(defun number-to-string-radix (num &optional to-radix)
  "Extended `number-to-string', so it accepts RADIX argument to
output number in that TO-RADIX. TO-RADIX is default to 10 if it's
omitted.

NOTE: this implementation uses `calc-bin' and thus TO-RADIX can
only be from 2-36 as in `calc-number-radix'."
  (require 'calc-bin)
  (if to-radix
      (let ((calc-number-radix to-radix))
        (math-format-radix num))
   (number-to-string num)))

;; (with-ert-expectations
;;   (expect "AFFC"
;;     (number-to-string-radix 45052 16))
;;   (expect "10"
;;     (number-to-string-radix 10))
;;   (expect "1100"
;;       (number-to-string-radix 12 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhancement to pp
(eval-when-compile
  (require 'pp))
;;; an expansion for `pp-macroexpand-expression'
;;;###autoload
(defun pp-macroexpand-all-expression (expression)
  "Macroexpand EXPRESSION at ALL levels and pretty-print its
value.

NOTE: for symbols from `make-symbol', though they are interned
symbols different from symbols in `obarray'. The form output by
`macroexpand-all' won't show this difference and thus the output
might no be equivalent to the original.

TODO Can we adapt `macroexpand-all' to show this change."
  (interactive
   (list (read-from-minibuffer "Macroexpand ALL: " nil read-expression-map t
			       'read-expression-history)))
  (pp-display-expression (macroexpand-all expression) "*Pp Macroexpand-ALL Output*"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extra List facilities
(defun assoc-keys (alist)
  "Convenient function to extract all keys into a list from an
associate list."
  (mapcar #'car alist))

;; (with-ert-expectations
;;   (expect '(pine oak maple)
;;     (assoc-keys '((pine . cones)
;;                   (oak . acorns)
;;                   (maple . seeds))))
;;   (expect '(a "b")
;;     (assoc-keys '((a . 1) ("b" 2 3))))
;;   (expect nil
;;     (assoc-keys nil)))

(defun assoc-values (alist)
  "Convenient function to extract all values into a list from an
associate list."
  (mapcar #'cdr alist))

;; (with-ert-expectations
;;   (expect '(cones acorns seeds)
;;     (assoc-values '((pine . cones)
;;                   (oak . acorns)
;;                   (maple . seeds))))
;;   (expect '(1 (2 3))
;;     (assoc-values '((a . 1) ("b" 2 3))))
;;   (expect nil
;;     (assoc-values nil)))

(defun plist-keys (plist)
  "Convenient function to extract all keys into a list from an
property list."
  (loop while plist
        collect (car plist)
        do (setq plist (cddr plist))))

;; (with-ert-expectations
;;   (expect '(pine numbers color)
;;    (plist-keys '(pine cones numbers (1 2 3) color "blue")))
;;   (expect '(bar foo quux)
;;    (plist-keys '(bar t foo 69 quux (a))))
;;   (expect nil
;;     (plist-keys nil)))

(defun plist-values (plist)
  "Convenient function to extract all values into a list from an
property list."
  (loop while plist
        collect (cadr plist)
        do (setq plist (cddr plist))))

;; (with-ert-expectations
;;   (expect '(cones (1 2 3) "blue")
;;    (plist-values '(pine cones numbers (1 2 3) color "blue")))
;;   (expect '(t 69 (a))
;;    (plist-values '(bar t foo 69 quux (a))))
;;   (expect nil
;;     (plist-values nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: SQLite completion with company
;; quite basic
(defconst sqlite-keywords
  '("ABORT" "ACTION" "ADD" "AFTER" "ALL" "ALTER" "ANALYZE" "AND" "AS" "ASC" "ATTACH" "AUTOINCREMENT" "BEFORE" "BEGIN" "BETWEEN" "BY" "CASCADE" "CASE" "CAST" "CHECK" "COLLATE" "COLUMN" "COMMIT" "CONFLICT" "CONSTRAINT" "CREATE" "CROSS" "CURRENT_DATE" "CURRENT_TIME" "CURRENT_TIMESTAMP" "DATABASE" "DEFAULT" "DEFERRABLE" "DEFERRED" "DELETE" "DESC" "DETACH" "DISTINCT" "DROP" "EACH" "ELSE" "END" "ESCAPE" "EXCEPT" "EXCLUSIVE" "EXISTS" "EXPLAIN" "FAIL" "FOR" "FOREIGN" "FROM" "FULL" "GLOB" "GROUP" "HAVING" "IF" "IGNORE" "IMMEDIATE" "IN" "INDEX" "INDEXED" "INITIALLY" "INNER" "INSERT" "INSTEAD" "INTERSECT" "INTO" "IS" "ISNULL" "JOIN" "KEY" "LEFT" "LIKE" "LIMIT" "MATCH" "NATURAL" "NO" "NOT" "NOTNULL" "NULL" "OF" "OFFSET" "ON" "OR" "ORDER" "OUTER" "PLAN" "PRAGMA" "PRIMARY" "QUERY" "RAISE" "RECURSIVE" "REFERENCES" "REGEXP" "REINDEX" "RELEASE" "RENAME" "REPLACE" "RESTRICT" "RIGHT" "ROLLBACK" "ROW" "SAVEPOINT" "SELECT" "SET" "TABLE" "TEMP" "TEMPORARY" "THEN" "TO" "TRANSACTION" "TRIGGER" "UNION" "UNIQUE" "UPDATE" "USING" "VACUUM" "VALUES" "VIEW" "VIRTUAL" "WHEN" "WHERE" "WITH" "WITHOUT")
  "The list below shows all possible keywords used by any build
  of SQLite regardless of compile-time options. see
  https://www.sqlite.org/lang_keywords.html")

(defconst sqlite-commands
  '(".backup" ".bail" ".clone" ".databases" ".dump" ".echo" ".eqp" ".exit" ".explain" ".fullschema" ".headers" ".help" ".import" ".indices" ".load" ".log" ".mode" ".nullvalue" ".once" ".open" ".output" ".print" ".prompt" ".quit" ".read" ".restore" ".save" ".schema" ".separator" ".shell" ".show" ".stats" ".system" ".tables" ".timeout" ".timer" ".trace" ".vfsname" ".width")
  "The list of commands of SQLite3 as output by '.help' .")

(defun company-sqlite--prefix ()
  (save-excursion
    (let* ((end (point))
           (start (if (search-backward-regexp "[^_a-zA-Z.]" (point-at-bol) t)
                      (1+ (point))
                    (point-at-bol))))
      (buffer-substring-no-properties start end))))

(defun company-sqlite (command &optional arg &rest ignored)
  "`company-mode' completion back-end for SQLite."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-sqlite))
    (prefix (when (or (eq major-mode 'sql-interactive-mode)
                      (eq major-mode 'sql-mode))
              (company-sqlite--prefix)))
    (candidates
     (let* ((sqlite-table-list
             (sql-sqlite-completion-object sql-buffer nil))
            (choices (remove-if-not
                      (lambda (c) (string-prefix-p arg c t))
                      (append (if (string-prefix-p "." arg)
                                  sqlite-commands
                                sqlite-keywords)
                              sqlite-table-list nil))))
       (or choices (company-other-backend))))
    (sorted t)
    ;; ignore case to select candidates but changed to candidate case
    (ignore-case 't)))

(defun company-sqlite-enabler ()
  (add-to-list 'company-backends #'company-sqlite))
(add-hook 'sql-interactive-mode-hook #'company-sqlite-enabler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Integrated Compilation
;; TODO make a dwim version, that can work according to current major mode of
;; the buffer.
(defun compile-dwim ()
  "`compile' or `recompile' according to project root, and try to
reuse last compilation setup.

Two use scenarios, use proj-root to decide whether:
1. Just start/switch to compile a new project
2. Still compile for the last project
   In this case, the `recompile' command will be called in
   '*compilation*' buffer to reuse configuration.
"
  (interactive)
  (let* ((proj-root (expand-file-name
                     (or (vc-find-root default-directory ".git/")
                         default-directory)))
         (compilation-buf (get-buffer "*compilation*"))
         (compile-the-same-prj-p (and (buffer-live-p compilation-buf)
                                      ;; remember to canonicalize paths for comparison
                                      (string-equal
                                       (expand-file-name
                                        (buffer-local-value 'default-directory
                                                            compilation-buf))
                                       proj-root))))
    (if compile-the-same-prj-p
        ;; recompile
        (with-current-buffer compilation-buf
          (let ((current-prefix-arg '(4)))
            (call-interactively #'recompile)))
      ;; new compilation
      (let ((default-directory proj-root)
            (confirm-switch-prompt (format "%s: Switch to NEW project? "
                                           (propertize proj-root 'face 'hi-red-b))))
        (if (y-or-n-p confirm-switch-prompt)
            (call-interactively #'compile)
          (message (concat (propertize "Abort compilation.\n" 'face 'hi-blue-b)
                           (propertize "For recompilation, " 'face 'hi-black-b)
                           "switch to buffer within the last project.")))))))

(provide 'carltonfpkg)
;;; carltonfpkg.el ends here
