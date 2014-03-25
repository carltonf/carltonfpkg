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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhanced Buffer Management
;;;
;;; switch to previous buffer
(defun switch-to-previous-buffer ()
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
;;; Enhanced Dired
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
;;; recentloc
;;;
;;; If we need to jump around two places often, use marks `C-x C-x' and etc
;;;
;;; A much more powerful register-based location jumping mechanism.
;;;
;;; TODO:
;;; 1. nice listing of recorded locations with context
;;; 2. maybe we could enhance f10 like this too
;;; 3. Move out as a separate library
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

(defun my-recentloc-cycle-ring (&optional ARG)
  "Cycle through `my-recentloc-ring'. Repeated
`my-recentloc-cycling-key' jumps to older recorded locations. Any
key other than `my-recentloc-cycling-key' will abort cycling. C-g
will take you back to where you starts. Enter will confirm the
current location. With a prefix ARG, it jumps back to where you
start cycling.

`hl-line-mode' is temporarily enabled when you are cycling."
  (interactive "P")
  (flet ((goto-marker (marker)
                      (if (markerp marker)
                          (progn
                            (switch-to-buffer (marker-buffer marker))
                            (goto-char marker))
                        nil)))
    (cond (ARG
           (unless (goto-marker my-cycling-start-marker)
             (message "Abort: No start location of cycling.")))
          ((ring-empty-p my-recentloc-ring)
           (message "Abort: Recentloc has no records."))
          (t
           (setq my-cycling-start-marker (point-marker)) ;save start marker
           ;; A macro to enable hl-line mode only while cycling. It is used only
           ;; after you have switched to the desired buffer and should wrap
           ;; around time-consuming operations within that buffer.
           (macrolet ((with-temp-hl-line
                       (&rest body)
                       `(let ((old-hl-line-mode hl-line-mode))
                          (if (null (setq old-hl-line-mode hl-line-mode))
                              (hl-line-mode))
                          ,@body
                          (if (null old-hl-line-mode) (hl-line-mode -1)))))
             (let* ((recentloc-ring-len (ring-length my-recentloc-ring))
                    (ring-idx 0)
                    (cur-marker (ring-ref my-recentloc-ring ring-idx))
                    (last-read-key my-recentloc-cycling-key))
               (while (eq last-read-key my-recentloc-cycling-key)
                 (if (= ring-idx recentloc-ring-len)
                     ; stay in the same marker twice
                     (progn (message "Recentloc cycle wrapping...")
                            (setq ring-idx -1))
                   (message "Recentloc cycling to %s..." cur-marker)
                   (goto-marker cur-marker))
                 (with-temp-hl-line
                  (recenter)
                  (setq ring-idx (1+ ring-idx))
                  (setq cur-marker (ring-ref my-recentloc-ring ring-idx))
                  (setq last-read-key (read-key))))))
           ;; don't replay the last-input-event if it's return (which works as
           ;; confirmation to terminate cycling)
           (let ((last-input-event last-input-event))
             (case last-input-event
               (?\r nil)                ;won't replay enter
               (?\C-G                   ;with canceling, go back where we start
                (switch-to-buffer (marker-buffer my-cycling-start-marker))
                (goto-char my-cycling-start-marker))
               (otherwise               ;replay whatever other keys
                (setq unread-command-events (list last-input-event)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; enhanced shell/consoles/repls
;;;
(defvar myi-debug-shell-name "*debug-shell*"
  "Workhorse in-Emacs shell.")
(defvar myi-extra-shell-name "*extra-shell*"
  "A secondary in-Emacs shell")

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

(defcustom myi-console-mode-user-list nil
  "User-defined console list to be included in `myi-consoles' in
  addition to `myi-console-mode-list'. The value should be a list
  of major mode symbols.")

(defvar myi-console-mode-list '(shell-mode eshell-mode)
  "Default list of modes whose buffers are included in
`myi-consoles'. On switching a list of console buffers will be
constructed so that there is no separate console list, only
console mode list.")
(defvar myi-console-buffer-history nil
  "`myi-consoles' console selection history.")

(defcustom myi-console-autostarts (list (cons myi-debug-shell-name #'shell)
                                        (cons myi-extra-shell-name #'shell)
                                        ;; `eshell' ARG works different from `shell's
                                        (cons "*eshell*" (lambda (ignore) (eshell))))
  "Association list of autostart consoles. The elements of the
  list should be a CONS of format (BUFNAME . FUNC), of which FUNC
  will be passed with BUFNAME only.")

(defun myi-consoles (&optional prompt-choices)
  "An all-in-one command for all interactive buffers, i.e.
various REPL buffers, `eshell', `shell' buffers. With no
argument, switch to last visited console. If the current buffer
is already one of the consoles, prompt to open another console
in current window."
  (interactive "P")
  (let* ((console-mode-full-list (append myi-console-mode-list
                                         myi-console-mode-user-list))
         console-buffer-list
         (last-console-choice (car myi-console-buffer-history))
         ;; default to the last chosen
         (console-choice last-console-choice)
         ;; nil if not, t o/w
         (is-in-console (member major-mode console-mode-full-list)))
    (when (or prompt-choices
              (null myi-console-buffer-history)
              is-in-console)
      (mapc (lambda (buf)
              (if (and (member (buffer-local-value 'major-mode buf)
                               console-mode-full-list))
                  (add-to-list 'console-buffer-list
                               (buffer-name buf)
                               ;; appending
                               t)))
            (buffer-list))
      ;; autostarts
      (mapc (lambda (buf-func-pair)
              (add-to-list 'console-buffer-list
                           (car buf-func-pair) t))
            myi-console-autostarts)
      ;; while in a console already, change default to the one before the last.
      (when is-in-console
        (setq last-console-choice (cadr myi-console-buffer-history)))
      (setq console-choice
            (ido-completing-read "Console: "
                                 console-buffer-list
                                 nil t nil 'myi-console-buffer-history
                                 ;; make the last selected one the default
                                 last-console-choice)))
    (let ((console-buffer (get-buffer console-choice)))
      (unless (eq (current-buffer) console-buffer)
        ;; only popup other window when NOT in a console
        (funcall (if is-in-console
                     #'switch-to-buffer
                   #'switch-to-buffer-other-window) console-choice)
        ;; also bury last visited console buffer, since a switch within a
        ;; console usually means a mistake in the first, so don't mangle the
        ;; buffer list.
        (if is-in-console
            (bury-buffer (other-buffer)))
        ;; handle autostart buffers `switch-to-buffer-other-window' will create
        ;; non-existent buffer.
        (if (not console-buffer)
            (funcall (cdr (assoc console-choice myi-console-autostarts))
                     console-choice))))))

;;; custom shells
;;; A template, which can be used for almost every kind of shells.
;;! make the function below can define buffers like standard `shell'
;;! maybe we can write a function to ask what shell to execute similar
;; to the `term' but with predefined list to do auto-complete

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
;;; enhanced doc lookup
;;;
(require 'man)

(defun myi-list-opened-man-buffers ()
  "Return a list of opened man page buffers."
  (let ((man-buf-list))
    (mapc (lambda (curbuf)
            (when (eq (buffer-local-value 'major-mode curbuf)
                      'Man-mode)
              (add-to-list 'man-buf-list (buffer-name curbuf))))
          (buffer-list))
    man-buf-list))

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
                                       (if (member man-def Man-topic-history)
                                           man-def
                                         nil)))
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

(defvar Info-node-boundary-min nil
  "Minimum buffer position of current node content.")
(defvar Info-node-boundary-max nil
  "Maximum buffer position of current node content.")

(defadvice Info-goto-node (after myi-info-mode-record-node-boundaries)
  (setq Info-node-boundary-min (point-min)
        Info-node-boundary-max (point-max)))
(ad-activate #'Info-goto-node)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; enhanced utility buffers
;;;
;; This leads to default *scratch* buffer, used for non-important scratch
(require 'ido)

(defvar myi-auxiliary-buffer-list '("*scratch*" "*scratch-text*")
  "List of auxiliary buffers, which can be easily accessed
through `myi-auxiliary-buffers-switch'. New buffers can also be
added by using `myi-auxiliary-buffer-add/remove-the-current'.")

(defvar myi-auxiliary-buffer-history nil
  "Auxiliary buffer selection history.")

(defun myi-auxiliary-buffers-switch (&optional add-remove)
  "Fast switch to one buffer in `myi-auxiliary-buffer-list'.
If the current buffer is one of the buffers, try to switch to
another one with `ido' prompt.

ADD-REMOVE - negative, remove the current buffer from auxiliary
list. Otherwise, add the current buffer into auxiliary list."
  (interactive "p")                     ;add-remove shall be 1, if omitted
  (if (not (eq add-remove 1))
      (myi-auxiliary-buffer-add/remove-the-current add-remove)
    (let* ((is-auxiliary-buffer (member (buffer-name (current-buffer))
                                        myi-auxiliary-buffer-list))
           (last-aux-buf (car myi-auxiliary-buffer-history))
           (last-2nd-aux-buf (cadr myi-auxiliary-buffer-history))
           (aux-chosen-buffer last-aux-buf))
      (when (or is-auxiliary-buffer
                (null myi-auxiliary-buffer-history))
        ;; TODO `ido-completing-read' will damage CHOICES list in
        ;; `ido-make-choice-list' if DEF is not nil. I think this is a bug, but
        ;; here is a workaround.
        (setq aux-chosen-buffer (ido-completing-read "Auxiliaries: "
                                                     (copy-tree myi-auxiliary-buffer-list)
                                                     nil t nil 'myi-auxiliary-buffer-history
                                                     last-2nd-aux-buf)))
      ;; create the buffer if does not existed
      (switch-to-buffer (get-buffer-create aux-chosen-buffer))
      ;; don't fill the buffer list with auxiliary buffers.
      (when is-auxiliary-buffer
        (bury-buffer (other-buffer))))))

(defun myi-auxiliary-buffer-add/remove-the-current (arg)
  "Interactively add current buffer to
`myi-auxiliary-buffer-history', with negative Prefix, remove the
current buffer."
  (let ((current-buf-name (buffer-name (current-buffer))))
    (if (< arg 0)
        (progn (setq myi-auxiliary-buffer-list
                     (delete current-buf-name myi-auxiliary-buffer-list))
               (setq myi-auxiliary-buffer-history
                     (delete current-buf-name myi-auxiliary-buffer-history))
               (message "Remove %s from auxiliaries" current-buf-name))
      (add-to-list 'myi-auxiliary-buffer-list current-buf-name)
      (add-to-history 'myi-auxiliary-buffer-history current-buf-name)
      (message "Add %s to auxiliaries" current-buf-name))))

(defun my-quickly-take-notes  (beg end)
  "Quicly save text in the region to *scratch-text*"
  (interactive "r")
  (let ((str (filter-buffer-substring beg end)))
    (set-buffer (get-buffer-create "*scratch-text*"))
    (goto-char (point-max))
    ;; insert header
    (insert "\n;;;; ---------------- "
            (format-time-string current-time-format
                                (current-time))
            " ----------------\n")
    (insert str)
    (message "Quick Note saved.")))

;;; In light of `help-go-back' and `help-go-forward', I found it's super
;;; convenient to pop up help window. Also `with-help-window' macro is good for
;;; us as well.
(defun myh-switch-to-help-window ()
  "Pop up the help window"
  (interactive)
  (switch-to-buffer-other-window (get-buffer (help-buffer))
                                 nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhanced Time/Date

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; insert date and time
;;; NOTE: though `C-uM-! date' is good, but I want a portable version,
;;; so I can do this on windows too.
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-date-format "%a %b %d %Z %Y"
  "Format of date to insert with `insert-current-date' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert (format-time-string current-date-time-format (current-time))))

(defun insert-current-date ()
  "insert the current date into current buffer.
Uses `current-date-format' for the formatting the date/time."
       (interactive)
       (insert (format-time-string current-date-format (current-time))))

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time))))

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
    (save-excursion
      (set-buffer result-buf)
      (toggle-read-only -1)
      (erase-buffer)
      (insert (format "Find strings satisfy: %s\n" description)
              "---------------------------------------------\n"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(\"[^\"]*\"\\|\'[^\']*\'\\)" end t)
        (let ((matched-str (substring (match-string-no-properties 1) 1 -1))
              (line-number (line-number-at-pos)))
          (if (funcall criterion matched-str)
              (save-excursion
                (set-buffer result-buf)
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
(defun my-org-source-auto-format (BEG END)
  "Surround the marked region with org inline source tag, the
  major mode used is the one where the region is in."
  (interactive "r")
  (let ((cur-mode-name (format "%s" major-mode))
        (old-buffer (current-buffer)))
    (with-temp-buffer
      (insert "#+BEGIN_SRC " cur-mode-name)
      ;; remove the ending "-mode"
      (backward-kill-word 1)
      (delete-backward-char 1)
      (insert "\n")
      ;; TODO insert marked region here
      (insert-buffer-substring-no-properties old-buffer BEG END)
      (insert "\n#+END_SRC\n")
      (kill-new (buffer-string)))))

(defun myi-calibre-path-to-org-link (query-str)
  "This function returns an Org-link by invoking 'calibredb
--formats -s title:<QUERY-STR>'.

NOTE: This is really primitive function. You should make sure the
query string is valid and yield one result by using external
command line tool 'calibredb'. Otherwise, this function will not
get correct Org link."
  (interactive "sCalibre Query String: ")
  (let ((info-buffer "*Calibre-Org-Link Result*")
        raw-info
        book-path
        book-description
        (is-parsing-correct nil))
    (with-current-buffer (get-buffer-create info-buffer)
      (call-process "/usr/bin/calibredb" nil t nil
                    "list" "-f" "formats" "--search"
                    (format "title:\"%s\"" query-str))
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
    (let ((prev-windows-conf (current-window-configuration)))
      (switch-to-buffer-other-window info-buffer)
      (setq is-parsing-correct
            (y-or-n-p "Is the parsing correct? Y to insert the link at current point."))
      ;; in case of incorrectness, give the use a chance to change
      (when is-parsing-correct
        (set-window-configuration prev-windows-conf)
        (org-insert-link nil book-path book-description)
        (kill-buffer info-buffer)))))

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


(provide 'carltonfpkg)
;;; carltonfpkg.el ends here
