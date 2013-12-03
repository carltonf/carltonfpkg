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
;; All should live under `cx-' namespace.


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
;;; enhanced shell
;;;
(defun my-debug-shell (&optional arg)
  (interactive "P")
  (let ((debug-shell-name "*debug-shell*")
        (new-default-directory default-directory))
    ;; make sure the new debug-shell always shows up in another window.
    (unless (eq (current-buffer) (get-buffer debug-shell-name))
      (switch-to-buffer-other-window debug-shell-name))
    (shell debug-shell-name)
    (when arg
      (process-send-string debug-shell-name
                           (concat "cd " new-default-directory " ; \\")) ; lined with `dirs'
      (shell-resync-dirs))))

(defun extra-shell ()
  (interactive)
  "An extra Emacs shell."
  (let ((extra-shell "*extra-shell*"))
    (unless (eq (current-buffer) (get-buffer extra-shell))
       (switch-to-buffer-other-window extra-shell))
     (shell extra-shell)))

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
(defun my-go-to-man (&optional last-man)
  "Go to man page, with prefix LAST-MAN, jump to last man page."
  (interactive "P")
  (if last-man
      (switch-to-buffer-other-window
       (let ((last-man-buf))
         (catch 'found
           (dolist (buf (buffer-list))
             ;; buffer list from `buffer-list' is sorted in the visiting order, so we
             ;; only need to find the first buffer with `Man-mode'
             (cond ((eq (buffer-local-value 'major-mode buf) 'Man-mode)
                    (setq last-man-buf buf)
                    (throw 'found nil)))))
         last-man-buf))
    (command-execute #'man)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; enhanced utility buffers
;;; 
;; This leads to default *scratch* buffer, used for non-important scratch
(defun my-scratch-switch (&optional is-text)
  "Fast switch to *scratch-text* buffer, a fundamental
buffer. With prefix argument, switch to *scratch* buffer, the
original Lisp Interaction Buffer."
  (interactive "P")
  (if is-text
      (switch-to-buffer (get-buffer-create "*scratch-text*"))
    (switch-to-buffer "*scratch*")))

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

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;; enable recentf support for dired directory.
(defun recentf-add-dired-directory ()
  "Add dired directory into recentf history. Note that this will
not record multiple-directory dired buffer case."
  (when (and (stringp dired-directory)
             (equal "" (file-name-nondirectory dired-directory)))
    (recentf-add-file dired-directory)))
(add-hook 'dired-mode-hook 'recentf-add-dired-directory)

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


(provide 'carltonfpkg)
;;; carltonfpkg.el ends here
