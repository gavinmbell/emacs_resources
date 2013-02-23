(require 'sendmail)
(provide 'request-queue)

; request-queue.el (0.8 of Wed Apr  3 00:13:05 EST 1996)
; major mode and major hacks for coping with req.
;
; "Hallelujah, Hallelujah, We're Mr. Bitter,
;  We'll take a bit of this and that."
; dk (dave@ccs.neu.edu)
;
; gnuish.
;
; READ THE README before trying to install this.

; USER INSTALLATION AND CUSTOMIZATION
; -----------------------------------
; first, check the "user customizations" section below to see that all the
; variables make sense for you.  see the documentation string after each
; variable for a description of what it controls.  if you don't like the
; defaults, just override them in your .emacs.  for instance, i know the
; options for the 'q' command, so i do
; (setq reqq-display-filter-helper nil)
; in my .emacs.
; next, add the line (autoload 'request "this file") (where "this file" is
; the filename of this elisp file,) to your .emacs.
; finally, start up emacs, type
; M-x request
; and you're done.
; once it's running, you can type C-h m for information about the mode.
; Some of the functions here are useful outside of request-queue-mode.  You
; might find it useful, for instance, to have the request-mail-mode functions
; available outside of reqmode.  to do this, add the following lines to
; your .emacs:
;  (define-key mail-mode-map "\C-c\C-r" 'request-mail-resolve)
;  (define-key mail-mode-map "\C-c\C-g" 'request-mail-give)
;  (define-key mail-mode-map "\C-c\C-l" 'request-mail-stall)
;  (define-key mail-mode-map "\C-c\C-p" 'request-mail-prioritize)
;  (define-key mail-mode-map "\C-c\C-t" 'request-mail-take)
;  (define-key mail-mode-map "\C-c\C-u" 'request-mail-requester)
;  (define-key mail-mode-map "\C-c\C-a" 'request-mail-action)
;
; (substitute 'mh-letter-mode-map' for 'mail-mode-map' if you use mh.)
; you can, of course, substitute your own keybindings if you like.

;
; SITE INSTALLATION AND CUSTOMIZATION
; The 'OS Customizations' section below contains most everything you'll need
; to modify for a site installation.  Just edit the defvars to reflect reality
; as your site sees it.  Again, check each variable's documentation for
; details.

; ABOUT THE CODE
; first off, my most sincere apologies.  this is UGLY code.  it's mostly a
; random collection of hacks put together based on what looked useful at the
; time.  i'm not the world's greatest elisp programmer, and this code proves
; it.
; anyway:
; individual functions are, to a varying degree, documented.  however, if
; you're thinking of calling them from somewhere else, you're probably wrong.
; most of the code here REALLY depends on being called from the request-queue
; buffer.  i'll fix that over time, but that's how the code is right now.
; i HAVE tried to select function and variable names that don't stomp on other
; peoples' namespaces.  if you find a conflict, let me know.
; what's REALLY b0rken and missing is the use of make-variable-buffer-local.
; i really need to do this, i know.

;;;
; the request-editing mode.
;;;

; configuration and support

; user customizations
; -------------------
(defvar reqq-resize-queue-window t
  "*If true, attempt to resize the queue window to 1/4 of the screen height,
 or reqq-window-min-height, whichever is larger, when displaying a request and
 the queue at the same time.  If this is nil, request-mode just lets emacs
 handle the resizing.")
(defvar reqq-queue-window-min-height 6
  "*The minimum height of the queue window when a request is being displayed.")
(defvar reqq-default-filter-args
  (concat "-owner " (user-login-name) " -unowned")
  "*The arguments to pass to request-queue-command when displaying a filtered
 queue.  Presumably, you'd want these arguments to trim down the list of items
 to, for instance, just the ones you own and the ones that are unowned.  The
 value should be a string of the arguments as they're passed on the command
 line.")
(defvar reqq-display-filter-helper t
 "*If non-nil, display (if possible) a summary of the possible filter arguments
 to request-queue-command.")
(defvar reqq-queue-buffer-name "*Request queue*"
  "Buffer name to use when displaying the queue")
(defvar reqq-helper-buffer-name "*Request queue arguments*"
  "Buffer name to use when displaying help for request-queue-command.")
(defvar req-mail-ignored-headers "^x-request-.+:"
  "*If non-nil, clear headers matching this regexp when yanking requests into a
mail message.  You may want to set this to nil for efficiency reasons if it's
covered by your default mail-yank-ignored-headers.")
(defvar reqq-hide-headers t
  "*If non-nil, hide the first set of headers when displaying a request.")
(defvar reqq-show-subject t
  "*If true, show Subject: header when hiding other headers.")
(defvar req-log-annoy-me nil
  "*If non-nil, put a line in the message area every time something happens
with the request queue.  It's actually probably a really bad idea to turn this
on, not only because it's hideously annoying, but because, in fact, emacs has
no real way of ensuring that processing output from a process happens on a
line-by-line basis.  So you may get notified about a log entry twice, or get
sent a message about several log entries at once.  Oh well, if you like having
your finger on the pulse of the queue, this is the way to do it, but don't come
crying to me when you go nuts.")
(defvar req-log-buffer-name "*Request Log*"
  "Buffer name to use when displaying the request log")
(defvar req-log-saved-lines 10
  "*If greater than 0, Number of lines to save in the request log buffer.  The
filter function which accepts output from the log process automatically trims
the buffer to this many lines.  If you don't want this trimming to happen
(i.e. you want the buffer to grow indefinitely,) set this to 0 or a negative.")
(defvar req-log-window-height 5
  "*The number of lines tall the request log window should be when it's
created.  Set to 0 to prevent the request log viewer from resizing the window
from emacs' default.")
(defvar reqq-kill-log t
  "*If non-nil, kill the request-log process when exiting request-queue-mode,
if one is running.")
(defvar reqq-auto-update nil
  "*If non-nil, update the request queue whenever an action is taken.")

; OS customizations
; -----------------
(defvar req-mail-cc-to "weblab"
  "Who should be CC'd on mail regarding requests")
(defvar req-mail-reply-to "weblab"
  "Reply-to: address for request mail")
(defvar request-queue-command "/weblab/software/bin/q"
  "Command executed to display the queue of active requests.")
(defvar reqq-helper-arg "-help"
  "Argument to request-queue-command which displays usage information")
(defvar reqq-filter-prompt (concat "queue-display command line: "
			    request-queue-command " ")
  "The prompt to display when asking for arguments to request-queue-command.")
(defvar reqq-request-display-command "/weblab/software/bin/reqshow"
  "Command executed to display the details of a single request.")
(defvar reqq-action-command "/weblab/software/bin/req"
  "Command to take actions on requests.  Should take arguments as in
   request-queue-action's alist.")
(defvar reqq-header-lines 3
  "The number of non-request lines in the output from request-queue-command.")
(defvar req-log-command "reqtail"
  "Command which displays the request queue log.")
(defvar req-log-command-args
  (concat "-" req-log-saved-lines "f")
  "arguments to the request log viewer, as a single string.")

; regexps.  be VERY careful about changing these, the code cares about the
; structure of them.  in general, the SECOND group of a regexp is the
; important one.  see the docstrings for more details
(defvar reqq-reqnum-regexp "\\(^[a-zA-Z ] *\\)\\([0-9]+\\)"
  "A regexp used to tell when we're looking at a request.  The regexp must
be divided into at least two parts, the second being the request number.")
(defvar req-mail-subject-regexp
  "\\(^subject: \\)\\(.*\\[weblab.*#[0-9]+\\].*\\)"
  "This regexp defines what a request subject looks like coming out of the
   request display.  This one cheats a little and hopes it gets lucky.  If
   you change this, note that it MUST be divided into two parts, the first
   being the RFC822 keyword (which is discarded once a match is made,) the
   second being the actual subject.")
(defvar req-mail-user-regexp
  "\\(^x-request-user: \\)\\(.*\\)"
  "This regexp defines what a request user looks like coming out of the
   request display.  The regexp MUST be divided into two groups, the first
   being the RFC822 keyword (usually X-Request-User: ), the second the
   actual list of users.")
(defvar req-log-reqnum-regexp
  "\\(.*\\) (#\\([0-9]+\\)) \\([^()]+\\)"
  "What a request log line looks like.  Again, the second group of the regexp
   must be the request number.  Keep in mind when constructing this regexp
   that the subject line (which appears in the log, usually,) may contain
   strings that look an awful lot like request numbers.  Ideally, once the
   format of a request log line stabilises a bit, we'll use this to get more
   information from the log, too.")
(defvar reqq-failed-regexp
  "Sorry, request [0-9]+ [^()]*"
  "This tries to describe what req prints when a command fails.  It's pretty
   heuristic, really, and depends a LOT on the internals of the req-operation
   code.  Unfortunately, we can't rely on exit statuses when a command fails,
   since 18.59 doesn't allow us to check them.  The important thing here is to
   make sure valid output isn't recognised as a failure message.")

; internal variables.  you really probably don't want to touch these.
(defvar reqq-current-reqnum "-1"
  "Internal -- Used to track request being viewed")
(defvar request-queue-mode-map nil "Keymap for editing the request queue")
(defvar request-comment-reqnum -1
  "Internal -- used to track request being commented on")
(defvar reqq-read-forward 1
  "Symbol to indicate request-read-reqnum should search starting on the next line.  Should be numeric.")
(defvar reqq-read-reverse -1
  "Symbol to indicate request-read-reqnum should search starting on the previous line.  Should be numeric.")
(defvar req-log-process-name "reqlog"
  "Internal name of the process accepting output from the request log")
(defvar request-comment-mode-map nil "Keymap for editing request comments")
(defvar request-mail-mode-map nil "Keymap for editing request mail")
(defvar request-log-mode-map nil "Keymap for handling the request action log")
(defvar reqq-old-window-configuration nil
  "Saved window configuration from before req-mode was started")
(defvar request-mail-old-window-configuration nil
  "Saved window configuration from before req-mail was started")
(defvar request-comment-old-window-configuration nil
  "Saved window configuration from before req-comment was started")

(defvar reqq-frame-height
  (if (>= (string-to-int emacs-version) 19)
      (function frame-height)
    (function screen-height)))

; keymaps
(if request-queue-mode-map nil
  (setq request-queue-mode-map (make-keymap))
  (suppress-keymap request-queue-mode-map)
  (define-key request-queue-mode-map " " 'find-or-scroll-request)
  (define-key request-queue-mode-map "." 'view-current-request)
  (define-key request-queue-mode-map "\177" 'find-or-scroll-request-backwards)
  (define-key request-queue-mode-map "n" 'next-request)
  (define-key request-queue-mode-map "p" 'previous-request)
  (define-key request-queue-mode-map "m" 'request-queue-send-mail)
  (define-key request-queue-mode-map "r" 'request-queue-resolve)
  (define-key request-queue-mode-map "t" 'request-queue-take)
  (define-key request-queue-mode-map "s" 'request-queue-stall)
  (define-key request-queue-mode-map "k" 'request-queue-kill)
  (define-key request-queue-mode-map "g" 'request-queue-give)
  (define-key request-queue-mode-map "S" 'request-queue-steal)
  (define-key request-queue-mode-map "M" 'request-queue-merge)
  (define-key request-queue-mode-map "c" 'request-queue-comment)
  (define-key request-queue-mode-map "P" 'request-queue-prio)
  (define-key request-queue-mode-map "R" 'request-queue-requester)
  (define-key request-queue-mode-map "\et" 'request-queue-untake)
  (define-key request-queue-mode-map "\es" 'request-queue-unstall)
  (define-key request-queue-mode-map "o" 'request-queue-open)
  (define-key request-queue-mode-map "U" 'request-queue-filtered-update)
  (define-key request-queue-mode-map "\C-cU" 'request-queue-prompted-filter)
  (define-key request-queue-mode-map "q" 'request-queue-quit)
  (define-key request-queue-mode-map "u" 'reqq-update-queue)
  (define-key request-queue-mode-map "l" 'request-log-watcher)
  (define-key request-queue-mode-map "x" 'reqq-widen-request))

; this is a little excessive, but it allows for customization
(if request-comment-mode-map nil
  (setq request-comment-mode-map (copy-keymap text-mode-map))
  (define-key request-comment-mode-map "\C-c\C-s" 'request-comment-send))

(if request-mail-mode-map
    nil
  (setq request-mail-mode-map (copy-keymap mail-mode-map))
  (define-key request-mail-mode-map "\C-c\C-r" 'request-mail-resolve)
  (define-key request-mail-mode-map "\C-c\C-g" 'request-mail-give)
  (define-key request-mail-mode-map "\C-c\C-l" 'request-mail-stall)
  (define-key request-mail-mode-map "\C-c\C-p" 'request-mail-prioritize)
  (define-key request-mail-mode-map "\C-c\C-t" 'request-mail-take)
  (define-key request-mail-mode-map "\C-c\C-u" 'request-mail-requester)
  (define-key request-mail-mode-map "\C-c\C-v" 'request-mail-pop-to-request)
  (define-key request-mail-mode-map "\C-c\C-s" 'request-mail-send)
  (define-key request-mail-mode-map "\C-c\C-y" 'request-mail-yank-original))

(if request-log-mode-map
    nil
  (setq request-log-mode-map (make-keymap))
  (suppress-keymap request-log-mode-map)
  (define-key request-log-mode-map " " 'req-log-find-request)
  (define-key request-log-mode-map "q" 'req-log-quit))

; the major mode
(put 'request-queue-mode 'mode-class 'special)
(defun request-queue-mode ()
 "Major mode for coping with the systems request queue.
'Current Request' below means the request on the line where the point is.
\\<request-queue-mode-map>
The following keys are active in this mode:
\\[find-or-scroll-request]  View or scroll the current request.\
  Also scrolls through requests.
\\[find-or-scroll-request-backwards]  Scroll the displayed request backwards,\
 stepping up through requests.
\\[next-request]  View the next request
\\[previous-request]  View the previous request
\\[view-current-request]  Reset the display and reread the current request
\\[reqq-widen-request]  Expand to show the mail headers on the current request

\\[request-queue-resolve]  Resolve the current request
\\[request-queue-kill]  Kill the current request
\\[request-queue-take]  Take the current request
\\[request-queue-untake]  Untake the current request
\\[request-queue-stall]  Stall the current request
\\[request-queue-unstall] Unstall the current request
\\[request-queue-steal]  Steal this request
\\[request-queue-open]  Explicitly reopen a resolved request
\\[request-queue-give]  Give this request to someone else (prompts)
\\[request-queue-prio]  Set a request's priority (prompts)
\\[request-queue-merge]  Merge this request with another (prompts)
\\[request-queue-requester]  Change the requesting user of the current request

\\[request-queue-comment]  Begin composing a comment on this request
\\[request-queue-send-mail]  Start a mail message to the requester\
 regarding this request
\\[request-log-watcher]  Start a process to monitor the request log

\\[reqq-update-queue]  Update the queue display
\\[request-queue-filtered-update]  Update the queue display, applying reqq-default-filter-args
\\[request-queue-prompted-filter]  Update the queue display, prompting for\
 filter arguments

\\[request-queue-quit]  Clean up and quit.

\\<request-comment-mode-map>
When commenting on a request, \\[request-comment-send] records the comment.

\\<request-mail-mode-map>
When sending mail, all the normal sendmail bindings are active.
Additionally:

\\[request-mail-resolve]  Marks the request resolved
\\[request-mail-give]  Prompts to give the request to someone else
\\[request-mail-stall]  Marks the request stalled
\\[request-mail-prioritize]  Prompts to prioritize the request
\\[request-mail-take]  Marks the request taken
\\[request-mail-requester]  Changes the requester
\\[request-mail-yank-original]  Yanks the text of the request into the message
\\[request-mail-pop-to-request]  Views the text of the request

\\<request-log-mode-map>
When in the log buffer, \\[req-log-find-request] displays the request related
to a log entry, and \\[req-log-quit] kills the process and deletes its buffer.

\\<request-queue-mode-map>
Variables controlling this mode:

reqq-resize-window         - If true, resize queue window when displaying
                             requests.
reqq-default-filter-args   - arguments to the queue-display command which
                             should filter the output in some useful fashion.
                             The contents of this variable should be a string
                             of the arguments to pass.
reqq-display-filter-helper - if true, display the legal arguments for the
                             queue command when prompting for arguments
reqq-hide-headers          - If true, hide the first set of headers on a
                             request when displaying the request, making it
                             much more readable.
                             \\[reqq-widen-request] unhides the headers.

reqq-annoy-me - If true, display chunks of the request log in the message
                area as emacs finds out about them.  See the variable
                documentation to find out why this is generally Not A Good
                Thing.

Entering this mode calls value of hook variable request-queue-mode-hook."
 ; there's nothing particularly fancy going on here.  even though the mode
 ; is somewhat complex, it doesn't need any really significant setups.
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'request-queue-mode)
  (setq mode-name "Request queue")
  (use-local-map request-queue-mode-map)
  (setq truncate-lines t)
  (set-syntax-table text-mode-syntax-table)
  (run-hooks 'request-queue-mode-hook))

; support code
(defun req ()
  "Display the req queue, and enter request-queue-mode."
  ; WHOOP WHOOP DO NOT CALL THIS FUNCTION FROM ELISP
  ; if you need to update the queue display, use reqq-update-queue.  this
  ; thing is ONLY interface code.  got it, bub?
  ; we try to be polite about the user's window configuration here, which
  ; is kinda dumb, i suppose, but since we're going to be taking over the
  ; screen while req's running, it seemed like a good idea.
  (interactive)
  (progn
    (setq reqq-old-window-configuration (current-window-configuration))
    (delete-other-windows)
    (reqq-update-queue)
    (request-queue-mode)))

(defun reqq-update-queue (&optional request-lines args)
  "Update the queue display, optionally applying a filter.  Takes two optional
arguments: REQUEST-LINES (the maximum number of lines of the queue to display,
and ARGS, a list containing the options to pass to the queue display command."
  ; yes, i KNOW i never use request-lines anywhere else.
  ; all this does is call 'q', and send its output to the queue-buffer.
  (interactive)
  (progn
    (switch-to-buffer reqq-queue-buffer-name)   ; creates buffer if necessary
    (if buffer-read-only (toggle-read-only))
    (erase-buffer)
    (message "Scanning the queue (this may take a few seconds)...")
    (display-queue nil (if request-lines
			   (concat args " :" request-lines) args))
    (if (not buffer-read-only) (toggle-read-only))
    (goto-char (point-min))
    (re-search-forward reqq-reqnum-regexp)   ; position point on first req.
    (beginning-of-line)
    (message "%d items." (reqq-count-requests))))

(defun display-queue (&optional buffer args)
  "display the request queue, by default to the current buffer.  Takes two
optional arguments: BUFFER (the buffer to send output to,) and ARGS, a string
of command-line arguments to the queue display command."
  ; really, the indentation code here should probably go someplace like
  ; reqq-update-queue, but it's unlikely anything else will want to call
  ; this.
  (let ((top (point)))
    (save-excursion
      (if args
	  (apply 'call-process request-queue-command nil
		 (if buffer buffer t) nil (string-to-list args))
	(call-process request-queue-command nil (if buffer buffer t) nil))
      (indent-rigidly top (point) 1)))) ; leave room for marks

(defun request-mark-line (mark-letter)
  "Put MARK-LETTER at the beginning of the current line."
  (save-excursion
    (beginning-of-line)
    (if buffer-read-only (toggle-read-only))
    (delete-char 1)
    (insert mark-letter)
    (if (not buffer-read-only) (toggle-read-only))))

(defun find-or-scroll-request (&optional reverse)
  "find the request associated with the request number on the line where the
point is.  if the request is already being displayed, scroll it.
Optional arg REVERSE means scroll backwards and back up through requests"
  ; most of this was written 'neath the swaying palm trees of san diego,
  ; in the terminal room at LISA VIII.  i hope you appreciate it.  i
  ; COULD'VE been out in the pool, but NO, i was in the terminal room,
  ; slaving away over a HOT X-TERMINAL, writing elisp code.
  (interactive)
  (let* ((reqnum (reqq-read-reqnum))
	 (request-buffer-name (request-buffer-from-number reqnum))
	 (request-buffer (get-buffer request-buffer-name))
	 (queue-buffer (current-buffer)))
    (if (and
	 (equal reqnum reqq-current-reqnum)
	 request-buffer)      ; this is where it starts to get stupid.
	(progn
	  (select-window (display-buffer request-buffer))
	  (if reverse
	      (if (save-excursion      ; can we scroll back any more?
		    (move-to-window-line 0)
		    (beginning-of-line)
		    (bobp))
		  (progn     ; nope.  find the next request.
		    (select-window (display-buffer queue-buffer))
		    (previous-request))
		(select-window (display-buffer request-buffer))
		(scroll-down))
	    (if (save-excursion
		  (move-to-window-line -1)
		  (end-of-line)
		  (eobp))
		(progn
		  (select-window (display-buffer queue-buffer))
		  (next-request))
	      (select-window (display-buffer request-buffer))
	      (scroll-up)))
	  (select-window (display-buffer queue-buffer)))
      (display-buffer (find-request reqnum))
      (select-window (display-buffer queue-buffer))
      (if reqq-resize-queue-window
	  (enlarge-window (- (max reqq-queue-window-min-height
				  window-min-height
				  (/ (funcall reqq-frame-height) 4))
			     (window-height)))))))

(defun find-or-scroll-request-backwards ()
  "Do the same thing as find-or-scroll-request, only in reverse.  That is,
 scroll backwards through request text and up through the queue."
  (interactive)
  (find-or-scroll-request t))

(defun next-request ()
  "Move to the next request and display it."
  (interactive)
  (progn
    (display-buffer (find-request (reqq-read-reqnum reqq-read-forward)))
    (forward-line 1)
    (if reqq-resize-queue-window
	(enlarge-window (- (max reqq-queue-window-min-height
				window-min-height
				(/ (funcall reqq-frame-height) 4))
			   (window-height))))))

(defun view-current-request ()
  "Clean up the display and view the current request, always rereading it."
  (interactive)
  (let ((current-request-buffer
	 (get-buffer (request-buffer-from-number reqq-current-reqnum))))
    (switch-to-buffer (get-buffer reqq-queue-buffer-name))
    (delete-other-windows)
    (if current-request-buffer (kill-buffer current-request-buffer))
    (find-or-scroll-request)))

(defun previous-request ()
  "Move to the previous request and display it."
  (interactive)
  (progn
    (display-buffer (find-request (reqq-read-reqnum reqq-read-reverse)))
    (forward-line -1)
    (if reqq-resize-queue-window
	(enlarge-window (- (max reqq-queue-window-min-height
				window-min-height
				(/ (funcall reqq-frame-height) 4))
			   (window-height))))))

(defun find-request (reqnum)
  "Return a buffer containing the text of request REQNUM, creating the
   buffer if necessary."
  (let* ((bufnam (request-buffer-from-number reqnum))
	 (request-buffer (get-buffer bufnam))
	 (current-request-buffer
	  (get-buffer (request-buffer-from-number reqq-current-reqnum)))
	 (caller-buffer (current-buffer)))
    (if request-buffer request-buffer
      (if current-request-buffer
	  (progn
	    (set-buffer current-request-buffer)
	    (if buffer-read-only (toggle-read-only))
	    (rename-buffer bufnam)
	    (erase-buffer)
	    (run-request-display-command reqnum)
	    (if (not buffer-read-only) (toggle-read-only))
	    (set-buffer caller-buffer)
	    (setq reqq-current-reqnum reqnum)
	    current-request-buffer)
	(let ((return-buffer (set-buffer (get-buffer-create bufnam))))
	  (run-request-display-command reqnum)
	  (if (not buffer-read-only) (toggle-read-only))
	  (set-buffer caller-buffer)
	  (setq reqq-current-reqnum reqnum)
	  return-buffer)))))

(defun request-queue-send-mail ()
  "Send mail about the current request."
  (interactive)
  (request-mail (reqq-read-reqnum)))

(defun run-request-display-command (number &optional buffer)
  "run the reqshow command on request NUMBER.  send output to BUFFER if
   specified, or to the current buffer if not.  this should be doing more
   than it is."
  (save-excursion
    (call-process reqq-request-display-command nil
		  (if buffer buffer t) nil
		  number)
    (if reqq-hide-headers (reqq-narrow-request
			   (if buffer buffer (current-buffer))))))

(defun reqq-widen-request (&optional buffer)
  (interactive)
  (let* ((req-buffer (if buffer buffer (find-request (reqq-read-reqnum))))
	 (req-window (get-buffer-window req-buffer)))
    (save-excursion
      (set-buffer req-buffer)
      (goto-char (point-min))
      (if buffer-read-only (toggle-read-only))
      (if (and reqq-show-subject
	       (re-search-forward
		(concat req-mail-subject-regexp "\n\n") (point-max) t))
	  (delete-region (match-beginning 0) (match-end 0)))
      (if (not buffer-read-only) (toggle-read-only))
      (widen))
    (if req-window
	(set-window-point req-window (point-min)))))

(defun reqq-narrow-request (&optional buffer)
  (interactive)
  (save-excursion
    (set-buffer (if buffer buffer (find-request (reqq-read-reqnum))))
    (goto-char (point-min))
    ; this next cruft is jim mokwa's fault.  he wanted this feature.
    (let ((subject-string (if (and reqq-show-subject
				   (re-search-forward
				    req-mail-subject-regexp (point-max) t))
			      (buffer-substring
			       (match-beginning 0) (match-end 0)))))
      (if (re-search-forward "^$" (point-max) t)
	(progn
	  (forward-line 1)
	  (narrow-to-region (point) (point-max))
	  (if subject-string
	      (progn
		(if buffer-read-only (toggle-read-only))
		(goto-char (point-min))
		(insert (concat subject-string "\n\n")))))))))
	    
(defun request-queue-filtered-update (&optional filter)
  "Apply a filter to the request queue, updating the queue display."
  (interactive)
  (reqq-update-queue nil (if filter filter reqq-default-filter-args)))

(defun request-queue-prompted-filter ()
  "Prompt for a filter, and apply it to the queue display."
  (interactive)
  (unwind-protect
      (save-window-excursion
	(if reqq-display-filter-helper
	    (with-output-to-temp-buffer reqq-helper-buffer-name
	      (set-buffer reqq-helper-buffer-name)
	      (call-process request-queue-command nil t nil reqq-helper-arg)))
	(request-queue-filtered-update
	 (read-string reqq-filter-prompt reqq-default-filter-args)))
    (if (get-buffer reqq-helper-buffer-name)
	(kill-buffer reqq-helper-buffer-name))))

(defun request-buffer-from-number (reqnum)
  "Construct a request buffer name from REQNUM.  Does NOT create any buffers,
just returns a string with the properly formed buffer name"
  (concat "*request-" reqnum "*"))

; simple key handlers
(defun request-queue-resolve ()
  (interactive)
  (request-queue-action ?r))
(defun request-queue-take ()
  (interactive)
  (request-queue-action ?t))
(defun request-queue-stall ()
  (interactive)
  (request-queue-action ?s))
(defun request-queue-kill ()
  (interactive)
  (request-queue-action ?k))
(defun request-queue-give ()
  (interactive)
  (request-queue-action ?g))
(defun request-queue-prio ()
  (interactive)
  (request-queue-action ?P))
(defun request-queue-steal ()
  (interactive)
  (request-queue-action ?T))
(defun request-queue-merge ()
  (interactive)
  (request-queue-action ?M))
(defun request-queue-untake ()
  (interactive)
  (request-queue-action ?U))
(defun request-queue-unstall ()
  (interactive)
  (request-queue-action ?S))
(defun request-queue-open ()
  (interactive)
  (request-queue-action ?o))
(defun request-queue-requester ()
  (interactive)
  (request-queue-action ?u))

(defun request-queue-quit ()
  (interactive)
  (let ((request-buffer
	 (get-buffer (request-buffer-from-number reqq-current-reqnum)))
	(queue-buffer
	 (get-buffer reqq-queue-buffer-name))
	(log-buffer
	 (get-buffer req-log-buffer-name)))
    (if (y-or-n-p "Really quit? ")
	(progn
	  (set-window-configuration reqq-old-window-configuration)
	  (setq reqq-old-window-configuration nil)
	  (if request-buffer (kill-buffer request-buffer))
	  (if queue-buffer (kill-buffer queue-buffer))
	  (if (and log-buffer reqq-kill-log) (kill-buffer log-buffer))))))

(defun request-queue-action (action)
  "perform an action on the request queue item where the point is"
  (let* ((success nil)
	 (output-buffer (generate-new-buffer "*request-action-output*"))
	 (queue-actions-alist '((?r . "-resolve")
				(?s . "-stall")
				(?t . "-take")
				(?k . "-kill")
				(?T . "-steal")
				(?U . "-untake")
				(?S . "-unstall")
				(?o . "-open")
				(?g . ("-give" . "To whom: "))
				(?P . ("-prio" . "Priority: "))
				(?M . ("-merge" . "With number: "))
				(?u . ("-user" . "Change requester to:"))))
	 (real-action (cdr (assoc action queue-actions-alist))))
    (save-excursion
	(unwind-protect
	  (message "Working...")
	  (if (consp real-action)
	      (call-process reqq-action-command nil
			    output-buffer
			    t
			    (car real-action)
			    (reqq-read-reqnum)
			    (prog1
				(read-from-minibuffer (cdr real-action))
			      (message "Working...")))
	    (call-process reqq-action-command nil
			  output-buffer
			  nil
			  real-action
			  (reqq-read-reqnum)))
	  (if (buffer-modified-p output-buffer)
	      (let ((cb (current-buffer))
		    (msg (progn
			   (set-buffer output-buffer)
			   (substring (buffer-string) 0 -1))))
		(message (concat "Working... " msg))
		(set-buffer cb)
		(if (not (string-match reqq-failed-regexp msg))
		    (progn
		      (if reqq-auto-update
			  (setq success t)
			(request-mark-line action))
		      (message "Working... Done."))))))
      (kill-buffer output-buffer))
    ; sigh.  can't really do this well within the save-excursion so we
    ; end up doing something really INCREDIBLY ugly here to make it look
    ; like the point never moved.  This may be somewhat weird if the request
    ; vanishes from the queue (i.e., has been resolved) while we're looking
    ; at it, but there's not much we can do, and this seems best from a UI
    ; standpoint.
    (if success
	(let ((p (point)))
	    (reqq-update-queue)
	    (goto-char p)))))

(defun reqq-read-reqnum (&optional direction)
  "Return the request number of the current request.  Optional arg DIRECTION means search on the previous line if direction = reqq-read-reverse or the next line if reqq-read-forward"
  (save-excursion
    (cond
     ((null direction)
      (beginning-of-line)
      (if (re-search-forward reqq-reqnum-regexp (point-max) t)
	  (buffer-substring (match-beginning 2) (match-end 2))
	(error "Not on a request line.")))
     ((= direction reqq-read-forward)
      (end-of-line)
      (if (re-search-forward reqq-reqnum-regexp (point-max) t)
	  (buffer-substring (match-beginning 2) (match-end 2))
	(error "No more requests!")))
     ((= direction reqq-read-reverse)
      (beginning-of-line)
      (if (re-search-backward reqq-reqnum-regexp (point-min) t)
	  (buffer-substring (match-beginning 2) (match-end 2))
	(error "No more requests!")))
     (t (error "Unknown argument to reqq-read-reqnum")))))

(defun reqq-count-requests ()
  "count the number of displayed requests"
  (- (count-lines (point-min) (point-max)) reqq-header-lines))

(defun string-to-list (string)
  (if (zerop (length string)) nil
    (let ((readmatch (read-from-string string)))
      (cons (symbol-name (car readmatch))
	    (string-to-list (substring string (cdr readmatch)))))))

;;;
; commenting on a request
;;;

(defun request-queue-comment ()
  "Enter a comment on a request in another buffer."
  (interactive)
  (let ((text-mode-map request-comment-mode-map)
	(comment-reqnum (reqq-read-reqnum)))
    (setq request-comment-old-window-configuration
	  (current-window-configuration))
    (switch-to-buffer-other-window
     (request-comment-buffer-from-number comment-reqnum))
    (setq request-comment-reqnum comment-reqnum)
    (make-local-variable 'request-comment-reqnum)
    (text-mode)
    (message
     (substitute-command-keys
      "Type \\[request-comment-send] to enter this comment into the\
 request system."))))

(defun request-comment-send ()
  (interactive)
  (message "Working...")
  (call-process-region (point-min) (point-max) reqq-action-command
		       nil nil nil "-comment" request-comment-reqnum "-")
  (message "Working... Done.")
  (kill-buffer (current-buffer))
  (set-window-configuration request-comment-old-window-configuration))

(defun request-comment-buffer-from-number (reqnum)
  "Return a proper buffer name for commenting on REQNUM.  Does not create or
view the buffer."
  ; long name, short function
  (concat "COMMENT: Request #" reqnum))

;;;
; request mail code
;;;

; not much to this one, but it's a lot prettier than the code that precedes it.
(defun request-mail (reqnum)
  "Send mail regarding request REQNUM."
  ; the grotty request-window code here is mostly an attempt to ensure that
  ; we don't end up with two windows on the queue buffer after the mail's been
  ; sent.  the problem is that the buffer changes out from under reqq's nose
  ; when we find a request, and we no longer technically have a window on it.
  (let* ((queue-buffer (current-buffer))
	 (request-window (get-buffer-window
			  (request-buffer-from-number reqq-current-reqnum)))
	 (request-buffer (find-request reqnum))
	 (request-user
	  (progn
	    (set-buffer request-buffer)
	    (reqq-widen-request request-buffer)
 	    (goto-char (point-min))
	    (re-search-forward req-mail-user-regexp)
	    (buffer-substring (match-beginning 2) (match-end 2))))
	 (request-subject
	  (progn
	    (set-buffer request-buffer)
	    (goto-char (point-max))
	    (re-search-backward req-mail-subject-regexp)
	    (buffer-substring (match-beginning 2) (match-end 2))))
	 (mail-mode-map request-mail-mode-map))
    (if reqq-hide-headers (reqq-narrow-request request-buffer))
    (set-buffer queue-buffer)
    (if request-window
	(set-window-buffer request-window request-buffer))
    (if (not request-mail-old-window-configuration) ; wow, that's a long name.
	(setq request-mail-old-window-configuration
	      (current-window-configuration)))
    (delete-other-windows)   ; this makes things look a little nicer.
    (mail-other-window nil request-user request-subject nil
		       req-mail-cc-to request-buffer)
    (save-excursion
      (my-position-on-field "Subject")
      (if req-mail-reply-to
	  (insert (concat "\nReply-to: " req-mail-reply-to))))))

(defun request-mail-send ()
  (interactive)
  (mail-send)
  (bury-buffer (current-buffer))
  (set-window-configuration request-mail-old-window-configuration)
  (set-buffer reqq-queue-buffer-name)
  (setq request-mail-old-window-configuration nil))

(defun request-mail-pop-to-request ()
  (interactive)
  (save-excursion
    (display-buffer mail-reply-buffer t)))

(defun request-mail-yank-original ()
  "Just like mail-yank-original: Yanks a request, quoted, into the current
mail message, clearing out headers according to req-mail-ignored-headers and
mail-yank-ignored-headers."
  (interactive)
  (let ((mail-yank-ignored-headers
	 (if req-mail-ignored-headers
	     (concat req-mail-ignored-headers "\\|"
		     mail-yank-ignored-headers)
	   mail-yank-ignored-headers)))
    (call-interactively 'mail-yank-original)))

(defun request-mail-resolve ()
  (interactive)
  (request-mail-action ?r))
(defun request-mail-take ()
  (interactive)
  (request-mail-action ?t))
(defun request-mail-stall ()
  (interactive)
  (request-mail-action ?l))
(defun request-mail-give ()
  (interactive)
  (request-mail-action ?g))
(defun request-mail-prioritize ()
  (interactive)
  (request-mail-action ?p))
(defun request-mail-requester ()
  (interactive)
  (request-mail-action ?u))

(defun request-mail-do ()
  "Move point to end of X-Request-Do-field.  Create field if none."
  (interactive)
  (expand-abbrev)
  (or (my-position-on-field "x-request-do" t)
      (progn (my-position-on-field "subject")
	     (insert "\nX-Request-Do: "))))

(defun request-mail-action (action)
  "Insert an X-Request-Do: header and action tag."
;; the logic for this one is pretty simple, even if the code is horrendous.
;; we take a single character off the command line, and grab the appropriate
;; header line entry out of the alist.  the only thing even vaguely interesting
;; is the fact that we use the consyness of the alist entry cdr to tell
;; whether or not we should prompt for additional information.  if so, the
;; prompt is in the cdr of the alist.  it should be possible to add more stuff
;; to the alist; just be sure to change the help text and prompts
;; appropriately.
  (interactive "cRequest action (r g l p u ?):")
  (let* ((actions-alist '((?r . "resolve")
			  (?l . "stall")
			  (?t . "take")
			  (?p . ("prio " . "Priority: "))
			  (?g . ("give " . "To whom: "))
			  (?u . ("requester " . "Requester: "))))
	 (real-action (cdr (assoc action actions-alist))))
    (cond ((= action ??)
	   (request-mail-action-helper)
	   (call-interactively 'request-mail-action))
	  ((null real-action)
	   (beep)
	   (message "Please type r, l, t, p, g, u, or ? for help.")
	   (sit-for 2)
	   (call-interactively 'request-mail-action))
	  (t
	   (expand-abbrev)
	   (save-excursion 
	       (progn (my-position-on-field "subject")
		      (insert "\nX-Request-Do: "
			      (if (consp real-action)
				  (concat (car real-action)
					  (read-from-minibuffer
					   (cdr real-action)))
				real-action))))))))

; these two are just support stuff.

(defun request-mail-action-helper ()
  (with-output-to-temp-buffer "*Help*"
    (princ "Enter the action to take on this request:
  <r>esolve:    mark the request as resolved.
  sta<l>l:      stall the request, pending further action or information
  <t>ake:       take this request
  <p>rioritize: specify a priority for this request (prompts for the priority)
  <g>ive:       give the request to someone else (prompts for user)
  req<u>ester:  mark someone else as the requester (prompts)
  or press C-g to quit.")))

; wow, is THIS a hack.  yuck.
(defun my-position-on-field (field &optional rest)
  "position on the header field FIELD, in either mail or mh-letter mode."
  (cond ((eq major-mode 'mh-letter-mode) (mh-position-on-field field rest))
	((eq major-mode 'mail-mode) (mail-position-on-field field rest))
	(t nil)))

;;;
; the request log watcher
;;;

(put 'request-log-mode 'mode-class 'special)
(defun request-log-mode ()
  "Major mode for watching the req logfile."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'request-log-mode)
  (setq mode-name "Request Log")
  (use-local-map request-log-mode-map)
  (if (not buffer-read-only) (toggle-read-only))
  (setq truncate-lines t))

(defun request-log-watcher ()
  "Start viewing the request log in a separate window."
  ; i apparently hadn't had enough sleep before writing this.  oit is uugly.
  ; owell.  it works.
  (interactive)
  (let* ((process-connection-type nil)
	 (current-window (selected-window))
	(pop-up-windows t)
	; don't even ASK about the logic behind this.
	(split-height-threshold
	 (- (window-height (next-window current-window 1)) 1))
	(req-log-buffer (get-buffer-create req-log-buffer-name))
        (log-process (or (get-process req-log-process-name)
			 (apply 'start-process req-log-process-name
				req-log-buffer req-log-command
				(string-to-list req-log-command-args)))))
      (set-process-filter log-process 'req-log-output-filter)
      (process-kill-without-query log-process)
      (pop-to-buffer req-log-buffer-name)
      (if (not (eq major-mode 'request-log-mode)) (request-log-mode))
      (if (not (= (window-height) req-log-window-height))
	  (enlarge-window (- req-log-window-height (window-height))))
      (goto-char (point-max))
      (select-window current-window)))

(defun req-log-output-filter (log-process output-string)
  "The NEW, IMPROVED ANSI standard output filter, now featuring SCROLLBACK
   CONTROL!"
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(if req-log-annoy-me (message output-string))
	(let* ((log-mark (process-mark log-process))
	       (moving (if (marker-buffer log-mark) (= (point) log-mark) nil)))
	  (set-buffer (process-buffer log-process))
	  (save-excursion
	    (goto-char (if (marker-buffer log-mark) log-mark (point-min)))
	    (if buffer-read-only (toggle-read-only))
	    (insert output-string)
 	    (set-marker log-mark (point))
	    (if (and
		 (> req-log-saved-lines 0)
		 (> (count-lines (point-min) (point-max)) req-log-saved-lines))
		(progn
		  (goto-char (point-min))
		  (forward-line (- (count-lines (point-min) (point-max))
				   req-log-saved-lines))
		  (delete-region (point-min) (point))))
	    (toggle-read-only))
	  (if moving (goto-char log-mark)))
      (set-buffer old-buffer))))

(defun req-log-read-reqnum ()
  "sorta like req-read-reqnum, but simpler."
  (end-of-line)
  (if (re-search-backward req-log-reqnum-regexp (point-min) t)
      (buffer-substring (match-beginning 2) (match-end 2))
    (error "Request not found.")))

(defun req-log-find-request ()
  "Find the request associated with the current log line."
  (interactive)
  (display-buffer (find-request (req-log-read-reqnum))))

(defun req-log-quit ()
  "Kill off the log-watcher buffer."
  (interactive)
  (let ((logbuf (get-buffer req-log-buffer-name)))
    (if (and (y-or-n-p "Really kill log process? ") logbuf)
	(kill-buffer logbuf))))
