;;; moo-client protocol

(defvar moo-mcp-regexp (concat "^#\\$#"
			       "\\([^ :*\\\"]+\\)"         ; request-name
			       "\\(\\*\\)?"                ; data-follows
			       "\\( +\\([^ :\\\"]+\\)\\( \\|$\\)\\)?"
					; authentication-key
			       "\\( *\\(.*\\)\\)$"))       ; key-value pairs

(defvar moo-mcp-quoting-prefix "@@@"
  "Prepended to all data sent after an mcp command.
Must not contain any special regexp characters.")

(defvar moo-mcp-incomplete nil)

(defvar moo-mcp-quoting-end "#$#END"
  "Signals end of mcp data.")

(defvar moo-mcp-cleanup-function nil)

(defun moo-mcp-init-connection ()
  "Make up a new auth-key and send it; send client options."
  (interactive)
  (make-local-variable 'moo-mcp-auth-key)
  (setq moo-mcp-auth-key (prin1-to-string (random)))
  (let ((proc (get-buffer-process (current-buffer))))
    (mud-send-string (concat "#$#authentication-key " moo-mcp-auth-key) proc)
    (mud-send-string (apply 'concat
			    "#$#client-options"
			    (mapcar (function
				     (lambda (entry)
				       (concat " " (car (car entry)))))
				    moo-mcp-request-table)) proc)))

(defun moo-mcp-dispatch (request-name data-follows auth-key keyval-string)
  "Figure out if we know what to do with the given request;
check the auth-key if it's important;
check that we have all the args we want;
if data-follows, start gathering it."
  (let ((entry (assoc (cons request-name data-follows) moo-mcp-request-table)))
    (if entry
	(if (and (moo-mcp-need-auth-key entry)
		 (not (equal auth-key moo-mcp-auth-key)))
	    (error "Illegal authentication key in %s" (buffer-name))
	  (let ((arglist (moo-mcp-arglist entry keyval-string)))
	    (if (listp arglist)
		(apply (moo-mcp-setup-function entry) arglist)
	      (moo-mcp-handle-unknown data-follows))))
      (moo-mcp-handle-unknown data-follows))))

(defun moo-mcp-setup-function (entry)
  "What function do we call to deal with this entry in the table?"
  (nth 2 (cdr entry)))

(defun moo-mcp-handle-unknown (data-follows)
  (if data-follows
      (let* ((start (point))
	     (line (progn
		     (beginning-of-line 2)
		     (buffer-substring start (point)))))
	(let ((buf (current-buffer)))
	  (set-buffer (moo-mcp-setup-data (generate-new-buffer
					   "Unknown data")))
	  (insert line)
	  (set-buffer buf)))
    (moo-mcp-remove-line)))

(defun moo-mcp-arglist (entry keyval-string)
  (let ((alist (moo-mcp-parse-keyvals keyval-string)))
    (if (listp alist)
	(catch 'moo-mcp-missing-arg
	  (mapcar
	   (function
	    (lambda (template)
	      (let ((a (assoc (car template) alist)))
		(cond (a
		       (cdr a))
		      ((not (eq 'required (cdr template)))
		       (cdr template))
		      (t
		       (throw 'moo-mcp-missing-arg 'moo-mcp-missing-arg))))))
	   (nth 1 (cdr (setq barr entry))))))))

(defvar moo-mcp-keyval-regexp (concat "\\([^ \\\":]+\\): +"
				      "\\([^ \\\"]+\\|"
				      "\"\\([^\\\"]\\|\\\\.\\)*\"\\)"
				      "\\( +\\|$\\)")
  "Recognize one keyword: value pair.")

(defun moo-mcp-parse-keyvals (keyval-string)
  (catch 'moo-mcp-failed-parse
    (let ((arglist nil)
	  (start 0))
      (while (< start (length keyval-string))
	(if (string-match moo-mcp-keyval-regexp keyval-string start)
	    (setq start (match-end 0)
		  arglist (cons
			   (cons
			    (mud-match-string 1 keyval-string)
			    (if (eq (elt keyval-string
					 (match-beginning 2))
				    ?\")
				(car (read-from-string
				      (mud-match-string 2 keyval-string)))
			      (mud-match-string 2 keyval-string)))
			   arglist))
	  (throw 'moo-mcp-failed-parse 'moo-mcp-failed-parse)))
      arglist)))
				
(defvar moo-mcp-request-table '()
  "Alist of information about known request types, keyed by string.")

(defun moo-mcp-register (request-name data-follows auth-key keys
				      setup-function)
  "Register a new mcp request type.
REQUEST-NAME is the name of the request type, e.g. \"edit\".
DATA-FOLLOWS is t if the MOO is expected to provide lines of data.
If you want a request to be able to handle both forms, set them up separately.
AUTH-KEY is t if this request type needs an authentication key to work.
KEYS is an alist of pairs (key . default-value).
  The key must be a string.
  The default-value must be a string, or the symbol 'required, which means
that the request must supply a value.
SETUP-FUNCTION is a symbol for the function that gets called to set up
request-specific details."
  (let* ((key (cons request-name data-follows))
	 (value (list auth-key keys setup-function))
	 (entry (assoc key moo-mcp-request-table)))
    (if entry
	(setcdr entry value)
      (setq moo-mcp-request-table
	    (cons (cons key value)
		  moo-mcp-request-table)))))

(defun moo-mcp-need-auth-key (entry)
  "Does this entry in the table need an authentication key?"
  (car (cdr entry)))

(defun moo-mcp-remove-line ()
  (let ((start (point)))
    (beginning-of-line 2)
    (delete-region start (point))))

(defun moo-mcp-setup-data (buffer)
  "Set up BUFFER to receive data.  Return BUFFER."
  (moo-mcp-remove-line)
  (setq moo-state 'unquoting
	mud-current-process (get-buffer-process (current-buffer))
	moo-buffer buffer))
  
(moo-mcp-register "edit" t nil
		  '(("type" . "text")
		    ("name" . 'required)
		    ("upload" . 'required))
		  'moo-mcp-start-edit)

(defun moo-mcp-start-edit (type name upload)
  (let ((buf (current-buffer)))
    (set-buffer (moo-mcp-setup-data (get-buffer-create name)))
    (erase-buffer)
    (setq moo-mcp-cleanup-function
	  (if (equal type "program")
	      'moo-mcp-cleanup-edit-program
	    'moo-mcp-cleanup-edit-text))
    (insert upload "\n")
    (set-buffer buf)))

(defun moo-mcp-cleanup-edit-program ()
  (moo-code-mode)
  (goto-char (point-max))
  (insert ".\n")
  (goto-char (point-min))
  (setq mud-select-buffer (current-buffer)))

(defun moo-mcp-cleanup-edit-text ()
  (mud-macro-expansion-mode)
  (goto-char (point-max))
  (insert ".\n")
  (goto-char (point-min))
  (setq mud-select-buffer (current-buffer)))

(moo-mcp-register "ftp" nil t
		  '(("host" . 'required)
		    ("directory" . 'required)
		    ("file" . 'required)
		    ("type" . 'required)
		    ("destination" . 'required))
		  'moo-mcp-do-ftp)

(defun moo-mcp-do-ftp (host directory file type destination)
  (moo-mcp-remove-line)
  (call-process "fetch-file" nil 0 nil
		host dir file type dest))

(moo-mcp-register "gopher" nil t
		  '(("host" . 'required)
		    ("port" . "70")
		    ("path" . "")
		    ("description" . "1"))
		  'moo-mcp-do-gopher)

(defvar moo-mcp-gopher-buffer nil
  "Buffer to place gopher stuff in.")

(defun moo-mcp-do-gopher (host port path description)
  (if (not (fboundp 'gopher-set-object-host))
      (load-library "gopher"))  ; gross hack to get around lack of provide
  (moo-mcp-remove-line)
  (let ((here (current-buffer)))
    (gopher-set-object-host gopher-root-node host)
    (setq port (car (read-from-string port)))
    (gopher-set-object-port gopher-root-node port)
    (gopher-dispatch-object
     (vector (aref description 0)
	     (substring description 1)
	     path
	     host
	     port)
     moo-mcp-gopher-buffer)
    (cond ((not (equal here (current-buffer)))
	   (setq moo-mcp-gopher-buffer (current-buffer))
	   (setq mud-select-buffer moo-mcp-gopher-buffer)
	   (set-buffer here)))))
  
