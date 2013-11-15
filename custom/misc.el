;;-----------------------------------
;; This file is essentially my meandering musings of things eLISP.
;; Here is a nice initial quick ref:
;; http://ergoemacs.org/emacs/elisp_basics.html
;;-----------------------------------


;;-----------------------------------
;; Launch a quick search to Google...
;;-----------------------------------
(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

(global-set-key "\C-c\C-g" 'google)

;;-----------------------------------
;; To have a terminal window *in* emacs quick at hand
;;-----------------------------------
(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(global-set-key "\C-ct" 'visit-term-buffer)

;;-----------------------------------
;; Handy when wanting to clean up a ton of buffers after a long editing session
;;-----------------------------------
(defun kill-other-buffers ()
  "Kill all buffers but the current one. Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))
;;(global-set-key "\C-c\C-k" 'kill-other-buffers)


;;-----------------------------------
;; Automatically save and restore sessions
;; (There are times I want to "get back to where I was")
;; http://stackoverflow.com/questions/4477376/some-emacs-desktop-save-questions-how-to-change-it-to-save-in-emacs-d-emacs
;; http://www.emacswiki.org/emacs/AnsiTerm
;;-----------------------------------
(let ((my-desktop-dirname "~/.emacs-desktops/"))
  (if (not (file-directory-p my-desktop-dirname))
      (progn
        (message "Creating desktop directory: %s" my-desktop-dirname)
        (make-directory my-desktop-dirname ))
      (message "Already present: %s" my-desktop-dirname)
    )
  )

(setq desktop-dirname             "~/.emacs-desktops/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      ;;desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil)

(defun load-my-desktop ()
  "Load the desktop and enable autosaving"
  (interactive)
  (let ((desktop-load-locked-desktop "ask"))
    (desktop-read)
    (desktop-save-mode 1)))

;;-----------------------------------
;; Moving lines around...
;;-----------------------------------
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(global-set-key [S-C-up] 'move-line-up)

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [S-C-down] 'move-line-down)

;;-----------------------------------
;; Be able to go backward with buffer traversing selection
;;-----------------------------------
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

;;-----------------------------------
;; flyspell...
;;-----------------------------------
;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(global-set-key (kbd "C-x C-r") 'sudo-edit)

;;-----------------------------------
;; Smarter move to beginning of line
;;-----------------------------------
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

  Move point to the first non-whitespace character on this line.
  If point is already there, move to the beginning of the line.
  Effectively toggle between the first non-whitespace character and
  the beginning of the line.

  If ARG is not nil or 1, move forward ARG - 1 lines first.  If
  point reaches the beginning or end of the buffer, stop there."

  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
;;(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;;-----------------------------------
;; eval and replace...
;;-----------------------------------

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)

;;-----------------------------------
;; smart open line (w/ above)
;;-----------------------------------
(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key [(shift return)] 'smart-open-line)
(global-set-key (kbd "M-o") 'smart-open-line)

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift return)] 'smart-open-line-above)
(global-set-key (kbd "M-O") 'smart-open-line-above)
