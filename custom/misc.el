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
