(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
  '((".*\\.bnc.s-cloud.net" "#99problems" "#eng-doc" "#ops" "#bazooka" "#weloveuptime" "#mothership" "#soundcloud" "#lol")))

(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                 "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "bnc.s-cloud.net:6697") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc-tls :server "bnc.s-cloud.net" 
               :port 6697
               :nick "gavin"
               :full-name "Gavin M. Bell"))))

;; switch to ERC with Ctrl+c e
(global-set-key (kbd "C-c e") 'erc-start-or-switch) ;; ERC
