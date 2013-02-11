(defun isbell-frills-eval-and-insert ()
  (interactive)
  (insert "\n")
  (eval-last-sexp t)
  (insert "\n"))
(global-set-key "\C-j" 'isbell-frills-eval-and-insert)

  
  