
(defun copy-current-word ()
  "Copy current word into kill ring"
  (interactive)
  (kill-new (current-word t t)))

(defun w3m-translate-word ()
  "Translate current word using http://www.wordreference.com"
  (interactive)
  (w3m (concat "http://www.wordreference.com/fren/" (current-word)) t))

