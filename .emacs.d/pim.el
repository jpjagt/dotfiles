(defun maak-belachelijk ()
  (interactive)
  (let ((initial-end (region-end)))
  (if (use-region-p)
      (save-excursion
        (goto-char (region-beginning))
        (while (<= (point) initial-end)
          (message (format "%s"(point)))
          (goto-char (+
                      (+ (floor (* 3 (/ (abs (random)) (float most-positive-fixnum)))) 1)
                      (point)))
          (if (<= (point) initial-end)
                  (call-interactively 'upcase-char)))))))
