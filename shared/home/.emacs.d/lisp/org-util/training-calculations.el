(defun calc-new-max (weight reps)
  "calculate a new max weight using elpy's formula"
  (interactive "nWeight lifted: \nnReps performed: ")
  (let* ((result (+ weight (* weight reps 0.033)))
         (formatted (format "%.2f" result)))
    (kill-new formatted)
    (message "New max = %s (copied to kill-ring)" formatted)
    result))
