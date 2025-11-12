;;; time-tracking.el --- Automatic Org-mode clocking by TODO state

;; Variables to define which states trigger clock-in/out
(defvar my/org-auto-clock-in-states '("ACTIVE")
  "List of TODO states that trigger an automatic clock-in.")
(defvar my/org-auto-clock-out-on-leave-states '("ACTIVE")
  "List of TODO states that, when left, trigger an automatic clock-out (unless
immediately entering another clock-in state).")

(defun my/org-auto-clock-by-state ()
  "Clock in when entering certain states; clock out when leaving them."
  (when (derived-mode-p 'org-mode)
    (let ((new org-state)
          (old org-last-state))
      (when (and new (member new my/org-auto-clock-in-states))
        (org-clock-in))
      (when (and old (member old my/org-auto-clock-out-on-leave-states)
                 (not (and new (member new my/org-auto-clock-in-states))))
        (when (org-clock-is-active) (org-clock-out))))))

(add-hook 'org-after-todo-state-change-hook #'my/org-auto-clock-by-state)

(provide 'time-tracking)
