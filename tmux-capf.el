;; tmux-capf.el ;; -*- lexical-binding: t -*-


(defconst tmux-capf-executable-path (expand-file-name "tmuxcomplete.sh" (file-name-directory load-file-name)))

(defun tmux-capf (&optional interactive)
  "Completion at point from tmux."
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions (list 'tmux-capf)))
        (completion-at-point))
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (candidates (split-string (shell-command-to-string (concat "bash " tmux-capf-executable-path))))
           (start (or (car bounds) (point)))
           (end (or (cdr bounds) (point))))
      (list
       start
       end
       candidates
       :exclusive 'no
       :company-kind (lambda (_) nil) ;;  TODO: which icons should be used?
       ))))

(provide 'tmux-capf)
