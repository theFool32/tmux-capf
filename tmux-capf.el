;; tmux-capf.el ;; -*- lexical-binding: t -*-


(defvar capf-tmux--min-length 5)

(defsubst capf-tmux--in-tmux-p ()
  (or (getenv "TMUX") (getenv "TMUX_PANE")))

(cl-defstruct (tmux-pane) session-id id pane-active window-active)

(defun capf-tmux--parse-fields (line)
  (let ((fields (split-string line " " t)))
    (destructuring-bind (session-id pane-id pane-active window-active) fields
      (make-tmux-pane :id pane-id
                      :session-id session-id
                      :pane-active pane-active
                      :window-active window-active))))

(defun capf-tmux--collect-panes ()
  (with-temp-buffer
    (unless (zerop (call-process "tmux" nil t nil "list-panes" "-a" "-F" "#{session_id} #{pane_id} #{pane_active} #{window_active}"
                                 ))
      (error "Failed: 'tmux list-panes -F #P"))
    (let* ((lines (split-string (buffer-string) "\n" t))
           (panes (mapcar 'capf-tmux--parse-fields lines)))
      (reverse panes))))

(defun capf-tmux--trim (str)
  (let ((left-trimed (if (string-match "\\`[ \t\n\r]+" str)
                         (replace-match "" t t str)
                       str)))
    (if (string-match "[ \t\n\r]+\\'" left-trimed)
        (replace-match "" t t left-trimed)
      left-trimed)))

(defun capf-tmux--split-line (line)
  (unless (string-match-p "\\`\\s-*\\'" line)
    (mapcar 'capf-tmux--trim (append (split-string line)
                                     (split-string line "[^a-zA-Z0-9_]+")))))

(defun capf-tmux--remove-space-candidates (candidates)
  (cl-remove-if (lambda (c) (string-match-p "\\`\\s-*\\'" c)) candidates))

(defun capf-tmux--parse-capture-output ()
  (goto-char (point-min))
  (let ((candidates nil))
    (while (not (eobp))
      (let* ((line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))
             (words (capf-tmux--split-line line)))
        (when words
          (setq candidates (append words candidates)))
        (forward-line 1)))
    candidates))

(defun capf-tmux--capture-pane (pane)
  (with-temp-buffer
    (unless (zerop (call-process "tmux" nil t nil
                                 "capture-pane" "-J" "-p" "-t" (tmux-pane-id pane)))
      (error "Failed: 'tmux capture-pane -J -p -t %s'" (tmux-pane-id pane)))
    (let* ((candidates (capf-tmux--parse-capture-output))
           (sorted (sort candidates 'string<)))
      (cl-delete-duplicates sorted :test 'equal)
      (capf-tmux--remove-space-candidates sorted))))

(defun capf-tmux--collect-candidates (panes)
  (cl-loop for pane in panes
           unless (and
                   (string-equal (tmux-pane-window-active pane) (tmux-pane-pane-active pane))
                   (string-equal (tmux-pane-session-id pane) (capf-tmux--current-session)))
           append (capf-tmux--capture-pane pane)))

(defun capf-tmux--filter-candidates (prefix candidates)
  (cl-loop with regexp = (format "\\`%s." prefix)
           for candidate in candidates
           when (and (length> candidate capf-tmux--min-length) (string-match-p regexp candidate))
           collect candidate))

(defun capf-tmux--current-session()
  (with-temp-buffer
    (unless (zerop (call-process "tmux" nil t nil
                                 "display-message" "-p" "#{session_id}"))
      (error "Failed: 'tmux display-message -p #{session_id}"))
    (capf-tmux--trim (buffer-string))))

(defun capf-tmux--candidates (prefix)
  (unless (capf-tmux--in-tmux-p)
    (error "Not running inside tmux!!"))
  (let* ((candidates (capf-tmux--collect-candidates
                      (capf-tmux--collect-panes)))
         (filtered (capf-tmux--filter-candidates prefix candidates)))
    filtered))

(defun tmux-capf (&optional interactive)
  "Completion at point from tmux."
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions (list 'tmux-capf)))
        (completion-at-point))
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (or (car bounds) (point)))
           (end (or (cdr bounds) (point)))
           (prefix (if (and start end)
                       (buffer-substring-no-properties start end)
                     ""))
           (candidates (capf-tmux--candidates prefix)))
      (list
       start
       end
       candidates
       :exclusive 'no
       :company-kind (lambda (_) (intern "tmux"))
       ))))

(provide 'tmux-capf)
