;;; terminology -- Helm Terminal Management
;;; Author: Tim Stewart <tim.j.stewart at gmail dot com>

(defun terminology-terminals ()
  "returns a list of all terminals that currently exist."
  (cl-remove-if-not #'terminology--terminal-buffer-p (buffer-list)))

(defun terminology--terminal-buffer-p (buffer)
  "Returns true if and only if *buffer* is a buffer."
  (cl-assert (bufferp buffer))
  (equal "vterm-mode"
         (symbol-name (buffer-local-value 'major-mode buffer))))

(defun terminology--terminal-buffer-name-p (buffer-name)
  "Returns true if and only if *buffer-name* is the name of a terminal buffer."
  (cl-assert (stringp buffer-name))
  (let ((buf (get-buffer buffer-name)))
    (if buf
        (terminology--terminal-buffer-p buf)
      nil)))

(defun terminology--terminal-buffer-names ()
  "returns a list of buffer names for all terminals that currently exist."
  (mapcar (lambda (buf) (buffer-name buf))
          (terminology-terminals)))

;; TODO: Make this customize-able (e.g. defgroup/defcustom).
(setq terminology--terminal-alist
      '(("sbt"     . ((commands . "[ -e sbt.sh ] && ./sbt.sh")
                      (insert-mode . t)))
        ("htop"    . ((commands . "htop")
                      (insert-mode . nil)))
        ("bastion" . ((commands . "ssh bastion")
                      (insert-mode . t)))))

(defun terminology--switch-to-terminal (buffer-name)
  "Switches to a terminal buffer named *buffer-name*"
  (cl-assert (stringp buffer-name))
  (if (terminology--terminal-buffer-name-p buffer-name)
      (pop-to-buffer (get-buffer buffer-name))
    (error "%s is not a terminal buffer.")))

(defun terminology--create-terminal (buffer-name)
  "Creates a new vterm buffer with the provided *buffer-name*."
  (cl-assert (stringp buffer-name))
  (vterm buffer-name)
  ;; TODO: Customize whether or not insert mode should be entered.
  (evil-insert-state)
  (let ((alist (assoc buffer-name terminology--terminal-alist)))
    (if alist
        ;; TODO: Allow multiple commands
        (let ((command (assoc 'commands alist)))
          (when command
            (vterm-send-string (cdr command))
            (vterm-send-return))))))

(defun terminology--helm-candidate-transformer (candidates _source)
  "Returns a list of candidates based on which candidates match the currently
entered text."
  (or candidates
      (list helm-pattern)))

(defun terminology-vterm-profile-names ()
  (mapcar 'car terminology--terminal-alist))

(defun terminology--vterm-kill (buffer-name)
  (cl-assert (stringp buffer-name))
  (kill-buffer-ask (get-buffer buffer-name)))

(defun terminology--vterm-rename (buffer-name)
  (cl-assert (stringp buffer-name))
  (let ((new-name (read-string "Enter new name: " buffer-name)))
    (when new-name
      (with-current-buffer buffer-name
        (rename-buffer new-name)))))

(defun terminology-helm-terminals ()
  "Command that lets the user switch an existing terminal buffer or create a new
vterm buffer."
  (interactive)
  (let* ((switch-to '((name . "Switch to Terminal:")
                      (candidates . terminology--terminal-buffer-names)
                      (action . (("Switch to Terminal" . terminology--switch-to-terminal)
                                 ("Rename Terminal Buffer" . terminology--vterm-rename)
                                 ("Kill Terminal" . terminology--vterm-kill)))))
         (create    '((name . "Create Terminal:")
                      (candidates . terminology-vterm-profile-names)
                      (filtered-candidate-transformer . terminology--helm-candidate-transformer)
                      (action . (("Create Terminal" . terminology--create-terminal))))))
    (helm :prompt "Terminal: "
          :sources '(switch-to create))))

(provide 'terminology)
