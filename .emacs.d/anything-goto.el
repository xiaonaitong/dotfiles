;;; anything-goto.el --- Quick listing of:
;;; modified from anything-git-goto
;;; support hg and git, may support other dvcs
(require 'anything)

;;; --------------------------------------------------------------------
;;; - Customization
;;;
(defvar *anything-goto-buffer-name*
  "*Anything goto*")

;;; anything-git-goto
;;; remove '-cmo', which scanning all directory for untracked and
;;; modified files
;;; this is much quicker than the default
(defvar git-goto-cmd
  "cd %s && git       \
  --no-pager ls-files \
  --exclude-standard  \
  --directory")

(defvar hg-goto-cmd
  "cd %s && hg locate")

(defun anything-goto-find-dvcs-repo (dir)
  "Recursively search for a .git/ or .hg/ directory."
  (if (string= "/" dir)
      nil ;; not in a dvcs repo
    (if (or (file-exists-p (expand-file-name ".git/" dir))
            (file-exists-p (expand-file-name ".hg/" dir)))
        dir
      (anything-goto-find-dvcs-repo (expand-file-name "../" dir)))))

(defun anything-goto-file  (file-content)
  "Visit the source for the file result."
  (setq full-file-path
           (expand-file-name file-content
                             (expand-file-name
                              (anything-goto-find-dvcs-repo file-content)
                              (anything-attr 'pwd))))
  (if (file-exists-p full-file-path)
      (find-file full-file-path))
  (kill-buffer *anything-goto-buffer-name*))

(defun anything-dvcs-ls-command (dir)
  "List all files in the dvcs repo"
  (let ((goto-cmd (if (file-exists-p (expand-file-name ".git/" dir))
                       git-goto-cmd
                     hg-goto-cmd)))
    (format goto-cmd (anything-goto-find-dvcs-repo  dir))))

(defvar anything-c-source-goto
  '((name . "Dvcs goto")
    (init . (lambda ()
              (let ((dcvs-root
                     (anything-goto-find-dvcs-repo  default-directory)))
                (and dcvs-root
                    (call-process-shell-command
                     (anything-dvcs-ls-command dcvs-root)
                     nil (anything-candidate-buffer 'global))))))
    (candidate-number-limit . 9999)
    (candidates-in-buffer)
    (action . anything-goto-file))
  "Dvcs goto.")

(defvar anything-c-source-goto-subdir
  '((name . "Dvcs goto")
    (init . (lambda ()
              (let ((dcvs-root
                     (anything-goto-find-dvcs-repo  default-directory)))
                (and dcvs-root
                    (call-process-shell-command
                     (concat (anything-dvcs-ls-command dcvs-root) " " (file-relative-name default-directory dcvs-root))
                     nil (anything-candidate-buffer 'global))))))
    (candidate-number-limit . 9999)
    (candidates-in-buffer)
    (action . anything-goto-file))
  "Dvcs goto.")

;;;###autoload
(defun anything-goto ()
  "Dvcs Find files."
  (interactive)
  (if (anything-goto-find-dvcs-repo default-directory)
      (anything-other-buffer
       '(anything-c-source-goto) *anything-goto-buffer-name*)
    (message "Not in a git or hg repo")))

(defun anything-goto-subdir ()
  "Dvcs Find files."
  (interactive)
  (if (anything-goto-find-dvcs-repo default-directory)
      (anything-other-buffer
       '(anything-c-source-goto-subdir) *anything-goto-buffer-name*)
    (message "Not in a git or hg repo")))

(provide 'anything-goto)


;;; anything-goto.el ends here
