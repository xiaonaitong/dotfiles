;;; sudo-tramp.el --- sudo on local and remote machine use tramp

;;; Commentary:

;;; Code:

(require 'tramp)

;;; TODO not quite proper, need to fix in the future
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

(defvar sudo-tramp-prefix "/sudo:"
  (concat "Prefix to be used by sudo commands when building tramp path"))

(defun sudo-file-name (filename)
  "Convert FILENAME to sudo format."
  (set 'splitname (split-string filename ":"))
  (if (> (length splitname) 1)
      (progn (set 'final-split (cdr splitname))
             (set 'sudo-tramp-prefix "/sudo:")
             )
    (progn (set 'final-split splitname)
           (set 'sudo-tramp-prefix "/sudo:root@localhost:"))
    )
  (set 'final-fn (concat sudo-tramp-prefix (mapconcat (lambda (e) e) final-split ":")))
  (message "splitname is %s" splitname)
  (message "sudo-tramp-prefix is %s" sudo-tramp-prefix)
  (message "final-split is %s" final-split)
  (message "final-fn is %s" final-fn)
  (message "%s" final-fn))

(defun sudo-find-file (filename &optional wildcards)
  "Call `find-file` with FILENAME with `sudo-tramp-prefix` prepended, WILDCARDS is optiional."
  (interactive "fFind file with sudo ")
  (let ((sudo-name (sudo-file-name filename)))
    (apply 'find-file
           (cons sudo-name (if (boundp 'wildcards) '(wildcards))))))

(defun sudo-dired (filename)
  "Call `dired` with FILENAME with `sudo-tramp-prefix` prepended."
  (interactive "Ddired directory with sudo ")
  (let ((sudo-name (sudo-file-name filename)))
    (message sudo-name)
    (apply 'dired (list sudo-name))))

(defun sudo-reopen-file ()
  "Reopen file as root by prefixing its name with sudo-tramp-prefix and by clearing `buffer-read-only`."
  (interactive)
  (let*
      ((file-name (expand-file-name (or buffer-file-name default-directory)))
       (sudo-name (sudo-file-name file-name)))
    (progn
      (setq buffer-file-name sudo-name)
      (rename-buffer sudo-name)
      (setq buffer-read-only nil)
      (message (concat "File name set to " sudo-name)))))

(provide 'sudo-tramp)
;;; sudo-tramp ends here
