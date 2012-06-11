(require 'moz)
(require 'json)
(require 'sgml-mode)

(defvar moz-eval-stack nil)
(defvar moz-eval-current nil)
(defvar moz-save-results "")

(defun moz-eval-dispatch-result (js fun)
  "Apply the json result"
  (save-current-buffer
    (set-buffer (car fun))
    (funcall (cdr fun) (let ((s (url-unhex-string js)))
			 (substring s 1 (1- (1- (length s))))))))

(defun moz-eval-recv (process-output)
  "Receive output from mozilla"
  (message "Receiving...")
  (save-current-buffer
    (let ((m (inferior-moz-process)))
      (set-buffer (process-buffer m))
      (setq moz-save-results (concat moz-save-results process-output))
      (when (string-match "^\\(\\w+\\)> $" moz-save-results)
	(let ((g (substring moz-save-results 0 (match-beginning 0)))
	      (e (match-end 0)))
	  (setq moz-save-results (substring moz-save-results e))
	  (moz-eval-dispatch-result g moz-eval-current))
	(remove-hook 'comint-output-filter-functions 'moz-eval-recv t)
	(moz-eval-next)))))

(defun moz-eval-async (js fin)
  "Send javascript JS, when finished, accept one JSON object and funcall fin with it"

  (cond
   (moz-eval-current
    (push (list js (current-buffer) fin) moz-eval-stack))
   (t
    (save-current-buffer
      (let ((g (current-buffer))
	    (m (inferior-moz-process)))
	(set-buffer (process-buffer m))
	(erase-buffer)
	(add-hook 'comint-output-filter-functions 'moz-eval-recv nil t)
	(setq moz-eval-current (cons g fin))
	(inferior-moz-input-sender m (concat "escape(" js ");")))))))

(defun moz-eval-next ()
  "Do next step"
  (setq moz-eval-current nil)
  (when moz-eval-stack
    (let ((g (pop moz-eval-stack)))
      (set-buffer (second g))
      (moz-eval-async (first g) (third g)))))


(defun moz-get-html ()
  "Insert the HTML in the mozilla body and pretty it up"
  (interactive)
  (moz-disable-auto-update)
  (moz-eval-async "content.document.body.innerHTML"
		  (lambda (k)
		    (save-excursion
		      (let ((m (point)))
			(insert-string k)
			(sgml-pretty-print m (point)))) )))

 
(defun moz-update (&rest ignored)
  "Update the remote mozrepl instance"
  (interactive)
  (comint-send-string (inferior-moz-process)
    (concat "content.document.body.innerHTML="
             (json-encode (buffer-string)) ";")))
 
(defun moz-enable-auto-update ()
  "Automatically the remote mozrepl when this buffer changes"
  (interactive)
  (add-hook 'after-change-functions 'moz-update t t))
 
(defun moz-disable-auto-update ()
  "Disable automatic mozrepl updates"
  (interactive)
  (remove-hook 'after-change-functions 'moz-update t))

(provide 'intermoz)