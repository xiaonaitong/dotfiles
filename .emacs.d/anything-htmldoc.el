(defvar anything-htmldoc-index-command
  (concat "python " (file-name-directory load-file-name) "anything-indexer.py %s")
  "python program used to generate index")

(defun anything-htmldoc-source (arg &optional mode)
  `((name . "Online Manual")
    (init . (lambda ()
              (anything-htmldoc-cache
               ,(format "/tmp/.anything-htmldoc-%s-cache" arg)
               (anything-candidate-buffer 'global)
               (lambda (buf)
                 (call-process-shell-command
                  ,(format anything-htmldoc-index-command arg)
                  nil buf)))))
    (candidate-number-limit . 9999)
    (candidates-in-buffer)
    (type . ,(if (string= mode "simple")
                 'simple-doc-line
               'doc-line))))

(defvar anything-htmldoc-field-delemiter "<->")

(defun anything-htmldoc-cache (cache-file cand-buffer create-index)
  (if (file-exists-p cache-file)
      (with-current-buffer cand-buffer
        (erase-buffer)
        (let ((b (find-file-noselect cache-file t t)))
          (unwind-protect
              (insert (with-current-buffer b
                        (buffer-substring (point-min) (point-max))))
            (kill-buffer b))))
    (progn
      (with-temp-file cache-file
        (prog1 nil
          (funcall create-index (current-buffer))))
      (anything-htmldoc-cache cache-file
                              cand-buffer create-index))))

(define-anything-type-attribute 'doc-line
  `((filtered-candidate-transformer anything-doc-line-transformer)
    (multiline)
    (action ("Go to" . anything-doc-line-goto)))
  "")

(define-anything-type-attribute 'simple-doc-line
  `((filtered-candidate-transformer anything-doc-line-transformer-simple)
    (action ("Go to" . anything-doc-line-goto)))
  "")

(defun anything-doc-line-goto (name-and-description-and-url)
  (message "%s" (second name-and-description-and-url))
  (message "%s" (car name-and-description-and-url))
  (browse-url (second (cdr name-and-description-and-url))))

(defun anything-doc-line-transformer (candidates source)
  (delq nil (mapcar 'anything-doc-line-transformer-1 candidates)))

(defun anything-doc-line-transformer-1 (candidate)
  (when (string-match (rx (group (+? nonl)) "<->" (group (+? nonl)) "<->" (group (+ nonl)))
                      candidate)
    (let ((name (match-string 1 candidate))
          (description (match-string 2 candidate))
          (link (match-string 3 candidate)))
      (cons (format "%s:\n %s\n %s"
                    (propertize name 'face compilation-info-face)
                    description
                    (propertize link 'face compilation-line-face))
            (list name description link)))))

(defun anything-doc-line-transformer-simple (candidates source)
  (delq nil (mapcar 'anything-doc-line-transformer-simple-1 candidates)))

(defun anything-doc-line-transformer-simple-1 (candidate)
  (when (string-match (rx (group (+? nonl)) "<->" (group (+ nonl)))
                      candidate)
    (let ((full-name (match-string 1 candidate))
          (link (match-string 2 candidate)))
      (if (string-match (rx (group (+ nonl)) "." (group (+ nonl))) full-name)
          (cons (format "%s.%s"
                        (propertize (match-string 1 full-name) 'face font-lock-constant-face)
                        (propertize (match-string 2 full-name) 'face font-lock-type-face))
                (list full-name "" link))
        (cons (format "%-50s%s"
                      (propertize full-name 'face font-lock-function-name-face)
                      ;; for c/c++ file show link
                      (let ((pos (anything-htmldoc-index-of (rx (or "/cpp/" "/c/")) link)))
                        (if pos
                            (substring link (anything-htmldoc-index-of (rx (or "/cpp/" "/c/")) link))
                          ;; for sphinx file show module
                          ;; link format
                          ;; context/api/package/module.html
                          (if (anything-htmldoc-end-index-of (rx (or "/api/" "/library/")) link)
                              (let ((api-pos (anything-htmldoc-end-index-of (rx (or "/api/" "/library/")) link))
                                    (html-pos (anything-htmldoc-index-of (rx ".html") link)))
                                (replace-regexp-in-string "/" "." (substring link api-pos html-pos)))
                            ""))))
              (list full-name "" link))
          ))))

(defun anything-htmldoc-index-of (needle s &optional ignore-case)
  "Returns index of NEEDLE in S, or nil.

If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
  (let ((case-fold-search ignore-case))
    (string-match-p needle s)))

(defun anything-htmldoc-end-index-of (needle s &optional ignore-case)
  "Returns end match index of NEEDLE in S, or nil.

If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
  (let ((case-fold-search ignore-case))
    (if (string-match needle s)
        (match-end 0))))

(defun anything-htmldoc-jquery ()
  (interactive)
  (anything-other-buffer (anything-htmldoc-source "jquery") "*anything doc jquery*"))

(defun anything-htmldoc-jdk6 ()
  (interactive)
  (anything-other-buffer (anything-htmldoc-source "jdk6" "simple") "*anything doc jdk*"))

(defun anything-htmldoc-jdk6-detail ()
  (interactive)
  (anything-other-buffer (anything-htmldoc-source "jdk6-detail" "simple") "*anything doc jdk6 detail*"))

(defun anything-htmldoc-jdk7 ()
  (interactive)
  (anything-other-buffer (anything-htmldoc-source "jdk7" "simple") "*anything doc jdk7*"))

(defun anything-htmldoc-jdk7-detail ()
  (interactive)
  (anything-other-buffer (anything-htmldoc-source "jdk7-detail" "simple") "*anything doc jdk7 detail*"))

(defun anything-htmldoc-stdc ()
  (interactive)
  (anything-other-buffer (anything-htmldoc-source "stdc" "simple") "*anything doc stdc*"))

(defun anything-htmldoc-stdcpp ()
  (interactive)
  (anything-other-buffer (anything-htmldoc-source "stdcpp" "simple") "*anything doc stdcpp*"))

(defun anything-htmldoc-jee6 ()
  (interactive)
  (anything-other-buffer (anything-htmldoc-source "jee6" "simple")
                         "*anything doc jee6*"))

(defun anything-htmldoc-spring ()
  (interactive)
  (anything-other-buffer (anything-htmldoc-source "spring" "simple")
                         "*anything doc spring*"))

(defun anything-htmldoc-netty ()
  (interactive)
  (anything-other-buffer (anything-htmldoc-source "netty" "simple") "*anything doc netty *"))

(defun anything-htmldoc-pymongo ()
  (interactive)
  (anything-other-buffer (anything-htmldoc-source "pymongo" "simple") "*anything doc pymongo *"))

(defun anything-htmldoc-py ()
  (interactive)
  (anything-other-buffer (anything-htmldoc-source "python_std" "simple") "*anything doc python *"))

(defun anything-htmldoc-mysql ()
  (interactive)
  (anything-other-buffer (anything-htmldoc-source "mysql" "simple") "*anything doc mysql *"))

(global-set-key (kbd "<f1> j q") 'anything-htmldoc-jquery)
(global-set-key (kbd "<f1> j j") 'anything-htmldoc-jdk6)
(global-set-key (kbd "<f1> 7 j") 'anything-htmldoc-jdk7)
(global-set-key (kbd "<f1> j d") 'anything-htmldoc-jdk6-detail)
(global-set-key (kbd "<f1> 7 d ") 'anything-htmldoc-jdk7-detail)
(global-set-key (kbd "<f1> ;") 'anything-htmldoc-stdc)
(global-set-key (kbd "<f1> SPC") 'anything-htmldoc-stdcpp)
(global-set-key (kbd "<f1> j e") 'anything-htmldoc-jee6)
(global-set-key (kbd "<f1> s") 'anything-htmldoc-spring)
(global-set-key (kbd "<f1> n") 'anything-htmldoc-netty)
(global-unset-key (kbd "<f1> p"))
(global-set-key (kbd "<f1> p y") 'anything-htmldoc-py)
(global-set-key (kbd "<f1> p m") 'anything-htmldoc-pymongo)
(global-unset-key (kbd "<f1> m"))
(global-set-key (kbd "<f1> m y") 'anything-htmldoc-mysql)

(provide 'anything-htmldoc)
