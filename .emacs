;;; -*- coding: utf-8 -*-
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "/mnt/shared/dotfiles/.emacs.d")
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; reset some default settings
(setq x-select-enable-clipboard t
      interprogram-paste-function 'x-cut-buffer-or-selection-value
      make-backup-files nil
      inhibit-startup-message t
      auto-save-default nil
      comint-prompt-read-only t
      default-major-mode 'text-mode
      column-number-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode 0)
(tool-bar-mode -1)
(recentf-mode 1)
(require 'linum)
(global-linum-mode 1)
(require 'hl-line)
(set-face-background 'hl-line "#111")

;;; Shell mode
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(dolist (hook '(ansi-color-for-comint-mode-on truncate-lines shell-init))
  (add-hook 'shell-mode-hook hook))

(defun truncate-lines ()
  "toggle truncate long line into multi-line,used as hook for shell-mode "
  (toggle-truncate-lines 1))

(defun shell-init ()
  "init shell, e.g setup PS1, so that it can be renderred correctly in emacs"
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer))
                      "export PS1=\"[\\h \\W]$\""))
;; clojure-mode
(add-to-list 'load-path "~/local/clojure-mode")
    (require 'clojure-mode)

(require 'paredit)

(defun paredit-mode-enable ()
  (paredit-mode 1))

(dolist (mode-hook '(clojure-mode-hook lisp-mode-hook emacs-lisp-mode-hook inferior-emacs-lisp-mode-hook
                       slime-repl-mode-hook slime-mode-hook  ielm-mode-hook inferior-scheme-mode-hook))
  (add-hook mode-hook 'paredit-mode-enable))
;; slime
(defun load-origin-slime ()
  "load origin slime instead of swank-clojure internal slime
   use it When needed to connect remote swank-clojure session or use lisp"
  (interactive)
  (add-to-list 'load-path "~/local/slime")
  (require 'slime)
  (slime-setup '(slime-js))
  (add-hook 'js2-mode-hook
            (lambda ()
              (slime-js-minor-mode 1)))
  (add-hook 'css-mode-hook
            (lambda ()
              (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
              (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css))))

(setq slime-net-coding-system 'utf-8-unix)
(add-hook 'slime-repl-mode-hook
          (defun clojure-mode-slime-font-lock ()
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))))

(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))
      (setq slime-protocol-version 'ignore)))

;;use ido
(require 'ido)
(ido-mode t)
;;color-theme
(require 'color-theme)
(require 'color-theme-calm-forest)
(eval-after-load "color-theme"
  '(progn
;     (color-theme-solarized-dark)
     (color-theme-calm-forest)))

;;key remap
(define-key key-translation-map [?\[] [?\(])
(define-key key-translation-map [?\]] [?\)])
(define-key key-translation-map [?\(] [?\[])
(define-key key-translation-map [?\)] [?\]])
;;aliases
(defalias 'rof 'recentf-open-files)
(defalias 'rb 'revert-buffer)
(defalias 'ora 'sql-oracle)
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'tm 'text-mode)
(defalias 'om 'org-mode)
;; shortening of often used commands
(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'sl 'sort-lines)
(defalias 'rr 'reverse-region)
(defalias 'rs 'replace-string)
(defalias 'ir 'indent-region)
(defalias 'sc 'slime-connect)
(defalias 'acm 'auto-complete-mode)
(defalias 'cb 'comment-box)
(defalias 'gs 'magit-status)
;personal key map
(global-set-key (kbd "<f5>") 'ido-switch-buffer)
(global-set-key (kbd "<f6>") 'rof)
(global-set-key (kbd "<f7>") 'split-window-vertically)
(global-set-key (kbd "<f8>") 'split-window-horizontally)
(global-set-key (kbd "<f9>") 'shell)
(global-set-key (kbd "<f10>") 'other-window)
(global-set-key (kbd "<f11> <f11>") 'delete-window)
(global-set-key (kbd "<f11> k") (function (lambda ()
                                            (interactive)
                                            (kill-buffer))))
(global-set-key (kbd "<f11> i") 'ido-kill-buffer)
(global-set-key (kbd "<f12> f") 'delete-other-windows)
(global-set-key (kbd "<f12> t") 'toggle-fullscreen)
(global-set-key (kbd "<f12> o") 'find-file)
(global-set-key (kbd "<f12> d") 'ido-dired)
(global-set-key (kbd "<f12> g") 'refresh-file)
(global-set-key (kbd "<f12> l") 'list-buffers)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-a") 'smart-line-beginning)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-h a") 'apropos)
(global-set-key (kbd "C-x C-j") 'dired-open-current-directory)
(global-set-key (kbd "M-0") 'kill-whole-line)
(setq kill-whole-line t)
(global-set-key (kbd "C-<f12>") 'join-line)
(global-set-key (kbd "C-<f6>") 'xsteve-ido-choose-from-recentf)
(global-set-key (kbd "M-j") 'new-line-at-end)
(global-set-key (kbd "M-<f12>") 'just-one-space)
(global-set-key (kbd "<f1> <f1>") 'woman)

;;; newline below current-line
(defun new-line-at-end ()
  (interactive)
  (progn
    (move-end-oF-line 1)
    (newline)))
;;; dired current buffer file directory
(defun dired-open-current-directory ()
  (interactive)
  (if (buffer-file-name)
      (dired default-directory)))

;;; full screen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
             (if (equal 'fullboth current-value)
                 (if (boundp 'old-fullscreen) old-fullscreen nil)
               (progn (setq old-fullscreen current-value)
                  'fullboth)))))
(toggle-fullscreen)

;;refresh buffer
(defun refresh-file ()
  "Refresh buffer from disk"
    (interactive)
    (revert-buffer t (not (buffer-modified-p)) t))

(defun smart-line-beginning ()
  "Move point to the beginning of text
on the current line; if that is already
the current position of point, then move
it to the beginning of the line."
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
;;mozRepl
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

;;default kill & copy current line
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line is copied.")
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

(defadvice kill-region (before slick-copy activate compile)
  "When called interactively with no active region, cut the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))
;;mozRepl
(autoload 'inferior-moz-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

;;; javascript
(add-to-list 'load-path "~/local/auto-complete-1.3.1")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/local/auto-complete-1.3.1/dict")
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'inferior-scheme-mode)
(add-to-list 'ac-modes 'shell-mode)
(setq ac-auto-start 2)
(setq ac-ignore-case nil)

;;; jslint
;(add-to-list 'load-path "/mnt/shared/lintnode")
;(require 'flymake-jslint)
(setq lintnode-location "/mnt/shared/lintnode")
(setq lintnode-jslint-excludes (list 'nomen 'undef 'plusplus 'onevar 'white))
(add-hook 'js-mode-hook
          (lambda ()
            (lintnode-hook)))

;;;zen-coding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

(defun forward-open-bracket ()
  "Move cursor to the next occurrence of left bracket or quotation mark."
  (interactive)
  (forward-char 1)
  (search-forward-regexp "(\\|{\\|\\[\\|<\\|〔\\|【\\|〖\\|〈\\|「\\|『\\|“\\|‘\\|‹\\|«")
  (backward-char 1)
  )

(defun backward-open-bracket ()
  "Move cursor to the previous occurrence of left bracket or quotation mark.."
  (interactive)
  (search-backward-regexp "(\\|{\\|\\[\\|<\\|〔\\|【\\|〖\\|〈\\|「\\|『\\|“\\|‘\\|‹\\|«")
  )

(defun forward-close-bracket ()
  "Move cursor to the next occurrence of right bracket or quotation mark."
  (interactive)
  (search-forward-regexp ")\\|\\]\\|}\\|>\\|〕\\|】\\|〗\\|〉\\|」\\|』\\|”\\|’\\|›\\|»")
  )

(defun backward-close-bracket ()
  "Move cursor to the next occurrence of right bracket or quotation mark."
  (interactive)
  (backward-char 1)
  (search-backward-regexp ")\\|\\]\\|}\\|>\\|〕\\|】\\|〗\\|〉\\|」\\|』\\|”\\|’\\|›\\|»")
  (forward-char 1)
  )

(defun forward-in-bracket ()
  "Move cursor to the next occurence of before-end-bracket"
  (interactive)
  (forward-char 2)
  (search-forward-regexp "</")
  (backward-char 2)
)

(defun backward-in-bracket ()
  "Move cursor to the next occurence of before-end-bracket"
  (interactive)
  (search-backward-regexp "</")
  (forward-char 0)
)

(add-hook 'sgml-mode-hook
 (lambda ()
 (define-key sgml-mode-map (kbd "<C-left>") 'backward-in-bracket)
 (define-key sgml-mode-map (kbd "<C-right>") 'forward-in-bracket)
 )
)

(global-set-key (kbd "<M-left>") 'backward-open-bracket) ; Alt+←
(global-set-key (kbd "<M-right>") 'forward-open-bracket) ; Alt+→
(global-set-key (kbd "<M-up>") 'backward-close-bracket)  ; Alt+↑
(global-set-key (kbd "<M-down>") 'forward-close-bracket) ; Alt+↓

;;; interactive html develop
(require 'moz)
(require 'json)
(setq moz-repl-host "10.0.2.2")

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

;;; paredit for javascript
(defun paredit-space-for-delimiter-p (endp delimiter)
  (and (not (if endp (eobp) (bobp)))
       (memq (char-syntax (if endp (char-after) (char-before)))
             (list ?\"  ;; REMOVED ?w ?_
                   (let ((matching (matching-paren delimiter)))
                     (and matching (char-syntax matching)))))))

;;; markdown-mode
(autoload 'markdown-mode "/mnt/shared/markdown-mode/markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.\\(text\\|md\\)\\'" . markdown-mode) auto-mode-alist))
;;;scheme
(require 'quack)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(quack-programs (quote ("mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(tab-width 4))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(magit-item-highlight ((t (:inherit highlight :background "#111")))))

(add-to-list 'load-path "/mnt/shared/expand-region")
(require 'expand-region)
(global-set-key (kbd "C-<prior>") 'er/expand-region)

;;; from http://www.xsteve.at/prg/emacs/power-user-tips.html
(defun xsteve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))

(setq ditaa-cmd "java -jar /home/ubuntu/local/ditaa/trunk/web/lib/ditaa0_9.jar")
(defun djcb-ditaa-generate ()
  (interactive)
  (shell-command
   (concat ditaa-cmd " " buffer-file-name " -o -E -s 0.7")))

(global-set-key "\M-n" '"\C-u1\C-v")
(global-set-key "\M-p" '"\C-u1\M-v")

;;;pylookup
(setq pylookup-dir "/mnt/shared/pylookup")
(add-to-list 'load-path pylookup-dir)

;; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; set search option if you want
;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)
(global-set-key (kbd "C-c h") 'pylookup-lookup)

;;magit dired key
(require 'magit-svn)

(defun magit-svn-start ()
  "start a magit status buffer with magit-svn-mode on"
  (interactive)
  (call-interactively 'magit-status)
  (call-interactively 'magit-svn-mode))

(add-hook 'dired-mode-hook
          (function (lambda ()
                      (local-set-key (kbd "V") 'magit-status)
                      (local-set-key (kbd "N") 'magit-svn-start)
                      (local-set-key (kbd "J") 'clojure-jack-in))))
;;; ethan-wspace
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)

;;python header
(define-skeleton python-header-skel
  "Insert bang ,code-system header and main func"
  nil
  "#! /bin/env python
# -* - coding: UTF-8 -* -

if __name__ == '__main__':
    print 'rock!'")

(add-hook 'python-mode-hook
          (function (lambda ()
                      (local-set-key (kbd "C-c p") 'python-header-skel))))
(require 'virtualenv)

;; html skeleton
(define-skeleton html-skel
  "Insert an empty html page"
  nil
  "<!DOCTYPE HTML>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <title></title>
</head>
<body>\n"
_
"
</body>
</html>
")
(add-hook 'html-mode-hoo
          (function (lambda ()
                      (local-set-key (kbd "C-c p") 'html-skel))))
