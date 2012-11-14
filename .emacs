;;; -*- coding: utf-8 -*-
(setq user-package-root "~/source/")
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/source/dotfiles/.emacs.d")
(setq package-archives
      '(("ELPA" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("SC" . "http://joseito.republika.pl/sunrise-commander/")))
(package-initialize)
;;; user info
(setq user-full-name "xiaonaitong"
      user-mail-address "xiaonaitong@gmail.com")
;;;mac specific
(setq mac-option-modifier 'super
      mac-command-modifier 'meta)
(when (eq system-type 'darwin)
      ;; default Latin font (e.g. Consolas)
      ;;(set-face-attribute 'default nil :family "Consolas")
      ;; default font size (point * 10)
      ;;
      ;; WARNING!  Depending on the default font,
      ;; if the size is not supported very well, the frame will be clipped
      ;; so that the beginning of the buffer may not be visible correctly.
      (set-face-attribute 'default nil :height 120)
      ;; use specific font for Korean charset.
      ;; if you want to use different font size for specific charset,
      ;; add :size POINT-SIZE in the font-spec.
      ;;(set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
      ;; you may want to add different for other charset in this way.
      (setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))
      (push "/usr/local/bin" exec-path))
;;; reset some default settings
(setq x-select-enable-clipboard t
      make-backup-files nil
      inhibit-startup-message t
      auto-save-default nil
      comint-prompt-read-only t
      default-major-mode 'text-mode
      column-number-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode 0)
(tool-bar-mode -1)
(setq recentf-max-saved-items 80)
(recentf-mode 1)
(require 'linum)
(global-linum-mode 1)
(require 'hl-line)
(set-face-background 'hl-line "#111")

;;auto mode
(setq auto-mode-alist
   (cons '("\\.vm\\'" . html-mode) auto-mode-alist))

;;; mooz/js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(eval-after-load 'js2-mode              ;from emacs-starter-kit-js,
                                        ;use js2-mode instead of js-mode
  '(progn (define-key js2-mode-map "{" 'paredit-open-curly)
          (define-key js2-mode-map "}" 'paredit-close-curly-and-newline)
          (add-hook 'js2-mode-hook 'esk-paredit-nonlisp)
          (setq js2-indent-level 2)
          (font-lock-add-keywords
           'js2-mode `(("\\(function *\\)("
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "\u0192")
                                 nil)))))))
;;; Shell mode
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(setenv "PAGER" "/bin/cat")
(dolist (hook '(ansi-color-for-comint-mode-on truncate-lines))
  (add-hook 'shell-mode-hook hook))

(defun truncate-lines ()
  "toggle truncate long line into multi-line,used as hook for shell-mode "
  (toggle-truncate-lines 1))

;; clojure-mode
(require 'clojure-mode)

(require 'paredit)

(defun paredit-mode-enable ()
  (paredit-mode 1))

(dolist (mode-hook '(clojure-mode-hook lisp-mode-hook emacs-lisp-mode-hook inferior-emacs-lisp-mode-hook
                       slime-repl-mode-hook slime-mode-hook  ielm-mode-hook inferior-scheme-mode-hook))
  (add-hook mode-hook 'paredit-mode-enable))
;; slime
(defun load-slime ()
  "load origin slime instead of swank-clojure internal slime
   use it When needed to connect remote swank-clojure session or use lisp"
  (interactive)
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
;(global-set-key (kbd "<f6>") 'rof)
(global-unset-key (kbd "<f10>"))
(global-set-key (kbd "<f7>") 'split-window-vertically)
(global-set-key (kbd "<f8>") 'split-window-horizontally)
(global-set-key (kbd "<f12>") 'shell)
(global-set-key (kbd "<f11>") 'other-window)
(global-set-key (kbd "<f9> <f9>") 'delete-window)
(global-set-key (kbd "<f9> f") 'delete-other-windows)
(global-set-key (kbd "<f9> i") 'ido-kill-buffer)
(global-set-key (kbd "<f9> k") 'kill-current-buffer)
(global-set-key (kbd "<f9> o") 'occur)
(global-set-key (kbd "<f9> t") 'esk-cleanup-buffer)
(global-set-key (kbd "<f9> e") 'esk-eval-and-replace)
(global-set-key (kbd "<f10> S") 'find-dired)
(global-set-key (kbd "<f10> d") 'ido-dired)
(global-set-key (kbd "<f10> f") 'my-anything)

(global-set-key (kbd "<f10> g") 'my-anything-git-repo-or-file-cache)
(global-set-key (kbd "<f10> k") 'kill-current-buffer)
(global-set-key (kbd "<f10> l") 'list-buffers)
(global-set-key (kbd "<f10> o") 'find-file)
(global-set-key (kbd "<f10> p") 'package-list-packages)
(global-set-key (kbd "<f10> P") 'package-list-packages-no-fetch)
(global-set-key (kbd "<f10> r") 'refresh-file)
(global-set-key (kbd "<f10> R") (lambda ()
                                  (interactive)
                                  (save-excursion (esk-sudo-edit))))
(global-set-key (kbd "<f10> s") 'find-name-dired)
(global-set-key (kbd "<f10> t") 'toggle-fullscreen)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-a") 'smart-line-beginning)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-h a") 'apropos)
(global-set-key (kbd "C-x C-j") 'dired-open-current-directory)
(global-set-key (kbd "M-0") 'kill-whole-line)
(setq kill-whole-line t)
(global-set-key (kbd "C-<f10>") 'join-line)
(global-set-key (kbd "C-<f11>") 'repeat)
(global-set-key (kbd "C-<f6>") 'xsteve-ido-choose-from-recentf)
(global-set-key (kbd "M-j") 'new-line-at-end)
(global-set-key (kbd "M-<f10>") 'just-one-space)
(global-set-key (kbd "<f1> <f1>") 'woman)

;;; programming layout
;;; from http://dacap.com.ar/programming/keyboard-layout/
(global-unset-key (kbd "M-m"))
(global-set-key (kbd "M-m s") 'save-buffer)
(global-set-key (kbd "M-m f") 'find-file)
(global-set-key (kbd "M-m g") 'goto-line)
(global-set-key (kbd "M-m m") 'back-to-indentation)
(global-set-key (kbd "M-m t") 'transpose-chars)
(global-set-key (kbd "M-m M-t") 'transpose-words)
(global-set-key (kbd "M-m M-S t") 'anspose-sexps)
(global-set-key (kbd "M-m c") 'capitalize-word)
(global-set-key (kbd "M-m d") 'downcase-word)
(global-set-key (kbd "M-m u") 'upcase-word)
;;; registers
(global-set-key (kbd "M-m r k ") 'point-to-register) ;(K = Down = Point = Put a register here)
(global-set-key (kbd "M-m r i") 'jump-to-register)   ;(I = Up = Jump)
(global-set-key (kbd "M-m r c") 'copy-to-register)
(global-set-key (kbd "M-m r v") 'insert-register)
;;; bookmarks
(global-set-key (kbd "M-m b k") 'bookmark-set)
(global-set-key (kbd "M-m b i") 'bookmark-jump)
(global-set-key (kbd "M-m b b") 'bookmark-bmenu-list)

;;; kill-current-buffer
(defun kill-current-buffer()
  (interactive)
  (kill-buffer))

;;; newline below current-line
(defun new-line-at-end ()
  (interactive)
  (progn
    (move-end-of-line 1)
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
;;; autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (expand-file-name "auto-complete-1.3.1/dict" user-package-root))
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'inferior-scheme-mode)
(add-to-list 'ac-modes 'shell-mode)
(setq ac-auto-start 2)
(setq ac-ignore-case nil)

;;; paredit for javascript
(defun paredit-space-for-delimiter-p (endp delimiter)
  (and (not (if endp (eobp) (bobp)))
       (memq (char-syntax (if endp (char-after) (char-before)))
             (list ?\"  ;; REMOVED ?w ?_
                   (let ((matching (matching-paren delimiter)))
                     (and matching (char-syntax matching)))))))
;;; restore paredit-non-lisp comment style
(defun my-overrided-paredit-comment-dwim ()
  "override paredit-comment-dwim, when not in lisp family language
  invoke default comment-dwim "
  (interactive)
  (if (not (member major-mode '(emacs-lisp-mode clojure-mode lisp-mode scheme-mode)))
      (call-interactively 'comment-dwim)
    (call-interactively 'paredit-comment-dwim)))

(eval-after-load 'paredit-mode
  (define-key paredit-mode-map
    [remap paredit-comment-dwim]
    'my-overrided-paredit-comment-dwim))
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

;;; markdown-mode
(autoload 'markdown-mode (expand-file-name "markdown-mode/markdown-mode.el" user-package-root)
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
 '(anything-command-map-prefix-key "<f6> a")
 '(clojure-swank-command "lein2 jack-in %s")
 '(quack-programs (quote ("mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(tab-width 4))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(magit-item-highlight ((t (:inherit highlight :background "#111")))))

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

(global-set-key "\M-n" '"\C-u1\C-v")
(global-set-key "\M-p" '"\C-u1\M-v")

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
(global-ethan-wspace-mode 0)

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
(add-hook 'html-mode-hook
          (function (lambda ()
                      (local-set-key (kbd "C-c p") 'html-skel))))
;; java skeleton
(define-skeleton java-header-skel
  "Insert an empty java class with main method"
  nil
  "public class Hello {
  public static void main(String[] args) {
    System.out.println(\"hello\");
  }
}
")
(add-hook 'java-mode-hook
          (function (lambda ()
                      (local-set-key (kbd "C-c p") 'java-header-skel))))
(require 'recentf-ext)

;;; anything-git-goto
;;; remove '-cmo', which scanning all directory for untracked and
;;; modified files
;;; this is much quicker than the default
(defvar git-goto-cmd
  "cd %s && git \
--no-pager ls-files")

;;; based on http://www.millingtons.eclipse.co.uk/glyn/dotemacs.html
(defun swap-windows ()
  "swap first 2 window, keep cursor window"
  (interactive)
  (let* ((w1 (first (window-list)))
         (w2 (second (window-list)))
         (b1 (window-buffer w1))
         (b2 (window-buffer w2))
         (s1 (window-start w1))
         (s2 (window-start w2)))
    (set-window-buffer w1 b2)
    (set-window-buffer w2 b1)
    (set-window-start w1 s2)
    (set-window-start w2 s1)))
(defun reverse-swap-windows ()
  "just like swap-windows but in opposite direction"
  (interactive)
  (swap-windows)
  (other-window 1))

(defun my-cycle (lst)
  (reverse (cons (car lst)
                 (reverse (cdr lst)))))
(defun cycle-windows ()
  "cycle windows"
  (interactive)
  (mapcar* 'set-window-buffer (window-list)
           (my-cycle (mapcar 'window-buffer (window-list)))))

(defun prev-other-window ()
  "prev other window"
  (interactive)
  (other-window -1))

(global-set-key (kbd "<f9> s") 'swap-windows)
(global-set-key (kbd "<f9> S") 'cycle-windows)
(global-set-key (kbd "<f9> r") 'reverse-swap-windows)
(global-set-key (kbd "M-<f11>") 'prev-other-window)
;;; load anything-config
(require 'anything-config)
(require 'anything-git-goto)
(defun my-anything ()
  "my anything sources contains bookmarks locatedb"
  (interactive)
  (anything-other-buffer '(anything-c-source-bookmarks
                           anything-c-source-buffers+
                           anything-c-source-recentf
                           anything-c-source-files-in-current-dir+
                           anything-c-source-locate)
                         "*my-anything-buffer*"))
(defvar cached-files nil
  "record cached filecache directories")
(defun my-anything-git-repo-or-file-cache ()
  "find file in git repo or file cached dir"
  (interactive)
  (if (anything-git-goto-find-git-repo default-directory)
      (call-interactively 'anything-git-goto )
    (progn
      (require 'filecache)
      (when (not (member default-directory cached-files))
        (setq cached-files (cons default-directory cached-files))
        (call-interactively 'file-cache-add-directory-using-find))
      (anything-other-buffer '(anything-c-source-file-cache)
                            "*my-anything-file-cache-buffer*"))))

;;; restore or create *scratch* buffer
(defun restore-scratch-buffer (&optional num)
  "create *scracth* buffer, if not present.
   with no prefix, create *scratch-n* buffer, n is auto incremented number
   with number prefix like 8, will create *scratch-8* buffer"
  (interactive "p")
  (when (= num 1)
      (progn
        (while (get-buffer (concat "*scratch-" (number-to-string num) "*"))
          (setq num (1+ num)))))
  (let ((buffer-name (concat "*scratch-" (number-to-string num) "*")))
    (switch-to-buffer (get-buffer-create buffer-name))))

(global-set-key (kbd "<f9> r s") 'restore-scratch-buffer)

(put 'narrow-to-region 'disabled nil)
;;; set default browser to chrome
(setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "google-chrome")

;;; magit submodule
(eval-after-load 'magit
  '(progn
     (define-key magit-mode-map (kbd "M") 'magit-key-mode-popup-submodule)))

(add-hook 'magit-status-mode-hook
          (lambda ()
            (make-local-variable 'magit-refresh-pending)
            (local-set-key (kbd ",") 'my-toggle-magit-refresh)))
;;; temporary solution for too much refresh time
(defun my-toggle-magit-refresh ()
  "tempoary disable magit refresh,
   for large repo, refresh take too much time
   when staging untracked files, we don't want it to refresh"
  (interactive)
  (setq magit-refresh-pending (not magit-refresh-pending)))

(require 'javap-handler)
(setq byte-compile-warnings '(not nresolved free-vars callargs redefine obsolete noruntime cl-functions interactive-only))

;;;whitespace mode style
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
