;;; anything-ack.el --- Ack command with anything interface
;;;
;;; modified from helm-ack.el
;;; helm-ack.el --- Ack command with helm interface

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-ack
;; Version: 0.02
;; Package-Requires: ((helm "1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'anything)

(defgroup anything-ack nil
  "Ack command with anything interface"
  :group 'anything)

(defcustom anything-c-ack-base-command "ack -H --nocolor --nogroup"
  "Base command of `ack'."
  :type 'string
  :group 'anything-ack)

(defcustom anything-c-ack-auto-set-filetype t
  "Setting file type automatically."
  :type 'boolean
  :group 'anything-ack)

(defcustom anything-c-ack-version nil
  "Ack version."
  :type 'integer
  :group 'anything-ack)

(defcustom anything-c-ack-insert-at-point 'symbol
  "Insert thing at point as search pattern.
You can set value same as `thing-at-point'."
  :type 'symbol
  :group 'anything-ack)

(defvar anything-c-ack-context-stack nil
  "Stack for returning the point before jump.")

(defun anything-c-ack-mode-to-type (mode)
  (case mode
    (actionscript-mode "ack")
    (ada-mode "ada")
    (asm-mode "asm")
    (batch-mode "batch")
    (c-mode "cpp")
    (clojure-mode "clojure")
    (c++-mode "cpp")
    (csharp-mode "csharp")
    (css-mode "css")
    (emacs-lisp-mode "elisp")
    (erlang-mode "erlang")
    ((fortan-mode f90-mode) "fortran")
    (go-mode "go")
    (groovy-mode "groovy")
    (haskell-mode "haskell")
    (html-mode "html")
    (java-mode "java")
    ((javascript-mode js-mode js2-mode) "js")
    (lisp-mode "lisp")
    (lua-mode "lua")
    (makefile-mode "make")
    (objc-mode "objc")
    ((ocaml-mode tuareg-mode) "ocaml")
    ((perl-mode cperl-mode) "perl")
    (php-mode "php")
    (python-mode "python")
    (ruby-mode "ruby")
    (scala-mode "scala")
    (scheme-mode "scheme")
    (shell-script-mode "shell")
    (sql-mode "sql")
    (tcl-mode "tcl")
    ((tex-mode latex-mode yatex-mode) "tex")))

(defsubst anything-c-ack-all-type-option ()
  (if (= anything-c-ack-version 1)
      "--all"
    ""))

(defun anything-c-ack-type-option ()
  (let ((type (anything-c-ack-mode-to-type major-mode)))
    (if type
        (format "--type=%s" type)
      (anything-c-ack-all-type-option))))

(defun anything-c-ack-thing-at-point ()
  (let ((str (thing-at-point anything-c-ack-insert-at-point)))
    (if (and str (typep str 'string))
        (substring-no-properties str)
      "")))

(defun anything-c-ack-default-pattern ()
  (if (null anything-c-ack-insert-at-point)
      ""
    (anything-c-ack-thing-at-point)))

(defun anything-c-ack-init-command ()
  (format "%s %s %s"
          anything-c-ack-base-command
          (or (and anything-c-ack-auto-set-filetype (anything-c-ack-type-option)) "")
          ""))

(defun anything-c-ack-save-current-context ()
  (let ((file (buffer-file-name anything-current-buffer))
        (curpoint (with-current-buffer anything-current-buffer
                    (point))))
    (push `((file  . ,file)
            (point . ,curpoint)) anything-c-ack-context-stack)))

;;;###autoload
(defun anything-ack-pop-stack ()
  (interactive)
  (let ((context (pop anything-c-ack-context-stack)))
    (unless context
      (error "Context stack is empty!!"))
    (let ((file (assoc-default 'file context))
          (curpoint (assoc-default 'point context)))
      (find-file file)
      (goto-char curpoint))))

(defvar anything-c-ack-command-stack nil
  "Command history stack for anything-ack")

(defun anything-c-ack-placeholders ()
  nil
)

(defun anything-c-ack-replace-placeholder (cmd)
  (loop with replaced = (copy-sequence cmd)
        for (holder . value) in (anything-c-ack-placeholders)
        do
        (setq replaced (replace-regexp-in-string holder value replaced))
        finally return replaced))

(defun anything-c-set-ack-version ()
  (with-temp-buffer
    (unless (zerop (call-process-shell-command "ack --version" nil t))
      (error "Failed: ack --version"))
    (goto-char (point-min))
    (if (re-search-forward "^ack \\([0-9]+\\)\.[0-9]+$" nil t)
        (setq anything-c-ack-version (string-to-number (match-string 1)))
      (error "Failed: ack version not found. Please set explicitly"))))

(defun anything-c-ack-init ()
  (unless anything-c-ack-version
    (anything-c-set-ack-version))
  (let ((cmd (read-string (format "[%s] Pattern (ack %s): " default-directory (anything-c-ack-default-pattern))
                          nil
                          'anything-c-ack-command-stack
                          (anything-c-ack-default-pattern))))
    (setq cmd (concat (anything-c-ack-init-command) cmd))
    (anything-attrset 'recenter t)
    (anything-attrset 'before-jump-hook 'anything-c-ack-save-current-context)
    (let ((filled (with-anything-current-buffer
                    (anything-c-ack-replace-placeholder cmd))))
      (with-current-buffer (anything-candidate-buffer 'global)
        (let ((ret (call-process-shell-command filled nil t)))
          (cond ((= ret 1) (error "no match"))
                ((not (= ret 0)) (error "Failed ack"))))))))

(defun anything-c-ack-source (arg)
  `((name . ,(if (< arg 0)
                 (format "Ack Seach(Only %s)"
                         (file-name-nondirectory (buffer-file-name)))
               "Ack Search"))
    (init . anything-c-ack-init)
    (candidates-in-buffer)
    (type . file-line)
    (candidate-number-limit . 9999)))

(defmacro with-anything-current-buffer (&rest body)
  "Eval BODY inside `anything-current-buffer'."
  (declare (indent 0) (debug t))
  `(with-current-buffer anything-current-buffer
     ,@body))

;;;###autoload
(defun anything-ack (arg)
  (interactive "p")
  (let ((buf (get-buffer-create "*anything ack*")))
    (anything-other-buffer (anything-c-ack-source arg) buf)))

(provide 'anything-ack)
;;; anything-ack.el ends here
