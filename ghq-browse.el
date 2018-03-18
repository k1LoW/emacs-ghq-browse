;;; ghq-browse.el --- Open repo url in browser.

;; Copyright (C) 2019 101000code/101000LAB

;; Author: k1LoW (Ken'ichiro OYAMA), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: https://github.com/k1LoW/emacs-ghq-browse
;; Version: 0.3.0
;; Package-Requires: ((f "0.20.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 'ghq-browse.el' is browse-url() using [ghq path style](https://github.com/motemen/ghq) like src/github.com/motemen/ghq
;; [https://github.com/k1LoW/emacs-ghq-browse]

;;; Code:

(require 'f)

(defvar ghq-browse/remote-alist
  '(("go.googlesource.com" ghq-browse/gocode-url)))

(defun ghq-browse/project-root ()
  "Get project root."
  (cl-loop for dir in '(".git/" ".git")
           when (locate-dominating-file default-directory dir)
           return it))

(defun ghq-browse/repo ()
  "Get ghq repogitory."
  (replace-regexp-in-string
   "\\(?:https://\\|git@\\)\\([^:/]+\\)[:/]\\([^.]+\\).*" "\\1/\\2"
   (replace-regexp-in-string
    "[\r\n]+\\'" ""
    (shell-command-to-string "git remote get-url origin"))))

(defun ghq-browse/branch ()
  "Get ghq repogitory."
  (replace-regexp-in-string
   "refs/[^\/]*/" ""
   (replace-regexp-in-string
    "[\r\n]+\\'" ""
    (shell-command-to-string "git symbolic-ref -q HEAD"))))

(defun ghq-browse/default-url (repo branch file start end)
  "Generate default URL using REPO BRANCH FILE START END."
  (if end
      (setq hash (format "L%s-L%s" start end))
    (setq hash (format "L%s" start)))
  (format "https://%s/+/%s/%s#%s"
          repo branch file hash))

(defun ghq-browse/gocode-url (repo branch file start end)
  "Generate gocode URL using REPO BRANCH FILE START END."
  (format "https://%s/+/%s/%s#%s"
          repo branch file start))

(defun ghq-browse/url ()
  "Get code URL."
  (let* ((project-root (expand-file-name (ghq-browse/project-root)))
         (repo (ghq-browse/repo))
         (branch (ghq-browse/branch))
         (file (f-relative (expand-file-name (buffer-file-name)) project-root))
         (start nil)
         (end nil))
    (setq start (line-number-at-pos))
    (when (use-region-p)
      (let* ((mark-line (line-number-at-pos (mark)))
             (point-line (line-number-at-pos (- (point) 1))))
        (setq start (min mark-line point-line))
        (setq end (max mark-line point-line))))
    (setq urlfunc (cadr (cl-find-if (lambda (lst)
                                      (string-match-p (car lst) repo))
                                    ghq-browse/remote-alist)))
    (if urlfunc
        (funcall urlfunc repo branch file start end)
        (ghq-browse/default-url repo branch file start end))))

(defun ghq-browse ()
  "Open url by browser using ghq path style."
  (interactive)
    (browse-url (ghq-browse/url)))

(provide 'ghq-browse)

;;; ghq-browse.el ends here

