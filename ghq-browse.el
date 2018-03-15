;;; ghq-browse.el --- Open url by browser using ghq path style.

;; Copyright (C) 2019 101000code/101000LAB

;; Author: k1LoW (Ken'ichiro OYAMA), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: https://github.com/k1LoW/emacs-ghq-browse
;; Version: 0.2.0
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

(defun ghq-browse/project-root ()
  "Get project root."
  (cl-loop for dir in '(".git/" ".git")
           when (locate-dominating-file default-directory dir)
           return it))

(defun ghq-browse ()
  "Open url by browser using ghq path style."
  (interactive)
  (let* ((root (expand-file-name (ghq-browse/project-root)))
         (repo (f-relative root (f-dirname (f-dirname (f-dirname (expand-file-name (ghq-browse/project-root)))))))
         (branch (replace-regexp-in-string
                  "refs/[^\/]*/" ""
                  (replace-regexp-in-string
                   "[\r\n]+\\'" ""
                   (shell-command-to-string "git symbolic-ref -q HEAD"))))
         (file (f-relative (expand-file-name (buffer-file-name)) root))
         (url "")
         (hash (concat "#L" (number-to-string (line-number-at-pos)))))
    (when (use-region-p)
      (let* ((mark-line (line-number-at-pos (mark)))
             (point-line (line-number-at-pos (- (point) 1)))
             (start (min mark-line point-line))
             (end (max mark-line point-line)))
        (setq hash (concat "#L" (number-to-string start) "-L" (number-to-string end)))))
    (setq url (concat "https://" repo "blob/" branch "/" file hash))
    (browse-url url)))

(provide 'ghq-browse)

;;; ghq-browse.el ends here
