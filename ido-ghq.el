;;; ido-ghq.el --- ghq with ido -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2018 Youhei SASAKI <uwabami@gfd-dennou.org>

;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;; $Lastupdate: 2018-03-07 02:04:09$
;; Version: 0.0.2
;; Package-Requires: nil
;; Keywords: tools
;; URL: https://github.com/uwabami/ido-ghq

;; This file is not part of GNU Emacs.
;;
;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentry:

;; original version is https://github.com/masutaka/emacs-helm-ghq

;;; Code:

(defgroup ido-ghq nil
  "ghq with ido interface"
  :prefix "ido-ghq-"
  :group 'ido)

(defcustom ido-ghq-command
  "ghq"
  "*A ghq command"
  :type 'string
  :group 'ido-ghq)

(defcustom ido-ghq-command-arg-root
  '("root")
  "*Arguments for getting ghq root path using ghq command"
  :type '(repeqt string)
  :group 'ido-ghq)

(defcustom ido-ghq-short-list nil
  "*Whether display full path or short path"
  :type 'boolean
  :group 'ido-ghq)

(defun ido-ghq--command-arg-list ()
  (if ido-ghq-short-list
      '("list")
    '("list" "--full-path")))

(defun ido-ghq--open-dired (file)
  (dired
   (if ido-ghq-short-list
       (format "%s%s" (ido-ghq--get-root) file)
     (format "%s" file))))

(defun ido-ghq--get-root ()
  (with-temp-buffer
    (unless (zerop (apply #'call-process
                          ido-ghq-command nil t nil
                          ido-ghq-command-arg-root))
      (error "Failed: Can't get ghq's root"))
    (replace-regexp-in-string "\n+$" "/"
                              (buffer-substring-no-properties
                               (goto-char (point-min))(goto-char (point-max))))))

(defun ido-ghq--list-candidates ()
  (with-temp-buffer
    (unless (zerop (apply #'call-process
                          ido-ghq-command nil t nil
                          (ido-ghq--command-arg-list)))
      (error "Failed: Can't get ghq list candidates"))
    (let ((paths))
      (goto-char (point-min))
      (while (not (eobp))
        (push
         (buffer-substring-no-properties
          (line-beginning-position) (line-end-position))
         paths)
        (forward-line 1))
      (reverse paths))))

;;; autoload
(defun ido-ghq-open ()
  "Use `ido-completing-read' to \\[dired] a ghq list"
  (interactive)
  (let ((path (ido-completing-read "Find ghq repo.: "
                                   (ido-ghq--list-candidates))))
    (if (ido-ghq--open-dired path)
        (message (format "Open ghq repository: %s" path)))))

(provide 'ido-ghq)
;;; ido-ghq.el ends here
