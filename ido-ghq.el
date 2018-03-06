;;; ido-ghq.el --- ghq with ido -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2018 Youhei SASAKI <uwabami@gfd-dennou.org>

;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;; $Lastupdate: 2018-03-06 22:53:37$
;; Version: 0.0.1
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

(defgroup ido-ghq
  "ghq with ido interface"
  :prefix "ido-ghq-"
  :group 'ido)

(defcustom ido-ghq-command
  "ghq"
  "ghq command"
  :type 'string
  :group 'ido-ghq)

(defcustom ido-ghq-command-ghq-arg-root
  '("root")
  "*Arguments for getting ghq root path using ghq command"
  :type '(repeqt string)
  :group 'ido-ghq)

(defcustom ido-ghq-command-ghq-arg-list
  '("list" "--full-path")
  "*Arguments for getting ghq list"
  :type '(repeqt string)
  :group 'ido-ghq)

;;
(defun ido-ghq--open-dired (file)
  (dired (file-name-directory file)))

(defun ido-ghq--list-candidates ()
  (with-temp-buffer
    (unless (zerop (apply #'call-process
                          ido-ghq-command nil t nil
                          ido-ghq-command-ghq-arg-list))
      (error "Failed: Can't get ghq list candidates"))
    (let ((paths))
      (goto-char (point-min))
      (while (not (eobp))
        (push
         (buffer-substring-no-properties
          (line-beginning-position) (line-end-position)) paths)
        (forward-line 1))
      (reverse paths))))

;;; autoload
(defun ido-ghq-open ()
  "Use `ido-completing-read' to \\[dired] a ghq list"
  (interactive)
  (ido-ghq--open-dired
   (ido-completing-read "Find ghq repo.: "
                        (my:ghq--list-candidates))))


;;; ido-ghq.el ends here
