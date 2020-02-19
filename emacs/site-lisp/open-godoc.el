;;; open-godoc.el ---

;; Copyright (C) 2018 by Ryo Fujimoto

;; Author: Ryo Fujimoto <fujimisakari@gmail.com>
;; URL: https://github.com/fujimisakari/emacs-open-godoc
;; Version: 1.0.0

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

;;; Commentary:
;;
;; open-godoc is to open godoc.org site from emacs.
;;

;; To use this package, add these lines to your init.el or .emacs file:
;;  (require 'open-godoc)
;;
;; ----------------------------------------------------------------
;;
;; to search document
;; M-x open-godoc
;;

;;; Code:

(defun open-godoc--generate-url (package func-name)
  "Generate url from package and function name."
  (let ((url (format "https://godoc.org/%s" package)))
    (if (null func-name)
        url
      (concat url (format "#%s" func-name)))))

(defun open-godoc--func-name (query)
  "Get function name from search query."
  (unless (eq (length (split-string query "\\.")) 1)
    (car (last (split-string query "\\.")))))

(defun open-godoc--third-party-package-p (cli-doc)
  "Check if it is a third party package."
  (if (string-match "\"\\(.+\\)\"" cli-doc) t))

(defun open-godoc--document-exist-p (cli-doc)
  "Deduce from the document result of cli that the document exists on godoc.org."
  (unless (string-match "exit status 1" cli-doc) t))

(defun open-godoc--document-from-cli (query)
  "Get document test from go doc commnad."
  (funcall 'shell-command-to-string (concat "go doc " query)))

(defun open-godoc--region-or-read-string ()
  "Get search query with region or cursor point."
  (let ((query))
    (cond
     (mark-active
      (setq query (buffer-substring-no-properties (region-beginning) (region-end))))
     ((thing-at-point 'symbol)
      (setq query (thing-at-point 'symbol))))
    (read-string "godoc.org search: " query)))

;;;###autoload
(defun open-godoc ()
  "Search from godoc.org site with search query."
  (interactive)
  (let* ((query (open-godoc--region-or-read-string))
         (cli-doc (open-godoc--document-from-cli query))
         package
         func-name)
    (unless (open-godoc--document-exist-p cli-doc)
      (error "search package dose not exsist"))
    (cond
     ((open-godoc--third-party-package-p cli-doc)
      (setq package (match-string 1 cli-doc))
      (setq func-name (open-godoc--func-name query)))
     (t
      ;; golang standard package
      (setq package (car (split-string query "\\.")))
      (setq func-name (open-godoc--func-name query))))
    (browse-url (open-godoc--generate-url package func-name))))

(provide 'open-godoc)

;;; open-godoc.el ends here
