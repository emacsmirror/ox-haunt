;;; ox-haunt.el --- Haunt-flavored HTML backend for the Org export engine -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jakob L. Kreuze

;; Author: Jakob L. Kreuze <zerodaysfordays@sdf.lonestar.org>
;; Version: 0.1
;; Package-Requires (org)
;; Keywords: convenience hypermedia wp
;; URL: https://git.sr.ht/~jakob/ox-haunt

;; This file is part of GNU Emacs.

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

;; This library implements an HTML back-end for the Org generic
;; exporter, producing output appropriate for Haunt's `html-reader'.

;;; Code:

(require 'ox)
(require 'ox-html)

(org-export-define-derived-backend 'haunt 'html
  :menu-entry
  '(?s "Export to Haunt"
       ((?H "As Haunt buffer" ox-haunt-export-as-html)
        (?h "As Haunt file" ox-haunt-export-to-html)
        (?o "As Haunt file and open"
            (lambda (a s v b)
              (if a (ox-haunt-export-to-html t s v b)
                (org-open-file (ox-haunt-export-to-html nil s v b)))))))
  :translate-alist
  '((template . ox-haunt-template)))

(defgroup org-export-haunt nil
  "Options for exporting Org mode files to Haunt HTML."
  :tag "Org Export Haunt"
  :group 'org-export)

(defcustom ox-haunt-tidy-html nil
  "Whether or not to format the HTML output with the external
HTML Tidy tool. Requires that `ox-haunt-tidy-executable' names a
valid executable."
  :type 'boolean)

(defcustom ox-haunt-tidy-executable "tidy"
  "The name of the HTML Tidy executable. Will not be run unless
`ox-haunt-tidy-html' is non-nil."
  :type 'string)

(defcustom ox-haunt-recognized-metadata '(:title :date)
  "A list of keywords to include in the Haunt metadata section."
  :type '(list symbol))

(defun ox-haunt--keyword-as-string (info keyword)
  "Obtain the value of KEYWORD as a plaintext string."
  (org-export-data-with-backend (plist-get info keyword) 'ascii info))

(defun ox-haunt--tidy-html (html)
  "Return the result of running HTML tidy on the given markup."
  (let ((command (format "%s -q -w 0 --show-body-only yes"
                         ox-haunt-tidy-executable)))
    (with-temp-buffer
      (insert html)
      (cl-flet ((silence (&rest args1) (ignore)))
        (advice-add 'message :around #'silence)
        (unwind-protect
            (shell-command-on-region 1 (buffer-end +1)
                                     command
                                     (current-buffer))
          (advice-remove 'message #'silence)))
      (buffer-string))))

(defun ox-haunt-template (contents info)
  "Return complete document string after HTML conversion."
  (concat
   ;; Output the Haunt metadata section.
   (with-temp-buffer
     (dolist (keyword ox-haunt-recognized-metadata)
       (when (plist-get info keyword)
         (insert (format "%s: %s\n"
                         (subseq (symbol-name keyword) 1)
                         (ox-haunt--keyword-as-string info keyword)))))
     (buffer-string))
   "---\n"
   ;; Output the article contents.
   (let ((html (concat "<article>" contents "</article>")))
     (if ox-haunt-tidy-html
         (ox-haunt--tidy-html html)
       html))))

;;;###autoload
(defun ox-haunt-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Haunt post buffer."
  (interactive)
  (org-export-to-buffer 'haunt "*Org Haunt Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun ox-haunt-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Haunt post file."
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :html-extension)
                                    org-html-extension
                                    "html")))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'haunt file
      async subtreep visible-only body-only ext-plist)))

(provide 'ox-haunt)
;;; ox-haunt.el ends here
