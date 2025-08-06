;;; org-webshot.el --- HTML to Org conversion utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: M Char
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))
;; Keywords: html, org

;;; Commentary:

;; This package provides a transient interface for running multiple HTML to
;; org-mode conversion utilities, comparing their results, and saving the
;; output flexibly.

;;; Code:

(require 'transient)
(require 'ediff)

(defgroup org-webshot nil
  "HTML to Org conversion interface."
  :group 'tools)

(defcustom org-webshot-html-tmp-path nil
  "The path in which to store temporarily the (intermediate) HTML file.

Note that `org-webshot-html-path' determines where the file is stored in
 the end. However, the HTML file is always first download in this
 temporary file for technical reasons. If `org-webshot-html-path' is not
 set to `nil', the file is then moved to the path specified in its
 value.

The value can be the file path, or a function that takes in the url and
returns the file path. Leave `nil' to let the package figure out a
temporary file path on its own."
  :type '(choice
          (const :tag "Default handler" nil)
          (string :tag "Custom path")
          (function :tag "Function returning path from URL"))
  :group 'org-webshot)

(defcustom org-webshot-html-path nil
  "Where to store the (intermediate) HTML file.

By default, the HTML file will be stored as a temporary file. However,
if you'd like to archive the HTML document as well, you may pass in the
path in which to store the file, or a function that takes in the url
and (potentially nil) title of the document, and returns the path."
  :type '(choice
          (const :tag "Temporary file" nil)
          (string :tag "File path")
          (function :tag "Function returning path from URL and title"))
  :group 'org-webshot)

;; Internal variables
(defvar org-webshot--tmp-html-file-prefix "org-webshot_"
  "Prefix of the temporary file in which the HTML file is first stored.")

;;;###autoload
(defun org-webshot ()
  "Open the HTML to Org converter transient interface."
  (interactive)
  (org-webshot--transient))

(transient-define-prefix org-webshot--transient ()
  "HTML to Org Converter Interface")

(defun org-webshot--make-html-tmp-path ()
  "Generates a path to a unique filename in which to store the temporary HTML document."
  (concat
   (file-name-as-directory temporary-file-directory)
   (make-temp-name org-webshot--tmp-html-file-prefix)
   ".html"))

(defun org-webshot--html-tmp-path (url)
  "Returns path of the temporary file in which to store the HTML document.

The default handler uses the built-in `make-temp-file' to generate a temporary file path."
  (cond
   ((stringp org-webshot-html-tmp-path)
    org-webshot-html-tmp-path)
   ((functionp org-webshot-html-tmp-path)
    (funcall org-webshot-html-tmp-path url))
   (t
    (org-webshot--make-html-tmp-path))))

(defun org-webshot--html-path (url title)
  "Determine path in which to store the HTML document, or `nil' to keep it in the temporary file."
  (cond
   ((stringp org-webshot-html-path)
    org-webshot-html-path)
   ((functionp org-webshot-html-path)
    (funcall org-webshot-html-path url title))
   (t
    (org-webshot--html-tmp-path url))))

(defun org-webshot--slugify-title (title)
  "From TITLE, make a filename slug meant to look nice as URL component.

This is a literal copy-paste from the function
`org-node-slugify-for-web' of the `org-node' package. Hope that's ok..."
  (thread-last title
               (org-link-display-format)
               (string-glyph-decompose)
               (seq-remove (lambda (char) (<= #x300 char #x331)))
               (concat)
               (string-glyph-compose)
               (downcase)
               (string-trim)
               (replace-regexp-in-string "[[:space:]]+" "-")
               (replace-regexp-in-string "[^[:alnum:]\\/-]" "")
               (replace-regexp-in-string "\\/" "-")
               (replace-regexp-in-string "--*" "-")
               (replace-regexp-in-string "^-" "")
               (replace-regexp-in-string "-$" "")))
  
(defun org-webshot--get-page-title (url &optional slugify)
  "Get title of web page by URL, or `nil' if not found.

Uses various utilities from `url.el'."
  (let (web-title-str coding-charset)
    (with-current-buffer (url-retrieve-synchronously url)
      ;; find title by grep the html code
      (goto-char (point-min))
      (when (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
        (setq web-title-str (match-string 1)))
      ;; find charset by grep the html code
      (goto-char (point-min))
      ;; downcase the charaset. e.g, UTF-8 is not acceptible for emacs, while utf-8 is ok.
      (setq coding-charset
            (or (when (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
                  (downcase (match-string 1)))
                "utf-8"))
      ;; decode the string of title.
      (when web-title-str
        (setq web-title-str (decode-coding-string web-title-str (intern coding-charset))))
      (kill-buffer))
    (if slugify
        (org-webshot--slufiy-title web-title-str)
      web-title-str)))

(defun org-webshot-download-website (url)
  "Download website as a single HTML file and store it in `org-webshot-html-path', which is returned."
  (interactive "sURL: ")
  (let* ((title (org-webshot--get-page-title url))
         (path (org-webshot--html-path url title))
         (status-code
          (call-process-shell-command
           (concat "monolith " url " > " path))))
    (message "%s %s %s" title path status-code)
    (if (= 0 status-code)
        path
      (user-error "Error calling monolith." status-code))))

(provide 'org-webshot)

;;; org-webshot.el ends here
