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

;;;; Requirements
(require 'transient)
(require 'ediff)
(require 'cl-lib)

;;;; Customization
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

(defcustom org-webshot-defuddle-to-markdown t
  "Defuddle output should be a Markdown file if non-nil, else an HTML file."
  :type 'boolean
  :group 'org-webshot)

(defcustom org-webshot-media-path 'org-webshot--default-media-path
  "Where to store the media files contained within the HTML document.

Can be a string specifying the path to the directory, or a function that
 takes in the path to the HTML file. If path is relative, then it is
 relative to the directory where the org file will be stored."
  :type '(choice
          (string :tag "Media directory path")
          (function :tag "Function returning path from HTML filename"))
  :group 'org-webshot)

(defcustom org-webshot-default-output-directory nil
  "The default directory where the converted Org files will be stored."
  :type '(choice
          (directory :tag "Output directory")
          (const nil))
  :group 'org-webshot)

(defcustom org-webshot-converters '()
  "Alist mapping converter type symbols to metadata plists.

See `org-webshot-converters-add' and `org-webshot-converters-remove'."
  :type '(repeat (cons symbol plist))
  :group 'org-webshot)

;;;; Variables

(defvar org-webshot--tmp-html-file-prefix "org-webshot_"
  "Prefix of the temporary file in which the HTML file is first stored.")

(defvar org-webshot--tmp-intermediate-file-prefix "org-webshot-int_"
  "Prefix of the intermediate file between the original HTML file and the final Org file.")

;;;; Commands

;;;###autoload
(defun org-webshot ()
  "Open the HTML to Org converter transient interface."
  (interactive)
  (org-webshot--transient))

(defun org-webshot-download-website (url)
  "Download website as a single HTML file and store it in `org-webshot-html-path', which is returned."
  (interactive "sURL: ")
  (let* ((title (org-webshot--get-page-title url))
         (path (org-webshot--html-path url title))
         (status-code (org-webshot--call-monolith url path)))
    (if (= 0 status-code)
        path
      (user-error "Error calling monolith." status-code))))

;;;; Functions

;;;;; Utilities

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

;;;;; Download HTML document

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

(defun org-webshot--call-monolith (url path)
  "Call monolith and return process call output."
  (call-process-shell-command
   (concat "monolith " url " > " path)))

;;;;; Converter

(defun org-webshot-converters-add (sym name run-fn config-struct)
  "Add or update a converter in `org-webshot-converters'.

SYM is the converter symbol.
NAME is a string for display.
RUN-FN is the converter function.
  Should have signature (config html-path out-dir title &optional media-dir)
CONFIG-STRUCT is the symbol of the config struct type."
  (let ((entry (assoc sym org-webshot-converters))
        (new-plist (list :name name :run run-fn :config config-struct)))
    (if entry
        (setcdr entry new-plist)
      (push (cons sym new-plist) org-webshot-converters))))

(defun org-webshot-converters-remove (sym)
  "Remove the converter with symbol SYM from `org-webshot-converters'."
  (setq org-webshot-converters
        (assq-delete-all sym org-webshot-converters)))

(defun org-webshot--default-media-path (title directory)
  "Returns path to within same directory as the file."
  (concat (file-name-as-directory directory) (file-name-as-directory title)))

(defun org-webshot-media-path (title directory)
  "Returns the path to the directory wherein the media files of the HTML document will be stored."
  (cond
   ((stringp org-webshot-media-path)
    org-webshot-media-path)
   ((functionp org-webshot-media-path)
    (funcall org-webshot-media-path title directory))))

;;;;;; Pandoc

(defun org-webshot--pandoc-convert-to-org (in-path in-format out-path out-media &optional pandoc-args)
  "Convert a file to an Org file, and extracts all media within to a directory.

OUT-MEDIA must be either relative to out-path, or an absolute path."
  (let* ((out-directory (file-name-directory out-path))
         ;; Pandoc expects media path to be relative to file path
         (out-media (if (f-relative-p out-media)
                        out-media
                      (file-relative-name out-media out-directory)))
         (out-filename (file-name-nondirectory out-path))
         (pandoc-args-str (or pandoc-args "")))
    (call-process-shell-command
     (format
      "cd %s && pandoc --from %s --to org %s --wrap=preserve --extract-media=\"%s\" %s -o %s"
      out-directory in-format pandoc-args-str out-media in-path out-filename))))

(defun org-webshot-pandoc-convert (config html-path out-dir title &optional media-dir)
  "Use Pandoc to convert a HTML file into an Org file."
  (let* ((pandoc-args (org-webshot-pandoc-config-pandoc-args config))
         (org-path (concat (file-name-as-directory out-dir) title ".org"))
         (media-path (or media-dir (org-webshot-media-path title out-dir)))
         (status (org-webshot--pandoc-convert-to-org html-path "html" org-path media-path pandoc-args)))
    (unless (= 0 status)
      (user-error "Convertion failed"))))

(cl-defstruct org-webshot-pandoc-config
  "Configuration for Pandoc converter."
  (pandoc-args
   ""
   :type string
   :documentation "Additional args passed to pandoc"))

(org-webshot-converters-add
 'pandoc
 "Pandoc"
 #'org-webshot-pandoc-convert
 'org-webshot-pandoc-config)

;;;;;; Defuddle

(defun org-webshot--make-intermediate-tmp-path ()
  "Generates a path to a unique filename in which to store the intermediate document."
  (concat
   (file-name-as-directory temporary-file-directory)
   (make-temp-name org-webshot--tmp-intermediate-file-prefix)
   (if org-webshot-defuddle-to-markdown ".md" ".html")))

(defun org-webshot--defuddle-call (html-path out-format out-path)
  "Call defuddle and return process call output."
  (call-process-shell-command
   (format "node scripts/defuddle.mjs %s %s > %s" html-path out-format out-path)))

(defun org-webshot--defuddle-html-file (path markdown)
  "Run the defuddle script to convert an HTML file into a simplified version, return path to it."
  (let* ((out-format (if markdown "md" "html"))
         (out-path (org-webshot--make-intermediate-tmp-path))
         (status-code (org-webshot--defuddle-call path out-format out-path)))
    (if (= 0 status-code)
        out-path
      (user-error "Error calling defuddle: %s" status-code))))

(defun org-webshot-defuddle-convert (config html-path out-dir title &optional media-dir)
  "Use Defuddle to convert a HTML file into an Org file."
  (let* ((pandoc-args (org-webshot-defuddle-config-pandoc-args config))
         (to-markdown (org-webshot-defuddle-config-to-markdown config))
         (org-path (concat (file-name-as-directory out-dir) title ".org"))
         (media-path (or media-dir (org-webshot-media-path title out-dir)))
         ;; Will error if failed, so we can assume returns path.
         (defuddled-path (org-webshot--defuddle-html-file html-path to-markdown))
         (status-code (org-webshot--pandoc-convert-to-org
                       defuddled-path
                       (if to-markdown "markdown" "html")
                       org-path media-path pandoc-args)))
    (unless (= 0 status-code)
      (user-error "Pandoc convertion failed: %s" status-code))))

(cl-defstruct (org-webshot-defuddle-config
               (:include org-webshot-pandoc-config (pandoc-args "--markdown-headings=atx")))
  "Configuration for Defuddle converter."
  (to-markdown
   org-webshot-defuddle-to-markdown
   :type boolean
   :documentation "Convert intermediate file to Markdown rather than HTML."))

(org-webshot-converters-add
 'defuddle
 "Defuddle"
 #'org-webshot-defuddle-convert
 'org-webshot-defuddle-config)


;;;; Transient
(transient-define-prefix org-webshot--transient ()
  "HTML to Org Converter Interface")


;;;; Footer

(provide 'org-webshot)

;;; org-webshot.el ends here
