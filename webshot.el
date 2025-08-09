;;; webshot.el --- HTML to Org conversion utilities -*- lexical-binding: t; -*-

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
(require 'cl-lib)
(require 'webshot-ui)

;;;; Customization
(defgroup webshot nil
  "HTML to Org conversion interface."
  :group 'tools)

(defcustom webshot-html-tmp-path nil
  "The path in which to store temporarily the (intermediate) HTML file.

Note that `webshot-html-path' determines where the file is stored in
 the end. However, the HTML file is always first download in this
 temporary file for technical reasons. If `webshot-html-path' is not
 set to `nil', the file is then moved to the path specified in its
 value.

The value can be the file path, or a function that takes in the url and
returns the file path. Leave `nil' to let the package figure out a
temporary file path on its own."
  :type '(choice
          (const :tag "Default handler" nil)
          (string :tag "Custom path")
          (function :tag "Function returning path from URL"))
  :group 'webshot)

(defcustom webshot-html-path nil
  "Where to store the (intermediate) HTML file.

By default, the HTML file will be stored as a temporary file. However,
if you'd like to archive the HTML document as well, you may pass in the
path in which to store the file, or a function that takes in the url
and (potentially nil) title of the document, and returns the path."
  :type '(choice
          (const :tag "Temporary file" nil)
          (string :tag "File path")
          (function :tag "Function returning path from URL and title"))
  :group 'webshot)

(defcustom webshot-defuddle-to-markdown t
  "Defuddle output should be a Markdown file if non-nil, else an HTML file."
  :type 'boolean
  :group 'webshot)

(defcustom webshot-media-path 'webshot--media-path-local-shared-title
  "Where to store the media files contained within the HTML document.

Can be a string specifying the path to the directory, or a function that
 takes in the path to the HTML file. If path is relative, then it is
 relative to the directory where the org file will be stored."
  :type '(choice
          (string :tag "Media directory path")
          (function :tag "Function returning path from HTML filename"))
  :group 'webshot)

(defcustom webshot-default-output-directory nil
  "The default directory where the converted Org files will be stored."
  :type '(choice
          (directory :tag "Output directory")
          (const nil))
  :group 'webshot)

(defcustom webshot-converters '()
  "Alist mapping converter type symbols to metadata plists.

See `webshot-converters-add' and `webshot-converters-remove'."
  :type '(repeat (cons symbol plist))
  :group 'webshot)

;;;; Variables

(defvar webshot--tmp-html-file-prefix "webshot_"
  "Prefix of the temporary file in which the HTML file is first stored.")

(defvar webshot--tmp-intermediate-file-prefix "webshot-int_"
  "Prefix of the intermediate file between the original HTML file and the final Org file.")

;;;; Commands

;;;###autoload
(defun webshot ()
  "Open the HTML to Org converter transient interface."
  (interactive)
  ;; Probably better not to reset everytime.
  ;; (webshot-ui-reset)
  (webshot--transient))

(defun webshot-download-website (url)
  "Download website as a single HTML file and store it in `webshot-html-path', which is returned."
  (interactive "sURL: ")
  (let* ((title (webshot--get-page-title url))
         (path (webshot--html-path url title))
         (status-code (webshot--call-monolith url path)))
    (if (= 0 status-code)
        path
      (user-error "Error calling monolith." status-code))))

;;;; Functions

;;;;; Utilities

(defun webshot--slugify-title (title)
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

(defun webshot--make-html-tmp-path ()
  "Generates a path to a unique filename in which to store the temporary HTML document."
  (concat
   (file-name-as-directory temporary-file-directory)
   (make-temp-name webshot--tmp-html-file-prefix)
   ".html"))

(defun webshot--html-tmp-path ()
  "Returns path of the temporary file in which to store the HTML document.

The default handler uses the built-in `make-temp-file' to generate a temporary file path."
  (cond
   ((stringp webshot-html-tmp-path)
    webshot-html-tmp-path)
   ((functionp webshot-html-tmp-path)
    (funcall webshot-html-tmp-path))
   (t
    (webshot--make-html-tmp-path))))

(defun webshot--html-path (url title)
  "Determine path in which to store the HTML document, or `nil' to keep it in the temporary file."
  (cond
   ((stringp webshot-html-path)
    webshot-html-path)
   ((functionp webshot-html-path)
    (funcall webshot-html-path url title))
   (t
    (webshot--html-tmp-path))))
  
(defun webshot--get-page-title (url &optional slugify)
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
        (webshot--slufiy-title web-title-str)
      web-title-str)))

(defun webshot--call-monolith (url path)
  "Call monolith and return process call output."
  (call-process-shell-command
   (concat "monolith " url " > " path)))

;;;;; Converter

(cl-defmacro webshot-define-converter
    (short-sym display-name
               slots
               (config html-path out-dir title &optional media-dir)
               &rest body)
  (let* ((the-sym (cadr short-sym))
         (fn-name (intern (format "webshot-convert--%s" the-sym)))
         (struct-name (intern (format "webshot-convert--%s-config" the-sym)))
         (struct-doc (format "Configuration for %s." display-name)))
    `(progn
       ;; 2. Generate the cl-defstruct form
       (cl-defstruct ,struct-name
         ,struct-doc
         ,@slots) ; Splice the user-provided slot definitions here

       ;; Generate the converter function (as before)
       (defun ,fn-name (,config ,html-path ,out-dir ,title &optional ,media-dir)
         (unless (file-exists-p ,html-path)
           (user-error "HTML file does not exist: %s" ,html-path))
         (unless (stringp ,title)
           (user-error "Title must be a string: %S" ,title))
         (make-directory ,out-dir t)
         (let ((media-dir (or ,media-dir (webshot-media-path ,title ,out-dir))))
           ,@body))

       ;; 3. Register the converter using the generated struct name
       (webshot--converters-add
        ,short-sym
        ,display-name
        #',fn-name
        ',struct-name))))

(defun webshot--converters-add (sym name run-fn config-struct)
  "Add or update a converter in `webshot-converters'.

SYM is the converter symbol.
NAME is a string for display.
RUN-FN is the converter function.
  Should have signature (config html-path out-dir title &optional media-dir)
CONFIG-STRUCT is the symbol of the config struct type."
  (let ((entry (assoc sym webshot-converters))
        (new-plist (list :name name :run run-fn :config config-struct)))
    (if entry
        (setcdr entry new-plist)
      (push (cons sym new-plist) webshot-converters))))

(defun webshot-converters-remove (sym)
  "Remove the converter with symbol SYM from `webshot-converters'."
  (setq webshot-converters
        (assq-delete-all sym webshot-converters)))

(defun webshot--media-path-local-shared-title (title directory)
  "Returns path to within same directory as the file."
  (concat (file-name-as-directory directory) (file-name-as-directory title)))

(defun webshot-media-path (title directory)
  "Returns the path to the directory wherein the media files of the HTML document will be stored."
  (cond
   ((stringp webshot-media-path)
    webshot-media-path)
   ((functionp webshot-media-path)
    (funcall webshot-media-path title directory))))

;;;;;; Pandoc

(defun webshot--pandoc-convert-to-org (in-path in-format out-path out-media &optional pandoc-args)
  "Convert a file to an Org file, and extracts all media within to a directory.

OUT-MEDIA must be either relative to out-path, or an absolute path."
  (let* ((out-directory (file-name-directory out-path))
         ;; Pandoc expects media path to be relative to file path
         (out-media (if (f-relative-p out-media)
                        out-media
                      (file-relative-name out-media out-directory)))
         (out-filename (file-name-nondirectory out-path))
         (pandoc-args-str (or pandoc-args ""))
         (cmd (format
               "cd %s && pandoc --from %s --to org %s --wrap=preserve --extract-media=\"%s\" %s -o %s"
               out-directory in-format pandoc-args-str out-media in-path out-filename)))
    (message "Pandoc: %s" cmd)
    (call-process-shell-command cmd)))

(webshot-define-converter
 'pandoc
 "Pandoc" 
 ((pandoc-args "" :type string :documentation "Additional args for pandoc"))
 (config html-path out-dir title media-dir)
 (let* ((pandoc-args (webshot-convert--pandoc-config-pandoc-args config))
        (org-path (concat (file-name-as-directory out-dir) title ".org"))
        (status (webshot--pandoc-convert-to-org html-path "html" org-path media-dir pandoc-args)))
   (unless (= 0 status)
     (user-error "Convertion failed: %s" status))))
 

;;;;;; Defuddle

(defun webshot--make-intermediate-tmp-path ()
  "Generates a path to a unique filename in which to store the intermediate document."
  (concat
   (file-name-as-directory temporary-file-directory)
   (make-temp-name webshot--tmp-intermediate-file-prefix)
   (if webshot-defuddle-to-markdown ".md" ".html")))

(defun webshot--defuddle-call (html-path out-format out-path)
  "Call defuddle and return process call output."
  (call-process-shell-command
   (format "node scripts/defuddle.mjs %s %s > %s" html-path out-format out-path)))

(defun webshot--defuddle-html-file (path markdown)
  "Run the defuddle script to convert an HTML file into a simplified version, return path to it."
  (let* ((out-format (if markdown "md" "html"))
         (out-path (webshot--make-intermediate-tmp-path))
         (status-code (webshot--defuddle-call path out-format out-path)))
    (if (= 0 status-code)
        out-path
      (user-error "Error calling defuddle: %s" status-code))))

(webshot-define-converter
 'defuddle
 "Defuddle" 
 ((pandoc-args
   "--markdown-headings=atx"
   :type string
   :documentation "Additional args for pandoc")
  (to-markdown
   t
   :type boolean
   :documentation "Convert intermediate file to Markdown rather than HTML."))
 (config html-path out-dir title media-dir)
 (let* ((pandoc-args (webshot-convert--defuddle-config-pandoc-args config))
         (to-markdown (webshot-convert--defuddle-config-to-markdown config))
         (org-path (concat (file-name-as-directory out-dir) title ".org"))
         ;; Will error if failed, so we can assume returns path.
         (defuddled-path (webshot--defuddle-html-file html-path to-markdown))
         (status-code (webshot--pandoc-convert-to-org
                       defuddled-path
                       (if to-markdown "markdown" "html")
                       org-path media-dir pandoc-args)))
    (unless (= 0 status-code)
      (user-error "Pandoc convertion failed: %s" status-code))))


;;;; Footer

(provide 'webshot)

;;; webshot.el ends here
