;;; webshot-ui.el --- HTML to Org conversion utilities -*- lexical-binding: t; -*-

;;;; Requirements
(require 'cl-lib)
(require 'transient)
(require 'ediff)

;;;; Variables
(defvar webshot-ui--html-path nil
  "Current HTML file path for the transient interface.")

(defvar webshot-ui--html-out-path nil
  "Current HTML file path of the (to be) download HTML file for the transient interface.")

(defvar webshot-ui--url nil
  "Current URL for the transient interface.")

(defvar webshot-ui--title nil
  "Current title for the transient interface.")

(defvar webshot-ui--output-directory nil
  "Current output directory for the transient interface.")

(defvar webshot-ui--converter-instance nil
  "Converter instance that is being constructed in the UI.")

(defvar webshot-ui--converter-instances '()
  "List of converter instances created in the UI.
Each instance is a plist with :type, :name, :config keys.")

(defvar webshot-ui--results nil
  "Results from the last conversion run.
Alist of (instance-name . org-file-path).")

(defvar webshot-ui--temp-output-dir nil
  "Temporary directory for converter outputs.")

;;;; Functions

(defun webshot-ui--get-temp-output-dir ()
  "Get or create temporary output directory for conversions."
  (unless (and webshot-ui--temp-output-dir
               (file-directory-p webshot-ui--temp-output-dir))
    (setq webshot-ui--temp-output-dir 
          (make-temp-file "webshot-output-" t)))
  webshot-ui--temp-output-dir)

(defun webshot-ui--infer-title ()
  "Infer title from HTML file or URL."
  (cond
   ((and webshot-ui--html-path (file-exists-p webshot-ui--html-path))
    (with-temp-buffer
      (insert-file-contents webshot-ui--html-path nil 0 2000) ; Read first 2KB
      (goto-char (point-min))
      (if (re-search-forward "<title>\\([^<]*\\)</title>" nil t)
          (webshot--slugify-title (match-string 1))
        "webpage")))
   (webshot-ui--url
    (or (webshot--get-page-title webshot-ui--url t) "webpage"))
   (t "webpage")))

(defun webshot-ui--update-title-if-needed ()
  "Update title if not manually set."
  (unless webshot-ui--title
    (setq webshot-ui--title (webshot-ui--infer-title))))

(defun webshot-ui--converter-instance-name (type config)
  "Generate a unique name for a converter instance."
  (let ((base-name (plist-get (cdr (assoc type webshot-converters)) :name))
        (config-hash (secure-hash 'md5 (prin1-to-string config))))
    (format "%s-%s" base-name (substring config-hash 0 8))))

(defun webshot-ui--find-instance (type config)
  "Find existing converter instance with same type and config."
  (cl-find-if (lambda (instance)
                (and (eq (plist-get instance :type) type)
                     (equal (plist-get instance :config) config)))
              webshot-ui--converter-instances))

(defun webshot-ui--commit-instance ()
  "Commit in-progress converter instance."
  (let* ((instance webshot-ui--converter-instance)
         (existing (webshot-ui--find-instance
                    (plist-get instance :type)
                    (plist-get instance :config))))
    (push instance webshot-ui--converter-instances))
  (setq webshot-ui--converter-instance nil))

;;;; UI Commands

(defun webshot-ui-set-url (url)
  "Set URL for download."
  (interactive "sURL: ")
  (setq webshot-ui--url url
        webshot-ui--html-path nil
        webshot-ui--title nil)
  (webshot-ui--update-title-if-needed))

(defun webshot-ui-set-html-file (file)
  "Set HTML file path."
  (interactive "fHTML File: ")
  (setq webshot-ui--html-path (expand-file-name file)
        webshot-ui--url nil)
  (webshot-ui--update-title-if-needed))

(defun webshot-ui-set-html-out-file (file)
  "Set HTML file path of the website that is to be downloaded."
  (interactive "fOutput file path: ")
  (setq webshot-ui--html-out-path (expand-file-name file)))

(defun webshot-ui-set-title (title)
  "Set title string."
  (interactive "sTitle: ")
  (setq webshot-ui--title (webshot--slugify-title title)
        webshot-ui--html-out-path
        (webshot--html-path
         webshot-ui--url
         webshot-ui--title)))

(defun webshot-ui-set-output-directory (dir)
  "Set output directory."
  (interactive "DOutput Directory: ")
  (setq webshot-ui--output-directory (expand-file-name dir)))

(defun webshot-ui-download-html ()
  "Download HTML from URL."
  (interactive)
  (unless webshot-ui--url
    (user-error "No URL set"))
  (message "Downloading HTML from %s..." webshot-ui--url)
  (setq webshot-ui--html-path (webshot-download-website webshot-ui--url))
  (webshot-ui--update-title-if-needed)
  (message "Downloaded to %s" webshot-ui--html-path))

(defun webshot-ui-open-html-eww ()
  "Open HTML file in eww."
  (interactive)
  (unless webshot-ui--html-path
    (user-error "No HTML file available"))
  (unless (file-exists-p webshot-ui--html-path)
    (user-error "HTML file does not exist: %s" webshot-ui--html-path))
  (eww-open-file webshot-ui--html-path))

(defun webshot-ui-select-converter ()
  "Prompt user to select converter for building."
  (interactive)
  (let* ((converter-types (mapcar #'car webshot-converters))
         (type (intern (completing-read "Converter type: " 
                                        (mapcar #'symbol-name converter-types))))
         (config-struct (plist-get (cdr (assoc type webshot-converters)) :config))
         (config (funcall (intern (format "make-%s" config-struct))))
         (name (webshot-ui--converter-instance-name type config)))
    (setq webshot-ui--converter-instance
          (list :type type :name name :config config))))

(defun webshot-ui-run-converter ()
  "Run the current converter instance."
  (interactive)
  (unless (and
           webshot-ui--html-path
           webshot-ui--converter-instance
           webshot-ui--title
           (not (string-empty-p webshot-ui--title)))
    (user-error "Not all settings filled in."))
  
  (let* ((instance webshot-ui--converter-instance)
         (name (plist-get instance :name))
         (config (plist-get instance :config))
         (converter (cdr (assoc (plist-get instance :type) webshot-converters)))
         (run-fn (plist-get converter :run))
         (out-dir
          (file-name-concat (webshot-ui--get-temp-output-dir) name))
         (media-dir (webshot--media-path-local-shared-title
                     webshot-ui--title  out-dir)))
    ;; Errors on fail
    (funcall run-fn config webshot-ui--html-path out-dir
             webshot-ui--title media-dir)
    (webshot-ui--commit-instance)))

(defun webshot-ui-view-result ()
  "View a conversion result."
  (interactive)
  (unless webshot-ui--results
    (user-error "No conversion results available"))
  (let* ((choices (mapcar #'car webshot-ui--results))
         (choice (completing-read "View result: " choices))
         (file-path (cdr (assoc choice webshot-ui--results))))
    (find-file file-path)))

(defun webshot-ui-ediff-results ()
  "Compare conversion results using ediff."
  (interactive)
  (unless webshot-ui--results
    (user-error "No conversion results available"))
  (when (< (length webshot-ui--results) 2)
    (user-error "Need at least 2 results to compare"))
  
  (let* ((choices (mapcar #'car webshot-ui--results))
         (file1-name (completing-read "First file: " choices))
         (remaining-choices (remove file1-name choices))
         (file2-name (completing-read "Second file: " remaining-choices))
         (file1-path (cdr (assoc file1-name webshot-ui--results)))
         (file2-path (cdr (assoc file2-name webshot-ui--results))))
    (ediff-files file1-path file2-path)))

(defun webshot-ui-save-result ()
  "Save a conversion result to the output directory."
  (interactive)
  (unless webshot-ui--results
    (user-error "No conversion results available"))
  (unless webshot-ui--output-directory
    (setq webshot-ui--output-directory 
          (or webshot-default-output-directory
              (read-directory-name "Output directory: "))))
  
  (let* ((choices (mapcar #'car webshot-ui--results))
         (choice (completing-read "Save result: " choices))
         (temp-path (cdr (assoc choice webshot-ui--results)))
         (output-path (concat (file-name-as-directory webshot-ui--output-directory)
                             (file-name-nondirectory temp-path))))
    (copy-file temp-path output-path t)
    (message "Saved to %s" output-path)))

(defun webshot-ui-clear-instances ()
  "Clear all converter instances."
  (interactive)
  (when (yes-or-no-p "Clear all converter instances? ")
    (setq webshot-ui--converter-instances '())
    (message "Converter instances cleared")))

(defun webshot-ui-reset ()
  "Reset all UI state."
  (interactive)
  (when (or (not (called-interactively-p 'interactive))
            (yes-or-no-p "Reset all UI state? "))
    (setq webshot-ui--html-path nil
          webshot-ui--html-out-path
          (if (stringp webshot-html-path)
              webshot-html-path
            (webshot--html-tmp-path))
          webshot-ui--url nil
          webshot-ui--title nil
          webshot-ui--output-directory webshot-default-output-directory
          webshot-ui--converter-instances '()
          webshot-ui--results nil
          webshot-ui--temp-output-dir nil)))

;;;; Transient Interface

(defun webshot-ui--propertize-value (type value)
  ""
  (cond
   ((equal type 'string)
    (format "%s" (propertize (format "%s" value) 'face (if value 'transient-value 'transient-inactive-value))))
   ((equal type 'number)
    (format "%s"
            (propertize
             (format "%s" value)
             'face
             (if value 'transient-value 'transient-inactive-value))))
   ((equal type 'boolean)
    (propertize (format "%s" value) 'face 'transient-value))))

;;;;; Download webpage

(transient-define-prefix webshot--transient-download ()
  "Download HTML page as single file."
  :refresh-suffixes t
  ["Website download to single HTML file"
   (webshot-ui--url-suffix)
   (webshot-ui--title-suffix)
   (webshot-ui--html-out-suffix)
   (webshot-ui--html-tmp-suffix)
   (webshot-ui--download-suffix)])

(transient-define-suffix webshot-ui--url-suffix ()
  :description
  (lambda () 
    (format "URL: %s" (webshot-ui--propertize-value 'string webshot-ui--url)))
  :key "u"
  :transient t
  (interactive)
  (call-interactively #'webshot-ui-set-url))

;; TODO Quick switch to force use temporary file
(transient-define-suffix webshot-ui--html-out-suffix ()
  :description
  (lambda ()
    (format "Output path: %s"
            (webshot-ui--propertize-value 'string webshot-ui--html-out-path)))
  :key "o"
  :transient t
  (interactive)
  (call-interactively #'webshot-ui-set-html-out-file))

(transient-define-suffix webshot-ui--html-tmp-suffix ()
  :description "Temporary file"
  :format "    %k %d"
  :key "-tmp"
  :transient t
  (interactive)
  (setq webshot-ui--html-out-path (webshot--html-tmp-path)))

(transient-define-suffix webshot-ui--download-suffix ()
  :description "Download"
  :key "d"
  :inapt-if-not (lambda () (and webshot-ui--url webshot-ui--html-out-path))
  (interactive)
  (webshot-ui-download-html))

;;;;; Converter

(defun webshot--unique-letters-for-strings (strings)
  "Return a list of unique letters, one for each string in STRINGS.
Prefer the first unused character from each string; if exhausted,
choose a random unused letter from a–z."
  (let ((taken (make-hash-table :test #'equal))
        result)
    (dolist (s strings (nreverse result))
      (let ((chosen nil))
        ;; Try each letter in the string until we find an unused one
        (cl-loop for ch across s
                 for letter = (downcase (char-to-string ch))
                 unless (gethash letter taken)
                 do (setq chosen letter)
                    (puthash letter t taken)
                    (cl-return))
        ;; If all letters are taken, pick a random unused one
        (unless chosen
          (let* ((alphabet (mapcar #'char-to-string (number-sequence ?a ?z)))
                 (unused (cl-remove-if (lambda (l) (gethash l taken))
                                       alphabet)))
            (if unused
                (setq chosen (nth (random (length unused)) unused))
              (error "Ran out of letters"))))
        (push chosen result)))))

;; TODO No need for value-type, can be read from converter instance.
(defun webshot-ui--set-converter-value (slot-symbol value-type)
  ""
  (let ((slot-value (symbol-name slot-symbol))
        (config (plist-get webshot-ui--converter-instance :config)))
    (setf
     (cl-struct-slot-value (aref config 0) slot-symbol config)
     (cond ((equal value-type 'string)
            (read-string (format "%s: " slot-value)))
           ((equal value-type 'number)
            (read-number (format "%s: " slot-value)))
           ((equal value-type 'boolean)
            (yes-or-no-p (format "%s: " slot-value)))))))

(defun webshot-ui--get-config-value (config field)
  "Get the current value for KEY from the converter instance."
  (cl-struct-slot-value (type-of config) field config))

(transient-define-prefix webshot--transient-converter ()
  "Set up converter parameters and run.

Assumes each slot has a default value which is used to infer the type."
  :refresh-suffixes t 
  [:description
   (lambda ()
     (format "Converter setup: %s"
             (plist-get webshot-ui--converter-instance :type)))
   ("RET" "Convert" webshot-ui-run-converter)]
  [:setup-children
   (lambda (_)
     (let* ((converter-type (plist-get webshot-ui--converter-instance :type))
            (config-struct
             (plist-get (cdr (assoc converter-type webshot-converters)) :config))
            ;; cdr because the first element is cl-tag-slot
            (config-slots (cdr (cl-struct-slot-info config-struct)))
            (keys (webshot--unique-letters-for-strings
                   (mapcar (lambda (s) (symbol-name (car s))) config-slots)))
            config-sufixes)
              
       (setq config-sufixes
             (cl-mapcar
              (lambda (slot key)
                (let ((value (nth 1 slot))
                      (type (plist-get slot :type)))
                  (list
                   key 
                   (format "%s: %s"
                           (symbol-name (car slot))
                           (webshot-ui--propertize-value
                            type
                            (webshot-ui--get-config-value
                             (plist-get webshot-ui--converter-instance :config)
                             (car slot))))
                   (lambda ()
                     (interactive)
                     (webshot-ui--set-converter-value
                      (car slot)
                      type)
                     (message "%s" webshot-ui--converter-instance))
                   :transient t)))
              config-slots keys))

       (transient-parse-suffixes
        'webshot--transient-converter
        (vconcat (vector "Parameters") config-sufixes))
       ))])

;;;;; Main
(transient-define-prefix webshot--transient ()
  "HTML to Org Converter Interface"
  :refresh-suffixes t
  ["Input"
   (webshot-ui--file-suffix)
   ("d" "Download from URL" webshot--transient-download :transient t)
   ("w" "Open in eww" webshot-ui-open-html-eww
    :if (lambda () webshot-ui--html-path))]
  ["Configuration"
   ("t" webshot-ui--title-suffix)
   ("o" webshot-ui--output-dir-suffix)]
  ["Converters"
   ("c" "Create converter"
    (lambda ()
      (interactive)
      (webshot-ui-select-converter)
      (webshot--transient-converter))
    :transient t)
   (">" "Open tmp dir"
    (lambda () (interactive) (dired (webshot-ui--get-temp-output-dir)))
    :transient t)
   ("C" "Clear instances" webshot-ui-clear-instances
    :if (lambda () webshot-ui--converter-instances)
    :transient t)
   ("l" webshot-ui--list-instances-suffix :transient t)]
  ["Results" 
   :if (lambda () webshot-ui--results)
   ("v" "View result" webshot-ui-view-result)
   ("e" "Ediff results" webshot-ui-ediff-results
    :if (lambda () (>= (length webshot-ui--results) 2)))
   ("s" "Save result" webshot-ui-save-result)]
  ["Misc"
   ("q" "Quit" transient-quit-one)
   ("Q" "Reset & Quit" webshot-ui-reset)])

;; Dynamic suffixes for showing current state
(transient-define-suffix webshot-ui--file-suffix ()
  :description (lambda ()
                 (format
                  "HTML file: %s"
                  (webshot-ui--propertize-value 'string webshot-ui--html-path)))
  :key "f"
  :transient t
  (interactive)
  (call-interactively #'webshot-ui-set-html-file))

(transient-define-suffix webshot-ui--download-suffix ()
  :description "Download HTML from URL"
  :key "d"
  :if (lambda () webshot-ui--url)
  (interactive)
  (webshot-ui-download-html))

(transient-define-suffix webshot-ui--title-suffix ()
  :description (lambda () 
                 (format "Title: %s"
                         (webshot-ui--propertize-value 'string webshot-ui--title)))
  :key "t"
  :transient t
  (interactive)
  (call-interactively #'webshot-ui-set-title))

(transient-define-suffix webshot-ui--output-dir-suffix ()
  :description (lambda () 
                 (format "Output dir: %s" 
                         (webshot-ui--propertize-value 'string webshot-ui--output-directory)))
  :key "o"
  (interactive)
  (call-interactively #'webshot-ui-set-output-directory))

(transient-define-suffix webshot-ui--list-instances-suffix ()
  :description (lambda () 
                 (format "Instances (%d)" (length webshot-ui--converter-instances)))
  :key "l"
  :if (lambda () webshot-ui--converter-instances)
  (interactive)
  (let ((instances webshot-ui--converter-instances))
    (with-current-buffer (get-buffer-create "*Converter Instances*")
      (erase-buffer)
      (insert "Converter Instances:\n\n")
      (dolist (instance instances)
        (insert (format "• %s (%s)\n" 
                        (plist-get instance :name)
                        (plist-get instance :type))))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;; Footer
(provide 'webshot-ui)
