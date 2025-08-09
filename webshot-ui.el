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

(defvar webshot-ui--temp-output-dir nil
  "Temporary directory for converter outputs.")

(defvar webshot-ui--tmp-output-title "webshot"
  "Use a static title for the temporary output files as title can be changed inbetween converters.")

(defvar webshot-ui--convert-immediate nil
  "Whether the converter should save the results immediately or in a temporary directory `webshot-ui--temp-output-dir'")

(defvar webshot-ui--view-after-run nil
  "Immediately open the generated Org file after finish.")

;;;; Functions

(defun webshot-ui--get-temp-output-dir ()
  "Get or create temporary output directory for conversions."
  (unless (and webshot-ui--temp-output-dir
               (file-directory-p webshot-ui--temp-output-dir))
    (setq webshot-ui--temp-output-dir 
          (make-temp-file "webshot-output-" t)))
  webshot-ui--temp-output-dir)

(defun webshot-ui--converter-temp-output-path (instance)
  "Get the path of the org file in the temp dir."
  (file-name-concat
   (webshot-ui--get-temp-output-dir)
   (plist-get instance :name)
   (format "%s.org" webshot-ui--tmp-output-title)))

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

(defun webshot-ui--format-converter-fields (instance)
  "Format each slot and its value as a one line string."
  (let ((config (plist-get instance :config)))
    (mapconcat (lambda (slot)
                 (format "%s: \"%s\""
                         (car slot)
                         (cl-struct-slot-value (type-of config) (car slot) config)))
               (cdr (cl-struct-slot-info (type-of config)))
               ", ")))

(defun webshot-ui--completing-read-converter-instance (prompt &optional excludes return-if-one)
  "Query user to select one of converter instances."
  (let ((instances (cl-remove-if
                    (lambda (x) (member x excludes))
                    webshot-ui--converter-instances)))
    (if (= (length instances) 0)
        (user-error "No instances to select"))
    (if (and (= (length instances) 1) return-if-one)
        (car instances)
      (let* ((choices (mapcar
                       (lambda (instance)
                         (cons
                          (format "%s: %s"
                                  (propertize (plist-get instance :name) 'face '(:weight bold))
                                  (propertize
                                   (webshot-ui--format-converter-fields instance)
                                   'face '(:slant italic)))
                          instance))
                       instances))
             (choice (completing-read prompt choices nil t))
             (instance (cdr (assoc choice choices))))
        instance))))

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
  (interactive "FOutput file path: ")
  (when (string-empty-p file)
    (setq file (webshot--html-tmp-path)))
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

(defun webshot-ui-run-converter (immediate-p &optional view-p)
  "Run the current converter instance."
  (interactive)
  (unless (and
           webshot-ui--html-path
           webshot-ui--converter-instance
           webshot-ui--title
           (or (not immediate-p) webshot-ui--output-directory)
           (not (string-empty-p webshot-ui--title)))
    (user-error "Not all settings filled in."))
  
  (let* ((instance webshot-ui--converter-instance)
         (name (plist-get instance :name))
         (config (plist-get instance :config))
         (converter (cdr (assoc (plist-get instance :type) webshot-converters)))
         (run-fn (plist-get converter :run))
         (title (if immediate-p webshot-ui--title webshot-ui--tmp-output-title))
         (out-dir
          (if immediate-p
              webshot-ui--output-directory
            (file-name-directory (webshot-ui--converter-temp-output-path instance))))
         (media-dir
          (if immediate-p
              nil ;; infer from settings
            (webshot--media-path-local-shared-title
             title out-dir)))
         (file-name (concat (file-name-as-directory out-dir) title ".org")))
    ;; Errors on fail
    (funcall run-fn config webshot-ui--html-path out-dir title media-dir)
    (message "Conversion finished: %s" file-name)
    (if immediate-p
        (when webshot-ui-reset-after-done (webshot-ui-reset))
      (webshot-ui--commit-instance))
    (when view-p (find-file file-name))
    file-name))

(defun webshot-ui-view-result ()
  "View a conversion result."
  (interactive)
  (unless webshot-ui--converter-instances
    (user-error "No conversion results available"))
  (let* ((instance
          (webshot-ui--completing-read-converter-instance
           "View result: "))
         (file-path (webshot-ui--converter-temp-output-path instance)))
    (find-file file-path)))

(defun webshot-ui-ediff-results ()
  "Compare conversion results using ediff."
  (interactive)
  (unless webshot-ui--converter-instances
    (user-error "No conversion results available"))
  (when (< (length webshot-ui--converter-instances) 2)
    (user-error "Need at least 2 results to compare"))
  
  (let* ((choice1 (webshot-ui--completing-read-converter-instance "First: "))
         (choice2 (webshot-ui--completing-read-converter-instance "Second: " (list choice1)))
         (file1-path (webshot-ui--converter-temp-output-path choice1))
         (buf1 (find-file-noselect file1-path))
         (file2-path (webshot-ui--converter-temp-output-path choice2))
         (buf2 (find-file-noselect file2-path)))

    ;; Make sure the headers arent collapsed
    (with-current-buffer buf1
      (org-fold-show-all))
    (with-current-buffer buf2
      (org-fold-show-all))
    
    (ediff-buffers
     buf1 buf2
     ;; ediff-buffer-local startup-hooks:
     ;; Adds a hook that kills the two buffers after quiting ediff
     (list (lambda ()
             (add-hook
              'ediff-quit-hook
              (lambda ()
                (when (buffer-live-p buf1) (kill-buffer buf1))
                (when (buffer-live-p buf2) (kill-buffer buf2)))
              ;; t here makes it buffer-local
              nil t))))))

(defun webshot-ui-clear-instances ()
  "Clear all converter instances."
  (interactive)
  (when (yes-or-no-p "Clear all converter instances? ")
    (setq webshot-ui--converter-instances '())
    (delete-directory webshot-ui--temp-output-dir t)
    (setq webshot-ui--temp-output-dir nil)
    (webshot-ui--get-temp-output-dir)))

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
          webshot-ui--convert-immediate nil
          webshot-ui--output-directory webshot-default-output-directory
          webshot-ui--view-after-run webshot-view-after-run
          webshot-ui--converter-instances '()
          webshot-ui--converter-instances nil
          webshot-ui--temp-output-dir nil)))

;;;; Transient Interface

(defun webshot-ui--propertize-value (type value)
  "Add text property to value based on its type and contents"
  (cond
   ((equal type 'string)
    (format "%s"
            (propertize
             (format "%s" (if (string-empty-p value) "<empty>" value))
             'face
             (if value
                 (if (string-empty-p value)
                     (list 'transient-value '(:slant italic transient-value))
                   'transient-value)
               'transient-inactive-value))))
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
   ("RET" "Convert"
    (lambda ()
      (interactive)
      (let ((immediate webshot-ui--convert-immediate)
            (view webshot-ui--view-after-run))
        (setq webshot-ui--convert-immediate nil
              webshot-ui--view-after-run webshot-view-after-run)
        (webshot-ui-run-converter immediate view)
        (unless immediate (webshot--transient)))))]
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
                      type))
                   :transient t)))
              config-slots keys))

       (transient-parse-suffixes
        'webshot--transient-converter
        (vconcat (vector "Parameters") config-sufixes))
       ))])

;;;;; Main

(transient-define-prefix webshot--transient ()
  "HTML to Org Converter Interface"
  :value (lambda () (if webshot-view-after-run '("View after run") nil))
  :refresh-suffixes t
  ["HTML document"
   ("d" "Download from URL" webshot--transient-download :transient t)
   (webshot-ui--file-suffix)
   ("w" "Open in eww" webshot-ui-open-html-eww
    :if (lambda () webshot-ui--html-path))]
  ["Configuration"
   (webshot-ui--title-suffix)
   (webshot-ui--output-dir-suffix)
   ("-v" "--view" "View after run")]
  ["Run converter"
   :if (lambda () webshot-ui--html-path)
   (webshot-ui--run-suffix)]
  [:description
   (lambda ()
     (concat "Compare converters"
             (when webshot-ui--converter-instances
               (format " (%s)" (length webshot-ui--converter-instances)))))
   :if (lambda () webshot-ui--html-path)
   ("c" "Create"
    (lambda ()
      (interactive)
      (webshot-ui-select-converter)
      (webshot--transient-converter))
    :transient t)
   ;; (">" "Open directory"
    ;; (lambda () (interactive) (dired (webshot-ui--get-temp-output-dir)))
    ;; :if (lambda () webshot-ui--converter-instances)
    ;; :transient t)
   ("C" "Clear" webshot-ui-clear-instances
    :if (lambda () webshot-ui--converter-instances)
    :transient t)
   ("v" "View" webshot-ui-view-result
    :if (lambda () webshot-ui--converter-instances)
    :transient t)
   ("e" "Ediff" webshot-ui-ediff-results
    :if (lambda () webshot-ui--converter-instances)
    :inapt-if-not (lambda () (>= (length webshot-ui--converter-instances) 2)))
   (webshot-ui--save-suffix)]
  ["Misc"
   ("R" "Reset" webshot-ui-reset :transient t)
   ("q" "Quit" transient-quit-one)])

(transient-define-suffix webshot-ui--run-suffix (args)
  :description "Run immediate"
  :key "r"
  :inapt-if-not (lambda ()
                  (and webshot-ui--output-directory 
                       webshot-ui--title))
  (interactive (list (transient-args 'webshot--transient)))

  (let ((view (transient-arg-value "View after run" args)))
    (webshot-ui-select-converter)
    (setq webshot-ui--convert-immediate t)
    (setq webshot-ui--view-after-run view)
    (webshot--transient-converter)))

(transient-define-suffix webshot-ui--save-suffix (args)
  "Save a conversion result to the output directory.

Currently this reruns the converter but respecting the user specified
settings, such as where to store the directory, the media files, and how
to name the Org file. The reason is that simply copying the files over
can lead to invalid Org links if the media folder has a different
relative path to the Org file."
  :description "Save"
  :key "s"
  :if (lambda () webshot-ui--converter-instances)
  :inapt-if-not (lambda ()
                  (and webshot-ui--output-directory 
                       webshot-ui--title))
  (interactive (list (transient-args 'webshot--transient)))

  (let ((view (transient-arg-value "View after run" args))
        (instance (webshot-ui--completing-read-converter-instance "Converter: ")))
    (setq webshot-ui--converter-instance instance)
    (webshot-ui-run-converter t view)))

(transient-define-suffix webshot-ui--file-suffix ()
  :description (lambda ()
                 (format
                  "Path: %s"
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
  :transient t
  (interactive)
  (call-interactively #'webshot-ui-set-output-directory))

;;;; Footer
(provide 'webshot-ui)
