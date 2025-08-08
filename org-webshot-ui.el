;;; org-webshot-ui.el --- HTML to Org conversion utilities -*- lexical-binding: t; -*-

;;;; Requirements
(require 'cl-lib)
(require 'transient)
(require 'ediff)

;;;; Variables
(defvar org-webshot-ui--html-path nil
  "Current HTML file path for the transient interface.")

(defvar org-webshot-ui--html-out-path nil
  "Current HTML file path of the (to be) download HTML file for the transient interface.")

(defvar org-webshot-ui--url nil
  "Current URL for the transient interface.")

(defvar org-webshot-ui--title nil
  "Current title for the transient interface.")

(defvar org-webshot-ui--output-directory nil
  "Current output directory for the transient interface.")

(defvar org-webshot-ui--converter-instance nil
  "Converter instance that is being constructed in the UI.")

(defvar org-webshot-ui--converter-instances '()
  "List of converter instances created in the UI.
Each instance is a plist with :type, :name, :config keys.")

(defvar org-webshot-ui--results nil
  "Results from the last conversion run.
Alist of (instance-name . org-file-path).")

(defvar org-webshot-ui--temp-output-dir nil
  "Temporary directory for converter outputs.")

;;;; Functions

(defun org-webshot-ui--get-temp-output-dir ()
  "Get or create temporary output directory for conversions."
  (unless (and org-webshot-ui--temp-output-dir
               (file-directory-p org-webshot-ui--temp-output-dir))
    (setq org-webshot-ui--temp-output-dir 
          (make-temp-file "org-webshot-output-" t)))
  org-webshot-ui--temp-output-dir)

(defun org-webshot-ui--infer-title ()
  "Infer title from HTML file or URL."
  (cond
   ((and org-webshot-ui--html-path (file-exists-p org-webshot-ui--html-path))
    (with-temp-buffer
      (insert-file-contents org-webshot-ui--html-path nil 0 2000) ; Read first 2KB
      (goto-char (point-min))
      (if (re-search-forward "<title>\\([^<]*\\)</title>" nil t)
          (org-webshot--slugify-title (match-string 1))
        "webpage")))
   (org-webshot-ui--url
    (or (org-webshot--get-page-title org-webshot-ui--url t) "webpage"))
   (t "webpage")))

(defun org-webshot-ui--update-title-if-needed ()
  "Update title if not manually set."
  (unless org-webshot-ui--title
    (setq org-webshot-ui--title (org-webshot-ui--infer-title))))

(defun org-webshot-ui--converter-instance-name (type config)
  "Generate a unique name for a converter instance."
  (let ((base-name (plist-get (cdr (assoc type org-webshot-converters)) :name))
        (config-hash (secure-hash 'md5 (prin1-to-string config))))
    (format "%s-%s" base-name (substring config-hash 0 8))))

(defun org-webshot-ui--find-instance (type config)
  "Find existing converter instance with same type and config."
  (cl-find-if (lambda (instance)
                (and (eq (plist-get instance :type) type)
                     (equal (plist-get instance :config) config)))
              org-webshot-ui--converter-instances))

(defun org-webshot-ui--commit-instance ()
  "Commit in-progress converter instance."
  (let* ((instance org-webshot-ui--converter-instance)
         (existing (org-webshot-ui--find-instance
                    (plist-get instance :type)
                    (plist-get instance :config))))
    (push instance org-webshot-ui--converter-instances))
  (setq org-webshot-ui--converter-instance nil))

;;;; UI Commands

(defun org-webshot-ui-set-url (url)
  "Set URL for download."
  (interactive "sURL: ")
  (setq org-webshot-ui--url url
        org-webshot-ui--html-path nil
        org-webshot-ui--title nil)
  (org-webshot-ui--update-title-if-needed))

(defun org-webshot-ui-set-html-file (file)
  "Set HTML file path."
  (interactive "fHTML File: ")
  (setq org-webshot-ui--html-path (expand-file-name file)
        org-webshot-ui--url nil)
  (org-webshot-ui--update-title-if-needed))

(defun org-webshot-ui-set-html-out-file (file)
  "Set HTML file path of the website that is to be downloaded."
  (interactive "fOutput file path: ")
  (setq org-webshot-ui--html-out-path (expand-file-name file)))

(defun org-webshot-ui-set-title (title)
  "Set title string."
  (interactive "sTitle: ")
  (setq org-webshot-ui--title (org-webshot--slugify-title title)
        org-webshot-ui--html-out-path
        (org-webshot--html-path
         org-webshot-ui--url
         org-webshot-ui--title)))

(defun org-webshot-ui-set-output-directory (dir)
  "Set output directory."
  (interactive "DOutput Directory: ")
  (setq org-webshot-ui--output-directory (expand-file-name dir)))

(defun org-webshot-ui-download-html ()
  "Download HTML from URL."
  (interactive)
  (unless org-webshot-ui--url
    (user-error "No URL set"))
  (message "Downloading HTML from %s..." org-webshot-ui--url)
  (setq org-webshot-ui--html-path (org-webshot-download-website org-webshot-ui--url))
  (org-webshot-ui--update-title-if-needed)
  (message "Downloaded to %s" org-webshot-ui--html-path))

(defun org-webshot-ui-open-html-eww ()
  "Open HTML file in eww."
  (interactive)
  (unless org-webshot-ui--html-path
    (user-error "No HTML file available"))
  (unless (file-exists-p org-webshot-ui--html-path)
    (user-error "HTML file does not exist: %s" org-webshot-ui--html-path))
  (eww-open-file org-webshot-ui--html-path))

(defun org-webshot-ui-select-converter-type ()
  "Prompt user to select converter type."
  (interactive)
  (let* ((converter-types (mapcar #'car org-webshot-converters))
         (type (intern (completing-read "Converter type: " 
                                        (mapcar #'symbol-name converter-types))))
         (config-struct (plist-get (cdr (assoc type org-webshot-converters)) :config)))
    (if config-struct
        type
      (user-error "No config struct found for converter %s" type))))

(defun org-webshot-ui-run-converter ()
  "Run the current converter instance."
  (interactive)
  (unless (and
           org-webshot-ui--html-path
           org-webshot-ui--converter-instance
           org-webshot-ui--title
           (not (string-empty-p org-webshot-ui--title)))
    (user-error "Not all settings filled in."))
  
  (let* ((instance org-webshot-ui--converter-instance)
         (name (plist-get instance :name))
         (config (plist-get instance :config))
         (converter (cdr (assoc (plist-get instance :type) org-webshot-converters)))
         (run-fn (plist-get converter :run))
         (out-dir
          (file-name-concat (org-webshot-ui--get-temp-output-dir) name))
         (media-dir (org-webshot--media-path-local-shared-title
                     org-webshot-ui--title  out-dir)))
    ;; Errors on fail
    (funcall run-fn config org-webshot-ui--html-path out-dir
             org-webshot-ui--title media-dir)
    (org-webshot-ui--commit-instance)))

(defun org-webshot-ui-view-result ()
  "View a conversion result."
  (interactive)
  (unless org-webshot-ui--results
    (user-error "No conversion results available"))
  (let* ((choices (mapcar #'car org-webshot-ui--results))
         (choice (completing-read "View result: " choices))
         (file-path (cdr (assoc choice org-webshot-ui--results))))
    (find-file file-path)))

(defun org-webshot-ui-ediff-results ()
  "Compare conversion results using ediff."
  (interactive)
  (unless org-webshot-ui--results
    (user-error "No conversion results available"))
  (when (< (length org-webshot-ui--results) 2)
    (user-error "Need at least 2 results to compare"))
  
  (let* ((choices (mapcar #'car org-webshot-ui--results))
         (file1-name (completing-read "First file: " choices))
         (remaining-choices (remove file1-name choices))
         (file2-name (completing-read "Second file: " remaining-choices))
         (file1-path (cdr (assoc file1-name org-webshot-ui--results)))
         (file2-path (cdr (assoc file2-name org-webshot-ui--results))))
    (ediff-files file1-path file2-path)))

(defun org-webshot-ui-save-result ()
  "Save a conversion result to the output directory."
  (interactive)
  (unless org-webshot-ui--results
    (user-error "No conversion results available"))
  (unless org-webshot-ui--output-directory
    (setq org-webshot-ui--output-directory 
          (or org-webshot-default-output-directory
              (read-directory-name "Output directory: "))))
  
  (let* ((choices (mapcar #'car org-webshot-ui--results))
         (choice (completing-read "Save result: " choices))
         (temp-path (cdr (assoc choice org-webshot-ui--results)))
         (output-path (concat (file-name-as-directory org-webshot-ui--output-directory)
                             (file-name-nondirectory temp-path))))
    (copy-file temp-path output-path t)
    (message "Saved to %s" output-path)))

(defun org-webshot-ui-clear-instances ()
  "Clear all converter instances."
  (interactive)
  (when (yes-or-no-p "Clear all converter instances? ")
    (setq org-webshot-ui--converter-instances '())
    (message "Converter instances cleared")))

(defun org-webshot-ui-reset ()
  "Reset all UI state."
  (interactive)
  (when (or (not (called-interactively-p 'interactive))
            (yes-or-no-p "Reset all UI state? "))
    (setq org-webshot-ui--html-path nil
          org-webshot-ui--html-out-path
          (if (stringp org-webshot-html-path)
              org-webshot-html-path
            (org-webshot--html-tmp-path))
          org-webshot-ui--url nil
          org-webshot-ui--title nil
          org-webshot-ui--output-directory org-webshot-default-output-directory
          org-webshot-ui--converter-instances '()
          org-webshot-ui--results nil
          org-webshot-ui--temp-output-dir nil)))

;;;; Transient Interface

;;;;; Download webpage

(transient-define-prefix org-webshot--transient-download ()
  "Download HTML page as single file."
  :refresh-suffixes t
  ["Website download to single HTML file"
   (org-webshot-ui--url-suffix)
   (org-webshot-ui--title-suffix)
   (org-webshot-ui--html-out-suffix)
   (org-webshot-ui--html-tmp-suffix)
   (org-webshot-ui--download-suffix)])

(transient-define-suffix org-webshot-ui--url-suffix ()
  :description (lambda () 
                 (format "URL: %s" (or org-webshot-ui--url "Not set")))
  :key "u"
  :transient t
  (interactive)
  (call-interactively #'org-webshot-ui-set-url))

;; TODO Quick switch to force use temporary file
(transient-define-suffix org-webshot-ui--html-out-suffix ()
  :description
  (lambda ()
    (format "Output path: %s" (or org-webshot-ui--html-out-path "Not set")))
  :key "o"
  :transient t
  (interactive)
  (org-webshot-ui-set-html-out-file))

(transient-define-suffix org-webshot-ui--html-tmp-suffix ()
  :description ""
  :format "    %k %d"
  :key "-tmp"
  :transient t
  (interactive)
  (setq org-webshot-ui--html-out-path (org-webshot--html-tmp-path)))

(transient-define-suffix org-webshot-ui--download-suffix ()
  :description "Download"
  :key "d"
  :inapt-if-not (lambda () org-webshot-ui--url)
  (interactive)
  (org-webshot-ui-download-html))

;;;;; Converter

(defun org-webshot--unique-letters-for-strings (strings)
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

;; TODO No need for default-value, can be read from converter instance.
(defun org-webshot-ui--set-converter-value (slot-symbol default-value)
  ""
  (let ((slot-value (symbol-name slot-symbol))
        (config (plist-get org-webshot-ui--converter-instance :config)))
    (setf
     (cl-struct-slot-value (aref config 0) slot-symbol config)
     (cond ((stringp default-value)
            (read-string (format "%s: " slot-value)))
           ((numberp default-value)
            (read-number (format "%s: " slot-value)))
           ((booleanp default-value)
            (yes-or-no-p (format "%s: " slot-value)))))))

(transient-define-prefix org-webshot--transient-converter ()
  "Set up converter parameters and run.

Assumes each slot has a default value which is used to infer the type."
  :refresh-suffixes t
  [:description
   (lambda ()
     (format "Converter setup: %s"
             (plist-get org-webshot-ui--converter-instance :type)))
   ("RET" "Convert"
    (lambda () (interactive) (org-webshot-ui-run-converter)))]
   [:setup-children
   (lambda (_)
     (let* ((converter-type (org-webshot-ui-select-converter-type))
            (config-struct
             (plist-get (cdr (assoc converter-type org-webshot-converters)) :config))
            (config (funcall (intern (format "make-%s" config-struct))))
            (name (org-webshot-ui--converter-instance-name converter-type config))
            ;; cdr because the first element is cl-tag-slot
            (config-slots (cdr (cl-struct-slot-info config-struct)))
            (keys (org-webshot--unique-letters-for-strings
                   (mapcar (lambda (s) (symbol-name (car s))) config-slots)))
            config-sufixes)
       
       (setq org-webshot-ui--converter-instance
             (list :type converter-type :name name :config config))
       
       (setq config-sufixes
             (cl-mapcar
              (lambda (slot key)
                ;; The arguments of the transient sufix
                (list
                 key 
                 (symbol-name (car slot))
                 (lambda ()
                   (interactive)
                   (org-webshot-ui--set-converter-value
                    (car slot)
                    (cadr slot)))
                 :transient t))
              config-slots keys))
        
       (transient-parse-suffixes
        'org-webshot--transient-converter
        (vconcat (vector "Parameters") config-sufixes))
       ))])

;;;;; Main
(transient-define-prefix org-webshot--transient ()
  "HTML to Org Converter Interface"
  :refresh-suffixes t
  ["Input"
   (org-webshot-ui--file-suffix)
   ("d" "Download from URL" org-webshot--transient-download :transient t)
   ("w" "Open in eww" org-webshot-ui-open-html-eww
    :if (lambda () org-webshot-ui--html-path))]
  ["Configuration"
   ("t" org-webshot-ui--title-suffix)
   ("o" org-webshot-ui--output-dir-suffix)]
  ["Converters"
   ("c" "Create converter" org-webshot--transient-converter :transient t)
   (">" "Open tmp dir"
    (lambda () (interactive) (dired (org-webshot-ui--get-temp-output-dir)))
    :transient t)
   ("C" "Clear instances" org-webshot-ui-clear-instances
    :if (lambda () org-webshot-ui--converter-instances)
    :transient t)
   ("l" org-webshot-ui--list-instances-suffix :transient t)]
  ["Results" 
   :if (lambda () org-webshot-ui--results)
   ("v" "View result" org-webshot-ui-view-result)
   ("e" "Ediff results" org-webshot-ui-ediff-results
    :if (lambda () (>= (length org-webshot-ui--results) 2)))
   ("s" "Save result" org-webshot-ui-save-result)]
  ["Misc"
   ("q" "Quit" transient-quit-one)
   ("Q" "Reset & Quit" org-webshot-ui-reset)])

;; Dynamic suffixes for showing current state
(transient-define-suffix org-webshot-ui--file-suffix ()
  :description (lambda () 
                 (format "HTML file: %s" 
                         (if org-webshot-ui--html-path
                             ;; (file-name-nondirectory org-webshot-ui--html-path)
                             org-webshot-ui--html-path
                           "Not set")))
  :key "f"
  :transient t
  (interactive)
  (call-interactively #'org-webshot-ui-set-html-file))

(transient-define-suffix org-webshot-ui--download-suffix ()
  :description "Download HTML from URL"
  :key "d"
  :if (lambda () org-webshot-ui--url)
  (interactive)
  (org-webshot-ui-download-html))

(transient-define-suffix org-webshot-ui--title-suffix ()
  :description (lambda () 
                 (format "Title: %s" (or org-webshot-ui--title "Not set")))
  :key "t"
  :transient t
  (interactive)
  (call-interactively #'org-webshot-ui-set-title))

(transient-define-suffix org-webshot-ui--output-dir-suffix ()
  :description (lambda () 
                 (format "Output dir: %s" 
                         (or org-webshot-ui--output-directory 
                             "Not set")))
  :key "o"
  (interactive)
  (call-interactively #'org-webshot-ui-set-output-directory))

(transient-define-suffix org-webshot-ui--list-instances-suffix ()
  :description (lambda () 
                 (format "Instances (%d)" (length org-webshot-ui--converter-instances)))
  :key "l"
  :if (lambda () org-webshot-ui--converter-instances)
  (interactive)
  (let ((instances org-webshot-ui--converter-instances))
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
(provide 'org-webshot-ui)
