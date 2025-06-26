;;; org-table-highlight.el --- Highlight Org table columns and rows -*- lexical-binding: t; -*-

;; Author: Lei Zhe
;; URL: https://github.com/llcc/org-table-highlight
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: org-table, convenience

;;; Commentary:

;; This package provides utilities to highlight columns and rows in Org-mode tables.
;; It supports cycling through color palettes, clearing highlights, and working
;; with both column and row overlays.

;;; Code:

(defgroup org-table-highlight nil
  "Highlight columns and rows in Org tables."
  :prefix "org-table-highlight-"
  :group 'org)

(defcustom org-table-highlight-color-palette
  '("#FFE4B5" "#C1FFC1" "#B0E0E6" "#FFB6C1" "#D8BFD8" "#F4A460" "#ADD8E6")
  "List of pastel colors used to highlight Org table columns and rows."
  :type '(repeat color)
  :group 'org-table-highlight)

(defvar-local org-table-highlight--highlighted-columns 0
  "Number of Org table columns currently highlighted.")

(defvar-local org-table-highlight--highlighted-rows 0
  "Number of Org table rows currently highlighted.")

(defvar org-table-highlight--metadata nil
  "Metadata for Org table highlights.

Format is:
  ((BUFFER-NAME
     (((:name TABLE-NAME :before-string STR :after-string STR)
       :col ((COL COLOR) ...) :row ((ROW COLOR) ...)))
   ...))")

;; Internal helper functions
(defun org-table-highlight--table-bounds ()
  "Return the (START . END) buffer positions of the current Org table."
  (when (org-at-table-p)
    (cons (save-excursion (org-table-begin))
          (save-excursion (org-table-end)))))

(defun org-table-highlight--next-color ()
  "Return the next color from the palette using COUNTER."
  (nth (mod org-table-highlight--highlighted-columns
            (length org-table-highlight-color-palette))
       org-table-highlight-color-palette))

(defun org-table-highlight--make-overlay (start end face-prop symbol val)
  "Create an overlay from START to END with FACE-PROP, and SYMBOL set to VAL."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face face-prop)
    (overlay-put ov symbol val)
    ov))

(defun org-table-highlight--remove-overlays (start end prop &optional value)
  "Delete overlays between START and END that have PROP (and optionally VALUE)."
  (dolist (ov (overlays-in start end))
    (when (and (overlay-get ov prop)
               (or (not value) (equal (overlay-get ov prop) value)))
      (delete-overlay ov))))

(defun org-table-highlight--get-table-name ()
  "Try to get the Org table name via #+NAME."
  (when-let (table (org-element-lineage
                    (org-element-context) 'table t))
    (plist-get (cadr table) :name)))

(defun org-table-highlight--overlayp (prop &optional value)
  "Return non-nil if an overlay with PROP (and optional VALUE) exists at point.

PROP should be either 'org-table-highlight-column or 'org-table-highlight-row.
If VALUE is non-nil, only return true if PROP equals VALUE."
  (cl-some (lambda (ov)
             (let ((ov-val (overlay-get ov prop)))
               (and ov-val (or (not value) (equal ov-val value)))))
           (overlays-at (point))))

(defcustom org-table-highlight-table-context-length 20
  "Number of characters before and after an Org table to save as context.

This context helps identify the table uniquely when it lacks a `#+NAME:`
property. It is used to match and restore highlights across sessions
by storing a short prefix and suffix string around the table position."
  :type 'integer
  :group 'org-table-highlight)

(defun org-table-highlight--table-context ()
  "Return contextual metadata for the Org table at point.

This includes the table's name (if any), a short string before the table,
and a short string after it, used to help identify the table if it has
no `#+NAME:` property. The length of these strings is controlled by
`org-table-highlight-table-context-length`."
  (when (org-at-table-p)
    (save-excursion
      (let* ((table-name (org-table-highlight--get-table-name))
             (begin (org-table-begin))
             (before-string
              (buffer-substring-no-properties
               (max (point-min) (- begin org-table-highlight-table-context-length))
               begin))
             (after-string
              (buffer-substring-no-properties
               begin
               (min (point-max) (+ begin org-table-highlight-table-context-length)))))
        (list :name table-name
              :before-string before-string
              :after-string after-string)))))

;;;###autoload
(defun org-table-highlight-column (&optional color)
  "Highlight the current Org table column with a cycling or user-supplied COLOR.
With \\[universal-argument] prefix, prompt for color."
  (interactive
   (list (when current-prefix-arg (read-color "Column color: " t))))
  (when (and (org-at-table-p)
             (not (org-table-highlight--overlayp 'org-table-highlight-column)))
    (let* ((buf-name (buffer-name))
           (table-context (org-table-highlight--table-context))
           (col (org-table-current-column))
           (chosen-color (or color (org-table-highlight--next-color)))
           (bounds (org-table-highlight--table-bounds)))
      (cl-incf org-table-highlight--highlighted-columns)
      (org-table-highlight--update-metadata buf-name table-context :col col chosen-color)
      (save-restriction
        (narrow-to-region (car bounds) (cdr bounds))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line-end (line-end-position))
                  (pos (line-beginning-position))
                  (i 0))
              (while (and (< i col)
                          (re-search-forward "[|\\|+]" line-end t))
                (setq pos (point))
                (setq i (1+ i)))
              (when (re-search-forward "[|\\|+]" line-end t)
                (org-table-highlight--make-overlay
                 pos (1- (point)) `(:background ,chosen-color)
                 'org-table-highlight-column col)))
            (forward-line 1)))))))

;;;###autoload
(defun org-table-highlight-row (&optional color)
  "Highlight the current Org table row with a cycling or user-supplied COLOR.
With \\[universal-argument] prefix, prompt for color."
  (interactive
   (list (when current-prefix-arg (read-color "Row color: " t))))
  (when (and (org-at-table-p)
             (not (org-table-highlight--overlayp 'org-table-highlight-row)))
    (let* ((buf-name (buffer-name))
           (table-context (org-table-highlight--table-context))
           (row (org-table-current-line))
           (chosen-color (or color (org-table-highlight--next-color)))
           (start (line-beginning-position))
           (end (line-end-position)))
      (cl-incf org-table-highlight--highlighted-rows)
      (org-table-highlight--update-metadata buf-name table-context :row row chosen-color)
      (unless (org-table-highlight--overlayp 'org-table-highlight-row)
        (org-table-highlight--make-overlay start end `(:background ,chosen-color)
                                           'org-table-highlight-row row)))))

(defun org-table-highlight-restore ()
  "Restore highlights for the Org table at point using stored metadata."
  (interactive)
  (when (org-at-table-p)
    (when-let* ((buf-name (buffer-name))
                (table-context (org-table-highlight--table-context))
                (buf-entry (assoc buf-name org-table-highlight--metadata)))
      (let* ((table-list (cadr buf-entry))
             (table-entry (assoc table-context table-list)))

        ;; Reapply column highlights
        (dolist (col-entry (plist-get (cdr table-entry) :col))
          (let ((col (car col-entry))
                (color (cdr col-entry)))
            (save-excursion
              (org-table-goto-column col)
              (org-table-highlight-column color))))

        ;; Reapply row highlights
        (dolist (row-entry (plist-get (cdr table-entry) :row))
          (let ((row (car row-entry))
                (color (cdr row-entry)))
            (save-excursion
              (goto-char (org-table-begin))
              (forward-line (1- row))
              (org-table-highlight-row color))))))))

(advice-add 'org-table-align :after #'org-table-highlight-restore)
(advice-add 'org-table-next-field :after #'org-table-highlight-restore)

;;;###autoload
(defun org-table-highlight-clear-column-highlights (&optional all)
  "Clear highlights in current Org table column.
With prefix argument ALL, clear all column highlights."
  (interactive "P")
  (when (or all (org-table-highlight--overlayp 'org-table-highlight-column))
    (when-let ((buf-name (buffer-name))
               (table-context (org-table-highlight--table-context))
               (bounds (org-table-highlight--table-bounds)))
      (let ((col (if all nil (org-table-current-column))))
        (org-table-highlight--remove-metadata
         buf-name table-context :col col)
        (org-table-highlight--remove-overlays
         (car bounds) (cdr bounds)
         'org-table-highlight-column col)))))

;;;###autoload
(defun org-table-highlight-clear-row-highlights (&optional all)
  "Clear highlights in current Org table row.
With prefix argument ALL, clear all row highlights."
  (interactive "P")
  (when (or all (org-table-highlight--overlayp 'org-table-highlight-column))
    (when-let ((buf-name (buffer-name))
               (table-context (org-table-highlight--table-context))
               (bounds (org-table-highlight--table-bounds)))
      (let ((row (if all nil (org-table-current-line))))
        (org-table-highlight--remove-metadata buf-name table-context :row row)
        (org-table-highlight--remove-overlays (car bounds) (cdr bounds)
                                              'org-table-highlight-row row)))))

;;;###autoload
(defun org-table-highlight-clear-all-highlights ()
  "Clear all column and row highlights in current Org table."
  (interactive)
  (when-let ((buf-name (buffer-name))
             (table-context (org-table-highlight--table-context)) 
             (bounds (org-table-highlight--table-bounds)))
    (org-table-highlight--remove-metadata buf-name table-context nil)
    (org-table-highlight--remove-overlays
     (car bounds) (cdr bounds) 'org-table-highlight-column)
    (org-table-highlight--remove-overlays
     (car bounds) (cdr bounds) 'org-table-highlight-row)))

(defun org-table-highlight--update-metadata (buf-name table-context type index color)
  "Update highlight metadata for BUF-NAME and TABLE-CONTEXT.
TYPE is :col or :row.  INDEX is the column or row number.  COLOR is the
highlight color."
  (let ((buf-entry (assoc buf-name org-table-highlight--metadata)))
    (if buf-entry
        (let* ((table-list (cadr buf-entry))
               (table-entry (assoc table-context table-list)))
          (if table-entry
              ;; Update existing table entry in place
              (let* ((plist (cdr table-entry))
                     (existing (plist-get plist type))
                     (filtered (cl-remove-if
                                (lambda (item)
                                  (= (car item) index))
                                existing))
                     (newlist (cons (cons index color) filtered))
                     (new-plist (plist-put plist type newlist)))
                ;; Modify plist in place by setting cdr of table-entry
                (setcdr table-entry new-plist))
            ;; Table does not exist, add new entry to table-list in place
            (setcdr buf-entry
                    (list (append table-list
                                  (list
                                   (list table-context type
                                         (list (cons index color)))))))))
      ;; Buffer does not exist, add new buffer entry
      (push (list buf-name (list (list table-context type (list (cons index color)))))
            org-table-highlight--metadata))
    (org-table-highlight-save-metadata)))

(defun org-table-highlight--remove-plist-key (plist key)
  "Return a copy of PLIST with KEY and its value removed."
  (let (new-plist)
    (while plist
      (let ((k (pop plist))
            (v (pop plist)))
        (unless (eq k key)
          (setq new-plist (plist-put new-plist k v)))))
    new-plist))

(defun org-table-highlight--remove-metadata (buf-name table-name type &optional index)
  "Remove highlight metadata from `org-table-highlight--metadata'.

- If TYPE and INDEX are non-nil: remove the specific INDEX under TYPE.
- If TYPE is non-nil and INDEX is nil: remove all entries under TYPE.
- If both TYPE and INDEX are nil: remove the entire TABLE-NAME entry
- from BUF-NAME.  If BUF-NAME has no tables left, remove BUF-NAME from
  metadata."
  (let ((buf-entry (assoc buf-name org-table-highlight--metadata)))
    (when buf-entry
      (let ((table-entry (assoc table-name (cadr buf-entry))))
        (cond
         ;; Case: remove a specific index under type (e.g., column 2)
         ((and table-entry type index)
          (let* ((plist (cdr table-entry))
                 (entries (plist-get plist type))
                 (filtered (cl-remove-if (lambda (item) (= (car item) index)) entries)))
            (if filtered
                (setcdr table-entry (plist-put plist type filtered))
              (setcdr table-entry
                      (org-table-highlight--remove-plist-key (cdr table-entry) type)))))

         ;; Case: remove all of a type (e.g., all rows)
         ((and table-entry type (null index))
          (setcdr table-entry (org-table-highlight--remove-plist-key (cdr table-entry) type)))

         ;; Case: remove entire table entry
         ((and table-entry (null type))
          (setcdr buf-entry
                  (list (assoc-delete-all table-name (cadr buf-entry) #'equal)))))

        (when (null (or (plist-get (cdr table-entry) :row)
                        (plist-get (cdr table-entry) :col)))
          (setcdr buf-entry
                  (list (assoc-delete-all table-name (cadr buf-entry) #'equal)))))

      ;; If buf-entry has no more tables, remove the buffer entirely
      (when (null (cadr buf-entry))
        (setq org-table-highlight--metadata
              (assoc-delete-all buf-name org-table-highlight--metadata #'equal)))))

  (org-table-highlight-save-metadata))

(defcustom org-table-highlight-metadata-file
  (locate-user-emacs-file "org-table-highlight-metadata.el")
  "File where Org table highlight metadata is saved."
  :type 'file
  :group 'org-table-highlight)

(defun org-table-highlight-save-metadata ()
  "Save `org-table-highlight--metadata` to `org-table-highlight-metadata-file'."
  (interactive)
  (condition-case nil
      (with-temp-file org-table-highlight-metadata-file
        (insert ";;; org-table-highlight saved metadata. ")
        (insert "Do not edit this file.\n\n")
        (prin1 org-table-highlight--metadata (current-buffer))
        (pp-buffer)
        (insert "\n"))
    (error "Cannot save metadata to %s" org-table-highlight-metadata-file)))

;;;###autoload
(defun org-table-highlight-load-metadata ()
  "Load Org table highlight metadata from `org-table-highlight-metadata-file'."
  (interactive)
  (when (file-exists-p org-table-highlight-metadata-file)
    (condition-case nil
        (progn (setq org-table-highlight--metadata
                     (with-temp-buffer
                       (insert-file-contents org-table-highlight-metadata-file)
                       (goto-char (point-min))
                       (read (current-buffer))))
               (message "org-table-highlight--metadata variable loaded"))
      (error "Cannot read metadata at %s" org-table-highlight-metadata-file))))

(defun org-table-highlight--get-table-position (context)
  "Get position of table beginning position based on context."
  (let ((name (plist-get context :name))
        (before-string (plist-get context :before-string))
        (after-string (plist-get context :after-string))
        point)

    (goto-char (point-min))
    
    (if name
        (progn (re-search-forward (format "#\\+NAME:[ \t]*%s" (regexp-quote name)) nil t)
               (setq point (1+ (point))))
       
      (if (and before-string (search-forward before-string (point-max) t))
          (progn
            (goto-char (match-end 0))
            (setq point (point))))
        
      (if (and after-string (search-forward after-string (point-max) t))
          (progn
            (goto-char (match-beginning 0))
            (setq point (point)))))
    point))

;;;###autoload
(defun org-table-highlight-apply-buffer-metadata ()
  "Apply highlight metadata to all tables in the current buffer."
  (interactive)
  (let* ((buf-name (buffer-name))
         (buf-entry (assoc buf-name org-table-highlight--metadata)))
    (when buf-entry
      (dolist (table-entry (cadr buf-entry))
        (let* ((table-context (car table-entry))
               (pos (org-table-highlight--get-table-position table-context))
               (plist (cdr table-entry)))
          (save-excursion
            (when (and pos (goto-char pos))
              ;; Apply columns
              (dolist (col-entry (plist-get plist :col))
                (let ((col (car col-entry))
                      (color (cdr col-entry)))
                  (org-table-goto-column col)
                  (org-table-highlight-column color)))

              ;; Apply rows
              (dolist (row-entry (plist-get plist :row))
                (let ((row (car row-entry))
                      (color (cdr row-entry)))
                  (goto-char (org-table-begin))
                  (forward-line (1- row))
                  (org-table-highlight-row color))))))))))

(defun org-table-highlight--collect-table-metadata (tbl)
  "Collect highlight metadata from TBL (an `org-element' table).

Returns a metadata entry of the form:
  ((:name NAME :before-string STR :after-string STR)
   :col ((N . COLOR)) :row ((N . COLOR)))
or nil if there are no highlight overlays."
  (let* ((begin (org-element-property :contents-begin tbl))
         (end (org-element-property :contents-end tbl))
         (overlays (overlays-in begin end))
         (col-entries '())
         (row-entries '()))
    (dolist (ov overlays)
      (let ((col (overlay-get ov 'org-table-highlight-column))
            (row (overlay-get ov 'org-table-highlight-row))
            (face (overlay-get ov 'face)))
        (let ((color (cond
                      ((and (listp face) (plist-get face :background))
                       (plist-get face :background))
                      ((and (symbolp face) (facep face))
                       (face-background face nil t)))))
          (when (and col color)
            (cl-pushnew (cons col color) col-entries :test #'equal))
          (when (and row color)
            (cl-pushnew (cons row color) row-entries :test #'equal)))))
    (when (or col-entries row-entries)
      (save-excursion
        (goto-char begin)
        (let ((context (org-table-highlight--table-context)))
          `(,context
            :col ,(nreverse col-entries)
            :row ,(nreverse row-entries)))))))

(defun org-table-highlight--collect-buffer-metadata ()
  "Collect highlight metadata from all tables in the current buffer.

Returns a list of entries of the form:
  ((:name NAME :before-string STR :after-string STR)
   :col ((N . COLOR)) :row ((N . COLOR)))."
  (interactive)
  (when (derived-mode-p 'org-mode)
   (let* ((buf-name (buffer-name))
          (buf-entry (assoc buf-name org-table-highlight--metadata))
          (table-entries
           (cl-remove-if-not #'identity
                             (org-element-map (org-element-parse-buffer) 'table
                               #'org-table-highlight--collect-table-metadata))))
     (setcdr buf-entry (list table-entries)))))

(add-hook 'kill-buffer-hook #'(lambda ()
                                (progn
                                  (org-table-highlight--collect-buffer-metadata)
                                  (org-table-highlight-save-metadata))))

(provide 'org-table-highlight)
;;; org-table-highlight.el ends here
