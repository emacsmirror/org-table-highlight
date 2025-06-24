;;; org-table-highlight.el --- Highlight Org table columns and rows -*- lexical-binding: t; -*-

;; Author: Lei Zhe
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines, tables, convenience

;;; Commentary:

;; This package provides utilities to highlight columns and rows in Org-mode tables.
;; It supports cycling through color palettes, clearing highlights, and working
;; with both column and row overlays.

;;; Code:

(defcustom org-table-highlight-color-palette
  '("#FFE4B5" "#C1FFC1" "#B0E0E6" "#FFB6C1" "#D8BFD8" "#F4A460" "#ADD8E6")
  "List of pastel colors used to highlight Org table columns and rows.")

(defvar-local org-table-highlighted-columns 0
  "Number of Org table columns currently highlighted.")

(defvar-local org-table-highlighted-rows 0
  "Number of Org table rows currently highlighted.")

(defvar org-table-highlight--metadata nil
  "Metadata for Org table highlights.

Format is:
  ((BUFFER-NAME
     ((TABLE-NAME :col ((COL COLOR) ...) :row ((ROW COLOR) ...)))
   ...))")

;; Internal helper functions
(defun org-table-highlight--table-bounds ()
  "Return the (START . END) buffer positions of the current Org table."
  (when (org-at-table-p)
    (cons (save-excursion (org-table-begin))
          (save-excursion (org-table-end)))))

(defun org-table-highlight--next-color (counter)
  "Return the next color from the palette using COUNTER."
  (nth (mod counter (length org-table-highlight-color-palette))
       org-table-highlight-color-palette))

(defun org-table-highlight--make-overlay (start end face-prop symbol val)
  "Create an overlay from START to END with FACE-PROP, and SYMBOL set to VAL."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face face-prop)
    (overlay-put ov symbol val)
    ov))

(defun org-table-highlight--remove-overlays (start end prop &optional value)
  "Efficiently remove overlays with PROP (and VALUE) between START and END."
  (save-restriction
    (narrow-to-region start end)
    (remove-overlays (point-min) (point-max)
                     prop (when value value))))

(defun org-table-highlight--get-table-name ()
  "Try to get the Org table name via #+NAME."
  (when-let (table (org-element-lineage
                    (org-element-context) 'table t))
    (plist-get (cadr table) :name)))

(defun org-table-highlight-column (&optional color)
  "Highlight the current Org table column with a cycling or user-supplied COLOR.
With \\[universal-argument] prefix, prompt for color."
  (interactive
   (list (when current-prefix-arg (read-color "Column color: " t))))
  (when (org-at-table-p)
    (let* ((buf-name (buffer-name))
           (table-name (org-table-highlight--get-table-name))
           (col (org-table-current-column))
           (chosen-color (or color
                             (org-table-highlight--next-color org-table-highlighted-columns)))
           (bounds (org-table-highlight--table-bounds)))
      (cl-incf org-table-highlighted-columns)
      (if table-name
          (org-table-highlight--update-metadata buf-name table-name :col col chosen-color)
        (message "Consider adding #+NAME: for this table to persist highlights."))
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
                (org-table-highlight--remove-overlays pos (1- (point)) 'org-table-highlight-column col)
                (org-table-highlight--make-overlay pos (point)
                                                   `(:background ,chosen-color)
                                                   'org-table-highlight-column col)))
            (forward-line 1)))))))

(defun org-table-highlight-row (&optional color)
  "Highlight the current Org table row with a cycling or user-supplied COLOR.
With \\[universal-argument] prefix, prompt for color."
  (interactive
   (list (when current-prefix-arg (read-color "Row color: " t))))
  (when (org-at-table-p)
    (let* ((buf-name (buffer-name))
           (table-name (org-table-highlight--get-table-name))
           (row (org-table-current-line))
           (chosen-color (or color
                             (org-table-highlight--next-color org-table-highlighted-rows)))
           (start (line-beginning-position))
           (end (line-end-position)))
      (cl-incf org-table-highlighted-rows)
      (if table-name
          (org-table-highlight--update-metadata buf-name table-name :row row chosen-color)
        (message "Consider adding #+NAME: for this table to persist highlights."))
      (org-table-highlight--remove-overlays start end 'org-table-highlight-row)
      (org-table-highlight--make-overlay start end `(:background ,chosen-color)
                                         'org-table-highlight-row row))))

(defun org-table-highlight-restore ()
  "Restore highlights for the Org table at point using stored metadata."
  (interactive)
  (when (org-at-table-p)
    (when-let* ((buf-name (buffer-name))
                (table-name (org-table-highlight--get-table-name))
                (buf-entry (assoc buf-name org-table-highlight--metadata)))
      (let* ((table-list (cadr buf-entry))
             (table-entry (assoc table-name table-list)))

        ;; Clear existing highlights in this table first
        (org-table-highlight-clear-all-highlights)

        ;; Reapply column highlights
        (dolist (col-entry (plist-get (cdr table-entry) :col))
          (let ((col (car col-entry))
                (color (cdr col-entry)))
            (save-excursion
              (org-table-goto-column col)
              (org-table-highlight-column color))
            (cl-decf org-table-highlighted-columns)))

        ;; Reapply row highlights
        (dolist (row-entry (plist-get (cdr table-entry) :row))
          (let ((row (car row-entry))
                (color (cdr row-entry)))
            (save-excursion
              (goto-char (org-table-begin))
              (forward-line (1- row))
              (org-table-highlight-row color))
            (cl-decf org-table-highlighted-rows)))))))

(advice-add 'org-table-align :after #'org-table-highlight-restore)
(advice-add 'org-table-next-field :after #'org-table-highlight-restore)

(defun org-table-highlight-clear-column-highlights (&optional all)
  "Clear highlights in current Org table column.
With prefix argument ALL, clear all column highlights."
  (interactive "P")
  (when-let ((buf-name (buffer-name))
             (table-name (org-table-highlight--get-table-name))
             (bounds (org-table-highlight--table-bounds)))
    (let ((col (org-table-current-column)))
      (org-table-highlight--remove-metadata
       buf-name table-name :col (or all col))
      (org-table-highlight--remove-overlays
       (car bounds) (cdr bounds)
       'org-table-highlight-column
       (unless all col)))))

(defun org-table-highlight-clear-row-highlights (&optional all)
  "Clear highlights in current Org table row.
With prefix argument ALL, clear all row highlights."
  (interactive "P")
  (when-let ((buf-name (buffer-name))
             (table-name (org-table-highlight--get-table-name))
             (bounds (org-table-highlight--table-bounds)))
    (let ((row (org-table-current-line)))
      (org-table-highlight--remove-metadata
       buf-name table-name :row (or all row))
      (org-table-highlight--remove-overlays
       (car bounds) (cdr bounds)
       'org-table-highlight-row
       (unless all row)))))

(defun org-table-highlight-clear-all-highlights ()
  "Clear all column and row highlights in current Org table."
  (interactive)
  (when-let ((buf-name (buffer-name))
             (table-name (org-table-highlight--get-table-name))
             (bounds (org-table-highlight--table-bounds)))
    (org-table-highlight--remove-metadata
     buf-name table-name nil)
    (org-table-highlight--remove-overlays
     (car bounds) (cdr bounds) 'org-table-highlight-column)
    (org-table-highlight--remove-overlays
     (car bounds) (cdr bounds) 'org-table-highlight-row)))

(defun org-table-highlight--update-metadata (buf-name table-name type index color)
  "Update highlight metadata for BUF-NAME and TABLE-NAME.
TYPE is :col or :row. INDEX is the column or row number. COLOR is the highlight color."
  (let ((buf-entry (assoc buf-name org-table-highlight--metadata)))
    (if buf-entry
        (let* ((table-list (cadr buf-entry))
               (table-entry (assoc table-name table-list)))
          (if table-entry
              ;; Update existing table entry in place
              (let* ((plist (cdr table-entry))
                     (existing (plist-get plist type))
                     (filtered (cl-remove-if (lambda (item) (= (car item) index)) existing))
                     (newlist (cons (cons index color) filtered))
                     (new-plist (plist-put plist type newlist)))
                ;; Modify plist in place by setting cdr of table-entry
                (setcdr table-entry new-plist))
            ;; Table does not exist, add new entry to table-list in place
            (when table-name
              (setcdr buf-entry (list (append table-list (list (list table-name type (list (cons index color))))))))))
      ;; Buffer does not exist, add new buffer entry
      (push (list buf-name (list (list table-name type (list (cons index color)))))
            org-table-highlight--metadata))
    (org-table-highlight-save-metadata)))

(defcustom org-table-highlight-metadata-file
  (locate-user-emacs-file "org-table-highlight-metadata.el")
  "File where Org table highlight metadata is saved."
  :type 'file)

(defun org-table-highlight-save-metadata ()
  "Save `org-table-highlight--metadata` to `org-table-highlight-metadata-file`."
  (interactive)
  (with-temp-file org-table-highlight-metadata-file
    (insert ";;; org-table-highlight saved metadata\n\n")
    (prin1 `(setq org-table-highlight--metadata
                  ,org-table-highlight--metadata)
           (current-buffer))))

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
  "Remove highlight metadata from `org-table-highlight--metadata`.

- If TYPE and INDEX are non-nil: remove the specific INDEX under TYPE.
- If TYPE is non-nil and INDEX is nil: remove all entries under TYPE.
- If both TYPE and INDEX are nil: remove the entire TABLE-NAME entry from BUF-NAME.
- If BUF-NAME has no tables left, remove BUF-NAME from metadata."
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

        (when (null (cddr table-entry))
          (setcdr buf-entry
                  (list (assoc-delete-all table-name (cadr buf-entry) #'equal)))))

      ;; If buf-entry has no more tables, remove the buffer entirely
      (when (null (cadr buf-entry))
        (setq org-table-highlight--metadata
              (assoc-delete-all buf-name org-table-highlight--metadata #'equal)))))

  (org-table-highlight-save-metadata))

(defun org-table-highlight-apply-buffer-metadata ()
  "Apply highlight metadata to all tables in the current buffer."
  (interactive)
  (let* ((buf-name (buffer-name))
         (buf-entry (assoc buf-name org-table-highlight--metadata)))
    (when buf-entry
      (dolist (table-entry (cadr buf-entry))
        (let ((table-name (car table-entry))
              (plist (cdr table-entry)))
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward (format "#\\+NAME:[ \t]*%s" (regexp-quote table-name)) nil t)
              (forward-line 1)
              (when (org-at-table-p)
                ;; Apply columns
                (dolist (col-entry (plist-get plist :col))
                  (let ((col (car col-entry))
                        (color (cdr col-entry)))
                    (org-table-goto-column col)
                    (org-table-highlight-column color)
                    (cl-decf org-table-highlighted-columns)))
                ;; Apply rows
                (dolist (row-entry (plist-get plist :row))
                  (let ((row (car row-entry))
                        (color (cdr row-entry)))
                    (goto-char (org-table-begin))
                    (forward-line (1- row))
                    (org-table-highlight-row color)
                    (cl-decf org-table-highlighted-rows)))))))))))

(add-hook 'after-init-hook #'org-table-highlight-load-metadata)
(add-hook 'org-mode-hook #'org-table-highlight-apply-buffer-metadata)

(provide 'org-table-highlight)
;;; org-table-highlight.el ends here
