;;; org-table-highlight.el --- Highlight Org table columns and rows -*- lexical-binding: t; -*-

;; Author: Lei Zhe
;; URL: https://github.com/llcc/org-table-highlight
;; Version: 0.4
;; Package-Requires: ((emacs "27.1"))
;; Keywords: org-table, convenience

;;; Commentary:

;; This package provides utilities to highlight columns and rows in Org-mode tables.
;; It supports cycling through color palettes, clearing highlights, and working
;; with both column and row overlays.

;;; Code:

(require 'org-element)
(require 'org-table)

(defgroup org-table-highlight nil
  "Highlight columns and rows in Org tables."
  :group 'org)

(defcustom org-table-highlight-color-palette
  '("#FFE4B5" "#C1FFC1" "#B0E0E6" "#FFB6C1" "#D8BFD8" "#F4A460" "#ADD8E6")
  "List of pastel colors used to highlight Org table columns and rows."
  :type '(repeat color)
  :group 'org-table-highlight)

(defvar org-table-highlight--metadata nil
  "Global metadata for Org table highlights across buffers.

This variable holds persistent information about highlighted rows and
columns in Org-mode tables, so highlights can be restored after buffer
reloads or Emacs restarts.

Structure:

  ((BUFFER-NAME
     (OTHM-TABLE ...)
   )
   ...)

Where:

- BUFFER-NAME is a string.
- Each OTHM-TABLE is a struct of type
  `org-table-highlight--metadata-table', describing highlight state for
  a specific Org table in that buffer.

Each `org-table-highlight--metadata-table' contains:

  - :context — an `org-table-highlight--metadata-context' struct that
    uniquely identifies the table:
      - :name (string or nil): the #+NAME: of the table if available.
      - :before-string: string content before the table begin.
      - :after-string: string content after the table end.

  - :col-highlights — a list of highlighted columns:
      ((COLUMN-INDEX :color COLOR) ...), where:
        - COLUMN-INDEX is an integer (1-based)
        - COLOR is a color string like \"#FFB6C1\"

  - `:row-highlights` — a list of highlighted rows:
      ((ROW-INDEX :color COLOR) ...), where:
        - ROW-INDEX is an integer (1-based)
        - COLOR is a string like \"#ADD8E6\"

Example:

  ((\"notes.org\"
     (#s(org-table-highlight--metadata-table
         :context #s(org-table-highlight--metadata-context
                    :name \"my-table\"
                    :before-string \"Text before the table\"
                    :after-string \"Text after the table\")
         :col-highlights ((2 :color \"#FFB6C1\") (3 :color \"#D8BFD8\"))
         :row-highlights ((1 :color \"#ADD8E6\") (4 :color \"#FFE4B5\")))))
   (\"tasks.org\" ...)
   ...)

This variable is updated when:
- Applying or removing column/row highlights
- Modifying table structure (inserting/deleting/moving rows/columns)
- Collecting metadata on buffer kill

It is saved to disk using `org-table-highlight-save-metadata`
and restored using `org-table-highlight-load-metadata`.")

(cl-defstruct org-table-highlight--metadata-context
  "Represents the context of an Org table within a buffer.
Includes identifying metadata such as a name, and text that occurs
before or after the table to help locate it uniquely."
  name            ; Unique name for the table
  before-string   ; String preceding the Org table
  after-string)   ; String following the Org table

(cl-defstruct org-table-highlight--metadata-table
  "Stores highlight metadata for a single Org table.
Includes the table's context and lists of highlighted columns and rows."
  context         ; An `org-table-highlight--metadata-context'
                  ; instance identifying the table
  col-highlights  ; Alist of (col-index :color COLOR) for highlighted columns
  row-highlights) ; Alist of (row-index :color COLOR) for highlighted rows

(cl-defstruct org-table-highlight--metadata-buffer
  "Top-level structure storing all table highlight metadata for a buffer.
Holds the buffer name and a list of`org-table-highlight--metadata-table'
instances associated with it."
  name            ; Buffer name (string)
  tables)         ; List of `org-table-highlight--metadata-table'
                  ; structs in the buffer

(defun org-table-highlight--metadata--get-buffer (buffer-name)
  "Return the buffer struct whose name matches BUFFER-NAME.

Returns nil if no such buffer is found."
  (cl-find buffer-name org-table-highlight--metadata
           :key #'org-table-highlight--metadata-buffer-name
           :test #'equal))

(defun org-table-highlight--metadata--get-table (buf-meta table-context)
  "Return the table struct from BUF-META whose context matches TABLE-CONTEXT.

Two contexts are considered equal if both their name and before-string
fields are equal.  Returns nil if no matching table is found.

Note: after-string is *not* used for matching."
  (cl-find-if
   (lambda (entry)
     (let ((ctx (org-table-highlight--metadata-table-context entry)))
       (and (equal (org-table-highlight--metadata-context-name ctx)
                   (org-table-highlight--metadata-context-name table-context))
            (equal (org-table-highlight--metadata-context-before-string ctx)
                   (org-table-highlight--metadata-context-before-string table-context)))))
   (org-table-highlight--metadata-buffer-tables buf-meta)))

(defun org-table-highlight--update-metadata
    (buffer-name table-context type index color predicate extend &optional remove)
  "Update highlight metadata for a specific Org table.

This function updates the internal `org-table-highlight--metadata' structure
by either adding or removing a highlight for a specific column or row in a
specific table within a specific buffer.

BUFFER-NAME is the name of the buffer.
TABLE-CONTEXT uniquely identifies the Org table.
TYPE is either \='col or \='row.
INDEX is the column or row number to update.
COLOR is the highlight color string (e.g. \"#FF0000\").
PREDICATE is string used to store a test condition for conditional highlighting.
EXTEND, if non-nil, extend the conditional highlight for whole row or column.
If REMOVE is non-nil, the entry at INDEX is removed; otherwise it's added."
  (let* ((buf-meta
          (or (org-table-highlight--metadata--get-buffer buffer-name)
              (unless remove
                (let ((new (make-org-table-highlight--metadata-buffer
                            :name buffer-name
                            :tables nil)))
                  (push new org-table-highlight--metadata)
                  new))))
         (table-meta
          (or (and buf-meta
                   (org-table-highlight--metadata--get-table buf-meta table-context))
              (unless remove
                (let ((new-entry (make-org-table-highlight--metadata-table
                                  :context table-context
                                  :col-highlights nil
                                  :row-highlights nil)))
                  (push new-entry (org-table-highlight--metadata-buffer-tables buf-meta))
                  new-entry)))))
    (when table-meta
      (let ((setf-fn
             (if (eq type 'col)
                 (lambda (tbl val)
                   (setf (org-table-highlight--metadata-table-col-highlights tbl) val))
               (lambda (tbl val)
                 (setf (org-table-highlight--metadata-table-row-highlights tbl) val)))))
        (if (null index)
            ;; Remove whole table
            (when remove (funcall setf-fn table-meta nil))
          ;; Modify highlights based on TYPE
          (let* ((getf (if (eq type 'col)
                           #'org-table-highlight--metadata-table-col-highlights
                         #'org-table-highlight--metadata-table-row-highlights))
                 (existing (funcall getf table-meta))
                 (filtered (cl-remove-if (lambda (x) (= (car x) index)) existing)))
            (funcall setf-fn table-meta
                     (if remove
                         filtered
                       (let ((indice (list index :color color)))
                         (when predicate
                           (setq indice (cons index (plist-put (cdr indice) :predicate predicate)))
                           (when extend
                             (setq indice (cons index (plist-put (cdr indice) :extend t)))))
                         (cons indice filtered)))))))

      ;; Clean up table if both highlights are empty
      (when (and (null (org-table-highlight--metadata-table-col-highlights table-meta))
                 (null (org-table-highlight--metadata-table-row-highlights table-meta)))
        (setf (org-table-highlight--metadata-buffer-tables buf-meta)
              (delete table-meta (org-table-highlight--metadata-buffer-tables buf-meta)))))

    ;; Clean up buffer if empty
    (when (and buf-meta
               (null (org-table-highlight--metadata-buffer-tables buf-meta)))
      (setq org-table-highlight--metadata
            (delete buf-meta org-table-highlight--metadata)))

    (org-table-highlight-save-metadata)))

(defun org-table-highlight--table-bounds ()
  "Return the (START . END) buffer positions of the current Org table."
  (when (org-at-table-p)
    (cons (save-excursion (org-table-begin))
          (save-excursion (org-table-end)))))

(defun org-table-highlight--next-color (counter)
  "Return the next color from the palette using COUNTER."
  (nth (mod counter (length org-table-highlight-color-palette))
       org-table-highlight-color-palette))

(defun org-table-highlight--make-overlay (start end face &rest properties)
  "Create an overlay from START to END with FACE and extra overlay PROPERTIES.

FACE is applied to the \='face property.
PROPERTIES is a plist of additional overlay properties like :symbol value."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'evaporate t)
    (while properties
      (let ((prop (pop properties))
            (val  (pop properties)))
        (overlay-put ov prop val)))
    ov))

(defun org-table-highlight--remove-overlays (start end prop &optional value)
  "Delete overlays between START and END that have PROP (and optionally VALUE)."
  (dolist (ov (overlays-in start end))
    (when (and (overlay-get ov prop)
               (or (not value) (equal (overlay-get ov prop) value)))
      (delete-overlay ov))))

(defun org-table-highlight--get-table-name ()
  "Try to get the Org table name via #+NAME."
  (when-let* ((table (org-element-lineage
                      (org-element-context) 'table t)))
    (plist-get (cadr table) :name)))

(defun org-table-highlight--overlayp (prop &optional value)
  "Return non-nil if an overlay with PROP (and optional VALUE) exists at point.

If VALUE is non-nil, only return true if PROP equals VALUE."
  (cl-some (lambda (ov)
             (let ((ov-val (overlay-get ov prop)))
               (and ov-val (or (not value) (equal ov-val value)))))
           (overlays-at (point))))

(defcustom org-table-highlight-table-context-length 20
  "Number of characters before and after an Org table to save as context.

This context helps identify the table uniquely when it lacks a #+NAME:
property.  It is used to match and restore highlights across sessions by
storing a short prefix and suffix string around the table position."
  :type 'integer
  :group 'org-table-highlight)

(defun org-table-highlight--table-context ()
  "Return contextual metadata for the Org table at point.

This includes the table's name (if any), a short string before the table,
and a short string after it, used to help identify the table if it has
no #+NAME:.  The length of these strings is controlled by
`org-table-highlight-table-context-length'."
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
        (make-org-table-highlight--metadata-context
         :name table-name
         :before-string before-string
         :after-string after-string)))))

(defun org-table-highlight--parse-comparator (expr)
  "Convert a comparator EXPR like \">100\" or \"=TODO\" to a comparison form.
Supports numeric and string values."
  (let* ((re "^\\(<=\\|>=\\|<\\|>\\|=\\|!=\\|/=\\)\\s-*\\(.+\\)$")
         (match (string-match re expr)))
    (if match
        (let* ((op (match-string 1 expr))
               (val-str (match-string 2 expr))
               (op-symbol (if (string= op "!=") '/= (intern op)))
               (is-num (string-match-p "\\`[0-9.]+\\'" val-str))
               (val-expr (if is-num
                             `(string-to-number val)
                           `val))
               (comp-val (if is-num
                             (string-to-number val-str)
                           val-str)))
          (cond
           ((member op-symbol '(= /=))
            ;; String equality/inequality or numeric
            (if is-num
                ;; numeric comparison
                `(,op-symbol ,val-expr ,comp-val)
              ;; string equality/inequality
              (if (eq op-symbol '=)
                  `(string= val ,comp-val)
                `(not (string= val ,comp-val)))))
           (t
            ;; For <, >, <=, >= only numeric supported
            (if is-num
                `(,op-symbol ,val-expr ,comp-val)
              (error "Operator %s not supported for non-numeric value %s" op val-str)))))
      (error "Invalid comparator expression: %s" expr))))

(defun org-table-highlight--parse-and-expr (expr)
  "Parse a subexpression EXPR with and logic."
  (let* ((parts (split-string expr "\\s-+and\\s-+"))
         (conditions (mapcar #'org-table-highlight--parse-comparator parts)))
    `(and ,@conditions)))

(defun org-table-highlight--parse-comparison (expr)
  "Parse expressions EXPR with `and` and `or`, like \">10 and <100 or =TODO\".
Returns a lambda that takes a string VAL."
  (let* ((or-parts (split-string expr "\\s-+or\\s-+"))
         (and-forms (mapcar #'org-table-highlight--parse-and-expr or-parts)))
    `(lambda (val) (or ,@and-forms))))

;;;###autoload
(defun org-table-highlight-column (&optional color predicate extend)
  "Highlight the current Org table column with a cycling or user-supplied COLOR.

With a prefix argument (\\[universal-argument]), prompt for a color.
With a double prefix argument, prompt for a conditional PREDICATE.
With a triple prefix argument, also EXTEND the highlight to the whole row."
  (interactive
   (list
    (when current-prefix-arg (read-color "Column color: " t))
    (when (member current-prefix-arg '((16) (64))) (read-string "Predicate expr (val): "))
    (when (equal current-prefix-arg '(64)) t)))
  (when (and (org-at-table-p)
             (not (org-table-highlight--overlayp 'org-table-highlight-column)))
    (let* ((buf-name (buffer-name))
           (table-context (org-table-highlight--table-context))
           (table-meta
            (when-let* ((buf-meta (org-table-highlight--metadata--get-buffer buf-name)))
              (org-table-highlight--metadata--get-table buf-meta table-context)))
           (highlighted-columns-count
            (if table-meta
                (length (org-table-highlight--metadata-table-col-highlights table-meta))
              0))
           (col (org-table-current-column))
           (chosen-color (or color (org-table-highlight--next-color
                                    highlighted-columns-count)))
           (bounds (org-table-highlight--table-bounds)))
      (cl-incf org-table-highlight--highlighted-columns)
      (org-table-highlight--update-metadata
       buf-name table-context 'col col chosen-color predicate extend)
      (save-restriction
        (narrow-to-region (car bounds) (cdr bounds))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((beg (point)) (end (line-end-position)) (i 0))
              (while (and (< i col)
                          (re-search-forward
                           (if (org-at-table-hline-p) "[|\\+]" "|")
                           end t))
                (setq beg (point))
                (setq i (1+ i)))
              (setq end (progn (skip-chars-forward (if (org-at-table-hline-p) "-" "^|"))
                               (point)))
              (when (or (null predicate)
                        (funcall (org-table-highlight--parse-comparison predicate)
                                 (string-trim (buffer-substring-no-properties beg end))))
                (if extend
                    (org-table-highlight--make-overlay
                     (save-excursion
                       (goto-char (line-beginning-position))
                       (back-to-indentation)
                       (point))
                     (save-excursion
                       (goto-char (line-end-position))
                       (skip-chars-backward "^|")
                       (point))
                     `(:background ,chosen-color)
                     'org-table-highlight-column col :predicate predicate :extend t)
                  (org-table-highlight--make-overlay
                   beg end `(:background ,chosen-color)
                   'org-table-highlight-column col :predicate predicate))))
            (forward-line 1)))))))

;;;###autoload
(defun org-table-highlight-row (&optional color)
  "Highlight the current Org table row with a cycling or user-supplied COLOR.
With a prefix argument (\\[universal-argument]), prompt for a color."
  (interactive
   (list (when current-prefix-arg (read-color "Row color: " t))))
  (when (and (org-at-table-p)
             (not (org-table-highlight--overlayp 'org-table-highlight-row)))
    (let* ((buf-name (buffer-name))
           (table-context (org-table-highlight--table-context))
           (table-meta
            (when-let* ((buf-meta (org-table-highlight--metadata--get-buffer buf-name)))
              (org-table-highlight--metadata--get-table buf-meta table-context)))
           (highlighted-rows-count
            (if table-meta
                (length (org-table-highlight--metadata-table-row-highlights table-meta))
              0))
           (row (org-table-current-line))
           (chosen-color (or color (org-table-highlight--next-color highlighted-rows-count)))
           (start (save-excursion
                    (goto-char (line-beginning-position))
                    (back-to-indentation)
                    (point)))
           (end (save-excursion
                  (goto-char (line-end-position))
                  (skip-chars-backward "^|")
                  (point))))
      (cl-incf org-table-highlight--highlighted-rows)
      (org-table-highlight--update-metadata buf-name table-context 'row row chosen-color nil nil)
      (unless (org-table-highlight--overlayp 'org-table-highlight-row)
        (org-table-highlight--make-overlay start end `(:background ,chosen-color)
                                           'org-table-highlight-row row)))))

(defun org-table-highlight-restore ()
  "Restore highlights for the Org table at point using stored metadata."
  (interactive)
  (when (org-at-table-p)
    (when-let* ((buffer-name (buffer-name))
                (table-context (org-table-highlight--table-context))
                (buf-meta (org-table-highlight--metadata--get-buffer buffer-name))
                (table-meta (org-table-highlight--metadata--get-table buf-meta table-context)))

      ;; Reapply column highlights
      (dolist (col-entry (org-table-highlight--metadata-table-col-highlights table-meta))
        (let ((col (car col-entry))
              (color (plist-get (cdr col-entry) :color))
              (predicate (plist-get (cdr col-entry) :predicate))
              (extend (plist-get (cdr col-entry) :extend)))
          (save-excursion
            (org-table-goto-column col)
            (org-table-highlight-column color predicate extend))))

      ;; Reapply row highlights
      (dolist (row-entry (org-table-highlight--metadata-table-row-highlights table-meta))
        (let ((row (car row-entry))
              (color (plist-get (cdr row-entry) :color)))
          (save-excursion
            (goto-char (org-table-begin))
            (org-table-goto-line row)
            (org-table-highlight-row color)))))))

;;;###autoload
(defun org-table-highlight-clear-column-highlights (&optional all)
  "Clear highlights in current Org table column.
With prefix argument ALL, clear all column highlights."
  (interactive "P")
  (when (or all (org-table-highlight--overlayp 'org-table-highlight-column))
    (when-let* ((buf-name (buffer-name))
                (table-context (org-table-highlight--table-context))
                (bounds (org-table-highlight--table-bounds)))
      (let ((col (if all nil (org-table-current-column))))
        (org-table-highlight--update-metadata
         buf-name table-context 'col col nil nil nil 'remove)
        (org-table-highlight--remove-overlays
         (car bounds) (cdr bounds)
         'org-table-highlight-column col)))))

;;;###autoload
(defun org-table-highlight-clear-row-highlights (&optional all)
  "Clear highlights in current Org table row.
With prefix argument ALL, clear all row highlights."
  (interactive "P")
  (when (or all (org-table-highlight--overlayp 'org-table-highlight-row))
    (when-let* ((buf-name (buffer-name))
                (table-context (org-table-highlight--table-context))
                (bounds (org-table-highlight--table-bounds)))
      (let ((row (if all nil (org-table-current-line))))
        (org-table-highlight--update-metadata
         buf-name table-context 'row row nil nil nil 'remove)
        (org-table-highlight--remove-overlays
         (car bounds) (cdr bounds) 'org-table-highlight-row row)))))

;;;###autoload
(defun org-table-highlight-clear-all-highlights (&optional keep-metadata)
  "Clear all column and row highlights in current Org table.

Keep metadata if KEEP-METADATA non-nils."
  (interactive)
  (let ((bounds (org-table-highlight--table-bounds)))
    (org-table-highlight--remove-overlays
     (car bounds) (cdr bounds) 'org-table-highlight-column)
    (org-table-highlight--remove-overlays
     (car bounds) (cdr bounds) 'org-table-highlight-row))
  (when (null keep-metadata)
    (when-let* ((buf-name (buffer-name))
               (table-context (org-table-highlight--table-context)))
      (org-table-highlight--update-metadata
       buf-name table-context 'col nil nil nil nil 'remove)
      (org-table-highlight--update-metadata
       buf-name table-context 'row nil nil nil nil 'remove))))

(defun org-table-highlight--remove-plist-key (plist key)
  "Return a copy of PLIST with KEY and its value removed."
  (let (new-plist)
    (while plist
      (let ((k (pop plist))
            (v (pop plist)))
        (unless (eq k key)
          (setq new-plist (plist-put new-plist k v)))))
    new-plist))

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
        (pp org-table-highlight--metadata (current-buffer))
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
  "Get position of table beginning position based on CONTEXT."
  (let ((name (org-table-highlight--metadata-context-name context))
        (before-string (org-table-highlight--metadata-context-before-string context))
        (after-string (org-table-highlight--metadata-context-after-string context))
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
  (when-let* ((buf-meta (org-table-highlight--metadata--get-buffer (buffer-name))))
    (dolist (table-meta (org-table-highlight--metadata-buffer-tables buf-meta))
      (let* ((table-context (org-table-highlight--metadata-table-context table-meta))
             (pos (org-table-highlight--get-table-position table-context)))
        (save-excursion
          (when (and pos (goto-char pos))
            ;; Apply columns
            (dolist (col-entry (org-table-highlight--metadata-table-col-highlights table-meta))
              (let ((col (car col-entry))
                    (color (plist-get (cdr col-entry) :color))
                    (predicate (plist-get (cdr col-entry) :predicate)))
                (org-table-goto-column col predicate)
                (org-table-highlight-column color)))

            ;; Apply rows
            (dolist (row-entry (org-table-highlight--metadata-table-row-highlights table-meta))
              (let ((row (car row-entry))
                    (color (plist-get (cdr row-entry) :color)))
                (goto-char (org-table-begin))
                (forward-line (1- row))
                (org-table-highlight-row color)))))))))

(defun org-table-highlight--collect-table-metadata (tbl)
  "Collect highlight metadata from TBL (an `org-element' table).

Returns a metadata entry of the form:
  ((:name NAME :before-string STR :after-string STR)
   :col ((N :color COLOR :predicate PREDICATE :extend t))
   :row ((N :color COLOR :predicate PREDICATE)))
or nil if there are no highlight overlays."
  (let* ((begin (org-element-property :contents-begin tbl))
         (end (org-element-property :contents-end tbl))
         (overlays (overlays-in begin end))
         (col-highlights '())
         (row-highlights '()))
    (when overlays
      (dolist (ov overlays)
        (let ((predicate (overlay-get ov :predicate))
              (extend (overlay-get ov :extend))
              (color (plist-get (overlay-get ov 'face) :background)))
          (when-let* ((col (overlay-get ov 'org-table-highlight-column))
                      (indice (list col :color color)))
            (when predicate
              (setq indice (cons col (plist-put (cdr indice) :predicate predicate)))
              (when extend
                (setq indice (cons col (plist-put (cdr indice) :extend t)))))
            (cl-pushnew indice col-highlights :test #'equal))
          (when-let* ((row (overlay-get ov 'org-table-highlight-row))
                      (indice (list row :color color)))
            (when predicate
              (setq indice (cons row (plist-put (cdr indice) :predicate predicate)))
              (when extend
                (setq indice (cons row (plist-put (cdr indice) :extend t)))))
            (cl-pushnew indice row-highlights :test #'equal))))
      (when (or col-highlights row-highlights)
        (save-excursion
          (goto-char begin)
          (let ((context (org-table-highlight--table-context)))
            (make-org-table-highlight--metadata-table
             :context context
             :col-highlights (nreverse col-highlights)
             :row-highlights (nreverse row-highlights))))))))

(defun org-table-highlight--collect-buffer-metadata ()
  "Collect highlight metadata from all tables in the current buffer.

Returns a list of entries of the form:
  ((:name NAME :before-string STR :after-string STR)
   :col ((N . COLOR)) :row ((N . COLOR)))."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let* ((buf-meta (org-table-highlight--metadata--get-buffer (buffer-name)))
           (table-meta
            (cl-remove-if-not #'identity
                              (org-element-map (org-element-parse-buffer) 'table
                                #'org-table-highlight--collect-table-metadata))))
      (when buf-meta
        (setf (org-table-highlight--metadata-buffer-tables buf-meta) table-meta)

        (when (null (org-table-highlight--metadata-buffer-tables buf-meta))
          (setq org-table-highlight--metadata
                (delete buf-meta org-table-highlight--metadata)))

        (org-table-highlight-save-metadata)))))

(defun org-table-highlight--fix-indice-1 (index ref-index handle entry table-meta)
  "Adjust a highlight ENTRY in TABLE-META depending on HANDLE and position.

Arguments:
- INDEX: The index of the highlight (i.e., (car ENTRY)).
- REF-INDEX: The index at which the table changed (insert/delete/move).
- HANDLE: One of:
  - insert / above → Inserted at REF-INDEX: increment if INDEX >= REF-INDEX.
  - delete / delete-row / delete-column:
      → Remove if INDEX == REF-INDEX.
      → Decrement if INDEX > REF-INDEX.
  - left / up → Shift left if after REF-INDEX, right if at REF-INDEX.
  - right / down → Shift right if before REF-INDEX, left if at REF-INDEX.

Returns non-nil if ENTRY remains, or nil if removed.
Also cleans up empty highlight lists inside TABLE-META."
  (pcase handle
    ;; Insertion shifts highlights at or after insertion index
    ((or 'insert 'above)
     (when (>= index ref-index)
       (setcar entry (1+ index))))

    ;; Deletion may remove or shift
    ((or 'delete-row 'delete-column)
     (cond
      ((= index ref-index)
       ;; Remove ENTRY from the correct list
       (pcase handle
         ('delete-row
          (setf (org-table-highlight--metadata-table-row-highlights table-meta)
                (cl-remove-if (lambda (r) (= (car r) index))
                              (org-table-highlight--metadata-table-row-highlights table-meta))))
         ('delete-column
          (setf (org-table-highlight--metadata-table-col-highlights table-meta)
                (cl-remove-if (lambda (c) (= (car c) index))
                              (org-table-highlight--metadata-table-col-highlights table-meta))))))
      ((> index ref-index)
       (setcar entry (1- index)))))

    ;; Reordering (left/up/down/right)
    ((or 'left 'up)
     (cond
      ((= index ref-index)
       (setcar entry (1+ index)))
      ((= index (1+ ref-index))
       (setcar entry (1- index)))))

    ((or 'right 'down)
     (cond
      ((= index ref-index)
       (setcar entry (1- index)))
      ((= index (1- ref-index))
       (setcar entry (1+ index))))))

  ;; Cleanup if table-meta has no highlights left
  (when (and (null (org-table-highlight--metadata-table-row-highlights table-meta))
             (null (org-table-highlight--metadata-table-col-highlights table-meta)))
    ;; Upstream should remove table-meta from buffer
    (message "Highlight table is now empty — should be removed upstream.")))

(defun org-table-highlight--fix-indice (handle)
  "Update highlight metadata after a column or row is inserted or deleted.

HANDLE must be either \='insert or \='delete.  This function adjusts the
metadata for the current Org table in `org-table-highlight--metadata' to
reflect changes caused by the insertion or deletion of a column or row at point.

It does the following:
1. Finds the column and row index at point.
2. Locates the corresponding highlight metadata entry using table context (such
   as name and nearby content).
3. Adjusts all metadata entries (i.e., highlighted columns and rows) that occur
   after the insertion/deletion point, shifting their indices accordingly or
   removing entries that are deleted.
4. Clears all overlays (without clearing metadata), then restores them based on
   the updated metadata.
5. Updates the metadata in `org-table-highlight--metadata' accordingly.

This function is intended to be called after structural edits (e.g., with
`org-table-insert-column', `org-table-delete-row', etc.)."
  (save-excursion
    (when-let* ((buf-meta (org-table-highlight--metadata--get-buffer (buffer-name)))
                (table-context (org-table-highlight--table-context))
                (table-meta (org-table-highlight--metadata--get-table buf-meta table-context)))

      (unless (member handle '(up down below above delete-row)) ;; ignore row editing operations
        (when-let* ((ref-column (org-table-current-column))
                    (col-highlights (org-table-highlight--metadata-table-col-highlights table-meta)))
          (dolist (col-highlight col-highlights)
            (let ((col (car col-highlight)))
              (org-table-highlight--fix-indice-1 col ref-column handle col-highlight table-meta)))))

      (unless (member handle '(left right delete-column)) ;; ignore column editing operations
        (when-let* ((ref-row (org-table-current-line))
                    (row-highlights (org-table-highlight--metadata-table-row-highlights table-meta)))
          (dolist (row-highlight row-highlights)
            (let ((row (car row-highlight)))
              (org-table-highlight--fix-indice-1 row ref-row handle row-highlight table-meta)))))

      (org-table-highlight-clear-all-highlights 'keep-metadata)
      (org-table-highlight-restore)
      (org-table-highlight--collect-buffer-metadata))))

(defun org-table-highlight-clear-buffer-overlays ()
  "Clear all Org table highlight overlays in the current buffer.

This removes both column and row highlights overlays across the entire
buffer, regardless of table context."
  (interactive)
  (org-table-highlight--remove-overlays
   (point-min) (point-max) 'org-table-highlight-column)
  (org-table-highlight--remove-overlays
   (point-min) (point-max) 'org-table-highlight-row)
  (message "All Org table highlight overlays removed from buffer."))

;;;###autoload
(defun org-table-highlight-list-all (&optional buffers-to-process)
  "List highlighted Org tables.

BUFFERS-TO-PROCESS is the list of buffer to display the highlight.
Behavior depends on the prefix argument (\\[universal-argument]):
- No prefix: List tables in the current buffer.
- One prefix: Prompt for a buffer to list.
- Two prefixes: List tables from all buffers with known highlight metadata."
  (interactive
   (let ((all-buffers-with-meta
          (cl-loop for b-meta in org-table-highlight--metadata
                   collect (get-buffer (org-table-highlight--metadata-buffer-name b-meta)))))
     (list
      (pcase current-prefix-arg
        ;; C-u C-u: Use all buffers
        ('(16) all-buffers-with-meta)

        ;; C-u: Prompt for one buffer
        ('(4) (if all-buffers-with-meta
                  (list (get-buffer
                         (completing-read "List highlights for buffer: "
                                          (mapcar #'buffer-name all-buffers-with-meta)
                                          nil t)))
                (progn (message "No highlight metadata found.") nil)))

        ;; No prefix: Use current buffer
        (_ (list (current-buffer)))))))

  (when buffers-to-process
    (with-help-window (get-buffer-create "*Org Table Highlights*")
      (let ((first-buffer t))
        (dolist (buffer buffers-to-process)
          ;; Print a header if processing multiple buffers
          (if first-buffer
              (setq first-buffer nil)
            (princ "\n\n"))
          (princ (format "--- Highlights in %s --\n\n" (buffer-name buffer)))
          (if-let* ((buffer-name (buffer-name buffer))
                    (buf-meta (org-table-highlight--metadata--get-buffer buffer-name))
                    (tables (org-table-highlight--metadata-buffer-tables buf-meta)))
              (dolist (table-meta tables)
                (let* ((context (org-table-highlight--metadata-table-context table-meta))
                       (name (org-table-highlight--metadata-context-name context))
                       (pos (with-current-buffer buffer
                              (org-table-highlight--get-table-position context)))
                       (num-cols (length (org-table-highlight--metadata-table-col-highlights table-meta)))
                       (num-rows (length (org-table-highlight--metadata-table-row-highlights table-meta))))
                  (princ (format "- Table %s" (or name "(unnamed)")))
                  (when pos
                    (insert-button
                     " [jump]"
                     'action `(lambda (_)
                                (pop-to-buffer ',buffer)
                                (goto-char ,pos))
                     'follow-link t))
                  (princ (format " (%d columns, %d rows highlighted)\n" num-cols num-rows))))
            ;; Message if a specific buffer in the list had no highlights
            (unless (org-table-highlight--metadata--get-buffer (buffer-name buffer))
              (princ (format "(No highlights found in %s)\n" (buffer-name buffer))))))))))

;;;###autoload
(define-minor-mode org-table-highlight-mode
  "Minor mode to enable or disable Org table highlighting.

When enabled:
- Highlights are automatically restored after table alignments or movements.
- Metadata is maintained and saved on buffer close.

When disabled:
- All highlights (overlays) in the current buffer are removed.
- All metadata for this buffer is cleared.
- Advices and hooks are disabled."
  :lighter " OrgTblHL"
  :group 'org-table-highlight
  (if org-table-highlight-mode
      (progn
        (advice-add 'org-table-align :after #'org-table-highlight-restore)
        (advice-add 'org-table-next-field :after #'org-table-highlight-restore)

        (advice-add 'org-table-insert-column :after
                    #'(lambda () (org-table-highlight--fix-indice 'insert)))
        (advice-add 'org-table-delete-column :after
                    #'(lambda () (org-table-highlight--fix-indice 'delete-column)))
        (advice-add 'org-table-move-column :after
                    #'(lambda (&optional move)
                        (org-table-highlight--fix-indice (or move 'right))))

        (advice-add 'org-table-insert-row :after
                    #'(lambda (&optional arg)
                        (org-table-highlight--fix-indice (if arg 'below 'above))))
        (advice-add 'org-table-kill-row :after
                    #'(lambda () (org-table-highlight--fix-indice 'delete-row)))
        (advice-add 'org-table-move-row :after
                    #'(lambda (&optional move)
                        (org-table-highlight--fix-indice (or move 'down))))

        (add-hook 'kill-buffer-hook #'org-table-highlight--collect-buffer-metadata nil t)

        ;; Restore highlights if metadata exists
        (org-table-highlight-apply-buffer-metadata)
        (message "org-table-highlight-mode enabled."))

    (progn
      ;; Remove ALL highlights in the buffer (overlays and metadata)
      (when (derived-mode-p 'org-mode)
        (org-table-highlight-clear-buffer-overlays))

      ;; Remove advices
      (advice-remove 'org-table-align #'org-table-highlight-restore)
      (advice-remove 'org-table-next-field #'org-table-highlight-restore)

      (advice-remove 'org-table-insert-column
                     #'(lambda () (org-table-highlight--fix-indice 'insert)))
      (advice-remove 'org-table-delete-column
                     #'(lambda () (org-table-highlight--fix-indice 'delete-column)))
      (advice-remove 'org-table-move-column
                     #'(lambda (&optional move)
                         (org-table-highlight--fix-indice (or move 'right))))

      (advice-remove 'org-table-insert-row
                     #'(lambda (&optional arg)
                         (org-table-highlight--fix-indice (if arg 'below 'above))))
      (advice-remove 'org-table-kill-row
                     #'(lambda () (org-table-highlight--fix-indice 'delete-row)))
      (advice-remove 'org-table-move-row
                     #'(lambda (&optional move)
                         (org-table-highlight--fix-indice (or move 'down))))

      (remove-hook 'kill-buffer-hook #'org-table-highlight--collect-buffer-metadata t)

      (message "org-table-highlight-mode disabled: all highlights and metadata cleared, while metadata remains uncleared.."))))

(provide 'org-table-highlight)
;;; org-table-highlight.el ends here
