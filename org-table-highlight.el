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

(defvar org-table-highlight-color-palette
  '("#FFE4B5" "#C1FFC1" "#B0E0E6" "#FFB6C1" "#D8BFD8" "#F4A460" "#ADD8E6")
  "List of pastel colors used to highlight Org table columns and rows.")

(defvar-local org-table-highlighted-columns 0
  "Number of Org table columns currently highlighted.")

(defvar-local org-table-highlighted-rows 0
  "Number of Org table rows currently highlighted.")

;; Internal helper functions
(defun org-table-highlight--bounds ()
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
  "Remove overlays from START to END that have PROP.
If VALUE is non-nil, only remove overlays where PROP equals VALUE."
  (dolist (ov (overlays-in start end))
    (when (and (overlay-get ov prop)
               (or (not value)
                   (equal (overlay-get ov prop) value)))
      (delete-overlay ov))))

;; Public interactive functions
(defun org-table-highlight-column ()
  "Highlight the current Org table column with a cycling background color."
  (interactive)
  (when (org-at-table-p)
    (global-hl-line-mode -1)
    (let* ((col (org-table-current-column))
           (color (org-table-highlight--next-color org-table-highlighted-columns))
           (end (save-excursion (org-table-end))))
      (cl-incf org-table-highlighted-columns)
      (save-excursion
        (goto-char (org-table-begin))
        (while (< (point) end)
          (let ((line-end (line-end-position))
                (pos (line-beginning-position))
                (i 0))
            (while (and (< i col)
                        (re-search-forward "[|\\|+]" line-end t))
              (setq pos (point))
              (setq i (1+ i)))
            (when (re-search-forward "[|\\|+]" line-end t)
              (org-table-highlight--remove-overlays pos (1- (point)) 'org-table-highlight-column)
              (org-table-highlight--make-overlay pos (1- (point))
                                       `(:background ,color)
                                       'org-table-highlight-column col)))
          (forward-line 1))))
    (global-hl-line-mode 1)))

(defun org-table-highlight-clear-column-highlights (&optional all)
  "Clear highlights in current Org table column.
With prefix argument ALL, clear all column highlights."
  (interactive "P")
  (when-let ((bounds (org-table-highlight--bounds)))
    (let ((col (org-table-current-column)))
      (org-table-highlight--remove-overlays
       (car bounds) (cdr bounds)
       'org-table-highlight-column
       (unless all col)))))

(defun org-table-highlight-row ()
  "Highlight the current Org table row with a cycling background color."
  (interactive)
  (when (org-at-table-p)
    (let* ((start (line-beginning-position))
           (end (line-end-position))
           (row (org-table-current-line))
           (color (org-table-highlight--next-color org-table-highlighted-rows)))
      (cl-incf org-table-highlighted-rows)
      (org-table-highlight--remove-overlays start end 'org-table-highlight-row)
      (org-table-highlight--make-overlay start end `(:background ,color)
                                      'org-table-highlight-row row))))

(defun org-table-highlight-clear-row-highlights (&optional all)
  "Clear highlights in current Org table row.
With prefix argument ALL, clear all row highlights."
  (interactive "P")
  (when-let ((bounds (org-table-highlight--bounds)))
    (let ((row (org-table-current-line)))
      (org-table-highlight--remove-overlays
       (car bounds) (cdr bounds)
       'org-table-highlight-row
       (unless all row)))))

(defun org-table-highlight-clear-all-highlights ()
  "Clear all column and row highlights in current Org table."
  (interactive)
  (when-let ((bounds (org-table-highlight--bounds)))
    (org-table-highlight--remove-overlays (car bounds) (cdr bounds) 'org-table-highlight-column)
    (org-table-highlight--remove-overlays (car bounds) (cdr bounds) 'org-table-highlight-row)
    (setq org-table-highlighted-columns 0)
    (setq org-table-highlighted-rows 0)))

(provide 'org-table-highlight)
;;; org-table-highlight.el ends here
