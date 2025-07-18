(require 'ert)
(require 'org)
(require 'org-table-highlight)

(defmacro org-table-highlight--with-temp-buffer (&rest body)
  "Create a temporary org-mode buffer and execute BODY."
  `(with-temp-buffer
     (org-mode)
     (insert "#+name: highlight-table\n")
     (insert "| A | B |\n|---+---|\n| 1 | 2 |\n| 3 | 4 |")
     (goto-char (org-table-begin))
     (org-table-align)
     ,@body))

(defmacro org-table-highlight--with-test-env (&rest body)
  "Isolate metadata and run BODY in a clean testing environment."
  `(let ((org-table-highlight-metadata-file
          (make-temp-file "org-table-highlight-test-metadata.el"))
         (org-table-highlight--metadata nil)
         (org-table-highlight--table-context 5)) ; reset global var
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p org-table-highlight-metadata-file)
         (delete-file org-table-highlight-metadata-file)))))

(ert-deftest org-table-highlight--test-highlight-column ()
  "Test column highlighting and overlay detection."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    ;; 1. Test highlighting conditionally. The first row should not
    ;; contain any overlay.
    (org-table-goto-column 1)
    (org-table-highlight-column "#ABCDEF" ">1" 'extend)
    (should-not (org-table-highlight--overlay-exist-p 'column))
    (org-table-goto-line 3)
    (should (org-table-highlight--overlay-exist-p 'column))
    (org-table-goto-column 2)
    (should (org-table-highlight--overlay-exist-p 'column))
    
    ;; 2. Test normal highlighting
    (org-table-goto-line 2)
    (org-table-goto-column 2)
    (org-table-highlight-column "#AB11EF")
    (should (org-table-highlight--overlay-exist-p 'column))
    ;; Check metadata
    (let ((buffer-name (buffer-name))
          (table-context (org-table-highlight--table-context))
          (meta org-table-highlight--metadata))
      (should (equal (org-table-highlight--metadata-table-col-highlights
                      (org-table-highlight--metadata-table
                       (org-table-highlight--metadata-buffer buffer-name)
                       table-context))
                     '((2 :color "#AB11EF")
                       (1 :color "#ABCDEF" :predicate ">1" :extend t))))))))

(ert-deftest org-table-highlight--test-highlight-row ()
  "Test column highlighting and overlay detection."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    (org-table-goto-line 2)
    (org-table-highlight-row "#ABCDEF")
    (should (org-table-highlight--overlay-exist-p 'row))
    ;; Check metadata
    (let ((buffer-name (buffer-name))
          (table-context (org-table-highlight--table-context))
          (meta org-table-highlight--metadata))
      (should (equal (org-table-highlight--metadata-table-row-highlights
                      (org-table-highlight--metadata-table
                       (org-table-highlight--metadata-buffer buffer-name)
                       table-context))
                     '((2 :color "#ABCDEF"))))))))

(ert-deftest org-table-highlight--test-clear-column-highlights ()
  "Test clear functions remove overlays and metadata."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    (org-table-goto-column 1)
    (org-table-highlight-column "#ABCDEF")
    (org-table-goto-column 2)
    (org-table-highlight-column "#ABCDEF")

    
    (org-table-highlight-clear-column-highlights)
    (should org-table-highlight--metadata)
    (should-not (org-table-highlight--overlay-exist-p 'column))
    
    (org-table-highlight-clear-column-highlights 'all)
    (org-table-goto-column 1)
    (should-not (org-table-highlight--overlay-exist-p 'column))
    (should-not org-table-highlight--metadata))))

(ert-deftest org-table-highlight--test-clear-row-highlights ()
  "Test clear functions remove overlays and metadata."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    (org-table-goto-line 1)
    (org-table-highlight-row "#ABCDEF")
    (org-table-goto-line 2)
    (org-table-highlight-row "#ABCDEF")
    
    (org-table-highlight-clear-row-highlights)
    (should org-table-highlight--metadata)
    (should-not (org-table-highlight--overlay-exist-p 'row))

    (org-table-highlight-clear-row-highlights 'all)
    (org-table-goto-line 1)
    (should-not (org-table-highlight--overlay-exist-p 'row))
    (should-not org-table-highlight--metadata))))

(ert-deftest org-table-highlight--test-insert-column ()
  "Test that inserting a column shifts the column highlight correctly.
A column is highlighted, then a new column is inserted before it.
The highlight should move to the right to stay on the same logical column."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    (org-table-goto-column 2)
    (org-table-highlight-column "#FF0000")
    (should (org-table-highlight--overlay-exist-p 'column))
    (org-table-insert-column)
    (org-table-goto-column 3)
    (should (org-table-highlight--overlay-exist-p 'column))
    (org-table-goto-column 2)
    (should-not (org-table-highlight--overlay-exist-p 'column))
    
    (let ((buffer-name (buffer-name))
          (table-context (org-table-highlight--table-context))
          (meta org-table-highlight--metadata))
      (should (equal (org-table-highlight--metadata-table-col-highlights
                      (org-table-highlight--metadata-table
                       (org-table-highlight--metadata-buffer buffer-name)
                       table-context))
                     '((3 :color "#FF0000"))))))))

(ert-deftest org-table-highlight--test-delete-column ()
  "Test that deleting a highlighted column removes its overlay and metadata."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    (org-table-goto-column 2)
    (org-table-highlight-column "#00FF00")
    (org-table-delete-column)
    (should-not (org-table-highlight--overlay-exist-p 'column))
    (should-not org-table-highlight--metadata)
    
    (org-table-insert-column)
    (org-table-highlight-column "#00FF00")
    (org-table-goto-column 1)
    (org-table-delete-column)
    (should (org-table-highlight--overlay-exist-p 'column))
    (should org-table-highlight--metadata)
    
    (let ((buffer-name (buffer-name))
          (table-context (org-table-highlight--table-context)))
      (should (equal (org-table-highlight--metadata-table-col-highlights
                      (org-table-highlight--metadata-table
                       (org-table-highlight--metadata-buffer buffer-name)
                       table-context))
                     '((1 :color "#00FF00"))))))))

(ert-deftest org-table-highlight--test-move-column ()
  "Test that moving a highlighted column right moves the overlay and metadata with it."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    (org-table-goto-column 2)
    (org-table-highlight-column "#0000FF")
    (org-table-move-column 'left)
    (should (org-table-highlight--overlay-exist-p 'column))
    (org-table-goto-column 2)
    (should-not (org-table-highlight--overlay-exist-p 'column))

    (let ((buffer-name (buffer-name))
          (table-context (org-table-highlight--table-context)))
      (should (equal (org-table-highlight--metadata-table-col-highlights
                      (org-table-highlight--metadata-table
                       (org-table-highlight--metadata-buffer buffer-name)
                       table-context))
                     '((1 :color "#0000FF"))))))))

(ert-deftest org-table-highlight--test-move-two-highlighted-columns ()
  "Test switching two highlighted columns by moving them sequentially."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    (org-table-goto-column 1)
    (org-table-highlight-column "#00CED1")
    (org-table-goto-column 2)
    (org-table-highlight-column "#FF6347")

    (org-table-goto-column 1)
    (org-table-move-column)

    ;; Check overlays exist
    (should (org-table-highlight--overlay-exist-p 'column))

    (let* ((buffer-name (buffer-name))
           (table-context (org-table-highlight--table-context))
           (col-highlights (org-table-highlight--metadata-table-col-highlights
                            (org-table-highlight--metadata-table
                             (org-table-highlight--metadata-buffer buffer-name)
                             table-context))))
      ;; The highlights should still be on columns 2 and 3 but swapped colors
      (should (equal col-highlights
                     ;; column 2 now has color from column 3, and column 3 has color from column 2
                     '((1 :color "#FF6347")
                       (2 :color "#00CED1"))))))))

(ert-deftest org-table-highlight--test-insert-row ()
  "Test that inserting a row above a highlighted row shifts the highlight downward."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    (org-table-goto-line 2)
    (org-table-highlight-row "#FFA500")
    (org-table-insert-row)
    (should-not (org-table-highlight--overlay-exist-p 'row))
    (org-table-goto-line 3)
    (should (org-table-highlight--overlay-exist-p 'row))

    (let ((buffer-name (buffer-name))
          (table-context (org-table-highlight--table-context)))
      (should (equal (org-table-highlight--metadata-table-row-highlights
                      (org-table-highlight--metadata-table
                       (org-table-highlight--metadata-buffer buffer-name)
                       table-context))
                     '((3 :color "#FFA500"))))))))

(ert-deftest org-table-highlight--test-insert-rows-and-columns ()
  "Test that inserting a row above a highlighted row shifts the highlight downward."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    (org-table-goto-line 2)
    (org-table-highlight-row "#FFA500")
    (org-table-goto-column 2)
    (org-table-highlight-column "#800080")
    (org-table-insert-row)
    (should-not (org-table-highlight--overlay-exist-p 'row))
    (org-table-goto-column 2)
    (should (org-table-highlight--overlay-exist-p 'column))

    (let* ((buffer-name (buffer-name))
           (table-context (org-table-highlight--table-context))
           (table (org-table-highlight--metadata-table
                   (org-table-highlight--metadata-buffer buffer-name)
                   table-context)))
      (should (equal (org-table-highlight--metadata-table-col-highlights table)
                     '((2 :color "#800080"))))
      (should (equal (org-table-highlight--metadata-table-row-highlights table)
                     '((3 :color "#FFA500"))))))))

(ert-deftest org-table-highlight--test-delete-row ()
  "Test that deleting a highlighted row removes its overlay and metadata."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    (org-table-goto-line 2)
    (org-table-highlight-row "#800080") 
    (org-table-kill-row)
    (should-not (org-table-highlight--overlay-exist-p 'row))
    (should-not org-table-highlight--metadata))))

(ert-deftest org-table-highlight--test-move-row ()
  "Test that moving a highlighted row down moves the overlay and metadata with it."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    (org-table-goto-line 2)
    (org-table-highlight-row "#00CED1")
    (org-table-move-row)
    (should (org-table-highlight--overlay-exist-p 'row))

    (let ((buffer-name (buffer-name))
          (table-context (org-table-highlight--table-context)))
      (should (equal (org-table-highlight--metadata-table-row-highlights
                      (org-table-highlight--metadata-table
                       (org-table-highlight--metadata-buffer buffer-name)
                       table-context))
                     '((3 :color "#00CED1"))))))))

(ert-deftest org-table-highlight--test-move-two-highlighted-rows ()
  "Test switching two highlighted rows by moving them sequentially."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    (org-table-goto-line 2)
    (org-table-highlight-row "#00CED1")
    (org-table-goto-line 3)
    (org-table-highlight-row "#FF6347")
    
    (org-table-goto-line 2)
    (org-table-move-row)

    (let* ((buffer-name (buffer-name))
           (table-context (org-table-highlight--table-context))
           (row-highlights (org-table-highlight--metadata-table-row-highlights
                            (org-table-highlight--metadata-table
                             (org-table-highlight--metadata-buffer buffer-name)
                             table-context))))
      (should (equal row-highlights
                     '((3 :color "#00CED1")
                       (2 :color "#FF6347"))))))))

(ert-deftest org-table-highlight--test-insert-rows-and-columns ()
  "Test switching two highlighted rows by moving them sequentially."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    (org-table-goto-line 2)
    (org-table-highlight-row "#00CED1")
    (org-table-goto-column 2)
    (org-table-highlight-column "#FF6347")
    
    (org-table-goto-line 3)
    (org-table-goto-column 2)
    (end-of-line)
    (org-table-insert-column)
    (org-table-next-field)

    (org-table-goto-line 2)
    (org-table-goto-column 3)
    (should (org-table-highlight--overlay-exist-p 'row))

    (org-table-goto-line 4)
    (org-table-goto-column 2)
    (should (org-table-highlight--overlay-exist-p 'column))

    (let* ((buffer-name (buffer-name))
           (table-context (org-table-highlight--table-context))
           (table (org-table-highlight--metadata-table
                   (org-table-highlight--metadata-buffer buffer-name)
                   table-context))
           (col-highlights (org-table-highlight--metadata-table-col-highlights table))
           (row-highlights (org-table-highlight--metadata-table-row-highlights table)))
      (should (equal row-highlights '((2 :color "#00CED1"))))
      (should (equal col-highlights '((2 :color "#FF6347"))))))))

(ert-deftest org-table-highlight--test-build-buffer-metadata ()
  "Test `org-table-highlight--build-buffer-metadata' returns correct table metadata."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    ;; https://github.com/llcc/org-table-highlight/issues/4
    (org-table-highlight--build-buffer-metadata)
    (should-not org-table-highlight--metadata)
    
    ;; Highlight a column and a row
    (org-table-goto-column 2)
    (org-table-highlight-column "#123456")
    (org-table-goto-line 2)
    (org-table-highlight-row "#654321")

    (org-table-highlight--build-buffer-metadata)
    
    ;; Now collect metadata
    (let* ((buffer-name (buffer-name))
           (buffer-meta (org-table-highlight--metadata-buffer buffer-name))
           (tables (org-table-highlight--metadata-buffer-tables buffer-meta))
           (col-highlights (org-table-highlight--metadata-table-col-highlights (car tables)))
           (row-highlights (org-table-highlight--metadata-table-row-highlights (car tables))))
      
      ;; Should only return one table
      (should (= (length tables) 1))
      
      ;; Verify column highlight
      (should (equal col-highlights '((2 :color "#123456"))))
      
      ;; Verify row highlight
      (should (equal row-highlights '((2 :color "#654321"))))))))

(ert-deftest org-table-highlight--test-restore-buffer ()
  "Test that row and column highlights are restored correctly."
  (org-table-highlight--with-test-env
   (org-table-highlight--with-temp-buffer
    (org-table-goto-line 2)
    (org-table-highlight-row "#00CED1")
    (org-table-goto-column 2)
    (org-table-highlight-column "#FF6347")

    (org-table-highlight-mode -1)
    (should-not (org-table-highlight--overlay-exist-p 'row))
    (should-not (org-table-highlight--overlay-exist-p 'column))

    (org-table-highlight-mode 1)
    (should (org-table-highlight--overlay-exist-p 'row))
    (should (org-table-highlight--overlay-exist-p 'column))

    ;; Save the buffer name and table context for later validation
    (let* ((buf-name (buffer-name))
           (table-context (org-table-highlight--table-context))
           (table-meta
            (org-table-highlight--metadata-table
             (org-table-highlight--metadata-buffer buf-name)
             table-context)))
      
      (should (equal (org-table-highlight--metadata-table-row-highlights table-meta)
                     '((2 :color "#00CED1"))))
      (should (equal (org-table-highlight--metadata-table-col-highlights table-meta)
                     '((2 :color "#FF6347"))))))))


