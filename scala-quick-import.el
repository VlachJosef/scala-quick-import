;;; scala-quick-import.el --- Support for importing types at point

;; Copyright (c) 2017 Josef Vlach

;; Homepage: https://github.com/VlachJosef/scala-quick-import
;; Package-Version:  0.1

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 's)
(require 'ag)
(require 'ido)
(require 'ensime)
(require 'projectile)

(defun scala-quick-import:normalize-import (import)
  (let* ((normalized (s-replace " " "" (s-collapse-whitespace (s-trim-left import))))
	 (prefix-and-class-list (s-split "{" normalized)))
    (pcase prefix-and-class-list
      (`(,n . nil) (cons import nil)) ;; return input as is
      (`(,prefix ,m . nil)
       (let* ((class-list (s-split "," (s-replace "}" "" m)))
              (value))
         (dolist (element class-list value)
           (setq value (cons (concat prefix element) value)))))
      (_ nil)))) ; input is broken, it is not valid scala import, let's return empty list

(defun scala-quick-import:normalize-and-sort (input search-term)
  (let ((all-imports (apply #'append (mapcar 'scala-quick-import:normalize-import input))))
    (sort (cl-remove-if-not (lambda (import) (string-match (format "\\.%s$" search-term) import)) all-imports) 'string<)))

(defun scala-quick-import:search-import (search-term)
  "Use ag to search lines in project starting with keyword import and containing text `search-term'
By default all import clauses are normalized, meaning that any import including `import selector clause'
enclosed in braces is broken down into its own import clause.
Prefix arguments:
   no arg  - normalize
   C-u     - normalize and copy current import to kill ring
   C--     - don't normalize
   C-- C-u - don't normalize and copy current import to kill ring"
  (interactive
   (list (read-from-minibuffer
          (projectile-prepend-project-name "Ag search for import: ")
          (projectile-symbol-or-selection-at-point))))
  (unless (and (executable-find "ag") (executable-find "sort") (executable-find "uniq"))
    (error "Commands 'ag', 'sort' and 'uniq' are required to use this command"))
  (scala-quick-import:search-import-body
   search-term
   (lambda ()
     (shell-command-to-string (format "ag import.*%s --nonumbers --noheading --nofilename --nobreak --ignore-case | sort | uniq" search-term)))))

(defun scala-quick-import:search-import-body (search-term run-ag-fn)
  (let ((identity2 (lambda (input search-term) input))
        (normalization-function)
        (copy-to-kill-ring))
    (pcase current-prefix-arg
      (`nil (setq normalization-function `scala-quick-import:normalize-and-sort)
            (setq copy-to-kill-ring nil) `identity2)
      (`- (setq normalization-function (lambda (input search-term) input))
          (setq copy-to-kill-ring nil))
      (`(,n . nil) (if (< 0 n)
        	       (progn (setq normalization-function `scala-quick-import:normalize-and-sort)
        		      (setq copy-to-kill-ring t))
        	     (progn (setq normalization-function `identity2)
        		    (setq copy-to-kill-ring t)))))
    (let* ((res-raw (funcall run-ag-fn))
           (lines (split-string (s-replace "import " "" (s-trim res-raw)) "\n"))
           (import (let ((x (funcall normalization-function lines search-term)))
                     (pcase (delete-dups x)
                       (`nil nil)
                       (`(,n . nil) n)
                       (_ (ido-completing-read "Select an import: " x))))))
      (if copy-to-kill-ring
          (progn (kill-new (format "import %s" import))
                 (message "%s added to kill ring" import))
        (if (eq nil import)
            (message "No import found for %s" search-term)

          (if (scala-quick-import:is-import-present search-term)
              (message "Import %s already present in a current buffer." import)
            (ensime-insert-import import)
            (message "import %s inserted succesfully." import)))))))

(defun scala-quick-import:is-import-present (search-term)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "^import .*[.{, ]%s[ ,}\r\n]" search-term) nil t 1)))

(provide 'scala-quick-import)
;;; scala-quick-import.el ends here
