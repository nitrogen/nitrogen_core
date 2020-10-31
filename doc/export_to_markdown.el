(eval-after-load "org"
 '(require 'ox-gfm nil t))
;;;###autoload
(require 'org)

(defun dired-org-to-markdown ()
 (let ((files
		(append
		 (let ((default-directory "~/org/blog"))
		  (mapcar #'expand-file-name
		   (file-expand-wildcards "**/*.org")))
		 (let ((default-directory "~/org/blog"))
		  (mapcar #'expand-file-name
		   (file-expand-wildcards "*.org")))
		)
	   ))
  (mapc
   (lambda (f)
	(with-current-buffer
	 (find-file-noselect f)
	 (org-gfm-export-to-markdown)))
   files))
 )

(dired-org-to-markdown)
