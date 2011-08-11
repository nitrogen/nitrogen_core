#!/usr/bin/emacs --script

(defun publish-nitrogen-docs ()
  (interactive)
  (setq org-publish-project-alist
        '(
          ("nitrogen-docs-org"
           :base-directory "doc/org-mode"
           :base-extension "org"
           :publishing-directory "doc/html"
           :recursive t
           :publishing-function org-publish-org-to-html
           :headline-levels 4     ; Just the default for this project.
           :auto-preamble t
           )
          ))
  (org-publish-all nil))

(publish-nitrogen-docs)
