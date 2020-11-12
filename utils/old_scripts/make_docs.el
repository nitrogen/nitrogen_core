#!/usr/bin/emacs --script


(require 'org)
(require 'ox)
(require 'ox-publish)
(require 'htmlize)
(require 'erlang)

(defun publish-nitrogen-docs ()
;  (interactive)
  ;; Republish everyhing every time - ignore timestamps
  (setq org-publish-use-timestamps-flag nil)

  (setq org-export-htmlize-output-type 'css)

  (setq org-publish-project-alist
        '(
          ("nitrogen-docs-org"
           :base-directory "doc/org-mode"
           :base-extension "org"
           :publishing-directory "doc/html"
           :recursive t
;           :publishing-function org-publish-org-to-html
           :publishing-function org-html-publish-to-html
           :headline-levels 4     ; Just the default for this project.
           :auto-preamble t
           )
          ))
  (org-publish-all nil))

(publish-nitrogen-docs)
