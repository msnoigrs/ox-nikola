;;; ox-nikola.el --- Export Nikola articles using org-mode.

;; Copyright (C) 2013  IGARASHI Masanao

;; Author: IGARASHI Masanao <syoux2@gmail.com>
;; Keywords: org, nikola
;; Version: 0.1

;;; Commentary:

;;; Code:

;;; Dependencies

(eval-when-compile (require 'cl))
(require 'ox)
(require 'ox-publish)
(require 'ox-rst)


;;; User Configurable Variables

(defgroup org-export-nikola nil
  "Options for exporting Org mode files to Nikola reStructuredText."
  :tag "Org Nikola"
  :group 'org-export)

(defcustom org-nikola-nikola-template ""
  "Default template in a Nikola article."
  :group 'org-export-nikola
  :type 'string)

(defcustom org-nikola-password ""
  "Default password in a Nikola article."
  :group 'org-export-nikola
  :type 'string)

(defcustom org-nikola-category ""
  "Default category in a Nikola article."
  :group 'org-export-nikola
  :type 'string)

(defcustom org-nikola-annotations ""
  "Default annotations metadata field in a Nikola article. True or other"
  :group 'org-export-nikola
  :type 'string)

(defcustom org-nikola-noannotations ""
  "Default noannotations metadata field in a Nikola article. True or other"
  :group 'org-export-nikola
  :type 'string)

(defcustom org-nikola-nocomments ""
  "Default nocomments metadata field in a Nikola article. True or other"
  :group 'org-export-nikola
  :type 'string)


;;; Define Back-End

(org-export-define-derived-backend 'nikola 'rst
  :menu-entry
  '(?n "Export to reStructuredText for Nikola"
       ((?R "As reStructuredText buffer" org-nikola-export-as-rst)
        (?r "As reStructuredText file" org-nikola-export-to-rst)))
  :translate-alist
  '((template . org-nikola-template))
  :options-alist
  '(
	(:nikola-slug "NIKOLA_SLUG" nil "")
	(:nikola-tags "NIKOLA_TAGS" nil "")
	(:nikola-link "NIKOLA_LINK" nil "")
	(:nikola-password "NIKOLA_PASSWORD" nil org-nikola-password)
	(:nikola-template "NIKOLA_TEMPLATE" nil org-nikola-nikola-template)
    (:nikola-category "NIKOLA_CATEGORY" nil org-nikola-category)
    (:nikola-annotations "NIKOLA_ANNOTATIONS" nil org-nikola-annotations)
    (:nikola-annotations "NIKOLA_NOANNOTATIONS" nil org-nikola-noannotations)
    (:nikola-nocomments "NIKOLA_NOCOMMENTS" nil org-nikola-nocomments)))


;;; Template

(defun org-nikola-template (contents info)
  "Return complete document string after reStructuredText conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (concat
   (org-nikola--front-matter info)
   contents))

(defun org-nikola--get-option (info property-name &optional default)
  (let ((property (org-export-data (plist-get info property-name) info)))
    (if (string= "" property) default property)))

(defun org-nikola--get-true-option (info property-name)
  (let ((property (org-export-data (plist-get info property-name) info)))
	(if	(or (string= (downcase property) "true")
			(string= (downcase property) "t"))
		"True" "")))

(defun org-nikola--front-matter (info)
  (let* ((title
		  (org-nikola--get-option info :title))
		 (slug
		  (org-nikola--get-option info :nikola-slug title))
		 (date
		  (org-nikola--get-option info :date))
		 (tags
		  (org-nikola--get-option info :nikola-tags))
		 (link
		  (org-nikola--get-option info :nikola-link))
		 (description
		  (org-nikola--get-option info :description))
		 (author
		  (org-nikola--get-option info :author))
		 (email
		  (org-nikola--get-option info :email))
		 (password
		  (org-nikola--get-option info :nikola-password ""))
		 (template
		  (org-nikola--get-option info :nikola-template ""))
		 (category
		  (org-nikola--get-option info :nikola-category ""))
		 (annotations
		  (org-nikola--get-true-option info :nikola-annotations))
		 (noannotations
		  (org-nikola--get-true-option info :nikola-noannotations))
		 (nocomments
		  (org-nikola--get-true-option info :nikola-nocomments)))
    (concat
     ".. title: "      title
     "\n.. slug: "     (replace-regexp-in-string "[　]+" "-"
												 (replace-regexp-in-string
												  "[\s-]+" "-" slug))
     "\n.. date: "     date
     "\n.. tags: "     tags
     "\n.. link: "     link
     "\n.. description: " description
     (cond ((and (not (string= "" author)) (plist-get info :with-author))
			(concat "\n.. author: " author)))
     (cond ((and (not (string= "" email)) (plist-get info :with-email))
			(format " <%s>" email)))
     (cond ((not (string= "" password)) (concat "\n.. password: " password)))
	 (cond ((not (string= "" template)) (concat "\n.. template: " template)))
     (cond ((not (string= "" category)) (concat "\n.. category: " category)))
     (cond ((not (string= "" annotations))
			(concat "\n.. annotations: " annotations)))
     (cond ((not (string= "" noannotations))
			(concat "\n.. noannotations: " noannotations)))
     (cond ((not (string= "" nocomments))
			(concat "\n.. nocomments: " nocomments))))))


;;; End-User functions

;;;###autoload
(defun org-nikola-export-as-rst
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reStructuredText buffer."
  (interactive)
  (org-export-to-buffer 'nikola "*Org nikola RST Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (rst-mode))))


;;;###autoload
(defun org-nikola-export-to-rst
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reStructuredText file"
  (interactive)
  (let ((outfile (org-export-output-file-name ".rst" subtreep)))
    (org-export-to-file 'nikola outfile
      async subtreep visible-only body-only ext-plist)))


;;;###autoload
(defun org-nikola-publish-to-rst (plist filename pub-dir)
  "Publish an org file to reStructuredText.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'nikola filename ".rst" plist pub-dir))


;;; provide

(provide 'ox-nikola)

;;; ox-nikola.el ends here
