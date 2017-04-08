(setq companyName "Bitcoin Unlimited")
(setq copyrightDate "2017")
(setq proprietaryNotice "")
(setq author "")

(defun doc-class()
  "Inserts a documentation template for a class"
  (interactive)
  (insert "/**\n")
  (insert "<comments>\n")
  (insert "\n")
  (insert "@short <1 line fn desc>\n")
  (insert "@author <your name>\n")
  (insert "@see <other classes>\n")
  (insert "*/\n")
 )

(defun doc-memberfn()
  "Inserts at the beginning of buffer a member function"
  (interactive)
  (insert "/**\n")
  (insert "<comments>\n")

  (insert "@see [other fns]\n")
  (insert "@return [one sentence]\n")
  (insert "@exception [exceptions]\n")
  (insert "@param [param name identifier] [param description]\n")
  (insert "*/\n")

  ;;  Describe a parameter. The param description can span multiple lines and will be terminated by a blank line,
  ;; the end of the comment, or another param entry. For this reason, param entries should normally be the last
  ;; part of the doc comment.
)

(defun doc-header()
  "Inserts at the beginning of buffer a function header comment"
  (interactive)
  (insert (concat "/**  Copyright " copyrightDate "  " companyName "\n"))
  (insert proprietaryNotice)
  (insert "\n<comments>\n")
  (insert "\n")
  (insert "*/\n")
)

(defun doc-c-src()
  ""
  (interactive)
  (insert (concat "/**  Copyright " copyrightDate "  " companyName "\n"))
  (insert proprietaryNotice)
  (insert "\n<comments>\n")
  (insert "\n")
  (insert "*/\n")
)


(defun insert-c-file-header()
  "Inserts at the beginning of buffer a function header comment"
  (interactive)
  (insert "\n")
  (insert "/*                     ")
  (insert (file-relative-name (buffer-file-name)))
  (insert (concat "                By: " my-name "\n"))
  (insert (concat "\n   " my-preamble "\n\n"))
  (insert "\n")
  (insert "Purpose:    \n")
  (insert "\n")
  (insert "*/\n")
  (insert "\n\n")
  (insert "/* Standard Headers */\n\n")
  (insert "/* Headers */\n")
)

(defun close_comment()
  "close a C comment by inserting a */ to match the previous line."
  (interactive)
  (let ((beg (point)))
    (previous-line 1)
    (beginning-of-line)
    (search-forward "*/")
    (backward-char 2)
    (let ((comment_column (current-column)))
      (goto-char beg)
      (indent-to-column comment_column)
      (insert "*/")
      )
    )
)
