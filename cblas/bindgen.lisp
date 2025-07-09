(in-package :cblas)

(defun substitute-keywords (subs form)
  "Substitute keywords following P-LIST SUBS in FORM"
  (labels ((walk (form)
			 (cond
			   ((keywordp form)
				(getf subs form form))
			   ((consp form)
				(mapcar #'walk form))
			   (t form))))
	(walk form)))

(defmacro defcblasfun (defn &rest args)
  "Define a CFFI CBLAS function for all specified scalar prefixes"
  (destructuring-bind (c-name prefixes) defn
	`(progn
	   ,@(mapcar
		  (lambda (prefix)
			`(defcfun ,(format nil c-name prefix)
				 ,@(substitute-keywords
					`(:scalar ,(cond ((string= prefix "s")  :float)
									 ((string= prefix "d")  :double)
									 ((string= prefix "c")  :pointer)
									 ((string= prefix "z")  :pointer)
									 ((string= prefix "sc") :float)
									 ((string= prefix "dz") :double)
									 ((string= prefix "cs") :float)
									 ((string= prefix "zd") :double))
					  :real   ,(cond ((string= prefix "s")  :float)
									 ((string= prefix "d")  :double)
									 ((string= prefix "c")  :float)
									 ((string= prefix "z")  :double)
									 (t                     nil)))
					args)))
		  prefixes))))
