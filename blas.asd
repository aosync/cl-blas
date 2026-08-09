(defsystem :blas
  :description "BLAS interface for Common Lisp"
  :author "Alejandro W. Sior <aho@sior.be>"
  :version "0.0.0"
  :serial t
  :depends-on (:cffi :trivial-garbage)
  :components ((:file "package")
			         (:file "blas")
			         (:module "cblas"
				        :serial t
				        :components ((:file "package")
							               (:file "cblas")
							               (:file "impl")))
               (:module "abaco"
				        :serial t
				        :components ((:file "package")))))
