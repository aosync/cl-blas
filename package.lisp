(defpackage blas
  (:use cl)
  (:export axpy
	   scal
	   copy
	   swap
	   dot
	   dotu
	   dotc
	   nrm2
	   asum
	   i-amax
	   rotg
	   rot
	   rotmg
	   rotm

	   gemv
	   hemv
	   symv
	   trmv
	   trsv
	   ger
	   geru
	   gerc
	   syr
	   her
	   syr2
	   her2

	   gbmv
	   hbmv
	   sbmv
	   tbmv
	   tbsv

	   hpmv
	   spmv
	   tpmv
	   tpsv
	   spr
	   hpr
	   spr2
	   hpr2

	   gemm
	   gemmtr
	   symm
	   hemm
	   trmm
	   trsm
	   syrk
	   herk
	   syr2k
	   her2k))

(in-package blas)

;; Level 1: vector-vector operations
(defgeneric axpy (n alpha x incx y incy)
  (:documentation "Add (alpha*x) to y"))
(defgeneric scal (n alpha x incx)
  (:documentation "Scale x by alpha"))
(defgeneric copy (n x incx y incy)
  (:documentation "Copy x into y"))
(defgeneric swap (n x incx y incy)
  (:documentation "Swap elements of x and y"))

(defgeneric dot (n x incx y incy)
  (:documentation "Return x^T*y"))
(defgeneric dotu (n x incx y incy)
  (:documentation "Return x^T*y for complex vectors x and y"))
(defgeneric dotc (n x incx y incy)
  (:documentation "Return x^H*y for complex vectors x and y"))

(defgeneric nrm2 (n x incx)
  (:documentation "2-norm of vector x"))
(defgeneric asum (n x incx)
  (:documentation "1-norm of vector x"))
(defgeneric i-amax (n x incx)
  (:documentation "Infinity-norm of vector x"))

(defgeneric rotg (a b)
  (:documentation "Generate Given's plane rotation"))
(defgeneric rot (n x incx y incy c s)
  (:documentation "Apply Given's plane rotation to vector x and vector y"))
(defgeneric rotmg (d1 d2 a b param)
  (:documentation "Generate a modified plane rotation"))
(defgeneric rotm (n x incx y incy param)
  (:documentation "Apply a modified plane rotation"))

;; Level 2: matrix-vector operations
(defgeneric gemv (trans m n alpha a lda x incx beta y incy)
  (:documentation "Set y to (alpha*A*x + beta*y)"))
(defgeneric hemv (uplo n alpha a lda x incx beta y incy)
  (:documentation "Set y to (alpha*A*x + beta*y) for self-adjoint matrix A"))
(defgeneric symv (uplo n alpha a lda x incx beta y incy)
  (:documentation "Set y to (alpha*A*x + beta*y) for self-transpose matrix A"))
(defgeneric trmv (uplo trans diag n a lda x incx)
  (:documentation "Set x to (A*x) for triangular matrix A"))
(defgeneric trsv (uplo trans diag n a lda x incx)
  (:documentation "Set x to (A^-1*x) for triangular matrix A"))

(defgeneric ger (m n alpha x incx y incy a lda)
  (:documentation "Add (alpha*x*y^T) to A"))
(defgeneric geru (m n alpha x incx y incy a lda)
  (:documentation "Add (alpha*x*y^T) to A"))
(defgeneric gerc (m n alpha x incx y incy a lda)
  (:documentation "Add (alpha*x*y^H) to A"))

(defgeneric syr (uplo n alpha x incx a lda)
  (:documentation "Add (alpha*x*x^T) to A"))
(defgeneric her (uplo n alpha x incx a lda)
  (:documentation "Add (alpha*x*x^H) to A"))
(defgeneric syr2 (uplo n alpha x incx y incy a lda)
  (:documentation "Add (alpha*x*y^T + alpha*y*x^T) to A"))
(defgeneric her2 (uplo n alpha x incx y incy a lda)
  (:documentation "Add (alpha*x*y^H + alpha*y*x^H) to A"))

;; Level 2: band storage matrix-vector operations
(defgeneric gbmv (trans m n kl ku alpha a lda x incx beta y incy)
  (:documentation "Set y to (alpha*A*x + beta*y) for banded matrix A"))
(defgeneric hbmv (uplo n k alpha a lda x incx beta y incy)
  (:documentation "Set y to (alpha*A*x + beta*y) for self-adjoint banded matrix A"))
(defgeneric sbmv (uplo n k alpha a lda x incx beta y incy)
  (:documentation "Set y to (alpha*A*x + beta*y) for self-transpose banded matrix A"))
(defgeneric tbmv (uplo trans diag n k a lda x incx)
  (:documentation "Set x to (A*x) for triangular banded matrix A"))
(defgeneric tbsv (uplo trans diag n k a lda x incx)
  (:documentation "Set x to (A^-1*x) for triangular banded matrix A"))

;; Level 2: packed storage matrix-matrix operations
(defgeneric hpmv (uplo n alpha ap x incx beta y incy)
  (:documentation "Set y to (alpha*A*x + beta*y) for self-adjoint packed matrix A"))
(defgeneric spmv (uplo n alpha ap x incx beta y incy)
  (:documentation "Set y to (alpha*A*x + beta*y) for self-transpose packed matrix A"))
(defgeneric tpmv (uplo trans diag n ap x incx)
  (:documentation "Set x to (A*x) for triangular packed matrix A"))
(defgeneric tpsv (uplo trans diag n ap x incx)
  (:documentation "Set x to (A^-1*x) for triangular packed matrix A"))

(defgeneric spr (uplo n alpha x incx ap)
  (:documentation "Add (alpha*x*x^T) to packed matrix A"))
(defgeneric hpr (uplo n alpha x incx ap)
  (:documentation "Add (alpha*x*x^H) to packed matrix A"))
(defgeneric spr2 (uplo n alpha x incx y incy ap)
  (:documentation "Add (alpha*x*y^T + alpha*y*x^T) to packed matrix A"))
(defgeneric hpr2 (uplo n alpha x incx y incy ap)
  (:documentation "Add (alpha*x*y^H + alpha*y*x^H) to packed matrix A"))

;; Level 3: matrix-matrix operations
(defgeneric gemm (transa transb m n alpha a lda b ldb beta c ldc)
  (:documentation "Set C to (alpha*A*B + beta*C)"))
(defgeneric gemmtr (uplo transa transb m n alpha a lda b ldb beta c ldc)
  (:documentation "Set upper/lower part of C to corresponding part of (alpha*A*B + beta*C)"))
(defgeneric symm (side uplo m n alpha a lda b ldb beta c ldc)
  (:documentation "Set C to (alpha*A*B + beta*C) for self-transpose matrix A"))
(defgeneric hemm (side uplo m n alpha a lda b ldb beta c ldc)
  (:documentation "Set C to (alpha*A*B + beta*C) for self-adjoint matrix A"))
(defgeneric trmm (side uplo transa diag m n alpha a lda b ldb)
  (:documentation "Set B to (alpha*A*B) for triangular matrix A"))
(defgeneric trsm (side uplo transa diag m n alpha a lda b ldb)
  (:documentation "Set B to (alpha*A^-1*B) for triangular matrix A"))

(defgeneric syrk (uplo trans n k alpha a lda beta c ldc)
  (:documentation "Set C to (alpha*A*A^T + beta*C)"))
(defgeneric herk (uplo trans n k alpha a lda beta c ldc)
  (:documentation "Set C to (alpha*A*A^H + beta*C)"))
(defgeneric syr2k (uplo trans n k alpha a lda b ldb beta c ldc)
  (:documentation "Set C to (alpha*A*B^T + alpha*B*A^T + beta*C)"))
(defgeneric her2k (uplo trans n k alpha a lda b ldb beta c ldc)
  (:documentation "Set C to (alpha*A*B^H + alpha*B*A^H + beta*C)"))
