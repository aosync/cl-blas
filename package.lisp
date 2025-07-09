(defpackage :blas
  (:use :cl)
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
