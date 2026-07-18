(in-package :cblas)

(defclass cblas-buffer ()
  ((buffer :initarg :buffer)))

(defclass cblas-sbuffer (cblas-buffer) ())
(defclass cblas-dbuffer (cblas-buffer) ()) 
(defclass cblas-cbuffer (cblas-buffer) ())
(defclass cblas-zbuffer (cblas-buffer) ())

(defmethod blas:axpy (n (alpha single-float) (x cblas-sbuffer) incx (y cblas-sbuffer) incy)
  (cblas-saxpy n alpha x incx y incy))
(defmethod blas:axpy (n (alpha double-float) (x cblas-dbuffer) incx (y cblas-dbuffer) incy)
  (cblas-daxpy n alpha x incx y incy))
(defmethod blas:axpy (n (alpha complex) (x cblas-cbuffer) incx (y cblas-cbuffer) incy)
  (cblas-caxpy n alpha x incx y incy))
(defmethod blas:axpy (n (alpha complex) (x cblas-zbuffer) incx (y cblas-zbuffer) incy)
  (cblas-zaxpy n alpha x incx y incy))

(defmethod blas:scal (n (alpha single-float) (x cblas-sbuffer) incx)
  (cblas-sscal n alpha x incx))
(defmethod blas:scal (n (alpha double-float) (x cblas-dbuffer) incx)
  (cblas-dscal n alpha x incx))
(defmethod blas:scal (n (alpha complex) (x cblas-cbuffer) incx)
  (cblas-cscal n alpha x incx))
(defmethod blas:scal (n (alpha complex) (x cblas-zbuffer) incx)
  (cblas-zscal n alpha x incx))

(defmethod blas:copy (n (x cblas-sbuffer) incx (y cblas-sbuffer) incy)
  (cblas-scopy n x incx y incy))
(defmethod blas:copy (n (x cblas-dbuffer) incx (y cblas-dbuffer) incy)
  (cblas-dcopy n x incx y incy))
(defmethod blas:copy (n (x cblas-cbuffer) incx (y cblas-cbuffer) incy)
  (cblas-ccopy n x incx y incy))
(defmethod blas:copy (n (x cblas-zbuffer) incx (y cblas-zbuffer) incy)
  (cblas-zcopy n x incx y incy))

(defmethod blas:swap (n (x cblas-sbuffer) incx (y cblas-sbuffer) incy)
  (cblas-sswap n x incx y incy))
(defmethod blas:swap (n (x cblas-dbuffer) incx (y cblas-dbuffer) incy)
  (cblas-dswap n x incx y incy))
(defmethod blas:swap (n (x cblas-cbuffer) incx (y cblas-cbuffer) incy)
  (cblas-cswap n x incx y incy))
(defmethod blas:swap (n (x cblas-zbuffer) incx (y cblas-zbuffer) incy)
  (cblas-zswap n x incx y incy))



(defmethod blas:dot (n (x cblas-sbuffer) incx (y cblas-sbuffer) incy)
  (cblas-sdot n x incx y incy))
(defmethod blas:dot (n (x cblas-dbuffer) incx (y cblas-dbuffer) incy)
  (cblas-ddot n x incx y incy))

(defmethod blas:dotu (n (x cblas-cbuffer) incx (y cblas-cbuffer) incy)
  (cffi:with-foreign-object (dotu :float 2)
	(cblas-cdotu-sub n x incx y incy dotu)
	(complex
	 (cffi:mem-aref dotu :float 0)
	 (cffi:mem-aref dotu :float 1))))
(defmethod blas:dotu (n (x cblas-zbuffer) incx (y cblas-zbuffer) incy)
  (cffi:with-foreign-object (dotu :double 2)
	(cblas-zdotu-sub n x incx y incy dotu)
	(complex
	 (cffi:mem-aref dotu :double 0)
	 (cffi:mem-aref dotu :double 1))))

(defmethod blas:dotc (n (x cblas-cbuffer) incx (y cblas-cbuffer) incy)
  (cffi:with-foreign-object (dotc :float 2)
	(cblas-cdotc-sub n x incx y incy dotc)
	(complex
	 (cffi:mem-aref dotc :float 0)
	 (cffi:mem-aref dotc :float 1))))
(defmethod blas:dotc (n (x cblas-zbuffer) incx (y cblas-zbuffer) incy)
  (cffi:with-foreign-object (dotc :double 2)
	(cblas-zdotc-sub n x incx y incy dotc)
	(complex
	 (cffi:mem-aref dotc :double 0)
	 (cffi:mem-aref dotc :double 1))))

(defmethod blas:nrm2 (n (x cblas-sbuffer) incx)
  (cblas-snrm2 n x incx))
(defmethod blas:nrm2 (n (x cblas-dbuffer) incx)
  (cblas-dnrm2 n x incx))
(defmethod blas:nrm2 (n (x cblas-cbuffer) incx)
  (cblas-scnrm2 n x incx))
(defmethod blas:nrm2 (n (x cblas-zbuffer) incx)
  (cblas-dznrm2 n x incx))

