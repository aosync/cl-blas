(defclass cblas-sbuffer ()
  ((buffer :initarg :buffer)))
(defclass cblas-dbuffer ()
  ((buffer :initarg :buffer)))
(defclass cblas-cbuffer ()
  ((buffer :initarg :buffer)))
(defclass cblas-zbuffer ()
  ((buffer :initarg :buffer)))

(defmacro defblasmethod (defn args &body body)
  (let ((scl '())
		(rcl nil))
	(dolist (arg args)
	  (when (and (consp arg) (eq (second arg) :scalar))
		(push (first arg) scl))
	  (when (and (consp arg) (eq (second arg) :return-scalar))
		(push (first arg) scl)
		(setf rcl (first arg))))
	(destructuring-bind (method c-name prefixes) defn
	  `(progn
		 ,@(mapcar
			(lambda (prefix &aux (bindings '()))
										; generate the with-foreign-objects bindings
										; generate the code that sets them
										; generate the optional gathering into return
			  `(defmethod ,method ,@(specialize-args args prefix)
										; substitute everything
			prefixes))))))

  
(defmethod blas:axpy (n (alpha single-float) (x cblas-sbuffer) incx (y cblas-sbuffer) incy)
  (cblas-saxpy n alpha (slot-value x 'buffer) incx (slot-value y 'buffer) incy))
(defmethod blas:axpy (n (alpha double-float) (x cblas-dbuffer) incx (y cblas-dbuffer) incy)
  (cblas-daxpy n alpha (slot-value x 'buffer) incx (slot-value y 'buffer) incy))
(defmethod blas:axpy (n (alpha complex) (x cblas-cbuffer) incx (y cblas-cbuffer) incy)
  (with-foreign-object (%alpha :float 2)
					   (setf (mem-aref %alpha :float 0) (realpart alpha))
					   (setf (mem-aref %alpha :float 1) (imagpart alpha))
					   (cblas-caxpy n %alpha (slot-value x 'buffer) incx (slot-value y 'buffer) incy)))
		   
(defmethod blas:axpy (n (alpha (complex double-float)) (x cblas-zbuffer) incx (y cblas-zbuffer) incy)
  (cblas-zaxpy n alpha (slot-value x 'buffer) incx (slot-value y 'buffer) incy))
; ----------- TESTS ------------


(defun print-foreign-array (array type n)
  (dotimes (i n)
	(format t "  ~a~%" (mem-aref array type i))))

(with-foreign-objects ((vec-a :float 100)
					   (vec-b :float 100))
  (setf (mem-aref vec-a :float 0) 1.0)
  (setf (mem-aref vec-a :float 1) 1.0)
  (setf (mem-aref vec-a :float 2) 1.0)
  (setf (mem-aref vec-b :float 0) 1.0)
  (setf (mem-aref vec-b :float 1) 2.0)
  (setf (mem-aref vec-b :float 2) 3.0)

  (let ((a (make-instance 'cblas-sbuffer :buffer vec-a))
		(b (make-instance 'cblas-sbuffer :buffer vec-b)))
	(format t "Before vec-b~%")
	(print-foreign-array vec-b :float 3)
	(time (dotimes (_ 10000000)
										(blas:axpy 100 2.0 a 1 b 1)))
	;  (cblas-saxpy 100 2.0 vec-a 1 vec-b 1)))
	(format t "After vec-b ~a~%" (cblas-sdot 3 vec-a 1 vec-b 1))
	(print-foreign-array vec-b :float 3)))
  
