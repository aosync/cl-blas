(defun print-foreign-array (array type n)
  (dotimes (i n)
	(format t "  ~a~%" (mem-aref array type i))))

(with-foreign-objects ((vec-a :double 2)
					   (vec-b :double 2))
  (setf (mem-aref vec-a :double 0) 1.0d0)
  (setf (mem-aref vec-a :double 1) 1.0d0)
  (setf (mem-aref vec-b :double 0) 1.0d0)
  (setf (mem-aref vec-b :double 1) 1.0d0)


  (let ((a (make-instance 'cblas-zbuffer :buffer vec-a))
		(b (make-instance 'cblas-zbuffer :buffer vec-b)))
	(format t "Before vec-b~%")
	(print-foreign-array vec-b :double 2)
	(blas:axpy 1 (complex 1.0d0 1.0d0) a 1 b 1)
	(format t "After vec-b~%")
	(print-foreign-array vec-b :double 2)))
  
