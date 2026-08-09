(in-package :abaco)

(defun parse-shape (data)
  (when (typep data 'sequence)
    (let ((n (parse-shape (elt data 0))))
      (if (every (lambda (sub)
                   (equalp (parse-shape sub) n))
                 data)
          (concatenate 'vector (vector (length data)) n)
          (error "dimensions of data are incongruent")))))

(defun natural-stride (dims)
  (let* ((rank    (length dims))
         (strides (make-array rank))
         (stride 1))
    (loop for i from (1- rank) downto 0 do
          (setf (aref strides i) stride)
          (setf stride (* stride (aref dims i))))
    strides))

(defmacro flatten-copy-t (fn-name type)
  (let* ((cffi-type (abaco-type-to-cffi type))
         (cffi-size (foreign-type-size cffi-type))
         (lisp-type (abaco-type-to-lisp type)))
    `(defun ,fn-name (data stride ptr stride-idx)
       (declare (type (simple-array fixnum (*)) stride)
                (type foreign-pointer ptr)
                (type fixnum stride-idx)
                (optimize (speed 3) (safety 0)))
       (if (= stride-idx (1- (length stride)))
           (loop for x of-type ,lisp-type across (the simple-vector data)
                 for i of-type fixnum from 0 do
                 (setf (mem-aref ptr ',cffi-type i) x))
           (loop for x across (the simple-vector data)
                 for off of-type fixnum from 0 by (* (aref stride stride-idx) ,cffi-size) do
                 (,fn-name x
                           stride
                           (inc-pointer ptr off)
                           (1+ stride-idx)))))))

(flatten-copy-t flatten-copy-s :s)
(flatten-copy-t flatten-copy-d :d)
(flatten-copy-t flatten-copy-c :c)
(flatten-copy-t flatten-copy-z :z)

(defun flatten-copy (data type stride ptr)
  (ecase type
    (:s (flatten-copy-s data stride ptr 0))
    (:d (flatten-copy-d data stride ptr 0))
    (:c (flatten-copy-c data stride ptr 0))
    (:z (flatten-copy-z data stride ptr 0))))

(defcstruct (complex-float :class complex-float-type)
  (real :float)
  (imag :float))

(defcstruct (complex-double :class complex-double-type)
  (real :double)
  (imag :double))

(defmethod translate-from-foreign (ptr (type complex-float-type))
  (with-foreign-slots ((real imag) ptr (:struct complex-float))
    (complex real imag)))

(defmethod translate-into-foreign-memory (value (type complex-float-type) ptr)
  (with-foreign-slots ((real imag) ptr (:struct complex-float))
    (setf real (realpart value))
    (setf imag (imagpart value))))

(defmethod expand-into-foreign-memory (value (type complex-float-type) ptr)
  `(with-foreign-slots ((real imag) ,ptr (:struct complex-float))
     (setf real (realpart ,value))
     (setf imag (imagpart ,value))))

(defmethod translate-from-foreign (ptr (type complex-double-type))
  (with-foreign-slots ((real imag) ptr (:struct complex-double))
    (complex real imag)))

(defmethod translate-into-foreign-memory (value (type complex-double-type) ptr)
  (with-foreign-slots ((real imag) ptr (:struct complex-double))
    (setf real (realpart value))
    (setf imag (imagpart value))))

(defmethod expand-into-foreign-memory (value (type complex-double-type) ptr)
  `(with-foreign-slots ((real imag) ,ptr (:struct complex-double))
     (setf real (realpart ,value))
     (setf imag (imagpart ,value))))

(defun abaco-type-of (data)
  (cond
    ((typep data 'single-float) :s)
    ((typep data 'double-float) :d)
    ((typep data '(complex single-float)) :c)
    ((typep data '(complex double-float)) :z)
    (t (error "unhandled data type ~a" (type-of data)))))

(defun abaco-type-to-cffi (type)
  (case type
    (:s :float)
    (:d :double)
    (:c '(:struct complex-float))
    (:z '(:struct complex-double))))

(defun abaco-type-to-lisp (type)
  (case type
    (:s 'single-float)
    (:d 'double-float)
    (:c '(complex single-float))
    (:z '(complex double-float))))

(defun hint-type (data)
  (if (typep data 'sequence)
      (hint-type (elt data 0))
      (abaco-type-of data)))

(defun validate-type (data)
  (let ((type (parse-type (first data))))
    (if (every (lambda (sub)
             (equal (parse-type sub) type))
           data)
        type
        (error "types of data are inconsistent"))))

(defclass storage ()
  ((ptr
    :initarg :ptr
    :accessor ptr)
   (ref 
    :initarg :ref
    :initform nil
    :accessor ref)))

(defun make-storage (n type)
  (let* ((ptr     (foreign-alloc (abaco-type-to-cffi type) :count n))
         (storage (make-instance 'storage :ptr ptr)))
    (tg:finalize storage
                 (lambda () (foreign-free ptr)))
    storage))

(defclass ndarray ()
  ((storage
    :initarg :storage
    :accessor storage)
   (shape
    :initarg :shape
    :accessor shape)
   (stride
    :initarg :stride
    :accessor stride)))

(defmethod add ((a ndarray) (b ndarray))
  (let* ((n (reduce #'* (shape a)))
         (result (make-instance 'ndarray
                               :data (make-blas n :s)
                               :shape (shape a)
                               :stride (stride a))))
    (blas:copy n (data a) 1 (data result) 1)
    (blas:axpy n 1.0 (data b) 1 (data result) 1)
    result))

(defun ndarray (data)
  (let* ((shape  (parse-shape data))
         (stride (natural-stride shape))
         (type   (hint-type data))
         (n      (reduce #'* shape))
         (result (make-storage n type)))
    (flatten-copy data type stride (ptr result))
    (make-instance 'ndarray :storage result :shape shape :stride stride)))


(ndarray #(1 2 3))

(let* ((a (ndarray
           ((1 2)
            (2 1))))
       (b (ndarray (1 1)))
       (c (@ a b)))
  (format t "~a~%" c))