
;;; 
;;; This example shows how to use ocl.lisp to build a simple program (sum the value un a, b and c and return it on c).
;;; It shows how to build and execute a kernel, set, read and write array arguments.
;;;
;;; Its just an example, not nice or compact.
;;; 
;;; If everithing ok, program output should be:
;;;
;;;  5.3 
;;;  6.1 
;;;  7.1 
;;;  8.1 
;;;  9.1 
;;;  10.1 
;;;  11.1 
;;;  12.2
;;;  [Runnig time info]
;;;



(load (merge-pathnames "ocl.lisp" (pathname-location (current-pathname))))
(use-package 'ocl)



(defvar *samples-x* (list 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0))
(defvar *samples-y* (list 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0))
(defvar *past-result* (list 0.3 0.1 0.1 0.1 0.1 0.1 0.1 0.2))

(defvar *number-of-samples* (length *samples-x*))
(defvar *source-buffer-size* *number-of-samples*)
(defvar *lisp-buffer* (make-array *number-of-samples* :element-type 'single-float :allocation :static))

(defvar *result-buffer-size* 8)
(defvar *global-work-size* 8)
(defvar *local-work-size* 1)
(defvar *float-size* 4)

(setf *program-source* "__kernel void Fitness(__global const float* a, __global const float* b, __global float* c) { c[get_global_id(0)] = c[get_global_id(0)]  + a[get_global_id(0)] + b[get_global_id(0)]; }")

;; Extension to OCL.LISP functionality
(defun check-cl (return-code)
  "Throws an OpenCL error if <return-code> is negative."
  (if (< return-code 0)
      (error "OpenCL error ~A" return-code)))

(defun convert-strings-to-foreign-array (strings &key (allocation :static))
  (let* ((count (length strings))
         (array (fli:allocate-foreign-object 
                 :type '(:pointer (:unsigned :char))
                 :nelems (1+ count)
                 :initial-element nil)))
    (loop for index from 0
          for string in strings
          do (setf (fli:dereference array :index index)
                   (fli:convert-to-foreign-string
                    string
                    :external-format :utf-8)))
    array))

(defun convert-float-foreign-array (floats)
  (fli:allocate-foreign-object
   :type (list :foreign-array :float floats)))

(defun build-program-from-source (context program-source &optional (compile-options ""))
  (fli:with-dynamic-foreign-objects 
    ((size :unsigned (length program-source))
     (error ocl::int))
  (let* ((array (convert-strings-to-foreign-array (list program-source) :allocation :dynamic))
         (program (ocl::|clCreateProgramWithSource| context 1 array size error)))
    (check-cl (fli:dereference error))
    (build-program program compile-options)
    program)))

(defun build-program (program &optional (compile-options ""))
  "Build <program> for <devices>."
  (let ((error (ocl::|clBuildProgram| program 0 nil compile-options nil nil)))
    (check-cl error)))

(defun create-kernel (program name)
  (fli:with-dynamic-foreign-objects
      ((error ocl::int))
    (let ((kernel (ocl::|clCreateKernel| program name error)))
      (check-cl (fli:dereference error))
      kernel)))

(defun set-kernel-argument (kernel argument-index argument-size argument-value)
  (let ((error (ocl::|clSetKernelArg| kernel argument-index argument-size argument-value)))
    (check-cl error)))

(defun create-read-buffer (context size &optional (host-ptr nil))
  (create-buffer-helper context size 34 host-ptr))

(defun create-write-buffer (context size &optional (host-ptr nil))
  (create-buffer-helper context size 1 host-ptr))

(defun create-buffer-helper (context size &optional (flags 0) (host-ptr nil))
  (fli:with-dynamic-foreign-objects ((error ocl::INT))
    (ocl::check2 (ocl::|clCreateBuffer| context flags size host-ptr error)
                 (fli:dereference error))))

;; Example initalization
(defun initialize-opencl-fli ()
  (fli:register-module "OpenCL"
                       :real-name 
                       #+macosx "/System/Library/Frameworks/OpenCL.framework/OpenCL"
                       #+win32 "C:/Windows/System32/OpenCL.dll"
                       #+linux "/usr/lib/libOpenCL.so" 
                       :connection-style :immediate)
  (ocl::doit))

(defun initialize-opencl-context ()  
  (setf *platforms* (ocl:platforms)
        *first-platform* (first (ocl:platforms))
        *devices* (devices *first-platform*)
        *device* (first *devices*)
        *context* (create-context *devices*)
        *queue* (create-queue *context* *device*)))

(defun all-stuff ()
  ;; Program source
  (setf *program* (build-program-from-source *context* *program-source*))
  (build-program *program*)
  (setf *kernel* (create-kernel *program* "Fitness"))
  ;; Create buffers
  (setf *source-buffer-x* (create-read-buffer *context* (* *float-size* *source-buffer-size*) *lisp-buffer*))
  (fli:with-dynamic-foreign-objects
    ((error ocl:buffer))
    (setf (fli:dereference error) *source-buffer-x*)
    (set-kernel-argument *kernel* 0 4 error))
  (setf *source-buffer-y* (create-read-buffer *context* (* *float-size* *source-buffer-size*) *lisp-buffer*))
  (fli:with-dynamic-foreign-objects
      ((error ocl:buffer))
    (setf (fli:dereference error) *source-buffer-y*)
    (set-kernel-argument *kernel* 1 4 error))
  (setf *result-buffer* (create-buffer-helper *context* (* *float-size* *result-buffer-size*) 0))
  (fli:with-dynamic-foreign-objects
      ((error ocl:buffer))
    (setf (fli:dereference error) *result-buffer*)
    (set-kernel-argument *kernel* 2 4 error))
  ;;; Write to buffers
  (let ((x-size (* *float-size* *source-buffer-size*)))
    ;; Create array for x values and clear
    (dotimes (i *source-buffer-size*) 
      (setf (aref *lisp-buffer* i) (nth i *samples-x*)))
    (check-cl (ocl::|clEnqueueWriteBuffer| *queue* *source-buffer-x* 1 0 x-size *lisp-buffer* 0 nil nil))
    ;; Copy values to lisp-array
    (dotimes (i *source-buffer-size*) 
      (setf (aref *lisp-buffer* i) (nth i *samples-y*)))
    (check-cl (ocl::|clEnqueueWriteBuffer| *queue* *source-buffer-y* 1 0 x-size *lisp-buffer* 0 nil nil))
    ;; Create array for y values and clear
    (setf *source-buffer-y* (create-write-buffer *context* x-size))
    (dotimes (i *source-buffer-size*) 
      (setf (aref *lisp-buffer* i) (nth i *past-result*)))
    (check-cl (ocl::|clEnqueueWriteBuffer| *queue* *result-buffer* 1 0 x-size *lisp-buffer* 0 nil nil)))  
  ;;; Enqueue kernel
  (fli:with-dynamic-foreign-objects
      ((global-work-size ocl::size-t)
       (local-work-size ocl::size-t)) 
    (setf (fli:dereference global-work-size) #(8)
          (fli:dereference local-work-size) #(1))
    (check-cl
     (ocl::|clEnqueueNDRangeKernel| *queue* *kernel* 1 0 global-work-size local-work-size 0 nil nil)))
  ;;; Read result
  (check-cl 
   (ocl::|clEnqueueReadBuffer| *queue* *result-buffer* 1 0 (* *float-size* *result-buffer-size*) *lisp-buffer* 0 nil nil))
  ;; Print result array
  (dotimes (i *number-of-samples*)
    (print (aref *lisp-buffer* i))))

(initialize-opencl-fli)
(initialize-opencl-context)	

(time (all-stuff))

