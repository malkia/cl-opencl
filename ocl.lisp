#+nil
(rename-package "OCL" (gensym))

(defpackage "OCL" (:use "CL")
  (:export
   "PLATFORMS" "PLATFORM-COUNT"
   "DEVICES" "DEVICE-COUNT"
   "OBJECT" "PLATFORM" "DEVICE" "CONTEXT" "QUEUE" "BUFFER" "PROGRAM" "KERNEL" "EVENT" "IMAGE-FORMAT" "SAMPLER"
   "RETAIN" "RELEASE"
   "CREATE-CONTEXT"
   "CREATE-QUEUE"
   "ENQUEUE-COPY-BUFFER"
   ))
(in-package "OCL")

(defparameter +c-types+
  '((STR          (:reference-pass :ef-mb-string))
    (INT          (:signed-integer-type 32))
    (UINT         (:unsigned-integer-type 32))
    (MASK         (:unsigned-integer-type 64))
    (BOOL          UINT)
    (PTR          :pointer)
    (LISP-ARRAY   :lisp-array)
    (PARAM        :signed)
    (SIZE-T       :unsigned)
    (CALLBACK     :pointer)
    (PLATFORM     :pointer)
    (DEVICE       :pointer)
    (CONTEXT      :pointer)
    (QUEUE        :pointer)
    (BUFFER       :pointer)
    (PROGRAM      :pointer)
    (KERNEL       :pointer)
    (EVENT        :pointer)
    (IMAGE-FORMAT :pointer)
    (VOID         :pointer)
    (SAMPLER      :pointer)))

(defparameter +c-structures+
  '((IMAGE-FORMAT)))

(defparameter +c-functions+
  '((|clGetPlatformIDs|              INT     ((UINT num-entries) ((PLATFORM) platforms) ((UINT) num-platforms)))
    (|clGetPlatformInfo|             INT     ((PLATFORM platform) (PARAM platform-info) (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clGetDeviceIDs|                INT     ((PLATFORM platform) (MASK device-type) (UINT num-entries) ((DEVICE) devices) ((UINT) num-devices)))
    (|clGetDeviceInfo|               INT     ((DEVICE device) (PARAM device-info) (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clCreateContext|               CONTEXT ((PTR context-properties) (UINT num-devices) ((DEVICE) devices) (CALLBACK notify) (PTR user-data) ((INT) error-returned)))
    (|clCreateContextFromType|       CONTEXT ((PTR context-properties) (MASK device-type) (CALLBACK notify) (PTR user-data) ((INT) error-returned)))
    (|clRetainContext|               INT     ((CONTEXT context)))
    (|clReleaseContext|              INT     ((CONTEXT context)))
    (|clGetContextInfo|              INT     ((CONTEXT context) (PARAM context-info) (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clCreateCommandQueue|          QUEUE   ((CONTEXT context) (DEVICE device) (MASK command-queue-properties) ((INT) error-returned)))
    (|clRetainQueue|                 INT     ((QUEUE queue)))
    (|clReleaseQueue|                INT     ((QUEUE queue)))
    (|clGetCommandQueueInfo|         INT     ((QUEUE queue) (PARAM command-queue-info) (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clSetCommandQueueProperty|     INT     ((QUEUE queue) (MASK command-queue-properties) (BOOL enabled) ((MASK) command-queue-properties-returned)))
    (|clCreateBuffer|                BUFFER  ((CONTEXT context) (MASK memory-flags) (SIZE-T memory-size) (LISP-ARRAY host-array) ((INT) error-returned)))
    (|clCreateImage2D|               BUFFER  ((CONTEXT context) (MASK memory-flags) ((IMAGE-FORMAT) format) (SIZE-T width) (SIZE-T height) (SIZE-T row-pitch) (LISP-ARRAY host-array) ((INT) error-returned)))
    (|clCreateImage3D|               BUFFER  ((CONTEXT context) (MASK memory-flags) ((IMAGE-FORMAT) format) (SIZE-T width) (SIZE-T height) (SIZE-T depth) (SIZE-T row-pitch) (SIZE-T slice-pitch) (LISP-ARRAY host-array) ((INT) error-returned)))
    (|clRetainMemObject|             INT     ((BUFFER buffer)))
    (|clReleaseMemObject|            INT     ((BUFFER bufffer)))
    (|clGetSupportedImageFormats|    INT     ((CONTEXT context) (MASK memory-flags) (PARAM image-type) (UINT num-entries) ((IMAGE-FORMAT) image-formats) ((UINT) num-image-formats-returned)))
    (|clGetMemObjectInfo|            INT     ((BUFFER buffer) (PARAM memory-info) (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clGetImageInfo|                INT     ((BUFFER buffer) (PARAM image-info) (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clCreateSampler|               SAMPLER ((CONTEXT context) (BOOL normalized-coords) (UINT addressing-mode) (UINT filter-mode) ((INT) error-returned)))
    (|clRetainSampler|               INT     ((SAMPLER sampler)))
    (|clReleaseSampler|              INT     ((SAMPLER sampler)))
    (|clGetSamplerInfo|              INT     ((SAMPLER sampler) (PARAM sampler-info) (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clCreateProgramWithSource|     PROGRAM ((CONTEXT context) (UINT string-count) (PTR strings) ((SIZE-T) string-lengths) ((INT) error-returned)))
    (|clCreateProgramWithBinary|     PROGRAM ((CONTEXT context) (UINT device-count) ((DEVICE) devices) ((SIZE-T) binary-lengths) (PTR binaries) ((INT) binary-status) ((INT) error-returned)))
    (|clRetainProgram|               INT     ((PROGRAM program)))
    (|clReleaseProgram|              INT     ((PROGRAM program)))
    (|clBuildProgram|                INT     ((PROGRAM program) (UINT device-count) ((DEVICE) devices) (STR options) (CALLBACK notify) (PTR user-data)))
    (|clUnloadCompiler|              VOID    )
    (|clGetProgramInfo|              INT     ((PROGRAM program) (PARAM program-info) (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-returned)))
    (|clCreateKernel|                KERNEL  ((PROGRAM program) (STR kernel-name) ((INT) error-returned)))
    (|clCreateKernelsInProgram|      KERNEL  ((PROGRAM program) (UINT kernel-count) ((KERNEL) kernels) ((UINT) kernel-count-returned)))
    (|clRetainKernel|                INT     ((KERNEL kernel)))
    (|clReleaseKernel|               INT     ((KERNEL kernel)))
    (|clSetKernelArg|                INT     ((KERNEL kernel) (UINT argument-index) (SIZE-T argument-size) (PTR argument-value)))
    (|clGetKernelInfo|               INT     ((KERNEL kernel) (PARAM kernel-info) (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clGetKernelWorkGroupInfo|      INT     ((KERNEL kernel) (DEVICE devicee) (PARAM kernel-workgroup-info) (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clWaitForEvents|               INT     ((UINT event-count) ((EVENT) events)))
    (|clGetEventInfo|                INT     ((EVENT event) (PARAM event-info (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned))))
    (|clRetainEvent|                 INT     ((EVENT event)))
    (|clReleaseEvent|                INT     ((EVENT event)))
    (|clGetEventProfilingInfo|       INT     ((EVENT event) (PARAM profiling-info) (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clFlush|                       INT     ((QUEUE queue)))
    (|clFinish|                      INT     ((QUEUE queue)))
    (|clEnqueueReadBuffer|           INT     ((QUEUE queue) (BUFFER buffer) (BOOL blocking-read)  (SIZE-T offset) (SIZE-T cb) (LISP-ARRAY array) (UINT waiting-event-count) ((EVENT) waiting-events) ((EVENT) event-returned)))
    (|clEnqueueWriteBuffer|          INT     ((QUEUE queue) (BUFFER buffer) (BOOL blocking-write) (SIZE-T offset) (SIZE-T cb) (LISP-ARRAY array) (UINT waiting-event-count) ((EVENT) waiting-events) ((EVENT) event-returned)))
    (|clEnqueueCopyBuffer|           INT     ((QUEUE queue) (BUFFER src-buffer) (BUFFER dst-buffer) (SIZE-T src-offset) (SIZE-T dst-offset) (SIZE-T size) (UINT waiting-event-count) ((EVENT) waiting-events) ((EVENT) event-returned)))
    (|clEnqueueReadImage|            INT     ((QUEUE queue) (BUFFER buffer) (BOOL blocking-read)  ((SIZE-T 3) origin) ((SIZE-T 3) region) (SIZE-T row-pitch) (SIZE-T slice-pitch) (LISP-ARRAY array) (UINT waiting-event-count) ((EVENT) waiting-events) ((EVENT) event-returned)))
    (|clEnqueueWriteImage|           INT     ((QUEUE queue) (BUFFER buffer) (BOOL blocking-write) ((SIZE-T 3) origin) ((SIZE-T 3) region) (SIZE-T row-pitch) (SIZE-T slice-pitch) (LISP-ARRAY array) (UINT waiting-event-count) ((EVENT) waiting-events) ((EVENT) event-returned)))
    (|clEnqueueCopyImage|            INT     ((QUEUE queue) (BUFFER src-buffer) (BUFFER dst-buffer) ((SIZE-T 3) src-origin) ((SIZE-T 3) region) (SiZE-T dst-offset) (UINT waiting-event-count) ((EVENT) waiting-events) ((EVENT) event-returned)))
    (|clEnqueueCopyImageToBuffer|    INT)
    (|clEnqueueCopyBufferToImage|    INT)
    (|clEnqueueMapBuffer|            INT)
    (|clEnqueueMapImage|             INT)
    (|clEnqueueUnmapMemObject|       INT)
    (|clEnqueueNDRangeKernel|        INT)
    (|clEnqueueTask|                 INT)
    (|clEnqueueNativeKernel|         INT)
    (|clEnqueueMarker|               INT     ((QUEUE queue) ((EVENT) event-returned)))
    (|clEnqueueWaitForEvents|        INT     ((QUEUE queue) (UINT event-count) ((EVENT) events)))
    (|clEnqueueBarrier|              INT     ((QUEUE queue)))
    (|clGetExtensionFunctionAddress| PTR     ((STR function-name)))
    (|clCreateFromGLBuffer|          BUFFER  ((CONTEXT context) (MASK memory-flags) (SIZE-T buffer-object) ((INT) error-returned)))
    (|clCreateFromGLTexture2D|       BUFFER  ((CONTEXT context) (MASK memory-flags) (SIZE-T texture-target) (SIZE-T mip-level) (SIZE-T texture-object) ((INT) error-returned)))
    (|clCreateFromGLTexture3D|       BUFFER  ((CONTEXT context) (MASK memory-flags) (SIZE-T texture-target) (SIZE-T mip-level) (SIZE-T texture-object) ((INT) error-returned)))
    (|clCreateFromGLRenderbuffer|    BUFFER  ((CONTEXT context) (MASK memory-flags) (SIZE-T render-buffer) ((INT) error-returned)))
    (|clGetGLObjectInfo|             INT     ((BUFFER buffer) ((UINT) gl-object-type) ((SIZE-T) gl-object-name)))
    (|clGetGLTextureInfo|            INT     ((BUFFER buffer) (UINT gl-texture-info) (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    (|clEnqueueAcquireGLObjects|     INT     ((QUEUE queue) (UINT buffer-count) ((BUFFER) buffers) (UINT waiting-event-count) ((EVENT) waiting-events) ((EVENT) event-returned)))
    (|clReleaseAcquireGLObjects|     INT     ((QUEUE queue) (UINT buffer-count) ((BUFFER) buffers) (UINT waiting-event-count) ((EVENT) waiting-events) ((EVENT) event-returned)))
    #+macosx
    (|clGetGLContextInfoAPPLE|       INT     ((CONTEXT context) (PTR platform-gl-context) (UINT gl-platform-info) (SIZE-T param-value-size) (PTR param-value) ((SIZE-T) param-value-size-returned)))
    #+macosx
    (|clSetMemObjectDestructoAPPLE|  INT     ((BUFFER buffer) (CALLBACK notify) (PTR user-data)))
    #+macosx
    (|clLogMessagesToSystemLogAPPLE| VOID    ((STR error-string) (PTR private-info) (SIZE-T cb) (PTR user-data)))
    #+macosx
    (|clLogMessagesToStdoutAPPLE|    VOID    ((STR error-string) (PTR private-info) (SIZE-T cb) (PTR user-data)))
    #+macosx
    (|clLogMessagesToStderrAPPLE|    VOID    ((STR error-string) (PTR private-info) (SIZE-T cb) (PTR user-data)))))

(fli:register-module "OpenCL"
                     :real-name #+macosx "/System/Library/Frameworks/OpenCL.framework/OpenCL"
                     #+win32 "C:/Windows/System32/OpenCL.dll"
                     #+linux "/usr/lib/libOpenCL.so"
                     :connection-style :immediate)

;;; Create all bindings
(defmacro doit ()
  `(progn
     ,@(loop for l in +c-types+ collect
             `(fli:define-c-typedef ,(first l) ,(second l)))
     ,@(loop for l in +c-functions+ collect
             `(fli:define-foreign-function (,(first l) ,(format nil "~A" (first l)))
                  ,(loop for p in (third l)
                         collect `(,(second p)
                                   ,(if (atom (first p))
                                        (first p)
                                      `(:pointer ,(first (first p))))))
                :result-type ,(second l)
                :module "OpenCL"
                :calling-convention #+win32 :stdcall #-win32 :cdecl))))

(doit)

#+nil
(pprint (sort (let (r) (do-symbols (s *package* r)
                         (when (eq (symbol-package s) *package*)
                           (push s r))
                         r))
              #'string>))

(defun check (return-code &optional (alternative-return-code nil alternative-return-code-supplied-p))
  "Throws an OpenCL error if the return-code is NEGATIVE, otherwise returns it, or returns an alternative-return-code if supplied. Use this with |clXxx| functions return an error."
  (if (< 0 return-code)
      (error "OpenCL error ~A" return-code)
    (if alternative-return-code-supplied-p
        alternative-return-code
      return-code)))

(defun check2 (return-code &optional (error-code nil error-code-supplied-p))
  "Throws an OpenCL error if the return-code is NEGATIVE, otherwise returns it, or returns an alternative-return-code if supplied. Use this with |clXxx| functions returning an object, and returning the error as reference (usually last parameter of the function call)"
  (format t "return-code ~A~&error-code ~A~&" return-code error-code)
  (check (if error-code-supplied-p
             error-code
           return-code) return-code))
  
(defun platform-count ()
  "Returns the number of OpenCL platforms."
  (fli:with-dynamic-foreign-objects ((count UINT))
    (check (|clGetPlatformIDs| 0 nil count)
           (fli:dereference count))))

(defun platforms ()
  "Returns a list with available OpenCL platforms."
  (let ((count (platform-count)))
    (fli:with-dynamic-foreign-objects ((platforms PLATFORM :nelems count))
      (check (|clGetPlatformIDs| count platforms nil)
             (loop for n :below count collect (fli:dereference platforms :index n))))))

(defun device-count (platform)
  "Returns the number of devices for the given platform."
  (fli:with-dynamic-foreign-objects ((count UINT))
    (check (|clGetDeviceIDs| platform #xFFFFFFFF 0 nil count)
           (fli:dereference count))))

(defun devices (platform)
  "Returns a list with the devices of the given platform."
  (let ((count (device-count platform)))
    (fli:with-dynamic-foreign-objects ((devices DEVICE :nelems count))
      (check (|clGetDeviceIDs| platform #xFFFFFFFFF count devices nil)
             (loop for n :below count collect (fli:dereference devices :index n))))))

(defun create-context (devices)
  "Creates an OpenCL context from the list of the devices."
  (let ((device-count (length devices)))
    (fli:with-dynamic-foreign-objects
        ((devices DEVICE :nelems device-count :initial-contents devices)
         (error INT))
      (check2 (|clCreateContext| nil device-count devices nil nil error)
              (fli:dereference error)))))

(defun create-queue (context device &optional (queue-flags 0 queue-flags-supplied-p))
  "Creates an OpenCL queue in the given context."
  (fli:with-dynamic-foreign-objects ((error INT))
    (values (|clCreateCommandQueue| context device queue-flags error)
            (fli:dereference error))))

(flet ((create-buffer-helper (context size &optional (flags 0) (host-ptr nil))
         (fli:with-dynamic-foreign-objects ((error INT))
           (check2 (|clCreateBuffer| context flags size host-ptr error)
                   (fli:dereference error)))))
  (defun create-read-buffer (context size &optional (host-ptr nil))
    (create-buffer-helper context size 2 host-ptr))
  (defun create-write-buffer (context size &optional (host-ptr nil))
    (create-buffer-helper context size 1 host-ptr))
  (defun create-buffer (context size &optional (host-ptr nil))
    (create-buffer-helper context size 0 host-ptr)))

(defun dalloc (type initial-contents)
  (let ((length (length initial-contents)))
    (if (zerop length)
        nil
      (fli:allocate-dynamic-foreign-object :type type :initial-contents initial-contents :nelems length))))

(defun enqueue-read-buffer (queue buffer lisp-array size &key (blocking 1) (waiting-events nil) (offset 0))
  (let ((waiting-count (length waiting-events)))
    (fli:with-dynamic-foreign-objects ((event EVENT))
      (check (|clEnqueueReadBuffer| queue buffer blocking offset size lisp-array waiting-count (dalloc EVENT waiting-events) event)
             (fli:dereference event)))))

(defun enqueue-copy-buffer (queue src-buffer dst-buffer size &key (waiting-events nil) (src-offset 0) (dst-offset 0))
  (let ((waiting-count (length waiting-events)))
    (fli:with-dynamic-foreign-objects ((event EVENT))
      (check (|clEnqueueCopyBuffer| queue src-buffer dst-buffer src-offset dst-offset size waiting-count (dalloc EVENT waiting-events) event)
             (fli:dereference event)))))

(defvar *platforms* (platforms))
(defvar *platform* (first *platforms*))
(defvar *devices* (devices *platform*))
(defvar *context* (create-context *devices*))
(defvar *queues* (loop for device in *devices* collect (create-queue *context* device)))
(defvar *queue* (first *queues*))
(defvar *buffer-size* (* 1024 1024))
(defvar *read-buffer* (create-read-buffer *context* *buffer-size*))
(defvar *write-buffer* (create-write-buffer *context* *buffer-size*))
(defvar *buffer* (create-buffer *context* *buffer-size*))
(defvar *lisp-buffer* (make-array (/ *buffer-size* 4) :element-type 'single-float :allocation :static))

#+nil
(platform-count)

(defclass object() ((object :initarg :object)))
(defclass platform  (object) ((devices)))
(defclass device    (object) ())
(defclass context   (object) ((devices :initarg :devices)))
(defclass queue     (object) ((device  :initarg :device)))
(defclass buffer    (object) ())
(defclass sampler   (object) ())
(defclass program   (object) ((source) (devices)))
(defclass kernel    (object) ((program) (name)))
(defclass event     (object) ())
(defclass parameter () ((name) (id) (type)))

(defmethod release ((context context)) (|clReleaseContext|   (slot-value context 'object)))
(defmethod release ((queue     queue)) (|clReleaseQueue|     (slot-value queue   'object)))
(defmethod release ((buffer   buffer)) (|clReleaseMemObject| (slot-value buffer  'object)))
(defmethod release ((sampler sampler)) (|clReleaseSampler|   (slot-value sampler 'object)))
(defmethod release ((program program)) (|clReleaseProgram|   (slot-value program 'object)))
(defmethod release ((kernel   kernel)) (|clReleaseKernel|    (slot-value kernel  'object)))
(defmethod release ((event     event)) (|clReleaseEvent|     (slot-value event   'object)))
(defmethod retain  ((context context)) (|clRetainContext|    (slot-value context 'object)))
(defmethod retain  ((queue     queue)) (|clRetainQueue|      (slot-value queue   'object)))
(defmethod retain  ((buffer   buffer)) (|clRetainMemObject|  (slot-value buffer  'object)))
(defmethod retain  ((sampler sampler)) (|clRetainSampler|    (slot-value sampler 'object)))
(defmethod retain  ((program program)) (|clRetainProgram|    (slot-value program 'object)))
(defmethod retain  ((kernel   kernel)) (|clRetainKernel|     (slot-value kernel  'object)))
(defmethod retain  ((event     event)) (|clRetainEvent|      (slot-value event   'object)))

(defparameter +parameters+
  '((platform profile                       #x0900 STRING)
    (platform version                       #x0901 STRIGN)
    (platform name                          #x0902 STRING)
    (platform vendor                        #x0903 STRING)
    (platform extensions                    #x0904 STRING)
    (device   type                          #x1000 MASK)
    (device   vendor-id                     #x1001 UINT)
    (device   max-compute-units             #x1002 UINT)
    (device   max-work-item-dimensions      #x1003 UINT)
    (device   max-work-item-sizes           #x1004 VOID)
    (device   max-work-group-size           #x1005 SIZE-T)
    (device   preferred-vector-width-char   #x1006 UINT)
    (device   preferred-vector-width-short  #x1007 UINT)
    (device   preferred-vector-width-int    #x1008 UINT)
    (device   preferred-vector-width-long   #x1009 UINT)
    (device   preferred-vector-width-float  #x100A UINT)
    (device   preferred-vector-width-double #x100B UINT)
    (device   max-clock-frequency           #x100c UINT)
    (device   address-bits                  #x100d UINT)
    (device   max-mem-alloc-size            #x100e ULONG)
    (device   image-support                 #x100f UINT)
    (device   max-read-image-args           #x1010 UINT)
    (device   max-write-image-args          #x1011 UINT)
    (device   image2d-max-width             #x1012 SIZE-T)
    (device   image2d-max-height            #x1013 SIZE-T)
    (device   image3d-max-width             #x1014 SIZE-T)
    (device   image3d-max-height            #x1015 SIZE-T)
    (device   image3d-max-depth             #x1016 SIZE-T)
    (device   max-samplers                  #x1017 UINT)
    (device   max-parameter-size            #x1018 SIZE-T)
    (device   mem-base-addr-align           #x1019 UINT)
    (device   min-data-type-align-size      #x101A UINT)
    (device   single-fp-config              #x101B ULONG)
    (device   global-mem-cache-type         #x101C UINT)
    (device   global-mem-cacheline-size     #x101D UINT)
    (device   global-mem-cache-size         #x101E ULONG)
    (device   global-mem-size               #x101F ULONG)
))    

(defun get-info (param object1 &optional object2 &key (f1 #'|clGetDeviceInfo|) (f2 nil))
  (assert (not (eq (null f1) (null f2))))
  (fli:with-dynamic-foreign-objects ((size :unsigned 0))
    (let ((e1 (if (null f2) (funcall f1 object1 param 0 nil size)
                (funcall f2 object1 object2 param 0 nil size))))
      (if (zerop e1)
          (let ((size (fli:dereference size)))
            (fli:with-dynamic-foreign-objects ((data :byte :nelems size))
              (let ((e2 (if (null f2) (funcall f1 object1 param size data nil)
                          (funcall f2 object1 object2 param size data nil))))
                (if (zerop e2)
                    (let ((array (make-array size :element-type '(unsigned-byte 8))))
                      (dotimes (n size) (setf (aref array n)
                                              (fli:dereference data :index n)))
                      (values array e2))
                  (values nil e2)))))
        (values nil e1)))))


(defmethod initialize-instance :after ((context context) &rest initargs &key &allow-other-keys)
  (with-slots (object devices) context
        (make-context devices)))

#|
(defmethod info-function ((platform platform)) #'cl-get-platform-info)
(defmethod info-function ((device     device)) #'cl-get-device-info)
(defmethod info-function ((context   context)) #'cl-get-context-info)

(defgeneric info (param object &optional object2))

(defmethod info (param (platform platform) &optional object2)
  (let ((platform (slot-value platform 'object)))
    (fli:with-dynamic-foreign-objects ((count :unsigned))
      (let ((error1 (cl-get-platform-info platform param 0 nil count)))
        (let ((count (fli:dereference count)))
          (fli:with-dynamic-foreign-objects ((param-value :byte :nelems count))
            (let ((error2 (cl-get-platform-info platform param count param-value nil)))
              (values param-value count error1 error2))))))))

(fli:define-foreign-callable ("notify" :result-type :void)
    ((error-info   (:reference-return :ef-mb-string))
     (private-info :pointer)
     (private-size :int)
     (user-data    :pointer))
  (format t "OpenCL: error    = ~A~&" error-info)
  (format t "OpenCL: private  = ~A ~A~&" private-size private-info)
  (format t "OpenCL: userdata = ~A~&" user-data))

(defmacro make-buffer-helper (buffer-flags)
  `(fli:with-dynamic-foreign-objects ((error s32))
     (values (cl-create-buffer context ,buffer-flags size nil error)
             (fli:dereference error))))
(defun make-buffer       (context size) (make-buffer-helper 0))
(defun make-read-buffer  (context size) (make-buffer-helper 2))
(defun make-write-buffer (context size) (make-buffer-helper 1))

(defparameter *platforms* (platforms))
(defparameter *platform* (first *platforms*))
(defparameter *devices* (devices (first *platforms*)))
(defparameter *device* (first *devices*))
(defparameter *context* (make-context *devices*))

(defconstant +error-codes+
  '((SUCCESS                         .   0)
    (DEVICE-NOT-FOUND                .  -1)
    (DEVICE-NOT-AVAILABLE            .  -2)
    (COMPILER-NOT-AVAILABLE          .  -3)
    (BUFFER-ALLOCATION-FAILURE       .  -4)
    (OUT-OF-RESOURCES                .  -5)
    (OUT-OF-HOST-MEMORY              .  -6)
    (PROFILING-NOT-AVAILABLE         .  -7)
    (MEMORY-COPY-OVERLAP             .  -8)
    (IMAGE-FORMAT-MISMATCH           .  -9)
    (IMAGE-FORMAT-NOT-SUPPORTED      . -10)
    (BUILD-PROGRAM-FAILURE           . -11)
    (MAP-FAILURE                     . -12)
    (INVALID-VALUE                   . -30)
    (INVALID-DEVICE-TYPE             . -31)
    (INVALID-PLATFORM                . -32)
    (INVALID-DEVICE                  . -33)
    (INVALID-CONTEXT                 . -34)   
    (INVALID-QUEUE-PROPERTIES        . -35)  
    (INVALID-COMMAND-QUEUE           . -36)        
    (INVALID-HOST-PTR                . -37)         
    (INVALID-MEM-OBJECT              . -38)   
    (INVALID-IMAGE-FORMAT-DESCRIPTOR . -39)   
    (INVALID-IMAGE-SIZE              . -40)         
    (INVALID-SAMPLER                 . -41)         
    (INVALID-BINARY                  . -42)         
    (INVALID-BUILD-OPTIONS           . -43)         
    (INVALID-PROGRAM                 . -44)         
    (INVALID-PROGRAM-EXECUTABLE      . -45)         
    (INVALID-KERNEL-NAME             . -46)         
    (INVALID-KERNEL-DEFINITION       . -47)         
    (INVALID-KERNEL                  . -48)
    (INVALID-ARG-INDEX               . -49)       
    (INVALID-ARG-VALUE               . -50)         
    (INVALID-ARG-SIZE                . -51)         
    (INVALID-KERNEL-ARGS             . -52)         
    (INVALID-WORK-DIMENSION          . -53)         
    (INVALID-WORK-GROUP-SIZE         . -54)         
    (INVALID-WORK-ITEM-SIZE          . -55)         
    (INVALID-GLOBAL-OFFSET           . -56)         
    (INVALID-EVENT-WAIT-LIST         . -57)         
    (INVALID-EVENT                   . -58)         
    (INVALID-OPERATION               . -59)         
    (INVALID-GL-OBJECT               . -60)         
    (INVALID-BUFFER-SIZE             . -61)         
    (INVALID-MIP-LEVEL               . -62)))


#+nil
(platform-count) 

#+nil
(platforms)

#+nil
(device-count (first (platforms)))

#+nil
(devices (first (platforms)))

(eval `(locally ,@(loop for l in +c-types+ collect `(fli:define-c-typedef ,(first l) ,(rest l)))))
|#

