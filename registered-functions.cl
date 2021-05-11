;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2021-05-10 00:04:26>

(in-package "POLARCL")

(defun function-authorizer (handler request registered-function)
  (log2:trace "Authorizing ~a for request ~a" registered-function request)
  ;; DONT call authenticate, authenticate calls the handler's authorizer!
  (default-authorizer handler request))

(defstruct registered-function name symbol (authorizer #'function-authorizer))

(defmacro with-func-from-path ((fsym handler request) &body body)
  `(destructuring-bind (fname &rest namespace)
       (reverse (path ,request))
     (let ((registered-function (gethash fname *registered-functions-ht*)))
       (unless registered-function
         (log2:warning "Unregistered function ~s" fname)
         (error "Function ~s is not registered" fname))
       (cond
         ((funcall (registered-function-authorizer registered-function)
                   ,handler
                   ,request
                   registered-function) 
          (let ((,fsym (registered-function-symbol registered-function)))
            (log2:debug "Executing ~a" ,fsym)
            ,@body))
         (t
          (error "Unauthorized"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2 Calling URL-specified functions
;;;
;;; URL-specified functions must be registered with REGISTER-FUNCTION.

(defvar *registered-functions-ht* (make-hash-table :test #'equal))

(defun register-function (path &key (symbol (symbol-from-path path)) (authorizer #'function-authorizer))
  (setf (gethash path *registered-functions-ht*)
        (make-registered-function :name path
                                  :symbol symbol
                                  :authorizer authorizer)))

(defun symbol-from-path (path)
  ;; The client uses $path to designate the function.  
  ;; Use . as the separator to make it look nicer in Javascript
  (destructuring-bind (package-name symbol-name)
      (cl-utilities:split-sequence #\. path)
    (find-symbol symbol-name (find-package (string-upcase package-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2-1 'Request Functions'
;;;
;;; rfuncs are called with the request & response and return their result by setting
;;; the response body & other repsonse parameters

(defclass rfunc-handler (handler)
  ())

(defmethod handle-response ((server http-server) (filter t) (handler rfunc-handler) (request t) (response t))
  (with-func-from-path (fsym handler request)
    (funcall fsym handler request response)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2-2 'Query
;;;
;;; qfunc receives the query parameters as keyword arguments. The result is returned
;;; as the response body

(defclass qfunc-handler (handler)
  ())

(defmethod handle-response ((server http-server) (filter t) (handler qfunc-handler) (request t) (response t))
  (with-func-from-path (fsym handler request)
    (log2:trace "Executing ~a" fsym) 
    (let ((args (loop
                   :for pair :in (parameters request)
                   :collect (intern (car pair) :keyword)
                   :collect (cadr pair))))
      (setf (http-body response)
            (apply fsym handler request response args)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
