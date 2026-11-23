;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2023-04-23 00:21:55>

(in-package "POLARCL")

(defstruct registered-function name symbol)

(defmacro with-func-from-path ((fsym handler request response) &body body)
  (declare (ignore handler response))
  `(let* ((fname (car (last (path ,request))))
          (entry (gethash fname *registered-functions-ht*)))
     (unless entry (error "Function is not registered"))
     (let ((,fsym (registered-function-symbol entry))) ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2 Calling URL-specified functions
;;;
;;; URL-specified functions must be registered with REGISTER-FUNCTION.

(defvar *registered-functions-ht* (make-hash-table :test #'equal))

(defun register-function (path &key (symbol (symbol-from-path path)))
  (setf (gethash path *registered-functions-ht*)
        (make-registered-function :name path
                                  :symbol symbol)))

(defun symbol-from-path (path)
  ;; The client uses $path to designate the function.  
  ;; Use . as the separator to make it look nicer in Javascript
  (destructuring-bind (package-name symbol-name)
      (cl-utilities:split-sequence #\. path)
    (find-symbol symbol-name (find-package (string-upcase package-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2-1 'Request functions'
;;;
;;; rfuncs are called with the request & response and return their through the
;;; response body & response headers

(defclass rfunc-handler (handler)
  ())

(defmethod handle-response ((server http-server) (filter t) (handler rfunc-handler) (connection t) (request t) (response t))
  (with-func-from-path (fsym handler request response)
    (funcall fsym handler request response)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2-2 'Query functions'
;;;
;;; qfunc receives the query parameters as keyword arguments. The result is returned
;;; as the response body

(defclass qfunc-handler (handler)
  ())

(defmethod handle-response ((server http-server) (filter t) (handler qfunc-handler) (connection t) (request t) (response t))
  (with-func-from-path (fsym handler request response)
    (log2:trace "Executing ~a" fsym) 
    (let ((args (loop
                   :for pair :in (parameters request)
                   :collect (intern (car pair) :keyword)
                   :collect (cadr pair))))
      (setf (http-body response)
            (apply fsym handler request response args)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
