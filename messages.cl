;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2017-03-01 21:02:25>

(in-package "POLARCL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP Request and Response

(defclass http-basic ()
  ((protocol :accessor http-protocol :initarg :protocol)
   (port :accessor http-port :initarg :port)
   (headers :accessor headers :initarg :headers :initform ())
   (body :accessor body :initarg :body :initform "")))
  
(defclass http-response (http-basic)
  ((status-code :accessor status-code :initarg :status-code)
   (status-text :accessor status-text :initarg :status-text)))

(defmethod print-object ((object http-response) stream)
  (format stream "<RESPONSE ~a ~a ~a>"
          (status-code object)
          (status-text object)
          (ignore-errors (subseq (body object) 0 10))))

          
(defun make-http-response (&rest args &key host port headers body status-code status-text)
  (declare (ignorable host port headers body status-code status-text))
  (apply #'make-instance 'http-response args))

(defclass http-request (http-basic)
  ((http-method :accessor http-method :allocation :class)
   ;; PATH is a list similar to pathname-directory. HTTP-PATH converts to a string.
   (path :accessor path :initarg :path :initform ())
   (parameters :accessor parameters :initarg :parameters)
   (fragment :accessor fragment :initarg :fragment)
   (http-version :accessor http-version :initarg :http-version :initform "HTTP/1.1")))

(defun path-string (request)
  (format () "/~{~a~^/~}" (path request)))

(defun format-request-info (request)
  (with-output-to-string (s)
    (format s "~a HOST ~a PATH ~{/~a~} QUERY ~{~a~^;~} USERAGENT ~a"
            (http-method request)
            (http-header request :|host|)
            (path request)
            (parameters request)
            (http-header request :|user-agent|))))

(defclass http-get (http-request)
  ((http-method :initform :get)))
(defun make-http-get (&rest args &key protocol host port headers body path parameters fragment http-version)
  (declare (ignorable host port headers body path parameters fragment http-version))
  (apply #'make-instance 'http-get args))

(defclass http-post (http-request)
  ((http-method :initform :post)))
(defun make-http-post (&rest args &key protocol host port headers body path parameters fragment http-version)
  (declare (ignorable host port headers body path parameters fragment http-version))
  (apply #'make-instance 'http-post args))

(defclass http-put (http-request)
  ((http-method :initform :put)))
(defun make-http-put (&rest args &key protocol host port headers body path parameters fragment http-version)
  (declare (ignorable host port headers body path parameters fragment http-version))
  (apply #'make-instance 'http-put args))

(defclass http-options (http-request)
  ((http-method :initform :options)))
(defun make-http-options (&rest args &key protocol host port headers body path parameters fragment http-version)
   (declare (ignorable host port headers body path parameters fragment http-version))
   (apply #'make-instance 'http-options args))

(defclass http-connect (http-request)
  ((http-method :initform :connect)))
(defun make-http-connect (&rest args &key protocol host port headers body path parameters fragment http-version)
  (declare (ignorable host port headers body path parameters fragment http-version))
  (apply #'make-instance 'http-connect args))

(defun http-host (request)
  (http-header request ':|host|))

(defun http-path (request)
  (format () "/~{~a~^/~}" (path request)))

(defun http-header (r name)
  ;; Don't modify the header here
  (cdr (assoc name (headers r))))

(defun set-http-header (r name value)
  (if (null (assoc name (headers r)))
    (push (cons name value) (headers r))
    (setf (cdr (assoc name (headers r))) value)))

(defsetf http-header set-http-header)

(defun http-body (r)
  (body r))

(defun set-http-body (r value)
  (setf (http-header r :|Content-Length|)
        (length value))
  (setf (body r) value))

(defsetf http-body set-http-body)


(defgeneric get-cookie (r name))
(defmethod  get-cookie ((r http-request) name)
  (let ((cookies (cdr (assoc :|Cookie| (headers r)))))
    (or (getf (extra-parameters r) name)
        (cadr (assoc name cookies)))))

(defgeneric set-cookie (r name value))
(defmethod set-cookie ((r http-request) name value)
  (set-cookie% r name value :|Cookie|))
(defmethod set-cookie ((r http-response) name value)
  (set-cookie% r name value :|Set-Cookie|))
(defun set-cookie% (r name value ckey)
  (when (null (assoc ckey (headers r)))
    (push (cons ckey nil) (headers r)))
  (let ((cookies (assoc ckey (headers r))))
    (cond
      ((null (assoc name (cdr cookies)))
       (push (list name value) (cdr cookies)))
      (t
       (rplacd (assoc name (cdr cookies))
               (list value))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predefined / template responses

(defun make-ok-response (request)
  (declare (ignorable request))
  (make-http-response :status-code "200"
                      :status-text "OK"
                      :headers (list (cons :|Access-Control-Allow-Origin|  "*")
                                     (cons :|Content-Type| "text/html")
                                     (cons :|Connection| "keep-alive")
                                     (cons :|Server| "PolarSeal"))))

(defun make-redirect-response (request location)
  (declare (ignorable request))
  (make-http-response :status-code "301"
                      :status-text "Moved Permanently"
                      :headers (list
                                (cons :|Location|  location)
                                (cons :|Connection| "close"))))

(defun make-authenticate-response (handler request)
  (declare (ignorable request))
  (make-http-response :status-code "401"
                      :status-text "Not Authorized"
                      :headers (list (cons :|WWW-Authenticate| (format () "Basic realm=~a" (handler-realm handler)))
                                     (cons :|Connection| "keep-alive"))))

(defun make-error-response (&key
                              (status-code 500)
                              (status-text "An error occurred")
                              (headers (list (cons :|Connection| "keep-alive")))
                              (body "An error occurred. We're sorry."))
  (make-http-response :status-code status-code
                      :status-text status-text
                      :headers headers
                      :body body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Accessors for Response

(defun http-transfer-encoding (response)
  (http-header response :|transfer-encoding|))
(defun http-content-encoding (response)
  (http-header response :|content-encoding|))
(defun http-content-length (response)
  (http-header response :|content-length|))
(defun http-content-type (response)
  (http-header response :|content-type|))
(defun http-charset (request)
  (let* ((content-type (http-content-type request))
         (encoding (if (null content-type)
                       *default-charset*
                       (or
                        (cadr (assoc "charset"
                                     (mapcar (lambda (e)
                                               (cl-utilities:split-sequence #\= e))
                                             (cl-utilities:split-sequence #\; content-type))
                                     :key #'strip-name
                                     :test #'string=))
                        *default-charset*))))
    (intern
     (string-upcase (strip-name encoding))
     :keyword)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
