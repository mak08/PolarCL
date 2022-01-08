;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2022-01-08 01:35:17>

(in-package "POLARCL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP Request and Response

(defclass http-basic ()
  ((connection :accessor connection :initarg :connection)
   (port :accessor http-port :initarg :port)
   (headers :accessor headers :initarg :headers :initform ())
   (body :accessor body :initarg :body :initform "")
   (authentication-state :accessor authentication-state :initform nil)))

(defmethod http-protocol ((request http-basic))
  (etypecase (connection request)
    (mbedtls:ssl-stream :https)
    (mbedtls:plain-stream :http)))

(defclass http-header ()
  ((field-name :accessor field-name :initarg :name) 
   (field-value :accessor field-value :initarg :value)))
(defmethod print-object ((object http-header) stream)
  (format stream "<HEADER ~a: ~a>"
          (field-name object)
          (field-value object)))

(defclass http-response (http-basic)
  ((request :reader request :initarg :request)
   (status-code :accessor status-code :initarg :status-code)
   (status-text :accessor status-text :initarg :status-text)))
(defmethod print-object ((object http-response) stream)
  (format stream "<RESPONSE ~a ~a ~a>"
          (status-code object)
          (status-text object)
          (ignore-errors (subseq (body object) 0 10))))
          
(defun make-http-response (&rest args &key request host port headers body status-code status-text)
  (declare (ignorable host port headers body status-code status-text))
  (apply #'make-instance 'http-response args))

(defclass http-request (http-basic)
  ((http-method :accessor http-method :allocation :class)
   ;; PATH is a list similar to pathname-directory. HTTP-PATH converts to a string.
   (path :accessor path :initarg :path :initform ())
   (parameters :accessor parameters :initarg :parameters)
   (fragment :accessor fragment :initarg :fragment)
   (http-version :accessor http-version :initarg :http-version :initform "HTTP/1.1")))

(defmethod print-object ((object http-request) stream)
  (format stream "<REQUEST ~a ~a ~a ~a>"
          (http-method object)
          (path object)
          (headers object)
          (ignore-errors (subseq (body object) 0 (min (length (body object)) 20)))))

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
(defun make-http-get (&rest args &key connection host port headers body path parameters fragment http-version)
  (declare (ignorable host port headers body path parameters fragment http-version))
  (apply #'make-instance 'http-get args))

(defclass http-head (http-request)
  ((http-method :initform :head)))
(defun make-http-head (&rest args &key connection host port headers body path parameters fragment http-version)
  (declare (ignorable host port headers body path parameters fragment http-version))
  (apply #'make-instance 'http-head args))

(defclass http-post (http-request)
  ((http-method :initform :post)))
(defun make-http-post (&rest args &key connection host port headers body path parameters fragment http-version)
  (declare (ignorable host port headers body path parameters fragment http-version))
  (apply #'make-instance 'http-post args))

(defclass http-put (http-request)
  ((http-method :initform :put)))
(defun make-http-put (&rest args &key connection host port headers body path parameters fragment http-version)
  (declare (ignorable host port headers body path parameters fragment http-version))
  (apply #'make-instance 'http-put args))

(defclass http-options (http-request)
  ((http-method :initform :options)))
(defun make-http-options (&rest args &key connection host port headers body path parameters fragment http-version)
   (declare (ignorable host port headers body path parameters fragment http-version))
   (apply #'make-instance 'http-options args))

(defclass http-connect (http-request)
  ((http-method :initform :connect)))
(defun make-http-connect (&rest args &key connection host port headers body path parameters fragment http-version)
  (declare (ignorable host port headers body path parameters fragment http-version))
  (apply #'make-instance 'http-connect args))

(defun http-host (request)
  (http-header request ':|host|))

(defun http-path (request)
  ;; HTTP request paths are :ABSOLUTE, see CREATE-REQUEST
  (format () "/~{~a~^/~}" (path request)))

(defun http-header (r name)
  ;; Don't modify the header here
  (let ((header (find name (headers r) :key #'field-name :test #'string-equal)))
    (when header
      (field-value header))))

(defun set-http-header (r name value)
  (let ((header (find name (headers r) :key #'field-name)))
    (if header
      (setf (field-value header) value)
      (push (make-instance 'http-header :name name :value value) (headers r)))))

(defsetf http-header set-http-header)

(defun http-body (r)
  (body r))

(defsetf http-body set-http-body)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Response body

(defmethod set-http-body ((message http-basic) value)
  (setf (http-header message :|Content-Length|)
        (length value))
  (setf (body message) value))

(defmethod set-http-body ((response http-response) (body string))
  (let* ((request (request response))
         (accept-encoding (http-header request :accept-encoding)))
    (log2:trace "Encoding: ~a" accept-encoding)
    (cond
      ((search "gzip" accept-encoding)
       (setf (http-header response :|Content-Encoding|)
             "gzip")
       (setf (body response)
             (zlib:gzip body)))
      ((search "compress" accept-encoding)
       (setf (http-header response :|Content-Encoding|)
             "compress")
       (setf (body response)
             (zlib:compress body)))
      (t
       (setf (body response)
             body)))
    (setf (http-header response :|Content-Length|)
          (length (body response))))
  response)

(defmethod set-http-body ((response http-response) (body vector))
  (let* ((request (request response))
         (accept-encoding (http-header request :accept-encoding)))
    (log2:trace "Encoding: ~a" accept-encoding)
    (cond
      ((search "gzip" accept-encoding)
       (setf (http-header response :|Content-Encoding|)
             "gzip")
       (setf (body response)
             (zlib:gzip-bytes body)))
      ((search "compress" accept-encoding)
       (setf (http-header response :|Content-Encoding|)
             "compress")
       (setf (body response)
             (zlib:compress-bytes body)))
      (t
       (setf (body response)
             body)))
    (setf (http-header response :|Content-Length|)
          (length (body response))))
  response)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predefined / template responses

(defun make-ok-response (request)
  (make-http-response :request request
                      :status-code "200"
                      :status-text "OK"
                      :headers (list (make-instance 'http-header :name :|Access-Control-Allow-Origin| :value "*")
                                     (make-instance 'http-header :name :|Content-Type| :value "text/html")
                                     (make-instance 'http-header :name :|Connection| :value "keep-alive")
                                     (make-instance 'http-header :name :|Server| :value "PolarCL"))))

(defun make-redirect-response (request location)
  (make-http-response :request request
                      :status-code "302"
                      :status-text "Found"
                      :headers (list
                                (make-instance 'http-header :name :|Location| :value location)
                                (make-instance 'http-header :name :|Connection| :value "close")
                                (make-instance 'http-header :name :|Server| :value "PolarCL"))))

(defun make-permanent-redirect-response (request location)
  (make-http-response :request request
                      :status-code "301"
                      :status-text "Moved Permanently"
                      :headers (list
                                (make-instance 'http-header :name :|Location| :value  location)
                                (make-instance 'http-header :name :|Connection| :value "close")
                                (make-instance 'http-header :name :|Server| :value "PolarCL"))))

(defun make-authenticate-response (handler request)
  (make-http-response :request request
                      :status-code "401"
                      :status-text "Not Authorized"
                      :headers (list (make-instance 'http-header :name :|WWW-Authenticate| :value (format () "Basic realm=~a" (handler-realm handler)))
                                     (make-instance 'http-header :name :|Access-Control-Allow-Origin| :value "*")
                                     (make-instance 'http-header :name :|Connection| :value "close")
                                     (make-instance 'http-header :name :|Server| :value "PolarCL"))))

(defun make-notfound-response (handler request)
  (make-http-response :request request
                      :status-code "404"
                      :status-text "Not Found"
                      :headers (list (make-instance 'http-header :name :|Connection| :value "close")
                                     (make-instance 'http-header :name :|Server| :value "PolarCL"))))

(defun make-error-response (request &key
                              (status-code "500")
                              (status-text "An error occurred")
                              (headers (list (make-instance 'http-header :name :|Connection| :value "close")
                                             (make-instance 'http-header :name :|Server| :value "PolarCL")))
                              (body "An error occurred. We're sorry."))
  (make-http-response :request request
                      :status-code status-code
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

(defparameter *default-charset* "utf-8")

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
