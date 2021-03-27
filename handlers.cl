;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Handling HTTP Requests
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2021-03-20 20:50:41>

(in-package "POLARCL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Supported Methods

(defvar +http-methods+
  '(:get :put :post :head :options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Request Processing
;;;
;;; Requests are processed either by a redirector, a handler, or by the 
;;; "fall through" handler which returns a 404 NOT FOUND response by default.
;;;
;;; If a matching redirector is found, a 300 response should be returned.
;;; No authentication or further processing is performed.
;;;
;;; Otherwise, the request is handled by calling HANDLE-RESPONSE
;;;
;;; Matching dispatchets
;;;
;;; The request handler is determined from the request URLs FQD and port as
;;; follows
;;;
;;; - Look at all locations defined for fqd:port

(defclass dispatcher ()
  ((filter :accessor dispatcher-filter :initarg :filter)
   (processor :accessor dispatcher-processor :initarg :processor)))

(defmethod print-object ((thing dispatcher) stream)
  (format stream "#<~a ~a>"
          (type-of (dispatcher-processor thing))
          (filter-path (dispatcher-filter thing))))

(defvar *redirectors* nil)
(defvar *handlers* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filters (request matching)

(defclass filter ()
  ((prio :reader filter-prio :initarg :prio :allocation :class)
   (protocol :accessor filter-protocol :initarg :protocol :initform '("HTTP" "HTTPS"))
   (port :accessor filter-port :initarg :port :initform '("80" "443" "8080" "4443"))
   (host :accessor filter-host :initarg :host :initform nil)
   (method :accessor filter-method :initarg :method :initform +http-methods+)
   ;; path is used both for exact and for prefix matches
   (path :accessor filter-path :initarg :path :initform "/index.html")))

(defmethod print-object ((thing filter) stream)
  (format stream "#[~a ~a ~a ~a]"
          (type-of thing)
          (filter-port thing)
          (filter-method thing)
          (filter-path thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finding handlers
;;;
;;; ### ToDo: Describe how filters match.

(defclass exact-filter (filter)
  ((prio :initform 0)))
(defclass regex-filter (filter)
  ((prio :initform 1)))
(defclass prefix-filter (filter)
  ((prio :initform 2)))

(defgeneric create-filter (type
                           &rest args
                           &key protocol port host method path))
(defmethod create-filter (type
                          &rest args
                          &key
                            (protocol nil)
                            (port nil)
                            (host nil)
                            (method :get)
                            (path "/"))
  (apply #'make-instance type args))

(defun create-regex-filter (path)
  (make-instance 'regex-filter :path (regex:make-regex path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Redirectors and Handlers


(defclass request-processor ()
  ())

(defclass redirector (request-processor)
  ((scheme :accessor redirector-scheme :initarg :scheme)
   (host :accessor redirector-host :initarg :host)
   (port :accessor redirector-port :initarg :port)
   (path :accessor redirector-path :initarg :path)
   (query :accessor redirector-query :initarg :query)))

(defclass handler (request-processor)
  ((realm :reader handler-realm :initarg :realm :initform "localhost")
   (authentication :reader handler-authentication :initarg :authentication :initform nil)))

(defmethod print-object ((thing handler) stream)
  (format stream "#<~a>" (type-of thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-handler (&key filter handler)
  (let ((dispatcher
         (make-instance 'dispatcher :filter filter :processor handler)))
    (push dispatcher *handlers*)
    (setf *handlers*
          (sort *handlers*
                (lambda (h1 h2)
                  (< (filter-prio h1)
                     (filter-prio h2)))
                :key #'dispatcher-filter))))

(defun register-redirector (&key filter redirector)
  (let ((dispatcher
         (make-instance 'dispatcher :filter filter :processor redirector)))
    (push dispatcher *redirectors*)
    (setf *redirectors*
          (sort *redirectors*
                (lambda (h1 h2)
                  (< (filter-prio h1)
                     (filter-prio h2)))
                :key #'dispatcher-filter))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric find-redirector (connection request))
(defgeneric find-handler (connection request))

(defmethod find-redirector ((connection t) (request t))
  (log2:trace "Searching for redirector ~a ~a" connection request)
  (let ((redirector
         (find-if (lambda (d) (match-filter (dispatcher-filter d) request)) *redirectors*)))
    (log2:debug "Found redirector ~a" redirector)
    redirector))

(defmethod find-handler ((connection t) (request t))
  ;; Returns the *request-handlers* entry for $method and $path whose handler
  ;; has the longest match with $path. The handlers entry may contain several
  ;; functions, all of which will be called (except for errors)
  (log2:trace "Searching handler for ~a" request)
  (let ((handler 
         (find-if (lambda (d) (match-filter (dispatcher-filter d) request)) *handlers*)))
    (log2:debug "Found handler: ~a" handler)
    handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standard request handling
;;;
;;; 1 - Search for a redirector. If found, redirect the request (eg, redirect HTTP to HTTPS)
;;; 2 - Search for a handler. If none was found, respond with *notfound-response* 
;;; 3 - Perform authentication specified by the handler. Basic authentication is the default.
;;;     CAUTION: Basic Authentication should be used only over HTTPS
;;; 4 - Create response object
;;; 5 - Call handle-response on the handler, request and response

(defgeneric handle-request (server connection request))

(defmethod handle-request ((server http-server) (connection t) (request t))
  (log2:debug "~a ~a ~a~%"
             (server-port server)
             (mbedtls:format-ip (mbedtls:peer connection))
             (format-request-info request))
  (condlet
   ;; 1 - Check if the request is redirected
   ((dispatcher (find-redirector connection request))
    (let ((redirector (dispatcher-processor dispatcher)))
      (log2:trace "Redirecting request ~a using ~a" request redirector)
      (destructuring-bind (request-host &optional request-port)
          (cl-utilities:split-sequence #\: (http-host request))
        (let* ((scheme (or (redirector-scheme redirector)
                           (http-protocol request)
                           "http"))
               (host (or (redirector-host redirector)
                         request-host
                         "localhost"))
               (port (or (redirector-port redirector)
                         request-port
                         (default-port scheme)))
               (path (merge-paths (redirector-path redirector)
                                  (http-path request))))
          (make-redirect-response request
                                  (format () "~a://~a:~a~a" scheme host port path))))))
   ;; 2 - Find handler
   ((dispatcher (find-handler connection request))
    (log2:trace "Handling request ~a using ~a" request dispatcher)
    (let ((handler (dispatcher-processor dispatcher)))
      (cond
        ;; Authentication
        ((and (handler-authentication handler)
              (not (authenticate handler request)))
         (log2:info "Authentication failed for ~a" (handler-realm handler))
         (make-authenticate-response handler request))
        (T
         (let ((response (make-ok-response request)))
           (handle-response server (dispatcher-filter dispatcher) handler request response)
           response)))))
   ;; 3 - No handler found. Reply with "Bad Request".
   (t
    (make-http-response :request request :status-code "404" :status-text "Not found"))))

(defun default-port (scheme)
  (cond
    ((string-equal scheme "http")
     "80")
    ((string-equal scheme "https")
     "443")))

(defun merge-paths (redirect-path request-path)
  (cond
    ((null redirect-path)
     (error "Redirector does not specify a location"))
    ((absolute-path-p redirect-path)
     redirect-path)
    (t
     (case (aref request-path (1- (length request-path)))
       (#\/
        (concatenate 'string request-path redirect-path))
       (otherwise
        (concatenate 'string request-path "/" redirect-path))))))

(defun absolute-path-p (path)
  (and (>= (length path) 1)
       (char= (aref path 0) #\/)))

(defgeneric handle-response (server filter handler request response))

(defmethod handle-response ((server http-server) (filter t) (handler t) (request t) (response t))
  (error "Hit basic handler"))


(defgeneric create-handler (type
                            &rest args
                            &key method redirector realm authentication authorization rootdir function contentfn))

(defmethod create-handler (type
                           &rest args
                           &key method redirector realm authentication authorization rootdir function contentfn)
  (apply #'make-instance type args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Matching handler to requests

(defun match-filter (filter request)
  (and (nnmember (http-protocol request) (filter-protocol filter))
       (nnmember (http-host request) (filter-host filter))
       (nnmember (http-port request) (filter-port filter))
       (nnmember (http-method request) (filter-method filter))
       (match-filter-path filter request)))

(defgeneric match-filter-path (filter request))

(defmethod match-filter-path ((filter exact-filter) request)
  (log2:trace "filter path: ~a request path: ~a" (filter-path filter) (http-path request))
  (string= (http-path request) (filter-path filter)))

(defmethod match-filter-path ((filter prefix-filter) request)
  (log2:trace "filter path: ~a request path: ~a" (filter-path filter) (http-path request))
  (let* ((prefix (filter-path filter))
         (length (length prefix))
         (path (http-path request))
         (mismatch (mismatch prefix path)))
    (or (null mismatch)
        (and (= mismatch length)
             (eql (aref path mismatch) #\/)))))

(defmethod match-filter-path ((filter regex-filter) request)
  (log2:trace "filter path: ~a request path: ~a" (filter-path filter) (http-path request))
  (regex:match-regex (filter-path filter) (http-path request)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Redirection

;;; ### Needs rework:
;;; Requirements:
;;; - redirect all or some paths ending with a directory to a file (eg. index.html) in that directory
;;; - redirect all or some paths from HTTP to HTTPS
;;; - positive and negative lists?
;;; - Define a simple syntax to install redirectors

;;; Matching and determining target
;;;
;;; - SOURCE and TARGET URLS are represented by a structure (scheme host port path query)
;;; - The corresponding parts are retrieved from the connection (scheme), server (port) and request (host, path and query).
;;;   The request HOST may be unspecified, all other parts are always specified.
;;;
;;; Matching
;;; - Match request against SOURCE URL as in normal request matching
;;;
;;; Compose RESULT URL by comparing REQUEST and TARGET:
;;; - If TARGET part is :wild, use REQUEST
;;; - If TARGET part is not :wild, use it.  

(defun create-redirector (&key scheme host port path query)
  (make-instance 'redirector :scheme scheme :host host :port port :path path :query query))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Users & Authentication

(defstruct user id hashed-password name authorizations)

(defvar *users* (make-hash-table :test #'equal))

(defun register-user (name realm password)
  ;;; ### password hashing is not implemented yet!
  (setf (gethash (credname name realm) *users*)
        (make-user :id 0 :hashed-password password :name name)))

(defun credname (name realm)
  (format nil "~a@~a" name realm))

(defun get-user-info (name)
  (gethash name *users*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun http-credentials (request)
  (let ((credentials (http-header request :|authorization|)))
    (when credentials
      (let ((credentials
             (base64:base64-string-to-string
              (cadr (cl-utilities:split-sequence #\space credentials)))))
        (when credentials
          (let ((user
                 (subseq credentials 0 (position #\: credentials)))
                (password
                 (subseq credentials (1+ (position #\: credentials)))))
            (values user password)))))))

(defun http-authenticated-user (handler request)
  (when (authenticate handler request)
    (http-credentials request)))

(defun authenticate (handler request)
  (multiple-value-bind (user password)
      (http-credentials request)
    (when user
      (let* ((credname (credname user (handler-realm handler)))
             (user-info (get-user-info credname)))
        (when user-info
          (string= password
                   (user-hashed-password user-info)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1-1 Serving static files
;;;
;;; file-handler serves static content. The request path is mapped to $rootdir.

(defclass file-handler (handler)
  ((rootdir :reader handler-rootdir :initarg :rootdir :initform "/var/www")))

(defmethod handle-response ((server http-server) (filter t) (handler file-handler) (request t) (response t))
  ;; 1 - Determine true path
  (log2:trace "GET-FILE: root directory ~a" (handler-rootdir handler))
  (let* ((request-path
          (path-string request))
         (path (realpath filter handler request))
         (dir (pathname-directory path))
         (name (pathname-name path)))
    (when (null name)
      (error "Missing filename: ~a" request-path))
    (when (or (find 'absolute dir)
              (find :up dir))
      (error "Invalid path: ~a" request-path))
    (setf (http-header response :|Content-Type|)
          (get-mime-for-extension (pathname-type path)))
    (unless (probe-file path)
      (log2:trace "Mapped path ~a not found" (truename path))
      (error "File ~a not found" request-path))
    (let ((if-modified-since (http-header request  :|if-modified-since|))
          (file-write-date (file-write-date path)))
      (cond
        ((or (null if-modified-since)
             (ignore-errors
               (> file-write-date (parse-date if-modified-since))))
         (setf (http-header response :|Last-Modified|) (format-date-imf nil file-write-date))
         (handler-case 
             (with-open-file (f path :element-type '(unsigned-byte 8))
               (log2:debug "File ~a length ~d" path (file-length f))
               (let ((buffer (make-array (file-length f) :element-type '(unsigned-byte 8))))
                 (read-sequence buffer f)
                 (setf (http-body response) buffer)))
           (error (e)
             (declare (ignore e))
             (error "An error occurred while reading ~a" request-path))))
        (t
         (setf (http-header response :|Last-Modified|) file-write-date)
         (setf (status-code response) 304)
         (setf (status-text response) "Not Modified"))))))

(defgeneric realpath (filter handler request))

(defmethod realpath ((filter exact-filter) (handler file-handler) request)
  (let* ((host (http-host request))
         (request-path
          (format () "~{~a~^/~}" (path request))))
    (log2:debug "Request path: ~a" request-path)
    (log2:debug "Handler root path: ~a" (handler-rootdir handler))
    (let ((realpath
           (merge-pathnames
            (merge-pathnames
             request-path
             (parse-namestring (handler-rootdir handler)))
            *content-root*)))
      (log2:debug "realpath: ~a" realpath)
      realpath)))

(defmethod realpath ((filter prefix-filter) (handler file-handler) request)
  (let* ((host (http-host request))
         (request-path
          (format () "~{~a~^/~}" (path request))))
    (log2:debug "Request path: ~a" request-path)
    (log2:debug "Handler root path: ~a" (handler-rootdir handler))
    (let ((realpath
           (merge-pathnames
            (merge-pathnames
             request-path
             (parse-namestring (handler-rootdir handler)))
            *content-root*)))
      (log2:debug "realpath: ~a" realpath)
      realpath)))

(defmethod realpath ((filter regex-filter) handler request)
  (error "Cannot determine realpath from ~a" filter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 

(defun post-file (handler request response)
  (declare (ignore handler request response))
  (error "Not implemented yet"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2 Calling URL-specified functions
;;;
;;; URL-specified functions must be registered with REGISTER-FUNCTION.

(defvar *registered-functions*
  ())

(defun register-function (symbol)
  (pushnew symbol *registered-functions*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2-1 'Request Functions'
;;;
;;; rfuncs are called with the request & response and return their result by setting
;;; the response body & other repsonse parameters

(defclass rfunc-handler (handler)
  ())

(defmethod handle-response ((server http-server) (filter t) (handler rfunc-handler) (request t) (response t))
  (with-func-from-path (fsym request)
    (funcall fsym handler request response)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2-2 'Query
;;;
;;; qfunc receives the query parameters as keyword arguments. The result is returned
;;; as the response body

(defclass qfunc-handler (handler)
  ())

(defmethod handle-response ((server http-server) (filter t) (handler qfunc-handler) (request t) (response t))
  (with-func-from-path (fsym request)
    (let ((args (loop
                   :for pair :in (parameters request)
                   :collect (intern (car pair) :keyword)
                   :collect (cadr pair))))
      (setf (http-body response)
            (apply fsym handler request response args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2-3 Dynamic content
;;;
;;; dynhtml-handler serves dynamic content.

(defclass dynhtml-handler (handler)
  ((contentfn :reader handler-contentfn :initarg :contentfn)))

(defmethod handle-response ((server http-server) (filter t) (handler dynhtml-handler) (request t) (response t))
  ;; Don't set the dynhtml-handler result as response body. If you want the response body automatically set,
  ;; use qfunc handler.
  (funcall (handler-contentfn handler) server handler request response))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
