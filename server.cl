
;;; ### Use mbedtls_net_set_nonblock
;;; ### Set listen backlog?
;;; ### Move handler dispatch loop to mbedtls?
;;; ### Use only accept() to dispatch?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    HTTP Server
;;; Author         Michael Kappert 2013
;;; Last Modified  <michael 2019-05-31 01:28:33>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Examples
;;;
;;;  (let ((s (make-instance 'https-server :port "4443" :max-handlers 4))) (run-http-server s))
;;;  (let ((s (make-instance 'http-server :port "8080" :max-handlers 500))) (run-http-server s :background nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ToDo:
;;; - check why clients hang until we close the connection after an error response.
;;;   Does not seem to happen after an OK response.
;;; - definition/use of supported request methods (eg, in get-request)
;;; - Notify failed TLS handshake
;;;   => Need to refactor ACCEPT? Store the plain socket somewhere?
;;; - Combining and cancelling handlers?
;;; - Path Remapping 
;;; - Response mime type determination is sloppy
;;; - Make QUIT a restricted function

;; (setf *print-miser-width* nil)

(in-package "POLARCL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP Server-  configuration variables

(defclass http-server ()
  ((port
    :reader server-port :initarg :port :initform  "8080")
   (hostname
    :reader server-hostname :initarg :hostname :initform "localhost")
   (database
    :reader server-database :initarg :database :initform "polarcl.sdb")
   (default-charset
    :reader server-default-charset :initarg :default-charset :initform "UTF-8")
   (keepalive
    :reader server-keepalive :initarg :keepalive :initform 10000
    :documentation "How long a connection waits for the next request")
   (max-keepalive-total-time
    :reader server-max-keepalive-total-time :initarg  :max-keepalive-total-time :initform 10000
    :documentation "Maximum time a connection keeps open")
   (nodelay
    :reader server-nodelay :initarg :nodelay :initform t
    :documentation "Disable Nagle's algorithm")
   (debug-level
    :reader server-debug-level :initarg :debug-level :initform 0
    :documentation "mbedTLS debugging")
   (log-level
    :reader server-log-level :initarg :log-level :initform 1
    :documentation "PolarCL loglevel")
   (mt-method
    :reader server-mt-method :initarg :mt-method :initform :pooled
    :documentation "Start threads on-demand (:ondemand) or use thread pool (:pooled)")
   (max-handlers
    :reader server-max-handlers :initarg :max-handlers :initform 20
    :documentation "Maximal number of active handlers / Size of thread pool")
   (server-running$
    :accessor server-running$ :initform nil)
   (socket-server :accessor socket-server :initform nil)))

(defclass https-server (http-server)
  ((cert-file
    :reader server-cert-file :initarg :cert-file :initform "/home/michael/certs/mbedTLS/localhost_cert.pem")
   (key-file
    :reader server-key-file :initarg :key-file :initform "/home/michael/certs/mbedTLS/localhost_key.pem")
   (key-file-pw
    :reader server-key-file-pw :initarg :key-file-pw :initform "")))

;;; This is mess.
;;; - Define variables in a central place, or keep in a structure?
;;; - Clarify how to write server-specific configurations
(defun reset ()
  (setf *redirectors* nil)
  (setf *handlers* nil)
  (setf *servers* nil)
  (setf *users* (make-hash-table :test #'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP Server implementation

(define-condition missing-server-feature (error)
  ((text  :accessor text :initarg :text))
  (:report (lambda (c s)
             (format s "Server error: ~a is not implemented" (text c)))))

(define-condition missing-server-feature-ctf (missing-server-feature)
  ()
  (:report (lambda (c s)
             (format s "Server error: Chunked transfer encoding is not implemented"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server run state
(defvar *servers* nil)

(defvar +server-running-lock+
  (bordeaux-threads:make-lock "server-running"))

(defun server-running (server)
  (bordeaux-threads:with-lock-held (+server-running-lock+)
    (server-running$ server)))

(defsetf server-running set-server-running)
(defun set-server-running (server value)
  (bordeaux-threads:with-lock-held (+server-running-lock+)
    (setf (server-running$ server) value)))

(defun stop-all-servers ()
  (map nil #'stop-server *servers*)
  (setf *servers* nil))

(defun stop-server (server)
  (log2:info "Stopping server ~a" server)
  ;; Threads cannot be killed when waiting in ACCEPT or POLL.
  ;; Call mbedtls-net-accept with a timeout and simply allow the threads to expire.
  (setf (server-running server) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multithreaded HTTP Server

(defun run-http-server (http-server &key (background t))
  (push http-server *servers*)
  (flet ((run ()
           (handler-case 
               (unwind-protect
                    (progn
                      (setf (socket-server http-server)
                            (create-server http-server))
                      (setf (server-running http-server) t)
                      (ecase (server-mt-method http-server)
                        (:pooled
                         (server-loop-pooled http-server))
                        (:ondemand
                         (server-loop-ondemand http-server))))
                 (when (socket-server http-server)
                   (log2:info "Releasing ~a:~a~%" (server-hostname http-server) (server-port http-server))
                   (mbedtls:close-socket (socket-server http-server))
                   (mbedtls:deallocate (socket-server http-server))
                   (setf (socket-server http-server) nil)))
             (error (e)
               (log2:error "### RUN-HTTP-SERVER Error: ~a" e)))))
    (cond
      (background
       (bordeaux-threads:make-thread #'run))
      (t
       (run)))))
  
(defmethod create-server ((server http-server))
  (mbedtls:create-plain-socket-server (server-hostname server)
                                      (server-port server)
                                      :keepalive (server-keepalive server)
                                      :nodelay (server-nodelay server)
                                      :debug-level (server-debug-level server)))

(defmethod create-server ((server https-server))
  (mbedtls:create-ssl-socket-server (server-hostname server)
                                    (server-port server)
                                    :keepalive (server-keepalive server)
                                    :nodelay (server-nodelay server)
                                    :debug-level (server-debug-level server)
                                    :server-cert (server-cert-file server)
                                    :server-key (server-key-file server)
                                    :server-key-pw (server-key-file-pw server)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "on demand threads": Create handler threads on-demand for incoming connections

(defvar *handler-count* 0)

(defparameter +handler-count-lock+
  (bordeaux-threads:make-lock "handler-count"))

(defun increase-handler-count ()
  (bordeaux-threads:with-lock-held (+handler-count-lock+)
    (incf *handler-count*)))

(defun decrease-handler-count ()
  (bordeaux-threads:with-lock-held (+handler-count-lock+)
    (decf *handler-count*)))

(defun server-loop-ondemand (http-server)
  (let ((server (socket-server http-server))
        (thread-id 0))
    (flet ((on-demand-loop% ()
             (do ()
                 ((not (server-running http-server))
                  (log2:info "Terminating.")
                  t)
               (handler-case 
                   (cond
                     ((>= *handler-count* (server-max-handlers http-server))
                      (log2:info "Handler count reached, waiting 0.5s")
                      (sleep 0.5))
                     (t
                      (log2:trace "Accepting a new connection")
                      (unwind-protect 
                           (mbedtls:with-server-connection-async ((connection server))
                             (increase-handler-count)
                             (log2:debug "Accepted a new connection (now active: ~a/~a)" *handler-count*  (server-max-handlers http-server))
                             (handle-connection http-server connection))
                        (decrease-handler-count))))
                 (mbedtls:stream-timeout (e)
                   (log2:info "Timeout: ~a" e))
                 (error (e)
                   (log2:error "~a" e))
                 (condition (e)
                   (log2:error "Unexpected non-local transfer on ~a" e))))))
      (bordeaux-threads:make-thread #'on-demand-loop% :name "ON-DEMAND-LOOP")
      (do ()
          ((not (server-running http-server))
           (log2:info "Terminating.")
           t)
        (sleep *loop-delay*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "thread pool": Create a fixed pool of handler. All threads are blocked in poll().
;;; Used to be prone to the thundering herd problem,
;;; but this is suppsoed to solved in newer kernels (> 2.4)
;;; According to siege tests, this method is 70% faster.

(defun server-loop-pooled (http-server)
  (let* ((server (socket-server http-server))
         (threads
          (loop
             :for k :below (server-max-handlers http-server)
             :for name = (format () "handler-~a:~a-~d"
                                 (server-hostname http-server)
                                 (server-port http-server)
                                 k)
             :collect (bordeaux-threads:make-thread
                       (lambda () (handler-thread http-server))
                       :name name))))
    ;; Killing the threads does not work reliably,
    ;; therefore don't join-threads here but just wait until *run* becomes false
    (do ()
        ((not (server-running http-server))
         (log2:info "Terminating.")
         t)
      (sleep *loop-delay*))))

(defun handler-thread (http-server)
  (log2:info "Accept loop started")
  (do ((requests 0)
       (server (socket-server http-server)))
      ((not (server-running http-server))
       ;; Quit handler loop
       t)
    (handler-case
        (progn
          (log2:trace "Accepting a new connection")
          (mbedtls:with-server-connection ((conn server))
            (when (and conn (server-running http-server))
              (incf requests)
              (log2:info "Accepted connection ~d" requests)
              (handle-connection http-server conn))))
      (mbedtls:stream-timeout (e)
        (log2:info "Timeout: ~a" e))
      (error (e)
        (log2:error "Caught error: ~a" e))
      (condition (e)
        (log2:error "Unexpected non-local transfer on ~a" e))))
  (log2:info "Accept loop finished, thread exiting"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HANDLE-CONNECTION

(defgeneric handle-connection (server connection))

(defmethod handle-connection ((http-server http-server)
                              (connection mbedtls:socket-stream))
  ;; Timeout handling:
  ;; - mbedtls:*keepalive-timeout* is bound only while attempting to read the next Request-Line.
  ;; - The shorter (mbedtls:timeout mbedtls:socket-stream) is used while reading the remaining request parts.
  ;; See mbedtls:refresh-buffer.
  (flet ((get-request-line ()
           (handler-case
               (mbedtls:get-line connection :timeout (mbedtls:keepalive connection))
             (mbedtls:stream-timeout (e)
               (log2:trace "Timeout on ~a: ~a" (mbedtls:peer connection) e)
               (return-from get-request-line nil))
             (mbedtls:stream-empty-read (errmsg)
               (log2:trace "Empty read on ~a: ~a" (mbedtls:peer connection) errmsg)
               (return-from get-request-line nil))
             (mbedtls:stream-read-error (errmsg)
               (log2:trace "Read error on ~a: ~a" (mbedtls:peer connection) errmsg)
               (return-from get-request-line nil))))
         (keepalive-p (request start-time)
           (let ((now (get-internal-real-time)))
             (and (string-equal (http-header request :|connection|)
                                "keep-alive")
                  (< (* (/ (- now start-time)
                           internal-time-units-per-second)
                        1000)
                     (server-max-keepalive-total-time http-server))))))
    (unwind-protect
         (let ((k -1))
           (tagbody
             :start
             (log2:debug "Waiting for request ~d" (incf k))
             (let* ((start-time (get-internal-real-time))
                    (request-line (get-request-line))
                    ;; ToDo: Create request now?
                    (request nil))
               (log2:trace "<<< ~a" request-line)
               (when (null request-line) (go :finish))
               (handler-case
                   (progn
                     (setf request
                           (create-request connection
                                           (mbedtls:server-port (socket-server http-server))
                                           request-line))
                     (read-request connection request)
                     (let* ((response
                             ;; Request Handling
                             (handle-request http-server connection request)))
                       (log2:debug "Keepalive: ~a from ~a" (http-header response :|Connection|) response)
                       (let ((keepalive
                              ;; KeepAlive only if the response says keep-alive.
                              (and (string= (http-header response :|Connection|) "keep-alive")
                                   (keepalive-p request start-time))))
                         ;; Response post-processing: KeepAlive?
                         (cond
                           (keepalive
                            (setf (http-header response :|Connection|) "Keep-Alive")
                            (setf (http-header response :|Keep-Alive|) "timeout=5, max=100"))
                           (t
                            (setf (http-header response :|Connection|) "close")))
                         (log2:debug "KeepAlive ~a: request ~a" connection k)
                         (handler-case
                             (progn
                               (write-response connection response)
                               (log2:info "~a ~a ~a ~a ~a"
                                          (server-port http-server)
                                          (status-code response)
                                          (if keepalive "A" "C")
                                          (mbedtls:format-ip (mbedtls:peer connection))
                                          (format-request-info request)))
                           (mbedtls:stream-write-error (e)
                             (log2:error "~a Write failed: ~a" (mbedtls:peer connection) e)))
                         ;; KeepAlive: wait for another request
                         (when keepalive
                           (log2:debug "Keep-Alive: ready")
                           (go :start)))))
                 (error (e)
                   (log2:warning "~a Caught error: ~a" (mbedtls:peer connection) e)
                   (handler-case
                       (write-response connection
                                       (make-error-response request
                                                            :body (format () "~a" e)
                                                            :status-code "400"
                                                            :status-text "Invalid request"))
                     (mbedtls:stream-write-error (e)
                       (log2:error "~a Write failed while sending error response: ~a" (mbedtls:peer connection) e))))))
             :finish)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading HTTP requests
;;; Procedure: 
;;; - parse header
;;; - construct request
;;; - retrieve body using header info

(defun parse-request-header (line)
  (declare (string line))
  (log2:trace "<<< ~a" line)
  (let* ((index (position #\: line))
         (key (intern (string-downcase
                       (subseq line 0 index))
                      :keyword))
         (value (subseq line (1+ index))))
    (make-instance 'http-header
                   :name key
                   :value (case key
                            (:|cookie|
                              (parse-cookies value))
                            (t
                             (string-left-trim " " value))))))

(defun parse-url-query (string)
  ;; Fixme: handle escaped chars =,& ?
  (when (stringp string)
    (loop
       :for pair :in (cl-utilities:split-sequence #\& string)
       :for sep = (position #\= pair)
       :collect (list (subseq pair 0 sep)
                      (when sep (subseq pair (1+ sep)))))))

(defun parse-request-line (line)
  (destructuring-bind (&optional (method "INVALID") (query-path "") (http-version "HTTP/1.1"))
      (cl-utilities:split-sequence #\space line)
    (values method
            query-path
            http-version)))

(defun create-request (connection port line)
  (multiple-value-bind (method query-path http-version)
      (parse-request-line line)
    (let* ((request
            (case (intern (string-upcase method) :keyword)
              (:get (make-http-get :connection connection :port port))
              (:head (make-http-head :connection connection :port port))
              (:post (make-http-post :connection connection :port port))
              (:put (make-http-put :connection connection :port port))
              (:options (make-http-options :connection connection :port port))
              (otherwise
               (error "Unsupported HTTP method ~a" method))))
           (url (net.uri:parse-uri query-path))
           (path (cdr (puri:uri-parsed-path  url)))
           (parameters (parse-url-query (puri:uri-query url))))
      (setf (path request) path)
      (setf (parameters request) parameters)
      (setf (http-version request) http-version)
      request)))

(defun read-request (connection request)
  (let* ((headers
          ;; Read header lines
          (loop
             :for line = (mbedtls:get-line connection)
             :while (and line
                         ;; Headers are terminated by an empty line
                         (> (length line) 0))
             :collect (parse-request-header line))))
    (unless (find ':|host| headers :key #'field-name)
      (error "Missing Host in ~a" request))
    (setf (headers request) headers)
    (typecase request
      (http-post
       ;; Read request body
       ;;   This should be deferred, but remember to clear the input if the connection is kept alive. 
       (get-body connection request)))
    request))

(defun get-body (stream request)
  (let* ((content-type (http-content-type request))
         (content-length (when (http-content-length request)
                           (parse-integer (http-content-length request)))))
    (case content-length
      (0
       (setf (body request) ""))
      (otherwise
       (let* ((transfer-encoding (http-transfer-encoding request))
              (content-encoding (http-content-encoding request))
              (content-charset (http-charset request))
              (chunks
               (cond
                 (content-length
                  (let* ((octets
                          (mbedtls:get-octets stream content-length))
                         (chunk (cond
                                  ((null content-encoding)
                                   octets)
                                  ((string= content-encoding "gzip")
                                   (zlib:gunzip octets))
                                  (t
                                   (error "Content encoding ~a not supported" content-encoding)))))
                    (list chunk)))
                 ((search "chunked" transfer-encoding)
                  ;; http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.6
                  (do ((chunk-size (get-chunk-size stream) (get-chunk-size stream))
                       (body ()))
                      ((= chunk-size 0)
                       (nreverse body))
                    (push (mbedtls:get-octets stream chunk-size) body)
                    (mbedtls:get-octets stream 2)))
                 (t
                  (log2:warning "No Content-length protocol, reading until EOS")
                  (list (mbedtls:get-octets stream nil)))))
              (body
               (cond
                 ((or (search "application/octet-stream" content-type)
                      (search "image/png" content-type))
                  (apply #'concatenate '(vector (unsigned-byte 8)) chunks))
                 ((or (search "text/" content-type)
                      (search "application/xml" content-type)
                      (search "application/x-www-form-urlencoded" content-type))
                  (apply #'concatenate 'string
                         (mapcar (lambda (chunk)
                                   (octets-to-string chunk :encoding content-charset))
                                 chunks)))
                 (t
                  (error "Unknown content type ~a" content-type)))))
         (setf (body request) body))))))

(defun get-chunk-size (stream)
  (let* ((hex-digits (mbedtls:get-line stream))
         (*read-base* 16))
    (cond
      ((= (length hex-digits) 0)
       0)
      (t
       (read-from-string hex-digits)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-response (stream response)
  (write-message-line stream "HTTP/1.1 ~a ~a"
                      (status-code response)
                      (status-text response))
  (write-headers (headers response) stream)
  (write-message-line stream "")
  (when (body response)
    (write-body stream (body response))))

(defun write-message-line (stream formatter &rest arguments)
  (let ((string (apply #'format nil formatter arguments)))
    (log2:trace ">>> ~a" string)
    (mbedtls:write-to-stream stream (string-to-octets string))
    (mbedtls:write-to-stream stream #(13 10))))

(defun write-body (stream body)
  (log2:trace ">>> Body: ~a" (subseq body 0 (min 20 (length body))))
  (etypecase body
    (string
      (mbedtls:write-to-stream stream (string-to-octets body)))
    (vector
      (mbedtls:write-to-stream stream body)))
  (log2:trace ">>> Body: wrote ~a chars/bytes" (length body)))

(defun write-headers (headers stream)
  (loop
     :for header :in headers
     :do (case (field-name header)
           (:|Cookie|
             (error "Use Set-Cookie instead of Cookie in HTTP response"))
           (t
            ;; This relies on print-object to print cookies
            (write-message-line stream "~a: ~a" (field-name header) (field-value header))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Aux functions

(defun strip-name (s)
  (string-trim '(#\space #\linefeed #\return) s))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
