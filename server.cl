;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    HTTP Server
;;; Author         Michael Kappert 2013
;;; Last Modified  <michael 2017-03-13 22:22:38>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Examples
;;;
;;;  (let ((s (make-instance 'https-server :port "4443" :max-handlers 4))) (run-http-server s))
;;;  (let ((s (make-instance 'http-server :port "8080" :max-handlers 500))) (run-http-server s :background nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ToDo:
;;; - definition/use of supported request methods (eg, in get-request)
;;; - Notify failed TLS handshake
;;;   => Need to refactor ACCEPT? Store the plain socket somewhere?
;;; - Combining and cancelling handlers?
;;; - Path Remapping 
;;; - Response mime type determination is sloppy
;;; - Make QUIT a restricted function

;; (declaim (optimize (debug 0) (safety 0) (speed 3) (space 0)))
;; (declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))
;; (setf *print-miser-width* nil)

(in-package "POLARCL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP Server-  configuration variables

(defclass http-server ()
  ((port
    :reader server-port :initarg :port :initform  "8080")
   (hostname
    :reader server-hostname :initarg :hostname :initform "localhost")
   (default-charset
    :reader server-default-charset :initarg :default-charset :initform "UTF-8")
   (keepalive
    :reader server-keepalive :initarg :keepalive :initform 5000
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

(defvar +server-running-lock+
  (bordeaux-threads:make-lock "server-running"))

(defun server-running (server)
  (bordeaux-threads:with-lock-held (+server-running-lock+)
    (server-running$ server)))

(defsetf server-running set-server-running)
(defun set-server-running (server value)
  (bordeaux-threads:with-lock-held (+server-running-lock+)
    (setf (server-running$ server) value)))


(defun stop-server (server)
  (log2:info "Stopping server")
  ;; Threads cannot be killed when waiting in ACCEPT or POLL.
  ;; Call mbedtls-net-accept with a timeout and simply allow the threads to expire.
  (setf (server-running server) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multithreaded HTTP Server

(defun run-http-server (http-server &key (background t))
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

(defparameter +handler-count-lock+
  (bordeaux-threads:make-lock "handler-count"))

(defun increase-handler-count (count)
  (bordeaux-threads:with-lock-held (+handler-count-lock+)
    (incf count)))

(defun decrease-handler-count (count)
  (bordeaux-threads:with-lock-held (+handler-count-lock+)
    (decf count)))

(defun server-loop-ondemand (http-server)
  (do ((server (socket-server http-server))
       (handler-count 0)
       (thread-id 0))
      ((not (server-running http-server))
       t)
    (cond
      ((>= handler-count (server-max-handlers http-server))
       (sleep 0.5))
      (t
       (log2:debug "Accepting a new connection")
       (mbedtls:with-server-connection-async ((connection server))
         (increase-handler-count handler-count)
         (log2:debug "Accepted a new connection (now active: ~a)" handler-count)
         (unwind-protect 
              (handle-connection http-server connection)
           (decrease-handler-count handler-count)))))))

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
             :for name = (format () "handler-~d-~d" k (server-port http-server))
             :collect (bordeaux-threads:make-thread
                       (lambda () (handler-thread name http-server))
                       :name name))))
    ;; Killing the threads does not work reliably,
    ;; therefore don't join-threads here but just wait until *run* becomes false
    ;; TODO: There is still a bug leaving one HTTP server thread running sometimes.
    (do ()
        ((not (server-running http-server))
         t)
      (sleep 1))))

(defun handler-thread (name http-server)
  (log2:debug "~a: Accept loop started" name)
  (do ((requests 0)
       (server (socket-server http-server)))
      ((not (server-running http-server))
       ;; Quit handler loop
       t)
    (handler-case
        (mbedtls:with-server-connection ((conn server))
          (log2:debug "~a: Accepting a new connection" name)
          (when (and conn (server-running http-server))
            (incf requests)
            (log2:debug "~a: Accepted connection ~d" name requests)
            (handle-connection http-server conn name)))
      (error (e)
        (log2:error "### ~a: Error: ~a" name e))))
  (log2:debug "~a: Accept loop finished, thread exiting" name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HANDLE-CONNECTION

(defgeneric handle-connection (server connection &optional handler-name))

(defmethod handle-connection ((http-server http-server)
                              (connection mbedtls:socket-stream)
                              &optional (name (bordeaux-threads:thread-name (bordeaux-threads:current-thread))))
  ;; Timeout handling:
  ;; - mbedtls:*keepalive-timeout* is bound only while attempting to read the next Request-Line.
  ;; - The shorter (mbedtls:timeout mbedtls:socket-stream) is used while reading the remaining request parts.
  ;; See mbedtls:refresh-buffer.
  (let ((server (socket-server http-server))
        (keepalive-total-time (server-max-keepalive-total-time http-server)))
    (flet ((get-request-line ()
             (handler-case
                 (mbedtls:get-line connection :timeout (mbedtls:keepalive connection))
               (mbedtls:stream-empty-read ()
                 (log2:warning "~a: Client sent zero bytes, aborting request" name)
                 (return-from get-request-line nil))
               (mbedtls:stream-read-error ()
                 (log2:warning "~a: Client inactiv, aborting request" name)
                 (return-from get-request-line nil))))
           (keepalive-p (request start-time)
             (let ((now (get-internal-real-time)))
               (and (string-equal (http-header request :|connection|)
                                  "keep-alive")
                    (< (* (/ (- now start-time)
                             internal-time-units-per-second)
                          1000)
                       keepalive-total-time)))))
      (unwind-protect
           (let ((k -1))
             (tagbody
               :start
               (log2:debug "~a: HANDLE-CONNECTION: Waiting for request ~d" name (incf k))
               (let ((start-time (get-internal-real-time))
                     (request-line (get-request-line)))
                 (when (null request-line) (go :finish))

                 (handler-case 
                     (let* ((request (get-request server connection request-line))
                            (response
                             ;; Request Handling
                             (handle-request http-server connection request)))
                       (log2:debug "Keepalive: ~a from ~a" (http-header response :|Connection|) response)
                       (let ((keepalive
                              ;; KeepAlive only if the response says keep-alive.
                              (and (string= (http-header response :|Connection|) "keep-alive")
                                   (keepalive-p request start-time))))
                         ;; Response post-processing: KeepAlive?
                         (setf (http-header response :|Connection|)
                               (if keepalive "keep-alive" "close"))
                         (log2:debug "~a: KeepAlive ~a: request ~a" name connection k)
                         (handler-case
                             (write-response connection response)
                           (mbedtls:stream-write-error (e)
                             (log2:error "~a: HANDLE-CONNECTION: Error ~a" name e)))
                         ;; KeepAlive: wait for another request
                         (when keepalive (go :start))))
                   (error (e)
                     (log2:warning "~a: HANDLE-CONNECTION: Error: ~a" name e)
                     (handler-case
                         (write-response connection
                                         (make-error-response :body (format () "~a" e)
                                                              :status-code "400"
                                                              :status-text "Invalid request"))
                       (mbedtls:stream-write-error (e)
                         (log2:error "~a: HANDLE-CONNECTION: Error ~a" name e))))))
               :finish))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading HTTP requests
;;; Procedure: 
;;; - parse header
;;; - construct request
;;; - retrieve body using header info

(defun get-request (server connection request-line)
  (labels ((parse-request-header (line)
             (declare (string line))
             (log2:trace "<<< ~a" line)
             (let* ((index (position #\space line))
                    (key (intern (string-downcase
                                  (subseq line 0 (1- index)))
                                 :keyword)) ; strip trailing double-colon
                    (value (subseq line (1+ index))))
               (cons key
                     (case key
                       (:|Cookie|
                         (loop
                            :for name-value-pair :in (cl-utilities:split-sequence #\; value)
                            :for (name value) = (cl-utilities:split-sequence #\= name-value-pair)
                            :collect (list (intern (string-left-trim " " (the simple-string name)) :keyword)
                                           value)))
                       (t
                        value)))))
           (parse-url-query (string)
             ;; Fixme: handle escaped chars =,& ?
             (loop
                :for pair :in (cl-utilities:split-sequence #\& string)
                :for sep = (position #\= pair)
                :collect (list (subseq pair 0 sep)
                               (when sep (subseq pair (1+ sep))))))
           (create-request (line)
             (log2:trace "<<< ~a" line)
             (destructuring-bind (&optional (method "INVALID") (query-path "") (http-version "HTTP/1.1"))
                 (cl-utilities:split-sequence #\space line)
               (let* ((url (net.uri:parse-uri query-path))
                      (protocol (etypecase connection
                                  (mbedtls:ssl-stream :https)
                                  (mbedtls:plain-stream :http)))
                      (request
                       (ecase (intern (string-upcase method) :keyword)
                         (:get (make-http-get :protocol protocol :port (mbedtls:server-port server)))
                         (:post (make-http-post :protocol protocol :port (mbedtls:server-port server)))
                         (:put (make-http-put :protocol protocol :port (mbedtls:server-port server)))
                         (:options (make-http-options :protocol protocol :port (mbedtls:server-port server))))))
                 (setf (path request)
                       (cdr (puri:uri-parsed-path  url)))
                 (setf (parameters request)
                       (parse-url-query (puri:uri-query url)))
                 (setf (http-version request)
                       http-version)
                 request))))
    ;; Read the request method and create an instance of the proper request subclass.
    (let ((request
           (create-request request-line)))
      ;; Read header lines
      (setf (headers request)
            (loop
               :for line = (mbedtls:get-line connection)
               :while (and line
                           ;; Headers are terminated by an empty line
                           (> (length line) 0))
               :collect (parse-request-header line)))
      (typecase request
        (http-post
         ;; Read request body
         ;;   This should be deferred, but remember to clear the input if the connection is kept alive. 
         (get-body connection request)))
      request)))

(defun get-body (stream request)
  (let* ((content-type (http-content-type request))
         (content-length (when (http-content-length request)
                           (parse-integer (http-content-length request))))
         (transfer-encoding (http-transfer-encoding request))
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
    (setf (body request) body)))

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
  (write-message-line stream "HTTP/1.0 ~a ~a"
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
  (log2:trace ">>> ~a" (subseq body 0 (min 20 (length body))))
  (etypecase body
    (string
      (mbedtls:write-to-stream stream (string-to-octets body)))
    (vector
      (mbedtls:write-to-stream stream body)))
  (mbedtls:write-to-stream stream #(13 10))
  (mbedtls:write-to-stream stream #(13 10)))

(defun write-headers (headers stream)
  (loop
     :for (header . value) :in headers
     :do (case header
           (:|Set-Cookie|
             (write-message-line stream "~a: ~{~{~a=~a~}~^; ~}" header value))
           (:|Cookie|
             (error "Use verb Set-Cookie instead of Cookie"))
           (t
             (write-message-line stream "~a: ~a" header value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Aux functions

(defun strip-name (s)
  (string-trim '(#\space #\linefeed #\return) s))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
