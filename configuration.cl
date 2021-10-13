;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2021-10-13 23:15:40>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ToDo
;;;
;;;   Administration:
;;;   - Refined configuration loading
;;;   - Starting & stopping


(in-package polarcl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load configuration

(defun load-configuration (path)
  "Terminate any running servers and bring up servers according to the configuration in $path.
The configuration file should contain SERVER, USER, REDIRECT and HANDLE statements.
It is loaded by LOAD. In particular, *package* and other globals are bound as usual."
  (cond
    ((probe-file path)
     (handler-case
         (progn
           (stop-all-servers)
           (sleep (1+ *loop-delay*))
           (reset)
           (load path))
       (error (e)
         (log2:info "~a" e))))
    (t
     (error "File ~a not found" path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defmacro server (&rest args &key protocol &allow-other-keys)
  (let ((class (ecase protocol
                 (:http 'http-server)
                 (:https 'https-server))))
    (remf args :protocol)
    `(let ((server (make-instance ',class ,@args)))
       (log2:info "Adding SERVER: ~a" server)
       (run-http-server server))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defmacro user (&key username realm password)
  `(let ()
     (log2:info "Adding USER ~a@~a" ,username ,realm)
     (register-user ,username ,realm ,password)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defmacro redirect (&key from to)
  (destructuring-bind (&key (method :get) scheme host port regex path (prefix ""))
      from
    (let*
        ((methods (if (atom method) (list method) method))
         (protocols (if (listp scheme) scheme (list scheme)))
         (filter
          (cond
            (regex
             `(create-filter 'regex-filter :method ',methods
                                           :protocol ',protocols
                                           :host ,host
                                           :port ',port
                                           :path (cl-ppcre:create-scanner ,regex)))
            (path
             `(create-filter 'exact-filter :method ',methods
                                           :protocol ',protocols
                                           :host ,host
                                           :port ',port
                                           :path ,path))
            (prefix
             `(create-filter 'prefix-filter :method ',methods
                                            :protocol ',protocols
                                            :host ,host
                                            :port ',port
                                            :path ,prefix))
            (t
             `(create-filter 'regex-filter :method ',methods
                                           :protocol ',protocols
                                           :host ,host
                                           :port ',port
                                           :path (cl-ppcre:create-scanner ".*")))))
         (redirector
          `(create-redirector ,@to)))
      `(let ()
         (log2:info "Adding REDIRECTOR ~a ~a" ,filter ,redirector) 
         (register-redirector :filter ,filter :redirector ,redirector)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defmacro handle (&key request handler)
  (let ((handler
         (cond
           ((atom handler)
            (list handler t))
           (t
            handler))))
  (destructuring-bind (&key host (method :get) path prefix)
      request
    (destructuring-bind (&key static dynamic query-function request-function realm (authentication :basic) (authorizer #'default-authorizer))
        handler
      (let*
          ((methods (if (atom method) (list method) method))
           (filter
            (cond
              (path
               `(create-filter 'exact-filter :host ,host :method ',methods :path ,path))
              (prefix
               `(create-filter 'prefix-filter :host ,host :method ',methods :path ,prefix))
              (t
               `(create-filter 'exact-filter :host ,host :method ',methods :path ,path))))
           (handler
            (cond
              (static
               `(create-handler 'file-handler :rootdir ,static :authentication ,authentication :authorizer ,authorizer :realm ,realm))
              (dynamic
               `(create-handler 'dynhtml-handler :contentfn ,dynamic :authentication ,authentication :authorizer ,authorizer :realm ,realm))
              (query-function
               `(create-handler 'qfunc-handler :authentication ,authentication :authorizer ,authorizer :realm ,realm))
              (request-function
               `(create-handler 'rfunc-handler :authentication ,authentication :authorizer ,authorizer :realm ,realm)))))
        `(let ()
           (log2:info "Adding HANDLER ~a ~a" ,filter ,handler)
           (register-handler :filter ,filter :handler ,handler)))))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
