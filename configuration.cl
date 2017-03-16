;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2017-03-16 00:46:20>

;;; ToDo:
;;;   Don't load configurations with LOAD.
;;;   Provide a load function that resets the *handlers* list etc.

(in-package polarcl)

(defmacro server (&rest args &key protocol &allow-other-keys)
  (let ((class (ecase protocol
                 (:http 'http-server)
                 (:https 'https-server))))
    (remf args :protocol)
    `(let ((server (make-instance ',class ,@args)))
       (run-http-server server))))

(defmacro user (&key username realm password)
  `(register-user ,username ,realm ,password))

(defmacro redirect (&key from to)
  (destructuring-bind (&key (method :get) scheme host port regex path (prefix ""))
      from
    (let*
        ((methods (if (atom method) (list method) method))
         (protocols (if (atom scheme) (list scheme) scheme))
         (filter
          (cond
            (regex
             `(create-filter 'regex-filter :method ',methods :protocol ',protocols :host ,host :path (regex:make-regex ,regex)))
            (path
             `(create-filter 'exact-filter :method ',methods :protocol ',protocols :host ,host :path ,path))
            (prefix
             `(create-filter 'prefix-filter :method ',methods :protocol ',protocols :host ,host :path ,prefix))))
         (redirector
          `(create-redirector ,@to)))
      `(register-redirector :filter ,filter :redirector ,redirector))))

(defmacro handle (&key request handler)
  (destructuring-bind (&key (method :get) path prefix)
      request
    (destructuring-bind (&key static dynamic realm (authentication :basic))
        handler
      (let*
          ((methods (if (atom method) (list method) method))
           (filter
             (cond
               (path
                `(create-filter 'exact-filter :method ',methods :path ,path))
               (prefix
                `(create-filter 'prefix-filter :method ',methods :path ,prefix))))
            (handler
             (cond
               (static
                `(create-handler 'file-handler :rootdir ,static :authentication ,authentication :realm ,realm))
               (dynamic
                `(create-handler 'dynhtml-handler :contentfn ,dynamic :authentication ,authentication :realm ,realm)))))
        `(register-handler :filter ,filter :handler ,handler)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
