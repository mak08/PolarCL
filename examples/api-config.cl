;;; Runnable companion to Manual.md. Load after (ql:quickload "polarcl").
(defpackage :polarcl-api-demo
  (:use :cl))
(in-package :polarcl-api-demo)

(defun health (server handler request response)
  (declare (ignore server handler request))
  (setf (polarcl:http-header response :|Content-Type|) "text/plain"
        (polarcl:http-body response) "ok"))

(defun greet (handler request response &key (|name| "world"))
  (declare (ignore handler request))
  (setf (polarcl:http-header response :|Content-Type|) "text/plain")
  (format nil "Hello, ~a!" |name|))

(defun echo-body (handler request response)
  (declare (ignore handler))
  (setf (polarcl:http-header response :|Content-Type|) "text/plain"
        (polarcl:http-body response) (polarcl:http-body request)))

(defun public-function (handler request registered-function)
  (declare (ignore handler request registered-function))
  t)

(polarcl:register-function "greet" :symbol 'greet :authorizer #'public-function)
(polarcl:register-function "echo" :symbol 'echo-body :authorizer #'public-function)

(polarcl:handle :request (:path "/health")
                :handler (:dynamic #'health :authentication nil))
(polarcl:handle :request (:path "/api/greet")
                :handler (:query-function t :authentication nil))
(polarcl:handle :request (:method :post :path "/api/echo")
                :handler (:request-function t :authentication nil))

;; Resolve this asset relative to this configuration file, not the Lisp cwd.
(polarcl:handle :request (:path "/index.html")
                :handler (:static (namestring (merge-pathnames "index.html" *load-truename*))
                          :authentication nil))
(polarcl:redirect :from (:path "/") :to (:path "/index.html"))

;; Start last, once functions and routes have been registered.
(polarcl:server :protocol :http :hostname "127.0.0.1" :port "8080"
                :mt-method :pooled :max-handlers 4)
