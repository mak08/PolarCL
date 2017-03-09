;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Predefined request handlers
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-03-08 20:19:58>

(defpackage polarcl-test
  (:use :cl :polarcl))

(in-package polarcl-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start servers on HTTP and HTTPS port

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Users

(register-user "admin" "admin" "admin")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File locations & handlers

#+()
(register-redirector
 ;; How do we avoid HTTPS redirection so we can call the admin function on the HTTP server?
 :filter (create-filter 'prefix-filter :path "" :port '("8080"))
 :redirector (create-redirector :scheme "HTTPS" :port '"4443"))


(register-redirector
 :filter (create-filter 'prefix-filter :path "/moved")
 :redirector (create-redirector :path "/here"))

(register-handler
 :filter (create-filter 'prefix-filter :method '(:get) :path "")
 :handler (create-handler 'dynhtml-handler
                          :contentfn (lambda (server handler request response)
                                       (declare (ignore server handler request response))
                                       "<!DOCTYPE html><html><body><b><em>Oops - you hit the fallback page.</em></b></body><html>")))

(register-handler
 :filter (create-filter 'exact-filter :method '(:get) :path "/favicon.ico")
 :handler (create-handler 'file-handler
                          :rootdir (namestring
                                    (merge-pathnames
                                     (make-pathname :directory '(:relative "data") :name "favicon" :type "ico")
                                     (asdf:system-source-directory :polarcl)))))

(register-handler 
 :filter (create-filter 'exact-filter :method '(:get) :path "/index.html")
 :handler (create-handler 'dynhtml-handler
                          :contentfn (lambda (server handler request response)
                                       (declare (ignore server handler request response))
                                       "<!DOCTYPE html><html><body><b><em>Thank you for calling!</em></b></body><html>")))

(register-handler 
 :filter (create-filter 'exact-filter :method '(:get) :path "/test.html")
 :handler (create-handler 'file-handler
                          :authentication nil
                          :rootdir (namestring
                                    (merge-pathnames
                                     (make-pathname :directory '(:relative "data") :name "index" :type "html")
                                     (asdf:system-source-directory :polarcl)))))

(register-redirector
 :filter (create-filter 'prefix-filter :method '(:get) :path "/")
 :redirector (create-redirector :path "/index.html"))

(register-handler
 :filter (create-filter 'exact-filter :method '(:get) :path "/quit")
 :handler (create-handler 'dynhtml-handler
                          :realm "admin"
                          :authentication :basic
                          :contentfn (lambda (server handler request response)
                                       (declare (ignore response))
                                       (if (string= (http-authenticated-user handler request)
                                                    "admin")
                                           (progn (stop-server server)
                                                  "<!DOCTYPE html><html><body><b><em>Goodby</em></b></body><html>")
                                           "<!DOCTYPE html><html><body><b><em>Declined.</em></b></body><html>"))))

(register-handler
 :filter (create-filter 'prefix-filter :method '(:get) :path "/content/lib")
 :handler (create-handler 'file-handler :rootdir "/home/michael/html5/lib"))

(register-handler
 :filter (create-filter 'prefix-filter :method '(:get) :path "/content/pages")
 :handler (create-handler 'file-handler :rootdir "/home/michael/html5/pages"))

(register-handler
 :filter (create-filter 'prefix-filter :method '(:get) :path "/content/pages/scgnweather")
 :handler (create-handler 'file-handler :rootdir "/home/michael/html5/pages/scgnweather" :authentication nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2a. Functions on request, response

(register-handler
 :filter (create-filter 'prefix-filter :method '(:get) :path "/content/rfun")
 :handler (create-handler 'rfunc-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2b. Functions using  URL query as keyword args

(register-handler
 :filter (create-filter 'prefix-filter :method '(:get) :path "/content/qfun")
 :handler (create-handler 'qfunc-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Examples

(defun |test-rfun| (handler request response)
  (declare (ignore handler))
  (setf (http-body response)
        (format () "Function TEST called with parameters ~s"
                (parameters request))))

(register-function '|test-rfun|)

(defun |test-qfun| (handler request response &rest args &key &allow-other-keys)
  (declare (ignore handler request response))
  (format () "Function test-qfun called with args ~s" args))

(register-function '|test-qfun|)

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
