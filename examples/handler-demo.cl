;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Predefined request handlers
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2021-06-01 21:10:04>

(defpackage polarcl-test
  (:use :cl :polarcl))

(in-package polarcl-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start servers on HTTP and HTTPS port

(server :hostname "aguas-13" ;; Hostname binds to the WLAN/LAN interface! 
        :protocol :http
        :mt-method :ondemand
        ;; :mt-method :pooled
        :port "8080"
        :max-handlers 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Users

(user :username "guest" :realm "virtualhelm" :password "_guest_01")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Redirection
(redirect
 :from (:regex ".*/")
 :to (:path "index.html"))


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
