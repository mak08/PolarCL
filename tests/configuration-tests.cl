;;; Run after loading PolarCL: (polarcl-tests:run-tests)
(defpackage :polarcl-tests
  (:use :cl)
  (:export :run-tests))
(in-package :polarcl-tests)

;; Exercise the real dispatcher without opening sockets.
(defclass test-connection () ())
(defmethod mbedtls:peer ((connection test-connection)) "127.0.0.1")
(defclass test-request (polarcl:http-get) ())
(defmethod polarcl::http-protocol ((request test-request)) :http)

(defun run-tests ()
  (let ((polarcl::*servers* nil)
        (polarcl::*redirectors* nil)
        (polarcl::*handlers* nil)
        (polarcl::*exact-handlers* nil)
        (polarcl::*prefix-handlers* nil)
        (polarcl::*regex-handlers* nil)
        (polarcl::*users* (make-hash-table :test #'equal))
        (polarcl::*registered-functions-ht* (make-hash-table :test #'equal))
        (connection (make-instance 'test-connection))
        (server (make-instance 'polarcl:http-server))
        (calls 0))
    (labels ((request ()
               (make-instance 'test-request :connection connection :port "8080"
                 :path '("api" "test") :parameters nil
                 :headers (list (make-instance 'polarcl:http-header
                                  :name :|host| :value "localhost:8080"))))
             (dispatch (handler)
               (polarcl:register-handler
                :filter (polarcl:create-filter 'polarcl:exact-filter
                          :path "/api/test")
                :handler handler)
               (polarcl::handle-request server connection (request)))
             (check-status (expected response)
               (assert (equal expected (polarcl:status-code response))))
             (allow (&rest args) (declare (ignore args)) t)
             (deny (&rest args) (declare (ignore args)) nil)
             (fail (&rest args) (declare (ignore args)) (error "callback failure"))
             (called (&rest args) (declare (ignore args)) (incf calls) "ok"))
      ;; Seed every registry; reset must remove dispatchable entries, too.
      (dolist (type '(polarcl:exact-filter polarcl:prefix-filter polarcl:regex-filter))
        (polarcl:register-handler :filter (polarcl:create-filter type :path "/api/test")
                                  :handler (polarcl:create-handler 'polarcl:rfunc-handler)))
      (polarcl:register-redirector
       :filter (polarcl:create-filter 'polarcl:exact-filter :path "/old")
       :redirector (polarcl:create-redirector :path "/new"))
      (polarcl:register-user "reader" "test" "password")
      (polarcl:register-function "test" :symbol #'called)
      (setf polarcl::*servers* (list server))
      (polarcl::reset)
      (assert (every #'null (list polarcl::*servers* polarcl::*redirectors*
                                 polarcl::*handlers* polarcl::*exact-handlers*
                                 polarcl::*prefix-handlers* polarcl::*regex-handlers*)))
      (assert (zerop (hash-table-count polarcl::*users*)))
      (assert (zerop (hash-table-count polarcl::*registered-functions-ht*)))
      (check-status "404" (polarcl::handle-request server connection (request)))
      (polarcl::reset) ; repeatable
      ;; Handler authorization: rejection is 401, callback errors are 500.
      (dolist (authorizer (list #'polarcl:default-authorizer #'deny #'fail))
        (check-status (if (eq authorizer #'fail) "500" "401")
          (dispatch (polarcl:create-handler 'polarcl:dynhtml-handler
                      :realm "test" :authentication :basic
                      :authorizer authorizer :contentfn #'called))))
      (assert (zerop calls))
      ;; Registered authorization must work identically for both callback styles.
      (dolist (type '(polarcl:rfunc-handler polarcl:qfunc-handler))
        (let ((handler (polarcl:create-handler type :realm "test")))
          (dolist (authorizer (list #'polarcl:function-authorizer #'deny #'fail))
            (polarcl:register-function "test" :symbol #'called :authorizer authorizer)
            (let ((response (dispatch handler)))
              (check-status (if (eq authorizer #'fail) "500" "401") response)
              (unless (eq authorizer #'fail)
                (assert (equal "Basic realm=test"
                               (polarcl:http-header response :|WWW-Authenticate|))))))
          (assert (zerop calls))
          (polarcl:register-function "test" :symbol #'fail :authorizer #'allow)
          (check-status "500" (dispatch handler))
          ;; Even an authorization condition raised by application code is an error.
          (polarcl:register-function "test"
            :symbol (lambda (&rest args)
                      (declare (ignore args))
                      (error 'polarcl::authorization-error :realm "test"))
            :authorizer #'allow)
          (check-status "500" (dispatch handler))
          (polarcl:register-function "test"
            :symbol (lambda (handler request response)
                      (declare (ignore handler request))
                      (setf (polarcl:status-code response) "403"
                            (polarcl:status-text response) "Forbidden"
                            (polarcl:http-body response) "application denial")
                      "application denial")
            :authorizer #'allow)
          (let ((response (dispatch handler)))
            (check-status "403" response)
            (assert (equal "Forbidden" (polarcl:status-text response)))
            (assert (equal "application denial" (polarcl:http-body response))))))
      (check-status "403"
        (dispatch (polarcl:create-handler 'polarcl:dynhtml-handler
                    :contentfn (lambda (server handler request response)
                                 (declare (ignore server handler request))
                                 (setf (polarcl:status-code response) "403")))))
      (polarcl:register-function "test" :symbol #'called :authorizer #'allow)
      (check-status "200" (dispatch (polarcl:create-handler 'polarcl:qfunc-handler)))
      (assert (= calls 1))))
  (format t "~&PolarCL configuration/authorization tests passed.~%")
  t)
