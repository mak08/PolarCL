;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Created        22/03/2000 11:15:16
;;; Last Modified  <michael 2023-01-28 18:45:26>

(defsystem "polarcl"
  :description "Web server based on mbedtls"
  :default-component-class cl-source-file.cl
  :depends-on ("bordeaux-threads" "puri" "cl-base64" "cffi" "local-time" "makros" "cl-mbedtls" "zlib" "cl-ppcre")
  :in-order-to ((test-op (test-op "polarcl/tests")))
  :serial t
  :components ((:file "package")
               (:file "ctypes")
               (:file "common")
               (:file "macros")
               (:file "http-date")
               (:file "octet-streams")
               ;; (:file "headers")
               (:file "messages")
               (:file "cookies")
               (:file "authentication")

               (:file "server")
               ;; (:file "client")
               (:file "mime-types")
               (:file "handlers")
               (:file "websocket-codec")
               (:file "websocket-compression")
               (:file "websocket-json")
               (:file "websockets")
               (:file "registered-functions")
               (:file "configuration")))

(defsystem "polarcl/tests"
  :description "PolarCL configuration, authorization and response regression tests"
  :default-component-class cl-source-file.cl
  :depends-on ("polarcl")
  :serial t
  :components ((:file "tests/configuration-tests")
               (:file "tests/authentication-tests")
               (:file "tests/head-response-tests")
               (:file "tests/request-charset-tests")
               (:file "tests/put-request-tests")
               (:file "tests/websocket-tests")
               (:file "tests/websocket-compression-tests"))
  :perform (test-op (operation system)
             (declare (ignore operation system))
             (uiop:symbol-call :polarcl-tests :run-tests)
             (uiop:symbol-call :polarcl-auth-tests :run-tests)
             (uiop:symbol-call :polarcl-head-tests :run-tests)
             (uiop:symbol-call :polarcl-charset-tests :run-tests)
             (uiop:symbol-call :polarcl-put-tests :run-tests)
             (uiop:symbol-call :polarcl-websocket-tests :run-tests)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
