;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Created        22/03/2000 11:15:16
;;; Last Modified  <michael 2017-03-08 00:12:46>

(defsystem "polarcl"
  :description "Web server based on mbedtls"
  :default-component-class cl-source-file.cl
  :depends-on ("cl-mbedtls" "zlib" "puri" "regex" "cl-base64" "cffi" "local-time")
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "octet-streams")
               ;; (:file "headers")
               (:file "messages")
               (:file "server")
               ;; (:file "client")
               (:file "mime-types")
               (:file "handlers")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
