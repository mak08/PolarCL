;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Created        22/03/2000 11:15:16
;;; Last Modified  <michael 2021-05-02 15:59:09>

(defsystem "polarcl"
  :description "Web server based on mbedtls"
  :default-component-class cl-source-file.cl
  :depends-on ("cl-mbedtls" "zlib" "puri" "regex" "cl-base64" "cffi" "local-time")
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "macros")
               (:file "http-date")
               (:file "octet-streams")
               ;; (:file "headers")
               (:file "messages")
               (:file "cookies")

               (:file "server")
               ;; (:file "client")
               (:file "mime-types")
               (:file "handlers")
               (:file "registered-functions")
               (:file "configuration")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
