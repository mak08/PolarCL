;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Created        22/03/2000 11:15:16
;;; Last Modified  <michael 2021-06-09 21:21:37>

(defsystem "polarcl"
  :description "Web server based on mbedtls"
  :default-component-class cl-source-file.cl
  :depends-on ("bordeaux-threads" "puri" "cl-base64" "cffi" "local-time" "makros" "cl-mbedtls" "zlib" "regex")
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
