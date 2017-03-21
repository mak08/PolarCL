;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2017-03-21 22:05:47>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logging settings

(setf (log2:log-level "mbedtls") log2:+info+)
(setf (log2:log-level "mbedtls:accept") log2:+info+)
(setf (log2:log-level "mbedtls:mbedtls-net-accept") log2:+info+)
(setf (log2:log-level "mbedtls:create-config") log2:+info+)
(setf (log2:log-level "mbedtls:create-ssl-env") log2:+info+)
(setf (log2:log-level "mbedtls:mbedtls-error-text") log2:+info+)

(setf (log2:log-level "polarcl") log2:+info+)
(setf (log2:log-level "polarcl:handler-thread") log2:+info+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -------
;;; Servers
;;; -------

;;; Start one server on port 8080 
(server :hostname "localhost"
        :protocol :http
        :mt-method :ondemand
        :port "8080"
        :max-handlers 10)

;;; Start another server on port 4443

(server :hostname "localhost"
        :protocol :https
        :port "4443"
        :max-handlers 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -----
;;; Users
;;; -----

(user :username "admin" :realm "admin" :password "admin")
(user :username "guest" :realm "localhost" :password "guest")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -----------
;;; Redirection
;;; -----------
;;; Redirection directive is not yet implemented

;;; Redirect any HTTP request on port 8080 to HTTPS port 4443.
#|
(redirect
 :from (:scheme "HTTP" :port "8080")
 :to (:scheme "HTTPS" :port "4443"))
|#

;;; Redirect any request that does not specify a file (ie, path ends with a folder)
;;; to the file "index.html" at the same path
(redirect
 :from (:regex ".*/")
 :to (:path "index.html"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ----------------
;;; Request handlers
;;; ----------------

;;; A :path filter must match the request path exactly.
;;; Use :prefix to serve a directory.
;;; This :static handler serves just one file, index.html.
;;; It uses the no authentication (default is :basic).

(handle
 :request (:path "/index.html")
 :handler (:static "/var/www/html/index.html" :authentication nil))

;;; A :static handler, serving any files below the path :prefix /content/pages.
;;; The real path is found by replacing the path prefix with the static root path.
;;; Login as 'guest' required
(handle
 :request (:method :get
           :prefix "/content/pages")
 :handler (:static "/var/www/html/michael" :realm "localhost"))

;;; A :dynamic handler calls the specified function on the matched request and
;;; and a default "OK" response. Login as 'admin' required.
(handle
 :request (:method :get
           :path "/quit")
 :handler (:dynamic (lambda (server handler request response)
                                       (declare (ignore response))
                                       (if (string= (http-authenticated-user handler request)
                                                    "admin")
                                           (progn (stop-all-servers)
                                                  "<!DOCTYPE html><html><body><b><em>Goodby</em></b></body><html>")
                                           "<!DOCTYPE html><html><body><b><em>Not authorized.</em></b></body><html>"))
                    :realm "admin"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
