;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2017-03-08 21:27:18>

(use-package "POLARCL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Servers

(server :hostname "bitweide.de"
       :port "8080"
       :max-threads 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Users

(user "admin" "admin" "admin")
(user "guest" "bitweide.de" "guest")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -----------------
;;; Redirection Rules
;;; -----------------
;;;
;;; - Forgo matching of path patterns for now, until we have a proper regex matcher
;;;   Example: (redirect "http://*:80" "https://*:443") 
;;;
;;; - Determine sequence of redirections if several rules match
;;; - Combine redirection


;;; Redirect any HTTP request on port 80 to HTTPS port 443.
(redirect
 :from (:protocol "HTTP" :port "80")
 :to (:protocol "HTTPS" :port "443"))

;;; Redirect any request that does not specify a file (ie, path ends with a folder)
;;; to the file "index.html" at the same path
(redirect
 :from (:path #'is-folder)
 :to (:path "index.html"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -----------------
;;; Handling requests
;;; -----------------
;;;

;;; A :path must match the request path exactly.
;;; The :static handler serves just one file, index.html.
;;; It uses the default authentication (:basic).
(handle
 :request (:path "index.html")
 :handler (:static "/var/www/html/polarcl/index.html"))

;;; A :static handler, serving any files below the path :prefix /content/pages.
;;; The real path is found by replacing the path prefix with the static root path.
(handle
 :request (:method :get
           :prefix "/content/pages")
 :handler (:static "/var/www/html/polarcl"))

;;; A :dynamic handler calls the specified function on the matched request and
;;; and a default "OK" response. The caller must authenticate to the "admin" realm and
;;; log on as "admin".
(handle
 :request (:method :get
           :path "/quit")
 :handler (:dynamic (lambda (handler request response)
                       (if (string= (http-authenticated-user handler request)
                                    "admin")
                           (stop-server)
                           (setf (http-body response) "<!DOCTYPE html><html><body><b><em>Request was declined.</em></b></body><html>"))
                       "Goodby")
           :realm :admin))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
