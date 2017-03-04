;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Handling XMLHttpRequest-style http requests
;;; Author         Michael Kappert 2013
;;; Last Modified  <michael 2015-11-08 21:25:20>

(defpackage "URI"
  (:use "COMMON-LISP" "RDPARSE")
  (:export "PARSE-URL"
           "URL-SCHEME"
           "URL-AUTHORITY"
           "URL-HOST"
           "URL-PORT"
           "URL-PATH"
           "URL-QUERY"
           "URL-FRAGMENT"
           "AUTHORITY-USERINFO"
           "AUTHORITY-HOST"
           "AUTHORITY-PORT"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
