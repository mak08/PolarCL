;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Handling XMLHttpRequest-style http requests
;;; Author         Michael Kappert 2013
;;; Last Modified  <michael 2018-12-05 20:50:01>

(defpackage "POLARCL"
  (:use "COMMON-LISP"
        "CFFI"
        "BABEL"
        "RDPARSE"
        #+sbcl "SB-MOP"
        #+ccl "CCL"
        #+clisp "CLOS"
        )
  (:shadow
   ;; CCL
   #+ccl "DEFCALLBACK"
   ;; BABEL
     "MAKE-EXTERNAL-FORMAT"
     "EXTERNAL-FORMAT"
     "LIST-CHARACTER-ENCODINGS"
     "STRING-SIZE-IN-OCTETS"
     ;; USOCKET
     "SOCKET-CONNECT"
     "SOCKET-ERROR")
  (:export "LOAD-CONFIGURATION"
           "SERVER"
           "USER"
           "REDIRECT"
           "HANDLE"
           "RUN-HTTP-SERVER"
           "STOP-SERVER"
           "STOP-ALL-SERVERS"
           "HTTP-SERVER"
           "HTTP-GET"
           "HTTP-REQUEST"
           "REGISTER-USER"
           "REGISTER-HANDLER"
           "REGISTER-REDIRECTOR"
           "REGISTER-FUNCTION"
           "CREATE-FILTER"
           "EXACT-FILTER"
           "PREFIX-FILTER"
           "REGEX-FILTER"
           "CREATE-HANDLER"
           "CREATE-REDIRECTOR"
           "DYNHTML-HANDLER"
           "FILE-HANDLER"
           "RFUNC-HANDLER"
           "QFUNC-HANDLER"
           "HTTP-AUTHENTICATED-USER"
           "MAKE-HTTP-GET"
           "HEADERS"
           "FIELD-NAME"
           "FIELD-VALUE"
           "GET-COOKIE"
           "SET-COOKIE"
           "COOKIE-NAME"
           "COOKIE-VALUE"
           "COOKIE-OPTIONS"
           "HTTP-BODY"
           "HTTP-HEADER"
           "STATUS-CODE"
           "STATUS-TEXT"
           "PATH"
           "PARAMETERS"
           "PARSE-URL-QUERY"
           "HTTP-VERSION"
           "JSON"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
