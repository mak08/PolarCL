;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Handling XMLHttpRequest-style http requests
;;; Author         Michael Kappert 2013
;;; Last Modified  <michael 2021-05-10 00:04:56>

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
  (:export "*CONTENT-ROOT*"
           "LOAD-CONFIGURATION"
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
           "HTTP-CREDENTIALS"
           "HTTP-AUTHENTICATED-USER"
           "AUTHENTICATE"
           "DECLINING-AUTHORIZER"
           "DEFAULT-AUTHORIZER"
           "FUNCTION-AUTHORIZER"
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
           "JSON"
           "FORMAT-DATE-IMF"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
