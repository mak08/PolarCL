;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Handling XMLHttpRequest-style http requests
;;; Author         Michael Kappert 2013
;;; Last Modified  <michael 2022-01-30 12:21:53>

(defpackage "POLARCL"
  (:use "COMMON-LISP"
        "CFFI"
        "BABEL"
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
           "PRINCIPAL" "MAKE-PRINCIPAL" "PRINCIPAL-ID" "PRINCIPAL-SOURCE" "PRINCIPAL-ATTRIBUTES"
           "AUTHENTICATION-CONTEXT" "AUTHENTICATION-CONTEXT-PRINCIPAL" "AUTHENTICATION-CONTEXT-SCHEME"
           "REQUEST-AUTHENTICATION" "REQUEST-PRINCIPAL"
           "AUTHENTICATION-POLICY-REALM" "AUTHENTICATION-POLICY-SCHEME"
           "REGISTER-AUTHENTICATION-PROVIDER" "AUTHENTICATION-SUCCESS" "AUTHENTICATION-REJECTED"
           "CREDENTIALS-USERNAME" "CREDENTIALS-PASSWORD" "CREDENTIALS-TOKEN"
           "REGISTER-TOKEN" "REVOKE-TOKEN" "REGISTER-DIGEST-USER"
           "REGISTER-USER"
           "REGISTER-HANDLER"
           "REGISTER-REDIRECTOR"
           "REGISTER-FUNCTION"
           "CREATE-FILTER"
           "EXACT-FILTER"
           "PREFIX-FILTER"
           "REGEX-FILTER"
           "CREATE-REDIRECTOR"
           "DYNHTML-HANDLER"
           "FILE-HANDLER"
           "RFUNC-HANDLER"
           "QFUNC-HANDLER"
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
           "GET-MIME-FOR-EXTENSION"
           "FORMAT-DATE-IMF"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
