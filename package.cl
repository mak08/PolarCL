;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Handling XMLHttpRequest-style http requests
;;; Author         Michael Kappert 2013
;;; Last Modified  <michael 2017-02-24 00:11:43>

(defpackage "POLARCL"
  (:use "COMMON-LISP"
        "CFFI"
        "BABEL"
        "RDPARSE"
        "URI"
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
  (:export "STOP-SERVER"
           "START-SERVER"
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
           "SET-COOKIE"
           "HTTP-BODY"
           "HTTP-HEADER"
           "STATUS-CODE"
           "STATUS-TEXT"
           "PATH"
           "PARAMETERS"
           "EXTRA-PARAMETERS"
           "HTTP-VERSION"
           "SPLIT-STRING"
           "JSON"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
