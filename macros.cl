;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2019-06-02 10:54:36>

(in-package "POLARCL")

(defmacro condlet (&rest branches)
  (destructuring-bind (branch . rest)
      branches
    (cond ((listp (car branch))
           (destructuring-bind ((var form) &rest body)
               branch
             (if (null rest)
                 `(let ((,var ,form))
                    (when ,var ,@body))
                 `(let ((,var ,form))
                    (if ,var (progn ,@body)
                        (condlet ,@rest))))))
          (t
           (destructuring-bind (tag &rest body)
               branch
             (assert (eq tag t))
             (assert (null rest))
             `(progn ,@body))))))


(defmacro nnmember (item listform &key (test 'string=))
  (let ((listvar (gensym "list-"))
        (resultvar (gensym "result-")))
    `(let* ((,listvar ,listform)
            (,resultvar (or (null ,listvar)
                            (member ,item ,listvar :test ',test))))
       (log2:trace "Checking ~a in ~a ==> ~a" ,item ,listvar ,resultvar)
       ,resultvar)))

(defmacro with-func-from-path ((fsym request) &body body)
  `(destructuring-bind (fname &rest namespace)
       (reverse (path ,request))
     (destructuring-bind (&optional package name)
         (cl-utilities:split-sequence #\: fname)
       (log2:debug "Searching function ~a:~a" (string-upcase package) name )
       ;; Function names are case sensitive!
       (let ((,fsym (find-symbol name (string-upcase package))))
         (unless ,fsym
           (log2:warning "Unknown function ~s" ,fsym)
           (error "Function ~a:~a does not exist" package name))
         (unless (member ,fsym *registered-functions*)
           (log2:warning "Unregistered function ~s" ,fsym)
           (error "Function ~s is not registered" ,fsym))
         (log2:debug "Executing ~a" ,fsym)
         ,@body))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
