;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2021-05-24 15:33:28>

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


(defmacro nnmember (item listform &key (test #'string=))
  (let ((listvar (gensym "list-"))
        (resultvar (gensym "result-")))
    `(let* ((,listvar ,listform)
            (,resultvar (or (null ,listvar)
                            (member ,item ,listvar :test ,test))))
       (log2:trace "Checking ~a in ~a ==> ~a" ,item ,listvar ,resultvar)
       ,resultvar)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
